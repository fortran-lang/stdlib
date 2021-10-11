program test_linalg_matrix_property_checks
  
  use stdlib_error, only: check
  use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
  use stdlib_linalg, only: is_square ,is_diagonal, is_symmetric, &
       is_skew_symmetric, is_hermitian, is_triangular, is_hessenberg
  
  implicit none
  
  real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
  real(dp), parameter :: dptol = 1000 * epsilon(1._dp)
  real(qp), parameter :: qptol = 1000 * epsilon(1._qp)

  logical :: warn
  
  ! whether calls to check issue a warning
  ! or stop execution
  warn = .false.

  !
  ! is_square
  !
  call test_is_square_rsp
  call test_is_square_rdp
  call test_is_square_rqp

  call test_is_square_csp
  call test_is_square_cdp
  call test_is_square_cqp

  call test_is_square_int8
  call test_is_square_int16
  call test_is_square_int32
  call test_is_square_int64

  !
  ! is_diagonal
  !
  call test_is_diagonal_rsp
  call test_is_diagonal_rdp
  call test_is_diagonal_rqp

  call test_is_diagonal_csp
  call test_is_diagonal_cdp
  call test_is_diagonal_cqp

  call test_is_diagonal_int8
  call test_is_diagonal_int16
  call test_is_diagonal_int32
  call test_is_diagonal_int64

  !
  ! is_symmetric
  !
  call test_is_symmetric_rsp
  call test_is_symmetric_rdp
  call test_is_symmetric_rqp

  call test_is_symmetric_csp
  call test_is_symmetric_cdp
  call test_is_symmetric_cqp

  call test_is_symmetric_int8
  call test_is_symmetric_int16
  call test_is_symmetric_int32
  call test_is_symmetric_int64

  !
  ! is_skew_symmetric
  !
  call test_is_skew_symmetric_rsp
  call test_is_skew_symmetric_rdp
  call test_is_skew_symmetric_rqp

  call test_is_skew_symmetric_csp
  call test_is_skew_symmetric_cdp
  call test_is_skew_symmetric_cqp

  call test_is_skew_symmetric_int8
  call test_is_skew_symmetric_int16
  call test_is_skew_symmetric_int32
  call test_is_skew_symmetric_int64

  !
  ! is_hermitian
  !
  call test_is_hermitian_rsp
  call test_is_hermitian_rdp
  call test_is_hermitian_rqp

  call test_is_hermitian_csp
  call test_is_hermitian_cdp
  call test_is_hermitian_cqp

  call test_is_hermitian_int8
  call test_is_hermitian_int16
  call test_is_hermitian_int32
  call test_is_hermitian_int64

  !
  ! is_triangular
  !
  call test_is_triangular_rsp
  call test_is_triangular_rdp
  call test_is_triangular_rqp

  call test_is_triangular_csp
  call test_is_triangular_cdp
  call test_is_triangular_cqp

  call test_is_triangular_int8
  call test_is_triangular_int16
  call test_is_triangular_int32
  call test_is_triangular_int64

  !
  ! is_hessenberg
  !
  call test_is_hessenberg_rsp
  call test_is_hessenberg_rdp
  call test_is_hessenberg_rqp

  call test_is_hessenberg_csp
  call test_is_hessenberg_cdp
  call test_is_hessenberg_cqp

  call test_is_hessenberg_int8
  call test_is_hessenberg_int16
  call test_is_hessenberg_int32
  call test_is_hessenberg_int64

contains

  subroutine test_is_square_rsp
    real(sp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_rsp" 
    A_true = reshape([1.,2.,3.,4.],[2,2])
    A_false = reshape([1.,2.,3.,4.,5.,6.],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_rsp

  subroutine test_is_square_rdp
    real(dp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_rdp" 
    A_true = reshape([1.,2.,3.,4.],[2,2])
    A_false = reshape([1.,2.,3.,4.,5.,6.],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_rdp

  subroutine test_is_square_rqp
    real(qp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_rqp" 
    A_true = reshape([1.,2.,3.,4.],[2,2])
    A_false = reshape([1.,2.,3.,4.,5.,6.],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_rqp

  subroutine test_is_square_csp
    complex(sp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_csp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.),cmplx(4.,1.)],[2,2])
    A_false = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         cmplx(4.,1.),cmplx(5.,0.),cmplx(6.,1.)],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_csp

  subroutine test_is_square_cdp
    complex(dp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_cdp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.),cmplx(4.,1.)],[2,2])
    A_false = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         cmplx(4.,1.),cmplx(5.,0.),cmplx(6.,1.)],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_cdp

  subroutine test_is_square_cqp
    complex(qp) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_cqp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.),cmplx(4.,1.)],[2,2])
    A_false = reshape([cmplx(1.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         cmplx(4.,1.),cmplx(5.,0.),cmplx(6.,1.)],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_cqp

  subroutine test_is_square_int8
    integer(int8) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_int8" 
    A_true = reshape([1,2,3,4],[2,2])
    A_false = reshape([1,2,3,4,5,6],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_int8

  subroutine test_is_square_int16
    integer(int16) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_int16" 
    A_true = reshape([1,2,3,4],[2,2])
    A_false = reshape([1,2,3,4,5,6],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_int16

  subroutine test_is_square_int32
    integer(int32) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_int32" 
    A_true = reshape([1,2,3,4],[2,2])
    A_false = reshape([1,2,3,4,5,6],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_int32

  subroutine test_is_square_int64
    integer(int64) :: A_true(2,2), A_false(2,3)
    write(*,*) "test_is_square_int64" 
    A_true = reshape([1,2,3,4],[2,2])
    A_false = reshape([1,2,3,4,5,6],[2,3])
    call check(is_square(A_true), &
         msg="is_square(A_true) failed.",warn=warn)
    call check((.not. is_square(A_false)), &
         msg="(.not. is_square(A_false)) failed.",warn=warn)
  end subroutine test_is_square_int64


  subroutine test_is_diagonal_rsp
    real(sp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    real(sp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    real(sp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_rsp" 
    A_true_s = reshape([1.,0.,0.,4.],[2,2])
    A_false_s = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf = reshape([1.,0.,0.,4.,0.,0.],[2,3])
    A_false_sf = reshape([1.,0.,3.,4.,0.,0.],[2,3])
    A_true_ts = reshape([1.,0.,0.,0.,5.,0.],[3,2])
    A_false_ts = reshape([1.,0.,0.,0.,5.,6.],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_rsp

  subroutine test_is_diagonal_rdp
    real(dp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    real(dp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    real(dp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_rdp" 
    A_true_s = reshape([1.,0.,0.,4.],[2,2])
    A_false_s = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf = reshape([1.,0.,0.,4.,0.,0.],[2,3])
    A_false_sf = reshape([1.,0.,3.,4.,0.,0.],[2,3])
    A_true_ts = reshape([1.,0.,0.,0.,5.,0.],[3,2])
    A_false_ts = reshape([1.,0.,0.,0.,5.,6.],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_rdp

  subroutine test_is_diagonal_rqp
    real(qp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    real(qp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    real(qp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_rqp" 
    A_true_s = reshape([1.,0.,0.,4.],[2,2])
    A_false_s = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf = reshape([1.,0.,0.,4.,0.,0.],[2,3])
    A_false_sf = reshape([1.,0.,3.,4.,0.,0.],[2,3])
    A_true_ts = reshape([1.,0.,0.,0.,5.,0.],[3,2])
    A_false_ts = reshape([1.,0.,0.,0.,5.,6.],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_rqp

  subroutine test_is_diagonal_csp
    complex(sp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    complex(sp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    complex(sp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_csp"
    A_true_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.)],[2,2]) 
    A_false_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_true_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,1.)],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_csp

  subroutine test_is_diagonal_cdp
    complex(dp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    complex(dp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    complex(dp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_cdp"
    A_true_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.)],[2,2]) 
    A_false_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_true_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,1.)],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_cdp

  subroutine test_is_diagonal_cqp
    complex(qp) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    complex(qp) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    complex(qp) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_cqp"
    A_true_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.)],[2,2]) 
    A_false_s = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_true_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,1.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,1.)],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_cqp

  subroutine test_is_diagonal_int8
    integer(int8) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    integer(int8) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    integer(int8) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_int8" 
    A_true_s = reshape([1,0,0,4],[2,2])
    A_false_s = reshape([1,0,3,4],[2,2])
    A_true_sf = reshape([1,0,0,4,0,0],[2,3])
    A_false_sf = reshape([1,0,3,4,0,0],[2,3])
    A_true_ts = reshape([1,0,0,0,5,0],[3,2])
    A_false_ts = reshape([1,0,0,0,5,6],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_int8

  subroutine test_is_diagonal_int16
    integer(int16) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    integer(int16) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    integer(int16) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_int16" 
    A_true_s = reshape([1,0,0,4],[2,2])
    A_false_s = reshape([1,0,3,4],[2,2])
    A_true_sf = reshape([1,0,0,4,0,0],[2,3])
    A_false_sf = reshape([1,0,3,4,0,0],[2,3])
    A_true_ts = reshape([1,0,0,0,5,0],[3,2])
    A_false_ts = reshape([1,0,0,0,5,6],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_int16

  subroutine test_is_diagonal_int32
    integer(int32) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    integer(int32) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    integer(int32) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_int32" 
    A_true_s = reshape([1,0,0,4],[2,2])
    A_false_s = reshape([1,0,3,4],[2,2])
    A_true_sf = reshape([1,0,0,4,0,0],[2,3])
    A_false_sf = reshape([1,0,3,4,0,0],[2,3])
    A_true_ts = reshape([1,0,0,0,5,0],[3,2])
    A_false_ts = reshape([1,0,0,0,5,6],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_int32

  subroutine test_is_diagonal_int64
    integer(int64) :: A_true_s(2,2), A_false_s(2,2) !square matrices
    integer(int64) :: A_true_sf(2,3), A_false_sf(2,3) !short and fat matrices
    integer(int64) :: A_true_ts(3,2), A_false_ts(3,2) !tall and skinny matrices
    write(*,*) "test_is_diagonal_int64" 
    A_true_s = reshape([1,0,0,4],[2,2])
    A_false_s = reshape([1,0,3,4],[2,2])
    A_true_sf = reshape([1,0,0,4,0,0],[2,3])
    A_false_sf = reshape([1,0,3,4,0,0],[2,3])
    A_true_ts = reshape([1,0,0,0,5,0],[3,2])
    A_false_ts = reshape([1,0,0,0,5,6],[3,2])
    call check(is_diagonal(A_true_s), &
         msg="is_diagonal(A_true_s) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_s)), &
         msg="(.not. is_diagonal(A_false_s)) failed.",warn=warn)
    call check(is_diagonal(A_true_sf), &
         msg="is_diagonal(A_true_sf) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_sf)), &
         msg="(.not. is_diagonal(A_false_sf)) failed.",warn=warn)
    call check(is_diagonal(A_true_ts), &
         msg="is_diagonal(A_true_ts) failed.",warn=warn)
    call check((.not. is_diagonal(A_false_ts)), &
         msg="(.not. is_diagonal(A_false_ts)) failed.",warn=warn)
  end subroutine test_is_diagonal_int64


  subroutine test_is_symmetric_rsp
    real(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_rsp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_rsp

  subroutine test_is_symmetric_rdp
    real(dp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_rdp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_rdp

  subroutine test_is_symmetric_rqp
    real(qp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_rqp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_rqp

  subroutine test_is_symmetric_csp
    complex(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_csp" 
    A_true = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(2.,1.),cmplx(4.,1.)],[2,2])
    A_false_1 = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_false_2 = reshape([cmplx(1.,1.),cmplx(2.,1.),cmplx(3.,1.), &
         cmplx(2.,1.),cmplx(5.,1.),cmplx(6.,2.)],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_csp

  subroutine test_is_symmetric_cdp
    complex(dp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_cdp" 
    A_true = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(2.,1.),cmplx(4.,1.)],[2,2])
    A_false_1 = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_false_2 = reshape([cmplx(1.,1.),cmplx(2.,1.),cmplx(3.,1.), &
         cmplx(2.,1.),cmplx(5.,1.),cmplx(6.,2.)],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_cdp

  subroutine test_is_symmetric_cqp
    complex(qp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_cqp" 
    A_true = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(2.,1.),cmplx(4.,1.)],[2,2])
    A_false_1 = reshape([cmplx(1.,1.),cmplx(2.,1.), &
         cmplx(3.,1.),cmplx(4.,1.)],[2,2])
    A_false_2 = reshape([cmplx(1.,1.),cmplx(2.,1.),cmplx(3.,1.), &
         cmplx(2.,1.),cmplx(5.,1.),cmplx(6.,2.)],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_cqp

  subroutine test_is_symmetric_int8
    integer(int8) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_int8" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_int8
  
  subroutine test_is_symmetric_int16
    integer(int16) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_int16" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_int16

  subroutine test_is_symmetric_int32
    integer(int32) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_int32" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_int32

  subroutine test_is_symmetric_int64
    integer(int64) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_symmetric_int64" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_symmetric(A_true), &
         msg="is_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_1)), &
         msg="(.not. is_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_symmetric(A_false_2)), &
         msg="(.not. is_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_symmetric_int64


  subroutine test_is_skew_symmetric_rsp
    real(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_rsp" 
    A_true = reshape([0.,2.,-2.,0.],[2,2])
    A_false_1 = reshape([0.,2.,-3.,0.],[2,2])
    A_false_2 = reshape([0.,2.,3.,-2.,0.,6.],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_rsp

  subroutine test_is_skew_symmetric_rdp
    real(dp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_rdp" 
    A_true = reshape([0.,2.,-2.,0.],[2,2])
    A_false_1 = reshape([0.,2.,-3.,0.],[2,2])
    A_false_2 = reshape([0.,2.,3.,-2.,0.,6.],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_rdp

  subroutine test_is_skew_symmetric_rqp
    real(qp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_rqp" 
    A_true = reshape([0.,2.,-2.,0.],[2,2])
    A_false_1 = reshape([0.,2.,-3.,0.],[2,2])
    A_false_2 = reshape([0.,2.,3.,-2.,0.,6.],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_rqp

  subroutine test_is_skew_symmetric_csp
    complex(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_csp" 
    A_true = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(2.,1.),cmplx(0.,0.)],[2,2])
    A_false_1 = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(3.,1.),cmplx(0.,0.)],[2,2])
    A_false_2 = reshape([cmplx(0.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         -cmplx(2.,1.),cmplx(0.,0.),cmplx(6.,0.)],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_csp

  subroutine test_is_skew_symmetric_cdp
    complex(dp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_cdp" 
    A_true = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(2.,1.),cmplx(0.,0.)],[2,2])
    A_false_1 = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(3.,1.),cmplx(0.,0.)],[2,2])
    A_false_2 = reshape([cmplx(0.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         -cmplx(2.,1.),cmplx(0.,0.),cmplx(6.,0.)],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_cdp

  subroutine test_is_skew_symmetric_cqp
    complex(qp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_cqp" 
    A_true = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(2.,1.),cmplx(0.,0.)],[2,2])
    A_false_1 = reshape([cmplx(0.,0.),cmplx(2.,1.), &
         -cmplx(3.,1.),cmplx(0.,0.)],[2,2])
    A_false_2 = reshape([cmplx(0.,0.),cmplx(2.,1.),cmplx(3.,0.), &
         -cmplx(2.,1.),cmplx(0.,0.),cmplx(6.,0.)],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_cqp

  subroutine test_is_skew_symmetric_int8
    integer(int8) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_int8" 
    A_true = reshape([0,2,-2,0],[2,2])
    A_false_1 = reshape([0,2,-3,0],[2,2])
    A_false_2 = reshape([0,2,3,-2,0,6],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_int8
  
  subroutine test_is_skew_symmetric_int16
    integer(int16) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_int16" 
    A_true = reshape([0,2,-2,0],[2,2])
    A_false_1 = reshape([0,2,-3,0],[2,2])
    A_false_2 = reshape([0,2,3,-2,0,6],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_int16

  subroutine test_is_skew_symmetric_int32
    integer(int32) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_int32" 
    A_true = reshape([0,2,-2,0],[2,2])
    A_false_1 = reshape([0,2,-3,0],[2,2])
    A_false_2 = reshape([0,2,3,-2,0,6],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_int32

  subroutine test_is_skew_symmetric_int64
    integer(int64) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_skew_symmetric_int64" 
    A_true = reshape([0,2,-2,0],[2,2])
    A_false_1 = reshape([0,2,-3,0],[2,2])
    A_false_2 = reshape([0,2,3,-2,0,6],[3,2]) !nonsquare matrix
    call check(is_skew_symmetric(A_true), &
         msg="is_skew_symmetric(A_true) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_1)), &
         msg="(.not. is_skew_symmetric(A_false_1)) failed.",warn=warn)
    call check((.not. is_skew_symmetric(A_false_2)), &
         msg="(.not. is_skew_symmetric(A_false_2)) failed.",warn=warn)
  end subroutine test_is_skew_symmetric_int64


  subroutine test_is_hermitian_rsp
    real(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_rsp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_rsp

  subroutine test_is_hermitian_rdp
    real(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_rdp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_rdp

  subroutine test_is_hermitian_rqp
    real(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_rqp" 
    A_true = reshape([1.,2.,2.,4.],[2,2])
    A_false_1 = reshape([1.,2.,3.,4.],[2,2])
    A_false_2 = reshape([1.,2.,3.,2.,5.,6.],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_rqp

  subroutine test_is_hermitian_csp
    complex(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_csp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(2.,1.),cmplx(4.,0.)],[2,2])
    A_false_1 = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_2 = reshape([cmplx(1.,0.),cmplx(2.,-1.),cmplx(3.,-1.), &
         cmplx(2.,1.),cmplx(5.,0.),cmplx(6.,-1.)],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_csp

  subroutine test_is_hermitian_cdp
    complex(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_cdp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(2.,1.),cmplx(4.,0.)],[2,2])
    A_false_1 = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_2 = reshape([cmplx(1.,0.),cmplx(2.,-1.),cmplx(3.,-1.), &
         cmplx(2.,1.),cmplx(5.,0.),cmplx(6.,-1.)],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_cdp

  subroutine test_is_hermitian_cqp
    complex(sp) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_cqp" 
    A_true = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(2.,1.),cmplx(4.,0.)],[2,2])
    A_false_1 = reshape([cmplx(1.,0.),cmplx(2.,-1.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_2 = reshape([cmplx(1.,0.),cmplx(2.,-1.),cmplx(3.,-1.), &
         cmplx(2.,1.),cmplx(5.,0.),cmplx(6.,-1.)],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_cqp

  subroutine test_is_hermitian_int8
    integer(int8) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_int8" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_int8

  subroutine test_is_hermitian_int16
    integer(int16) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_int16" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_int16

  subroutine test_is_hermitian_int32
    integer(int32) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_int32" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_int32

  subroutine test_is_hermitian_int64
    integer(int64) :: A_true(2,2), A_false_1(2,2), A_false_2(3,2)
    write(*,*) "test_is_hermitian_int64" 
    A_true = reshape([1,2,2,4],[2,2])
    A_false_1 = reshape([1,2,3,4],[2,2])
    A_false_2 = reshape([1,2,3,2,5,6],[3,2]) !nonsquare matrix
    call check(is_hermitian(A_true), &
         msg="is_hermitian(A_true) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_1)), &
         msg="(.not. is_hermitian(A_false_1)) failed.",warn=warn)
    call check((.not. is_hermitian(A_false_2)), &
         msg="(.not. is_hermitian(A_false_2)) failed.",warn=warn)
  end subroutine test_is_hermitian_int64


  subroutine test_is_triangular_rsp
    real(sp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    real(sp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    real(sp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    real(sp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    real(sp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    real(sp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_rsp" 
    !upper triangular
    A_true_s_u = reshape([1.,0.,3.,4.],[2,2])
    A_false_s_u = reshape([1.,2.,0.,4.],[2,2])
    A_true_sf_u = reshape([1.,0.,3.,4.,0.,6.],[2,3])
    A_false_sf_u = reshape([1.,2.,3.,4.,0.,6.],[2,3])
    A_true_ts_u = reshape([1.,0.,0.,4.,5.,0.],[3,2])
    A_false_ts_u = reshape([1.,0.,0.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1.,2.,0.,4.],[2,2])
    A_false_s_l = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf_l = reshape([1.,2.,0.,4.,0.,0.],[2,3])
    A_false_sf_l = reshape([1.,2.,3.,4.,0.,0.],[2,3])
    A_true_ts_l = reshape([1.,2.,3.,0.,5.,6.],[3,2])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_rsp

  subroutine test_is_triangular_rdp
    real(dp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    real(dp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    real(dp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    real(dp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    real(dp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    real(dp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_rdp" 
    !upper triangular
    A_true_s_u = reshape([1.,0.,3.,4.],[2,2])
    A_false_s_u = reshape([1.,2.,0.,4.],[2,2])
    A_true_sf_u = reshape([1.,0.,3.,4.,0.,6.],[2,3])
    A_false_sf_u = reshape([1.,2.,3.,4.,0.,6.],[2,3])
    A_true_ts_u = reshape([1.,0.,0.,4.,5.,0.],[3,2])
    A_false_ts_u = reshape([1.,0.,0.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1.,2.,0.,4.],[2,2])
    A_false_s_l = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf_l = reshape([1.,2.,0.,4.,0.,0.],[2,3])
    A_false_sf_l = reshape([1.,2.,3.,4.,0.,0.],[2,3])
    A_true_ts_l = reshape([1.,2.,3.,0.,5.,6.],[3,2])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_rdp

  subroutine test_is_triangular_rqp
    real(qp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    real(qp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    real(qp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    real(qp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    real(qp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    real(qp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_rqp" 
    !upper triangular
    A_true_s_u = reshape([1.,0.,3.,4.],[2,2])
    A_false_s_u = reshape([1.,2.,0.,4.],[2,2])
    A_true_sf_u = reshape([1.,0.,3.,4.,0.,6.],[2,3])
    A_false_sf_u = reshape([1.,2.,3.,4.,0.,6.],[2,3])
    A_true_ts_u = reshape([1.,0.,0.,4.,5.,0.],[3,2])
    A_false_ts_u = reshape([1.,0.,0.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1.,2.,0.,4.],[2,2])
    A_false_s_l = reshape([1.,0.,3.,4.],[2,2])
    A_true_sf_l = reshape([1.,2.,0.,4.,0.,0.],[2,3])
    A_false_sf_l = reshape([1.,2.,3.,4.,0.,0.],[2,3])
    A_true_ts_l = reshape([1.,2.,3.,0.,5.,6.],[3,2])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_rqp

  subroutine test_is_triangular_csp
    complex(sp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    complex(sp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    complex(sp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    complex(sp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    complex(sp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    complex(sp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_csp" 
    !upper triangular
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_csp

  subroutine test_is_triangular_cdp
    complex(dp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    complex(dp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    complex(dp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    complex(dp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    complex(dp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    complex(dp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_cdp" 
    !upper triangular
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_cdp

  subroutine test_is_triangular_cqp
    complex(qp) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    complex(qp) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    complex(qp) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    complex(qp) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    complex(qp) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    complex(qp) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_cqp" 
    !upper triangular
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(6.,0.)],[2,3])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(0.,0.)],[3,2])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.)],[2,2])
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(0.,0.), &
         cmplx(3.,1.),cmplx(4.,0.)],[2,2])
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(0.,0.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.), &
         cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(0.,0.),cmplx(0.,0.)],[2,3])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(0.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.)],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_cqp

  subroutine test_is_triangular_int8
    integer(int8) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    integer(int8) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    integer(int8) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    integer(int8) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    integer(int8) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    integer(int8) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_int8" 
    !upper triangular
    A_true_s_u = reshape([1,0,3,4],[2,2])
    A_false_s_u = reshape([1,2,0,4],[2,2])
    A_true_sf_u = reshape([1,0,3,4,0,6],[2,3])
    A_false_sf_u = reshape([1,2,3,4,0,6],[2,3])
    A_true_ts_u = reshape([1,0,0,4,5,0],[3,2])
    A_false_ts_u = reshape([1,0,0,4,5,6],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1,2,0,4],[2,2])
    A_false_s_l = reshape([1,0,3,4],[2,2])
    A_true_sf_l = reshape([1,2,0,4,0,0],[2,3])
    A_false_sf_l = reshape([1,2,3,4,0,0],[2,3])
    A_true_ts_l = reshape([1,2,3,0,5,6],[3,2])
    A_false_ts_l = reshape([1,2,3,4,5,6],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_int8

  subroutine test_is_triangular_int16
    integer(int16) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    integer(int16) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    integer(int16) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    integer(int16) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    integer(int16) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    integer(int16) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_int16" 
    !upper triangular
    A_true_s_u = reshape([1,0,3,4],[2,2])
    A_false_s_u = reshape([1,2,0,4],[2,2])
    A_true_sf_u = reshape([1,0,3,4,0,6],[2,3])
    A_false_sf_u = reshape([1,2,3,4,0,6],[2,3])
    A_true_ts_u = reshape([1,0,0,4,5,0],[3,2])
    A_false_ts_u = reshape([1,0,0,4,5,6],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1,2,0,4],[2,2])
    A_false_s_l = reshape([1,0,3,4],[2,2])
    A_true_sf_l = reshape([1,2,0,4,0,0],[2,3])
    A_false_sf_l = reshape([1,2,3,4,0,0],[2,3])
    A_true_ts_l = reshape([1,2,3,0,5,6],[3,2])
    A_false_ts_l = reshape([1,2,3,4,5,6],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_int16

  subroutine test_is_triangular_int32
    integer(int32) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    integer(int32) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    integer(int32) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    integer(int32) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    integer(int32) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    integer(int32) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_int32" 
    !upper triangular
    A_true_s_u = reshape([1,0,3,4],[2,2])
    A_false_s_u = reshape([1,2,0,4],[2,2])
    A_true_sf_u = reshape([1,0,3,4,0,6],[2,3])
    A_false_sf_u = reshape([1,2,3,4,0,6],[2,3])
    A_true_ts_u = reshape([1,0,0,4,5,0],[3,2])
    A_false_ts_u = reshape([1,0,0,4,5,6],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1,2,0,4],[2,2])
    A_false_s_l = reshape([1,0,3,4],[2,2])
    A_true_sf_l = reshape([1,2,0,4,0,0],[2,3])
    A_false_sf_l = reshape([1,2,3,4,0,0],[2,3])
    A_true_ts_l = reshape([1,2,3,0,5,6],[3,2])
    A_false_ts_l = reshape([1,2,3,4,5,6],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_int32

  subroutine test_is_triangular_int64
    integer(int64) :: A_true_s_u(2,2), A_false_s_u(2,2) !square matrices (upper triangular)
    integer(int64) :: A_true_sf_u(2,3), A_false_sf_u(2,3) !short and fat matrices
    integer(int64) :: A_true_ts_u(3,2), A_false_ts_u(3,2) !tall and skinny matrices
    integer(int64) :: A_true_s_l(2,2), A_false_s_l(2,2) !square matrices (lower triangular)
    integer(int64) :: A_true_sf_l(2,3), A_false_sf_l(2,3) !short and fat matrices
    integer(int64) :: A_true_ts_l(3,2), A_false_ts_l(3,2) !tall and skinny matrices
    write(*,*) "test_is_triangular_int64" 
    !upper triangular
    A_true_s_u = reshape([1,0,3,4],[2,2])
    A_false_s_u = reshape([1,2,0,4],[2,2])
    A_true_sf_u = reshape([1,0,3,4,0,6],[2,3])
    A_false_sf_u = reshape([1,2,3,4,0,6],[2,3])
    A_true_ts_u = reshape([1,0,0,4,5,0],[3,2])
    A_false_ts_u = reshape([1,0,0,4,5,6],[3,2])
    call check(is_triangular(A_true_s_u,'u'), &
         msg="is_triangular(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_u,'u')), &
         msg="(.not. is_triangular(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_u,'u'), &
         msg="is_triangular(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_u,'u')), &
         msg="(.not. is_triangular(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_u,'u'), &
         msg="is_triangular(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_u,'u')), &
         msg="(.not. is_triangular(A_false_ts_u,'u')) failed.",warn=warn)
    !lower triangular
    A_true_s_l = reshape([1,2,0,4],[2,2])
    A_false_s_l = reshape([1,0,3,4],[2,2])
    A_true_sf_l = reshape([1,2,0,4,0,0],[2,3])
    A_false_sf_l = reshape([1,2,3,4,0,0],[2,3])
    A_true_ts_l = reshape([1,2,3,0,5,6],[3,2])
    A_false_ts_l = reshape([1,2,3,4,5,6],[3,2])
    call check(is_triangular(A_true_s_l,'l'), &
         msg="is_triangular(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_s_l,'l')), &
         msg="(.not. is_triangular(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_sf_l,'l'), &
         msg="is_triangular(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_sf_l,'l')), &
         msg="(.not. is_triangular(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_triangular(A_true_ts_l,'l'), &
         msg="is_triangular(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_triangular(A_false_ts_l,'l')), &
         msg="(.not. is_triangular(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_triangular_int64
  

  subroutine test_is_hessenberg_rsp
    real(sp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    real(sp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    real(sp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    real(sp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    real(sp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    real(sp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_rsp" 
    !upper hessenberg
    A_true_s_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.],[3,3]) 
    A_false_s_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_false_sf_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_true_ts_u = reshape([1.,2.,0.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    A_false_ts_u = reshape([1.,2.,3.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.],[3,3]) 
    A_false_s_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,0.,12.],[3,4])
    A_false_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,11.,12.],[3,4])
    A_true_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,0.,10.,11.,12.],[4,3])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_rsp

  subroutine test_is_hessenberg_rdp
    real(dp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    real(dp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    real(dp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    real(dp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    real(dp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    real(dp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_rdp" 
    !upper hessenberg
    A_true_s_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.],[3,3]) 
    A_false_s_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_false_sf_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_true_ts_u = reshape([1.,2.,0.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    A_false_ts_u = reshape([1.,2.,3.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.],[3,3]) 
    A_false_s_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,0.,12.],[3,4])
    A_false_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,11.,12.],[3,4])
    A_true_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,0.,10.,11.,12.],[4,3])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_rdp

  subroutine test_is_hessenberg_rqp
    real(qp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    real(qp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    real(qp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    real(qp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    real(qp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    real(qp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_rqp" 
    !upper hessenberg
    A_true_s_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.],[3,3]) 
    A_false_s_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_u = reshape([1.,2.,0.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_false_sf_u = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[3,4])
    A_true_ts_u = reshape([1.,2.,0.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    A_false_ts_u = reshape([1.,2.,3.,0.,5.,6.,7.,0.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.],[3,3]) 
    A_false_s_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.],[3,3])
    A_true_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,0.,12.],[3,4])
    A_false_sf_l = reshape([1.,2.,3.,4.,5.,6.,0.,8.,9.,0.,11.,12.],[3,4])
    A_true_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,0.,10.,11.,12.],[4,3])
    A_false_ts_l = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_rqp

  subroutine test_is_hessenberg_csp
    complex(sp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    complex(sp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    complex(sp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    complex(sp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    complex(sp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    complex(sp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_csp" 
    !upper hessenberg
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(0.,0.),cmplx(12.,0.)],[3,4])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(0.,0.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_csp

  subroutine test_is_hessenberg_cdp
    complex(dp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    complex(dp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    complex(dp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    complex(dp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    complex(dp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    complex(dp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_cdp" 
    !upper hessenberg
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(0.,0.),cmplx(12.,0.)],[3,4])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(0.,0.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_cdp

  subroutine test_is_hessenberg_cqp
    complex(qp) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    complex(qp) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    complex(qp) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    complex(qp) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    complex(qp) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    complex(qp) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_cqp" 
    !upper hessenberg
    A_true_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_false_sf_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(0.,0.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_u = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(0.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(0.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_false_s_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(7.,1.),cmplx(8.,0.),cmplx(9.,1.)],[3,3]) 
    A_true_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(0.,0.),cmplx(12.,0.)],[3,4])
    A_false_sf_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.), &
         cmplx(4.,0.),cmplx(5.,1.),cmplx(6.,0.), &
         cmplx(0.,0.),cmplx(8.,0.),cmplx(9.,1.), &
         cmplx(0.,0.),cmplx(11.,1.),cmplx(12.,0.)],[3,4])
    A_true_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(0.,0.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    A_false_ts_l = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(3.,1.),cmplx(4.,0.), &
         cmplx(5.,1.),cmplx(6.,0.),cmplx(7.,1.),cmplx(8.,0.), &
         cmplx(9.,1.),cmplx(10.,0.),cmplx(11.,1.),cmplx(12.,0.)],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_cqp

  subroutine test_is_hessenberg_int8
    integer(int8) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    integer(int8) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    integer(int8) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    integer(int8) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    integer(int8) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    integer(int8) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_int8" 
    !upper hessenberg
    A_true_s_u = reshape([1,2,0,4,5,6,7,8,9],[3,3]) 
    A_false_s_u = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_u = reshape([1,2,0,4,5,6,7,8,9,10,11,12],[3,4])
    A_false_sf_u = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[3,4])
    A_true_ts_u = reshape([1,2,0,0,5,6,7,0,9,10,11,12],[4,3])
    A_false_ts_u = reshape([1,2,3,0,5,6,7,0,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1,2,3,4,5,6,0,8,9],[3,3]) 
    A_false_s_l = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,0,12],[3,4])
    A_false_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,11,12],[3,4])
    A_true_ts_l = reshape([1,2,3,4,5,6,7,8,0,10,11,12],[4,3])
    A_false_ts_l = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_int8

  subroutine test_is_hessenberg_int16
    integer(int16) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    integer(int16) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    integer(int16) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    integer(int16) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    integer(int16) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    integer(int16) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_int16" 
    !upper hessenberg
    A_true_s_u = reshape([1,2,0,4,5,6,7,8,9],[3,3]) 
    A_false_s_u = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_u = reshape([1,2,0,4,5,6,7,8,9,10,11,12],[3,4])
    A_false_sf_u = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[3,4])
    A_true_ts_u = reshape([1,2,0,0,5,6,7,0,9,10,11,12],[4,3])
    A_false_ts_u = reshape([1,2,3,0,5,6,7,0,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1,2,3,4,5,6,0,8,9],[3,3]) 
    A_false_s_l = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,0,12],[3,4])
    A_false_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,11,12],[3,4])
    A_true_ts_l = reshape([1,2,3,4,5,6,7,8,0,10,11,12],[4,3])
    A_false_ts_l = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_int16

  subroutine test_is_hessenberg_int32
    integer(int32) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    integer(int32) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    integer(int32) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    integer(int32) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    integer(int32) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    integer(int32) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_int32" 
    !upper hessenberg
    A_true_s_u = reshape([1,2,0,4,5,6,7,8,9],[3,3]) 
    A_false_s_u = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_u = reshape([1,2,0,4,5,6,7,8,9,10,11,12],[3,4])
    A_false_sf_u = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[3,4])
    A_true_ts_u = reshape([1,2,0,0,5,6,7,0,9,10,11,12],[4,3])
    A_false_ts_u = reshape([1,2,3,0,5,6,7,0,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1,2,3,4,5,6,0,8,9],[3,3]) 
    A_false_s_l = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,0,12],[3,4])
    A_false_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,11,12],[3,4])
    A_true_ts_l = reshape([1,2,3,4,5,6,7,8,0,10,11,12],[4,3])
    A_false_ts_l = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_int32

  subroutine test_is_hessenberg_int64
    integer(int64) :: A_true_s_u(3,3), A_false_s_u(3,3) !square matrices (upper hessenberg)
    integer(int64) :: A_true_sf_u(3,4), A_false_sf_u(3,4) !short and fat matrices
    integer(int64) :: A_true_ts_u(4,3), A_false_ts_u(4,3) !tall and skinny matrices
    integer(int64) :: A_true_s_l(3,3), A_false_s_l(3,3) !square matrices (lower hessenberg)
    integer(int64) :: A_true_sf_l(3,4), A_false_sf_l(3,4) !short and fat matrices
    integer(int64) :: A_true_ts_l(4,3), A_false_ts_l(4,3) !tall and skinny matrices
    write(*,*) "test_is_hessenberg_int64" 
    !upper hessenberg
    A_true_s_u = reshape([1,2,0,4,5,6,7,8,9],[3,3]) 
    A_false_s_u = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_u = reshape([1,2,0,4,5,6,7,8,9,10,11,12],[3,4])
    A_false_sf_u = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[3,4])
    A_true_ts_u = reshape([1,2,0,0,5,6,7,0,9,10,11,12],[4,3])
    A_false_ts_u = reshape([1,2,3,0,5,6,7,0,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_u,'u'), &
         msg="is_hessenberg(A_true_s_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_u,'u')), &
         msg="(.not. is_hessenberg(A_false_s_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_u,'u'), &
         msg="is_hessenberg(A_true_sf_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_u,'u')), &
         msg="(.not. is_hessenberg(A_false_sf_u,'u')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_u,'u'), &
         msg="is_hessenberg(A_true_ts_u,'u') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_u,'u')), &
         msg="(.not. is_hessenberg(A_false_ts_u,'u')) failed.",warn=warn)
    !lower hessenberg
    A_true_s_l = reshape([1,2,3,4,5,6,0,8,9],[3,3]) 
    A_false_s_l = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    A_true_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,0,12],[3,4])
    A_false_sf_l = reshape([1,2,3,4,5,6,0,8,9,0,11,12],[3,4])
    A_true_ts_l = reshape([1,2,3,4,5,6,7,8,0,10,11,12],[4,3])
    A_false_ts_l = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
    call check(is_hessenberg(A_true_s_l,'l'), &
         msg="is_hessenberg(A_true_s_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_s_l,'l')), &
         msg="(.not. is_hessenberg(A_false_s_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_sf_l,'l'), &
         msg="is_hessenberg(A_true_sf_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_sf_l,'l')), &
         msg="(.not. is_hessenberg(A_false_sf_l,'l')) failed.",warn=warn)
    call check(is_hessenberg(A_true_ts_l,'l'), &
         msg="is_hessenberg(A_true_ts_l,'l') failed.",warn=warn)
    call check((.not. is_hessenberg(A_false_ts_l,'l')), &
         msg="(.not. is_hessenberg(A_false_ts_l,'l')) failed.",warn=warn)
  end subroutine test_is_hessenberg_int64

end program
