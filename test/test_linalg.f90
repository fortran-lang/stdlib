program test_linalg
  
  use stdlib_error, only: check
  use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
  use stdlib_linalg, only: diag, eye, trace, outer_product
  
  implicit none
  
  real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
  real(dp), parameter :: dptol = 1000 * epsilon(1._dp)
  real(qp), parameter :: qptol = 1000 * epsilon(1._qp)

  logical :: warn
  
  ! whether calls to check issue a warning
  ! or stop execution
  warn = .false.

  !
  ! eye
  !
  call test_eye

  !
  ! diag
  !
  call test_diag_rsp
  call test_diag_rsp_k
  call test_diag_rdp
  call test_diag_rqp

  call test_diag_csp
  call test_diag_cdp
  call test_diag_cqp

  call test_diag_int8
  call test_diag_int16
  call test_diag_int32
  call test_diag_int64

  !
  ! trace
  !
  call test_trace_rsp
  call test_trace_rsp_nonsquare
  call test_trace_rdp
  call test_trace_rdp_nonsquare
  call test_trace_rqp

  call test_trace_csp
  call test_trace_cdp
  call test_trace_cqp

  call test_trace_int8
  call test_trace_int16
  call test_trace_int32
  call test_trace_int64

  !
  ! outer product
  !
  call test_outer_product_rsp
  call test_outer_product_rdp
  call test_outer_product_rqp

  call test_outer_product_csp
  call test_outer_product_cdp
  call test_outer_product_cqp

  call test_outer_product_int8
  call test_outer_product_int16
  call test_outer_product_int32
  call test_outer_product_int64


contains

  subroutine test_eye
    real(sp), allocatable :: rye(:,:)
    complex(sp) :: cye(7,7)
    integer :: i
    write(*,*) "test_eye"

    call check(all(eye(3,3) == diag([(1,i=1,3)])), &
               msg="all(eye(3,3) == diag([(1,i=1,3)])) failed.",warn=warn)
    
    rye = eye(3,4)
    call check(sum(abs(rye(:,1:3) - diag([(1.0_sp,i=1,3)]))) < sptol, &
          msg="sum(abs(rye(:,1:3) - diag([(1.0_sp,i=1,3)]))) < sptol failed", warn=warn)

    call check(all(eye(5) == diag([(1,i=1,5)])), &
          msg="all(eye(5) == diag([(1,i=1,5)] failed.",warn=warn)
    
    rye = eye(6)
    call check(sum(abs(rye - diag([(1.0_sp,i=1,6)]))) < sptol, &
          msg="sum(abs(rye - diag([(1.0_sp,i=1,6)]))) < sptol failed.",warn=warn)

    cye = eye(7)
    call check(abs(trace(cye) - cmplx(7.0_sp,0.0_sp,kind=sp)) < sptol, &
          msg="abs(trace(cye) - cmplx(7.0_sp,0.0_sp,kind=sp)) < sptol failed.",warn=warn)
  end subroutine test_eye

  subroutine test_diag_rsp
    integer, parameter :: n = 3
    real(sp) :: v(n), a(n,n), b(n,n)
    integer :: i,j
    write(*,*) "test_diag_rsp"
    v = [(i,i=1,n)]
    a = diag(v)
    b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
    call check(all(a == b), &
            msg="all(a == b) failed.",warn=warn)

    call check(all(diag(3*a) == 3*v), &
            msg="all(diag(3*a) == 3*v) failed.",warn=warn)
  end subroutine test_diag_rsp

  subroutine test_diag_rsp_k
    integer, parameter :: n = 4
    real(sp) :: a(n,n), b(n,n)
    integer :: i,j
    write(*,*) "test_diag_rsp_k"

    a = diag([(1._sp,i=1,n-1)],-1)
    
    b = reshape([((merge(1,0,i==j+1), i=1,n), j=1,n)], [n,n])

    call check(all(a == b), &
          msg="all(a == b) failed.",warn=warn)

    call check(sum(diag(a,-1)) - (n-1) < sptol, &
          msg="sum(diag(a,-1)) - (n-1) < sptol failed.",warn=warn)

    call check(all(a == transpose(diag([(1._sp,i=1,n-1)],1))), &
          msg="all(a == transpose(diag([(1._sp,i=1,n-1)],1))) failed",warn=warn)

    call random_number(a)
    do i = 1, n
      call check(size(diag(a,i)) == n-i, &
            msg="size(diag(a,i)) == n-i failed.",warn=warn)
    end do
    call check(size(diag(a,n+1)) == 0, &
        msg="size(diag(a,n+1)) == 0 failed.",warn=warn)
  end subroutine test_diag_rsp_k

  subroutine test_diag_rdp
    integer, parameter :: n = 3
    real(dp) :: v(n), a(n,n), b(n,n)
    integer :: i,j
    write(*,*) "test_diag_rdp"
    v = [(i,i=1,n)]
    a = diag(v)
    b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
    call check(all(a == b), &
            msg="all(a == b) failed.",warn=warn)

    call check(all(diag(3*a) == 3*v), &
            msg="all(diag(3*a) == 3*v) failed.",warn=warn)
  end subroutine test_diag_rdp

  subroutine test_diag_rqp
    integer, parameter :: n = 3
    real(qp) :: v(n), a(n,n), b(n,n)
    integer :: i,j
    write(*,*) "test_diag_rqp"
    v = [(i,i=1,n)]
    a = diag(v)
    b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
    call check(all(a == b), &
            msg="all(a == b) failed.", warn=warn)

    call check(all(diag(3*a) == 3*v), &
            msg="all(diag(3*a) == 3*v) failed.", warn=warn)
  end subroutine test_diag_rqp

  subroutine test_diag_csp
    integer, parameter :: n = 3
    complex(sp) :: a(n,n), b(n,n)
    complex(sp), parameter :: i_ = cmplx(0,1,kind=sp)
    integer :: i,j
    write(*,*) "test_diag_csp"
    a = diag([(i,i=1,n)]) + diag([(i_,i=1,n)])
    b = reshape([((merge(i + 1*i_,0*i_,i==j), i=1,n), j=1,n)], [n,n])
    call check(all(a == b), &
            msg="all(a == b) failed.",warn=warn)

    call check(all(abs(real(diag(a)) - [(i,i=1,n)]) < sptol), &
          msg="all(abs(real(diag(a)) - [(i,i=1,n)]) < sptol)", warn=warn)
    call check(all(abs(aimag(diag(a)) - [(1,i=1,n)]) < sptol), &
          msg="all(abs(aimag(diag(a)) - [(1,i=1,n)]) < sptol)", warn=warn)
  end subroutine test_diag_csp

  subroutine test_diag_cdp
    integer, parameter :: n = 3
    complex(dp) :: a(n,n)
    complex(dp), parameter :: i_ = cmplx(0,1,kind=dp)
    write(*,*) "test_diag_cdp"
    a = diag([i_],-2) + diag([i_],2)
    call check(a(3,1) == i_ .and. a(1,3) == i_, &
          msg="a(3,1) == i_ .and. a(1,3) == i_ failed.",warn=warn)
  end subroutine test_diag_cdp

  subroutine test_diag_cqp
    integer, parameter :: n = 3
    complex(qp) :: a(n,n)
    complex(qp), parameter :: i_ = cmplx(0,1,kind=qp)
    write(*,*) "test_diag_cqp"
    a = diag([i_,i_],-1) + diag([i_,i_],1)
    call check(all(diag(a,-1) == i_) .and. all(diag(a,1) == i_), &
          msg="all(diag(a,-1) == i_) .and. all(diag(a,1) == i_) failed.",warn=warn)
  end subroutine test_diag_cqp

  subroutine test_diag_int8
    integer, parameter :: n = 3
    integer(int8), allocatable :: a(:,:)
    integer :: i
    logical, allocatable :: mask(:,:)
    write(*,*) "test_diag_int8"
    a = reshape([(i,i=1,n**2)],[n,n])
    mask = merge(.true.,.false.,eye(n) == 1)
    call check(all(diag(a) == pack(a,mask)), &
          msg="all(diag(a) == pack(a,mask)) failed.", warn=warn)
    call check(all(diag(diag(a)) == merge(a,0_int8,mask)), &
          msg="all(diag(diag(a)) == merge(a,0_int8,mask)) failed.", warn=warn)
  end subroutine test_diag_int8
  subroutine test_diag_int16
    integer, parameter :: n = 4
    integer(int16), allocatable :: a(:,:)
    integer :: i
    logical, allocatable :: mask(:,:)
    write(*,*) "test_diag_int16"
    a = reshape([(i,i=1,n**2)],[n,n])
    mask = merge(.true.,.false.,eye(n) == 1)
    call check(all(diag(a) == pack(a,mask)), &
          msg="all(diag(a) == pack(a,mask))", warn=warn)
    call check(all(diag(diag(a)) == merge(a,0_int16,mask)), &
          msg="all(diag(diag(a)) == merge(a,0_int16,mask)) failed.", warn=warn)
  end subroutine test_diag_int16
  subroutine test_diag_int32
    integer, parameter :: n = 3
    integer(int32) :: a(n,n)
    logical :: mask(n,n)
    integer :: i, j
    write(*,*) "test_diag_int32"
    mask = reshape([((merge(.true.,.false.,i==j+1), i=1,n), j=1,n)], [n,n])
    a = 0
    a = unpack([1_int32,1_int32],mask,a)
    call check(all(diag([1,1],-1) == a), &
          msg="all(diag([1,1],-1) == a) failed.", warn=warn)
    call check(all(diag([1,1],1) == transpose(a)), &
          msg="all(diag([1,1],1) == transpose(a)) failed.", warn=warn)
  end subroutine test_diag_int32
  subroutine test_diag_int64
    integer, parameter :: n = 4
    integer(int64) :: a(n,n), c(0:2*n-1)
    logical :: mask(n,n)
    integer :: i, j
    
    write(*,*) "test_diag_int64"

    mask = reshape([((merge(.true.,.false.,i+1==j), i=1,n), j=1,n)], [n,n])
    a = 0
    a = unpack([1_int64,1_int64,1_int64],mask,a)

    call check(all(diag([1,1,1],1) == a), &
          msg="all(diag([1,1,1],1) == a) failed.", warn=warn)
    call check(all(diag([1,1,1],-1) == transpose(a)), &
          msg="all(diag([1,1,1],-1) == transpose(a)) failed.", warn=warn)


    ! Fill array c with Catalan numbers
    do i = 0, 2*n-1
      c(i) = catalan_number(i)
    end do
    ! Symmetric Hankel matrix filled with Catalan numbers (det(H) = 1)
    do i = 1, n
      do j = 1, n
        a(i,j) = c(i-1 + (j-1))
      end do
    end do
    call check(all(diag(a,-2) == diag(a,2)), &
          msg="all(diag(a,-2) == diag(a,2))", warn=warn)
  end subroutine test_diag_int64




  subroutine test_trace_rsp
    integer, parameter :: n = 5
    real(sp) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_rsp"
    a = reshape([(i,i=1,n**2)],[n,n])
    call check(abs(trace(a) - sum(diag(a))) < sptol, &
          msg="abs(trace(a) - sum(diag(a))) < sptol failed.",warn=warn)
  end subroutine test_trace_rsp

  subroutine test_trace_rsp_nonsquare
    integer, parameter :: n = 4
    real(sp) :: a(n,n+1), ans
    integer :: i
    write(*,*) "test_trace_rsp_nonsquare"

    ! 1   5   9  13  17
    ! 2   6  10  14  18
    ! 3   7  11  15  19
    ! 4   8  12  16  20
    a = reshape([(i,i=1,n*(n+1))],[n,n+1])
    ans = sum([1._sp,6._sp,11._sp,16._sp])

    call check(abs(trace(a) - ans) < sptol, &
          msg="abs(trace(a) - ans) < sptol failed.",warn=warn)
  end subroutine test_trace_rsp_nonsquare

  subroutine test_trace_rdp
    integer, parameter :: n = 4
    real(dp) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_rdp"
    a = reshape([(i,i=1,n**2)],[n,n])
    call check(abs(trace(a) - sum(diag(a))) < dptol, &
          msg="abs(trace(a) - sum(diag(a))) < dptol failed.",warn=warn)
  end subroutine test_trace_rdp

  subroutine test_trace_rdp_nonsquare
    integer, parameter :: n = 4
    real(dp) :: a(n,n-1), ans
    integer :: i
    write(*,*) "test_trace_rdp_nonsquare"

    !  1   25   81
    !  4   36  100  
    !  9   49  121  
    ! 16   64  144  
    a = reshape([(i**2,i=1,n*(n-1))],[n,n-1])
    ans = sum([1._dp,36._dp,121._dp])

    call check(abs(trace(a) - ans) < dptol, &
          msg="abs(trace(a) - ans) < dptol failed.",warn=warn)
  end subroutine test_trace_rdp_nonsquare

  subroutine test_trace_rqp
    integer, parameter :: n = 3
    real(qp) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_rqp"
    a = reshape([(i,i=1,n**2)],[n,n])
    call check(abs(trace(a) - sum(diag(a))) < qptol, &
          msg="abs(trace(a) - sum(diag(a))) < qptol failed.",warn=warn)
  end subroutine test_trace_rqp


  subroutine test_trace_csp
    integer, parameter :: n = 5
    real(sp) :: re(n,n), im(n,n)
    complex(sp) :: a(n,n), b(n,n)
    complex(sp), parameter :: i_ = cmplx(0,1,kind=sp)
    write(*,*) "test_trace_csp"

    call random_number(re)
    call random_number(im)
    a = re + im*i_

    call random_number(re)
    call random_number(im)
    b = re + im*i_

    ! tr(A + B) = tr(A) + tr(B)
    call check(abs(trace(a+b) - (trace(a) + trace(b))) < sptol, &
          msg="abs(trace(a+b) - (trace(a) + trace(b))) < sptol failed.",warn=warn)
  end subroutine test_trace_csp

  subroutine test_trace_cdp
    integer, parameter :: n = 3
    complex(dp) :: a(n,n), ans
    complex(dp), parameter :: i_ = cmplx(0,1,kind=dp)
    integer :: j
    write(*,*) "test_trace_cdp"
    
    a = reshape([(j + (n**2 - (j-1))*i_,j=1,n**2)],[n,n])
    ans = cmplx(15,15,kind=dp) !(1 + 5 + 9) + (9 + 5 + 1)i

    call check(abs(trace(a) - ans) < dptol, &
          msg="abs(trace(a) - ans) < dptol failed.",warn=warn)
  end subroutine test_trace_cdp

  subroutine test_trace_cqp
    integer, parameter :: n = 3
    complex(qp) :: a(n,n)
    complex(qp), parameter :: i_ = cmplx(0,1,kind=qp)
    write(*,*) "test_trace_cqp"
    a = 3*eye(n) + 4*eye(n)*i_ ! pythagorean triple
    call check(abs(trace(a)) - 3*5.0_qp < qptol, &
          msg="abs(trace(a)) - 3*5.0_qp < qptol failed.",warn=warn)
  end subroutine test_trace_cqp


  subroutine test_trace_int8
    integer, parameter :: n = 3
    integer(int8) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_int8"
    a = reshape([(i**2,i=1,n**2)],[n,n])
    call check(trace(a) == (1 + 25 + 81), &
          msg="trace(a) == (1 + 25 + 81) failed.",warn=warn)
  end subroutine test_trace_int8

  subroutine test_trace_int16
    integer, parameter :: n = 3
    integer(int16) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_int16"
    a = reshape([(i**3,i=1,n**2)],[n,n])
    call check(trace(a) == (1 + 125 + 729), &
          msg="trace(a) == (1 + 125 + 729) failed.",warn=warn)
  end subroutine test_trace_int16

  subroutine test_trace_int32
    integer, parameter :: n = 3
    integer(int32) :: a(n,n)
    integer :: i
    write(*,*) "test_trace_int32"
    a = reshape([(i**4,i=1,n**2)],[n,n])
    call check(trace(a) == (1 + 625 + 6561), &
          msg="trace(a) == (1 + 625 + 6561) failed.",warn=warn)
  end subroutine test_trace_int32

  subroutine test_trace_int64
    integer, parameter :: n = 5
    integer, parameter :: nd = 2*n-1 ! number of diagonals
    integer :: i, j
    integer(int64) :: c(0:nd), H(n,n)
    write(*,*) "test_trace_int64"

    ! Fill array with Catalan numbers
    do i = 0, nd
      c(i) = catalan_number(i)
    end do

    ! Symmetric Hankel matrix filled with Catalan numbers (det(H) = 1)
    do i = 1, n
      do j = 1, n
        H(i,j) = c(i-1 + (j-1))
      end do
    end do

    call check(trace(h) == sum(c(0:nd:2)), &
          msg="trace(h) == sum(c(0:nd:2)) failed.",warn=warn)

  end subroutine test_trace_int64


  subroutine test_outer_product_rsp
    integer, parameter :: n = 2
    real(sp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_rsp" 
    u = [1.,2.]
    v = [1.,3.]
    expected = reshape([1.,2.,3.,6.],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < sptol), &
         msg="all(abs(diff) < sptol) failed.",warn=warn)
  end subroutine test_outer_product_rsp

  subroutine test_outer_product_rdp
    integer, parameter :: n = 2
    real(dp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_rdp" 
    u = [1.,2.]
    v = [1.,3.]
    expected = reshape([1.,2.,3.,6.],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < dptol), &
         msg="all(abs(diff) < dptol) failed.",warn=warn)
  end subroutine test_outer_product_rdp

  subroutine test_outer_product_rqp
    integer, parameter :: n = 2
    real(qp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_rqp" 
    u = [1.,2.]
    v = [1.,3.]
    expected = reshape([1.,2.,3.,6.],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < qptol), &
         msg="all(abs(diff) < qptol) failed.",warn=warn)
  end subroutine test_outer_product_rqp

  subroutine test_outer_product_csp
    integer, parameter :: n = 2
    complex(sp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_csp" 
    u = [cmplx(1.,1.),cmplx(2.,0.)]
    v = [cmplx(1.,0.),cmplx(3.,1.)]
    expected = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(2.,4.),cmplx(6.,2.)],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < sptol), &
         msg="all(abs(diff) < sptol) failed.",warn=warn)
  end subroutine test_outer_product_csp

  subroutine test_outer_product_cdp
    integer, parameter :: n = 2
    complex(dp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_cdp" 
    u = [cmplx(1.,1.),cmplx(2.,0.)]
    v = [cmplx(1.,0.),cmplx(3.,1.)]
    expected = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(2.,4.),cmplx(6.,2.)],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < dptol), &
         msg="all(abs(diff) < dptol) failed.",warn=warn)
  end subroutine test_outer_product_cdp

  subroutine test_outer_product_cqp
    integer, parameter :: n = 2
    complex(qp) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_cqp" 
    u = [cmplx(1.,1.),cmplx(2.,0.)]
    v = [cmplx(1.,0.),cmplx(3.,1.)]
    expected = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(2.,4.),cmplx(6.,2.)],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) < qptol), &
         msg="all(abs(diff) < qptol) failed.",warn=warn)
  end subroutine test_outer_product_cqp
    
  subroutine test_outer_product_int8
    integer, parameter :: n = 2
    integer(int8) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_int8" 
    u = [1,2]
    v = [1,3]
    expected = reshape([1,2,3,6],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) == 0), &
         msg="all(abs(diff) == 0) failed.",warn=warn)
  end subroutine test_outer_product_int8

  subroutine test_outer_product_int16
    integer, parameter :: n = 2
    integer(int16) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_int16" 
    u = [1,2]
    v = [1,3]
    expected = reshape([1,2,3,6],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) == 0), &
         msg="all(abs(diff) == 0) failed.",warn=warn)
  end subroutine test_outer_product_int16

  subroutine test_outer_product_int32
    integer, parameter :: n = 2
    integer(int32) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_int32" 
    u = [1,2]
    v = [1,3]
    expected = reshape([1,2,3,6],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) == 0), &
         msg="all(abs(diff) == 0) failed.",warn=warn)
  end subroutine test_outer_product_int32

  subroutine test_outer_product_int64
    integer, parameter :: n = 2
    integer(int64) :: u(n), v(n), expected(n,n), diff(n,n)
    write(*,*) "test_outer_product_int64" 
    u = [1,2]
    v = [1,3]
    expected = reshape([1,2,3,6],[n,n])
    diff = expected - outer_product(u,v)
    call check(all(abs(diff) == 0), &
         msg="all(abs(diff) == 0) failed.",warn=warn)
  end subroutine test_outer_product_int64


  pure recursive function catalan_number(n) result(value)
    integer, intent(in) :: n
    integer :: value
    integer :: i
    if (n <= 1) then
      value = 1
    else
      value = 0
      do i = 0, n-1
        value = value + catalan_number(i)*catalan_number(n-i-1)
      end do
    end if
  end function

end program
