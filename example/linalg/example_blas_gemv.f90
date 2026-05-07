program example_gemv
  use stdlib_linalg, only: eye
  use stdlib_linalg_blas, only: sp,gemv
  implicit none
  real(sp) :: A(2, 2), B(2), C(2)
  B = [1.0,2.0]
  A = eye(2)
  
  ! Use legacy BLAS interface 
  call gemv('No transpose',m=size(A,1),n=size(A,2),alpha=1.0,a=A,lda=size(A,1),x=B,incx=1,beta=0.0,y=C,incy=1)

  print *, C ! returns 1.0 2.0

end program example_gemv
