program example_getrf
  use stdlib_linalg, only: eye
  use stdlib_linalg_lapack, only: dp,ilp,getrf
  implicit none
  real(dp) :: A(3, 3)
  integer(ilp) :: ipiv(3),info
  
  A = eye(3)
  
  ! LAPACK matrix factorization interface (overwrite result)
  call getrf(size(A,1),size(A,2),A,size(A,1),ipiv,info)
  print *, info ! info==0: Success!

end program example_getrf
