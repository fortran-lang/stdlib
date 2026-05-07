! Example program demonstrating the usage of hermitian
program example_hermitian
  use stdlib_linalg, only: hermitian
  implicit none

  complex, dimension(2, 2) :: A, AT

  ! Define input matrices
  A = reshape([(1,2),(3,4),(5,6),(7,8)],[2,2])

  ! Compute Hermitian matrices
  AT = hermitian(A)

  print *, "Original Complex Matrix:"
  print *, A
  print *, "Hermitian Complex Matrix:"
  print *, AT
  
end program example_hermitian
