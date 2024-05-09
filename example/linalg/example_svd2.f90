! Singular Values
program example_svd2
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: svd
  implicit none

  real(dp), allocatable :: A(:,:),s(:)

  ! We want to find the singular values of matrix: 
  ! 
  !    A = [ 3  2  2]
  !        [ 2  3 -2]  
  !   
  A = transpose(reshape([ 3, 2, 2, &
                          2, 3,-2], [3,2]))

  ! Get singular values
  s = svdvals(A)

  ! Singular values: [5, 3]
  print 1, '    '
  print 1, 'S = ',s
  print 1, '    '
  
  1 format(a,*(1x,f12.8))

end program example_svd2
