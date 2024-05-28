! Singular Values
program example_svdvals
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: svdvals
  implicit none

  real(dp), allocatable :: A(:,:),s(:)
  character(*), parameter :: fmt="(a,*(1x,f12.8))"

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
  print fmt, '    '
  print fmt, 'S = ',s
  print fmt, '    '
  
end program example_svdvals
