program example_repmat
  use stdlib_linalg, only: repmat
  implicit none
  real, allocatable :: a(:, :),b(:,:)
  a = reshape([1., 2., 3., 4.],[2, 2],order=[2, 1])
  b = repmat(a, 2, 2)
! A = reshape([1., 2., 1., 2.,&
!              3., 4., 3., 4.,&
!              1., 2., 1., 2.,&
!              3., 4., 3., 4.,&
!               ], [4, 4],order=[2, 1])
end program example_repmat
