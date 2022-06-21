program demo_diag2
use stdlib_linalg, only: diag
implicit none
real :: v(:)
real, allocatable :: A(:,:)
integer :: i
v = [1,2,3,4,5]
A = diag(v) ! creates a 5 by 5 matrix with elements of v on the diagonal
end program demo_diag2
