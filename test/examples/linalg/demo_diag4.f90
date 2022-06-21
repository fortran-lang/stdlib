program demo_diag4
use stdlib_linalg, only: diag
implicit none
integer, parameter :: n = 12
real :: A(n,n)
real :: v(n)
integer :: i
call random_number(A)
v = diag(A) ! v contains diagonal elements of A
end program demo_diag4
