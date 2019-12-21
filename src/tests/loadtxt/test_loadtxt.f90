program test_loadtxt
use iso_fortran_env, only: dp=>real64
use stdlib_experimental_io, only: loadtxt
implicit none

real(dp), allocatable :: d(:, :)
call loadtxt("array1.dat", d)
call print_array(d)

call loadtxt("array2.dat", d)
call print_array(d)

call loadtxt("array3.dat", d)
call print_array(d)

call loadtxt("array4.dat", d)
call print_array(d)

contains

subroutine print_array(a)
real(dp) :: a(:, :)
integer :: i
print *, "Array, shape=(", size(a, 1), ",", size(a, 2), ")"
do i = 1, size(a, 1)
    print *, a(i, :)
end do
end subroutine

end program
