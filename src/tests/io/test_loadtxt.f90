program test_loadtxt
use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
use stdlib_experimental_io, only: loadtxt
use stdlib_experimental_error, only: error_stop
implicit none

real(sp), allocatable :: s(:, :)
real(dp), allocatable :: d(:, :)

call loadtxt("array1.dat", s)
call print_array(s)

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
class(*),intent(in) :: a(:, :)
integer :: i
print *, "Array, shape=(", size(a, 1), ",", size(a, 2), ")"

 select type(a)
  type is(real(sp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  type is(real(dp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  class default
   call error_stop('The proposed type is not supported')
 end select

end subroutine

end program
