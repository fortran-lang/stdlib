program test_loadtxt_qp
use stdlib_kinds, only: qp
use stdlib_io, only: loadtxt
implicit none

real(qp), allocatable :: q(:, :)

call loadtxt("array4.dat", q)
call print_array(q)

contains

subroutine print_array(a)
class(*),intent(in) :: a(:, :)
integer :: i
print *, "Array, shape=(", size(a, 1), ",", size(a, 2), ")"

 select type(a)
  type is(real(qp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  class default
   write(*,'(a)')'The proposed type is not supported'
   error stop
 end select

end subroutine

end program
