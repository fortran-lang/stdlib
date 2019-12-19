program test_loadtxt
use stdlib_types, only: dp
use stdlib_io, only: loadtxt, savetxt
use stdlib_error, only: assert
implicit none

real(dp) :: d(3, 2), e(2, 3)
real(dp), allocatable :: d2(:, :)
d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
call savetxt("tmp.dat", d)
call loadtxt("tmp.dat", d2)
call assert(all(shape(d2) == [3, 2]))
call assert(all(abs(d-d2) < epsilon(1._dp)))

e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
call savetxt("tmp.dat", e)
call loadtxt("tmp.dat", d2)
call assert(all(shape(d2) == [2, 3]))
call assert(all(abs(e-d2) < epsilon(1._dp)))

end program
