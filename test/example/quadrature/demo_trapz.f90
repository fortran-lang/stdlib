program demo_trapz
use stdlib_quadrature, only: trapz
implicit none
real :: x(5) = [0., 1., 2., 3., 4.]
real :: y(5) = x**2
print *, trapz(y, x)
! 22.0
print *, trapz(y, 0.5)
! 11.0
end program demo_trapz
