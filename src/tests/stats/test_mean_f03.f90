program test_mean
use stdlib_error, only: check
use stdlib_kinds, only: sp, dp, int32
use stdlib_io, only: loadtxt
use stdlib_stats, only: mean
implicit none

real(dp), parameter :: dptol =10 * epsilon(1._dp)

real(dp), allocatable :: d(:, :)
real(dp), allocatable :: d8(:, :, :, :, :, :, :, :)
complex(dp), allocatable :: cd8(:, :, :, :, :, :, :, :)


!dp
call loadtxt("array3.dat", d)

call check( mean(d) - sum(d)/real(size(d), dp) < dptol)
call check( sum( abs( mean(d,1) - sum(d,1)/real(size(d,1), dp) )) < dptol)
call check( sum( abs( mean(d,2) - sum(d,2)/real(size(d,2), dp) )) < dptol)

!dp rank 8
allocate(d8(size(d,1), size(d,2), 3, 4, 5, 6, 7, 8), source=0.0_dp)
d8(:, :, 1, 4, 5 ,6 ,7 ,8)=d;
d8(:, :, 2, 4, 5 ,6 ,7 ,8)=d * 1.5_dp;
d8(:, :, 3, 4, 5 ,6 ,7 ,8)=d * 4._dp;

call check( mean(d8) - sum(d8)/real(size(d8), dp) < dptol)

call check( sum( abs( mean(d8,1) - sum(d8,1)/real(size(d8,1), dp) )) < dptol)
call check( sum( abs( mean(d8,2) - sum(d8,2)/real(size(d8,2), dp) )) < dptol)
call check( sum( abs( mean(d8,3) - sum(d8,3)/real(size(d8,3), dp) )) < dptol)
call check( sum( abs( mean(d8,4) - sum(d8,4)/real(size(d8,4), dp) )) < dptol)
call check( sum( abs( mean(d8,5) - sum(d8,5)/real(size(d8,5), dp) )) < dptol)
call check( sum( abs( mean(d8,6) - sum(d8,6)/real(size(d8,6), dp) )) < dptol)
call check( sum( abs( mean(d8,7) - sum(d8,7)/real(size(d8,7), dp) )) < dptol)
call check( sum( abs( mean(d8,8) - sum(d8,8)/real(size(d8,8), dp) )) < dptol)

!cdp rank 8
allocate(cd8(size(d,1), size(d,2), 3, 4, 5, 6, 7, 8))
cd8 = cmplx(1._dp, 1._dp, kind=dp)*d8

call check( abs(mean(cd8) - sum(cd8)/real(size(cd8), dp)) < dptol)

call check( sum( abs( mean(cd8,1) - sum(cd8,1)/real(size(cd8,1), dp) )) < dptol)
call check( sum( abs( mean(cd8,2) - sum(cd8,2)/real(size(cd8,2), dp) )) < dptol)
call check( sum( abs( mean(cd8,3) - sum(cd8,3)/real(size(cd8,3), dp) )) < dptol)
call check( sum( abs( mean(cd8,4) - sum(cd8,4)/real(size(cd8,4), dp) )) < dptol)
call check( sum( abs( mean(cd8,5) - sum(cd8,5)/real(size(cd8,5), dp) )) < dptol)
call check( sum( abs( mean(cd8,6) - sum(cd8,6)/real(size(cd8,6), dp) )) < dptol)
call check( sum( abs( mean(cd8,7) - sum(cd8,7)/real(size(cd8,7), dp) )) < dptol)
call check( sum( abs( mean(cd8,8) - sum(cd8,8)/real(size(cd8,8), dp) )) < dptol)
contains

end program
