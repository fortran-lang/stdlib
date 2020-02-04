program test_mean
use stdlib_experimental_error, only: assert
use stdlib_experimental_kinds, only: sp, dp, int32, int64
use stdlib_experimental_io, only: loadtxt
use stdlib_experimental_stats, only: mean
implicit none

real(dp), allocatable :: d(:, :)
real(dp), allocatable :: d8(:, :, :, :, :, :, :, :)
complex(dp), allocatable :: cd8(:, :, :, :, :, :, :, :)


!dp
call loadtxt("array3.dat", d)

call assert( mean(d) - sum(d)/real(size(d), dp) == 0.0_dp)
call assert( sum( abs( mean(d,1) - sum(d,1)/real(size(d,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d,2) - sum(d,2)/real(size(d,2), dp) )) == 0.0_dp)

!dp rank 8
allocate(d8(size(d,1), size(d,2), 3, 4, 5, 6, 7, 8))
d8(:, :, 1, 4, 5 ,6 ,7 ,8)=d;
d8(:, :, 2, 4, 5 ,6 ,7 ,8)=d * 1.5_dp;
d8(:, :, 3, 4, 5 ,6 ,7 ,8)=d * 4._dp;

call assert( mean(d8) - sum(d8)/real(size(d8), dp) == 0.0_dp)

call assert( sum( abs( mean(d8,1) - sum(d8,1)/real(size(d8,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,2) - sum(d8,2)/real(size(d8,2), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,3) - sum(d8,3)/real(size(d8,3), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,4) - sum(d8,4)/real(size(d8,4), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,5) - sum(d8,5)/real(size(d8,5), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,6) - sum(d8,6)/real(size(d8,6), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,7) - sum(d8,7)/real(size(d8,7), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d8,8) - sum(d8,8)/real(size(d8,8), dp) )) == 0.0_dp)

!cdp rank 8
allocate(cd8(size(d,1), size(d,2), 3, 4, 5, 6, 7, 8))
cd8 = cmplx(1._dp, 1._dp, kind=dp)*d8

call assert( mean(cd8) - sum(cd8)/real(size(cd8), dp) == 0.0_dp)

call assert( sum( abs( mean(cd8,1) - sum(cd8,1)/real(size(cd8,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,2) - sum(cd8,2)/real(size(cd8,2), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,3) - sum(cd8,3)/real(size(cd8,3), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,4) - sum(cd8,4)/real(size(cd8,4), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,5) - sum(cd8,5)/real(size(cd8,5), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,6) - sum(cd8,6)/real(size(cd8,6), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,7) - sum(cd8,7)/real(size(cd8,7), dp) )) == 0.0_dp)
call assert( sum( abs( mean(cd8,8) - sum(cd8,8)/real(size(cd8,8), dp) )) == 0.0_dp)
contains

end program
