program test_mean
use stdlib_experimental_error, only: assert
use stdlib_experimental_kinds, only: sp, dp, int32, int64
use stdlib_experimental_io, only: loadtxt
use stdlib_experimental_stats, only: mean
implicit none

real(sp) :: s1(3) = [1.0_sp, 2.0_sp, 3.0_sp]

real(sp), allocatable :: s(:, :)
real(dp), allocatable :: d(:, :)

real(dp), allocatable :: d3(:, :, :)
real(dp), allocatable :: d4(:, :, :, :)


!sp
call loadtxt("array3.dat", s)

call assert( mean(s) - sum(s)/real(size(s), sp) == 0.0_sp)
call assert( sum( abs( mean(s,1) - sum(s,1)/real(size(s,1), sp) )) == 0.0_sp)
call assert( sum( abs( mean(s,2) - sum(s,2)/real(size(s,2), sp) )) == 0.0_sp)

! check reduction of rank one array to scalar
call assert(mean(s1) - sum(s1) / real(size(s1), sp) == 0.0_dp)
call assert(mean(s1, dim=1) - sum(s1, dim=1) / real(size(s1, dim=1), sp) == 0.0_dp)


!dp
call loadtxt("array3.dat", d)

call assert( mean(d) - sum(d)/real(size(d), dp) == 0.0_dp)
call assert( sum( abs( mean(d,1) - sum(d,1)/real(size(d,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d,2) - sum(d,2)/real(size(d,2), dp) )) == 0.0_dp)


!int32
call loadtxt("array3.dat", d)

call assert( mean(int(d, int32)) - sum(real(int(d, int32),dp))/real(size(d), dp) == 0.0_dp)
call assert( sum(abs( mean(int(d, int32),1) - sum(real(int(d, int32),dp),1)/real(size(d,1), dp) )) == 0.0_dp)
call assert( sum(abs( mean(int(d, int32),2) - sum(real(int(d, int32),dp),2)/real(size(d,2), dp) )) == 0.0_dp)


!int64
call loadtxt("array3.dat", d)

call assert( mean(int(d, int64)) - sum(real(int(d, int64),dp))/real(size(d), dp) == 0.0_dp)
call assert( sum(abs( mean(int(d, int64),1) - sum(real(int(d, int64),dp),1)/real(size(d,1), dp) )) == 0.0_dp)
call assert( sum(abs( mean(int(d, int64),2) - sum(real(int(d, int64),dp),2)/real(size(d,2), dp) )) == 0.0_dp)


!dp rank 3
allocate(d3(size(d,1),size(d,2),3))
d3(:,:,1)=d;
d3(:,:,2)=d*1.5_dp;
d3(:,:,3)=d*4._dp;

call assert( mean(d3) - sum(d3)/real(size(d3), dp) == 0.0_dp)
call assert( sum( abs( mean(d3,1) - sum(d3,1)/real(size(d3,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d3,2) - sum(d3,2)/real(size(d3,2), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d3,3) - sum(d3,3)/real(size(d3,3), dp) )) == 0.0_dp)


!dp rank 4
allocate(d4(size(d,1),size(d,2),3,9))
d4 = 1.
d4(:,:,1,1)=d;
d4(:,:,2,1)=d*1.5_dp;
d4(:,:,3,1)=d*4._dp;
d4(:,:,3,9)=d*4._dp;

call assert( mean(d4) - sum(d4)/real(size(d4), dp) == 0.0_dp)
call assert( sum( abs( mean(d4,1) - sum(d4,1)/real(size(d4,1), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d4,2) - sum(d4,2)/real(size(d4,2), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d4,3) - sum(d4,3)/real(size(d4,3), dp) )) == 0.0_dp)
call assert( sum( abs( mean(d4,4) - sum(d4,4)/real(size(d4,4), dp) )) == 0.0_dp)

contains

end program
