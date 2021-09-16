program test_mean
use stdlib_error, only: check
use stdlib_kinds, only: sp, dp, int32, int64
use stdlib_io, only: loadtxt
use stdlib_stats, only: mean
use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
implicit none

real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

real(sp) :: s1(3) = [1.0_sp, 2.0_sp, 3.0_sp]

real(sp), allocatable :: s(:, :)
real(dp), allocatable :: d(:, :)

complex(dp), allocatable :: cs(:, :)
complex(dp), allocatable :: cd(:, :)

real(dp), allocatable :: d3(:, :, :)
real(dp), allocatable :: d4(:, :, :, :)

complex(dp), allocatable :: cd3(:, :, :)

!sp
call loadtxt("array3.dat", s)

call check( abs(mean(s) - sum(s)/real(size(s), sp)) < sptol)
call check( sum( abs( mean(s,1) - sum(s,1)/real(size(s,1), sp) )) < sptol)
call check( sum( abs( mean(s,2) - sum(s,2)/real(size(s,2), sp) )) < sptol)

! check reduction of rank one array to scalar
call check(abs(mean(s1) - sum(s1) / real(size(s1), sp)) < sptol)
call check(abs(mean(s1, dim=1) - sum(s1, dim=1) / real(size(s1, dim=1), sp)) < sptol)


!dp
call loadtxt("array3.dat", d)

call check( abs(mean(d) - sum(d)/real(size(d), dp)) < dptol)
call check( sum( abs( mean(d,1) - sum(d,1)/real(size(d,1), dp) )) < dptol)
call check( sum( abs( mean(d,2) - sum(d,2)/real(size(d,2), dp) )) < dptol)

!csp

call loadtxt("array3.dat", d)
cs = cmplx(1._sp, 1._sp,kind=sp)*d

call check( abs(mean(cs) - sum(cs)/real(size(cs), sp)) < sptol)
call check( sum( abs( mean(cs,1) - sum(cs,1)/real(size(cs,1), sp) )) < sptol)
call check( sum( abs( mean(cs,2) - sum(cs,2)/real(size(cs,2), sp) )) < sptol)

!cdp

call loadtxt("array3.dat", d)
cd = cmplx(1._dp, 1._dp,kind=dp)*d

call check( abs(mean(cd) - sum(cd)/real(size(cd), dp)) < dptol)
call check( sum( abs( mean(cd,1) - sum(cd,1)/real(size(cd,1), dp) )) < dptol)
call check( sum( abs( mean(cd,2) - sum(cd,2)/real(size(cd,2), dp) )) < dptol)

! check mask = .false.

call check( ieee_is_nan(mean(d, .false.)))
call check( any(ieee_is_nan(mean(d, 1, .false.))))
call check( any(ieee_is_nan(mean(d, 2, .false.))))

! check mask of the same shape as input
call check( abs(mean(d, d > 0) - sum(d, d > 0)/real(count(d > 0), dp)) < dptol)
call check( sum(abs(mean(d, 1, d > 0) - sum(d, 1, d > 0)/real(count(d > 0, 1), dp))) < dptol)
call check( sum(abs(mean(d, 2, d > 0) - sum(d, 2, d > 0)/real(count(d > 0, 2), dp))) < dptol)

!int32
call loadtxt("array3.dat", d)

call check( abs(mean(int(d, int32)) - sum(real(int(d, int32),dp))/real(size(d), dp)) < dptol)
call check( sum(abs( mean(int(d, int32),1) - sum(real(int(d, int32),dp),1)/real(size(d,1), dp) )) < dptol)
call check( sum(abs( mean(int(d, int32),2) - sum(real(int(d, int32),dp),2)/real(size(d,2), dp) )) < dptol)


!int64
call loadtxt("array3.dat", d)

call check( abs(mean(int(d, int64)) - sum(real(int(d, int64),dp))/real(size(d), dp)) < dptol)
call check( sum(abs( mean(int(d, int64),1) - sum(real(int(d, int64),dp),1)/real(size(d,1), dp) )) < dptol)
call check( sum(abs( mean(int(d, int64),2) - sum(real(int(d, int64),dp),2)/real(size(d,2), dp) )) < dptol)


!dp rank 3
allocate(d3(size(d,1),size(d,2),3))
d3(:,:,1)=d;
d3(:,:,2)=d*1.5;
d3(:,:,3)=d*4;

call check( abs(mean(d3) - sum(d3)/real(size(d3), dp)) < dptol)
call check( sum( abs( mean(d3,1) - sum(d3,1)/real(size(d3,1), dp) )) < dptol)
call check( sum( abs( mean(d3,2) - sum(d3,2)/real(size(d3,2), dp) )) < dptol)
call check( sum( abs( mean(d3,3) - sum(d3,3)/real(size(d3,3), dp) )) < dptol)

!cdp rank 3
allocate(cd3(size(d,1),size(d,2),3))
cd3(:,:,1)=d;
cd3(:,:,2)=d*1.5;
cd3(:,:,3)=d*4;
cd3 = cmplx(1._sp, 1._sp,kind=sp)*cd3

call check( abs(mean(cd3) - sum(cd3)/real(size(cd3), dp)) < dptol)
call check( sum( abs( mean(cd3,1) - sum(cd3,1)/real(size(cd3,1), dp) )) < dptol)
call check( sum( abs( mean(cd3,2) - sum(cd3,2)/real(size(cd3,2), dp) )) < dptol)
call check( sum( abs( mean(cd3,3) - sum(cd3,3)/real(size(cd3,3), dp) )) < dptol)


!dp rank 4
allocate(d4(size(d,1),size(d,2),3,9))
d4 = -1
d4(:,:,1,1)=d;
d4(:,:,2,1)=d*1.5;
d4(:,:,3,1)=d*4;
d4(:,:,3,9)=d*4;

call check( abs(mean(d4) - sum(d4)/real(size(d4), dp)) < dptol)
call check( sum( abs( mean(d4,1) - sum(d4,1)/real(size(d4,1), dp) )) < dptol)
call check( sum( abs( mean(d4,2) - sum(d4,2)/real(size(d4,2), dp) )) < dptol)
call check( sum( abs( mean(d4,3) - sum(d4,3)/real(size(d4,3), dp) )) < dptol)
call check( sum( abs( mean(d4,4) - sum(d4,4)/real(size(d4,4), dp) )) < dptol)

! check mask = .false.

call check( ieee_is_nan(mean(d4, .false.)))
call check( any(ieee_is_nan(mean(d4, 1, .false.))))
call check( any(ieee_is_nan(mean(d4, 2, .false.))))
call check( any(ieee_is_nan(mean(d4, 3, .false.))))
call check( any(ieee_is_nan(mean(d4, 4, .false.))))


! check mask of the same shape as input
call check( abs(mean(d4, d4 > 0) - sum(d4, d4 > 0)/real(count(d4 > 0), dp)) < dptol)
call check( any(ieee_is_nan(mean(d4, 1, d4 > 0))) )
call check( any(ieee_is_nan(mean(d4, 2, d4 > 0))) )
call check( any(ieee_is_nan(mean(d4, 3, d4 > 0))) )
call check( sum(abs(mean(d4, 4, d4 > 0) - sum(d4, 4, d4 > 0)/real(count(d4 > 0, 4), dp))) < dptol)

end program
