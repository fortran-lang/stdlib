! test_tolerant.f90 --
!     Compare real values in a "tolerant" way, that is, with a margin
!
!     Copy of the original test program
!
program test_tolerant
    use stdlib_tolerant

    implicit none

    integer, parameter :: dp = kind(1.0d0)
    real(kind=dp)      :: x, y, z
    real(kind=dp)      :: yfloor, yceil
    integer            :: i

    real(kind=dp)      :: eps3 = 3.0_dp * epsilon(eps3)

!---Illustrate Fuzzy Comparisons using EPS3. Any other magnitudes will
!   behave similarly.

    z = 1.0_dp
    i = 49
    x = 1.0_dp / i
    y = x * i

    write(*,*) 'x=1.d0/',i,', y=x*',i,', z=1.d0'
    write(*,*) 'y=',y,' z=',z
    write(*,3) x,y,z
3   format(' x=',z16,' y=',z16,' z=',z16)

!---floating-point y is not identical (.eq.) to floating-point z.
    if ( y == z ) write(*,*) 'fuzzy comparisons: y=z'
    if ( y /= z ) write(*,*) 'fuzzy comparisons: y<>z'

!---but y is tolerantly (and algebraically) equal to z.
    if ( y .teq. z ) then
        write(*,*) 'but y .teq. z is .true.'
        write(*,*) 'that is, y is computationally equal to z.'
    endif
    if( y .tne. z ) write(*,*) 'and y .tne. z is .true.'
    write(*,*) ' '

!---evaluate fuzzy floor and ceiling function values using a comparison
!   tolerance, ct, of eps3.
    x = 0.11_dp
    y = ( ( x * 11.0_dp) - x ) - 0.1_dp
    yfloor = tfloor( y, eps3 )
    yceil  = tceil( y, eps3 )

    z =1.0_dp

    write(*,*) 'x=0.11d0, y=x*11.d0-x-0.1d0, z=1.d0'
    write(*,*) 'x=',x,' y=',y,' z=',z
    write(*,3) x,y,z

!---floating-point y is not identical (.eq.) to floating-point z.
    if ( y == z ) write(*,*) 'fuzzy floor/ceil: y=z'
    if ( y /= z ) write(*,*) 'fuzzy floor/ceil: y<>z'
    if ( tfloor(y,eps3) == tceil(y,eps3) .and. tfloor(y,eps3) == z ) then
!---but tolerant floor/ceil of y is identical (and algebraically equal)
!   to z.
        write(*,*) 'but tfloor(y,eps3)=tceil(y,eps3)=z.'
        write(*,*) 'that is, tfloor/tceil return exact whole numbers.'
    endif
end program test_tolerant
