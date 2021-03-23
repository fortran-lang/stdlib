! stdlib_tolerant.f90 --
!     Compare real values in a "tolerant" way, that is, with a margin
!
!     Comments copied from the original code:
!**********************************************************************
!  ROUTINE:   FUZZY FORTRAN OPERATORS
!  PURPOSE:   Illustrate Hindmarsh's computation of EPS, and APL
!             tolerant comparisons, tolerant CEIL/FLOOR, and Tolerant
!             ROUND functions - implemented in Fortran.
!  PLATFORM:  PC Windows Fortran, Compaq-Digital CVF 6.1a, AIX XLF90
!  TO RUN:    Windows: DF EPS.F90
!             AIX: XLF90 eps.f -o eps.exe -qfloat=nomaf
!  CALLS:     none
!  AUTHOR:    H. D. Knoble <hdk@psu.edu> 22 September 1978
!  REVISIONS:
!**********************************************************************
!
module stdlib_tolerant
    implicit none

    private
    public :: tfloor, tceil, tround
    public :: operator(.teq.), operator(.tne.)
    public :: operator(.tgt.), operator(.tge.)
    public :: operator(.tlt.), operator(.tle.)

    integer, parameter, private       :: sp = kind(1.0)
    integer, parameter, private       :: dp = kind(1.0d0)

    real(kind=sp), parameter, private :: eps_sp  = epsilon(eps_sp)
    real(kind=dp), parameter, private :: eps_dp  = epsilon(eps_dp)
    real(kind=sp), parameter, private :: eps_sp3 = 3.0    * epsilon(eps_sp)
    real(kind=dp), parameter, private :: eps_dp3 = 3.0_dp * epsilon(eps_dp)

    interface tfloor
        module procedure tfloor_sp
        module procedure tfloor_dp
    end interface

    interface tceil
        module procedure tceil_sp
        module procedure tceil_dp
    end interface

    interface tround
        module procedure tround_sp
        module procedure tround_dp
    end interface

    interface operator(.teq.)
        module procedure teq_sp
        module procedure teq_dp
    end interface

    interface operator(.tne.)
        module procedure tne_sp
        module procedure tne_dp
    end interface

    interface operator(.tgt.)
        module procedure tgt_sp
        module procedure tgt_dp
    end interface

    interface operator(.tge.)
        module procedure tge_sp
        module procedure tge_dp
    end interface

    interface operator(.tlt.)
        module procedure tlt_sp
        module procedure tlt_dp
    end interface

    interface operator(.tle.)
        module procedure tle_sp
        module procedure tle_dp
    end interface

contains

! teq_sp, ... --
!     Tolerant comparison for single-precision numbers
!
logical function teq_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = abs(x-y) <= max( abs(x), abs(y) ) * eps_sp3

end function teq_sp

logical function tne_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = .not. (x .teq. y)

end function tne_sp

logical function tgt_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = (x - y) > max( abs(x), abs(y) ) * eps_sp3

end function tgt_sp

logical function tle_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = .not. (x .tgt. y)

end function tle_sp

logical function tlt_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = (x .tle. y) .and. (x .tne. y)

end function tlt_sp

logical function tge_sp( x, y ) result(cmp)
    real(kind=sp), intent(in) :: x, y

    cmp = (x .tgt. y) .or. (x .teq. y)

end function tge_sp

! teq_dp, ... --
!     Tolerant comparison for single-precision numbers
!
logical function teq_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = abs(x-y) <= max( abs(x), abs(y) ) * eps_dp3

end function teq_dp

logical function tne_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = .not. (x .teq. y)

end function tne_dp

logical function tgt_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = (x - y) > max( abs(x), abs(y) ) * eps_dp3

end function tgt_dp

logical function tle_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = .not. (x .tgt. y)

end function tle_dp

logical function tlt_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = (x .tle. y) .and. (x .tne. y)

end function tlt_dp

logical function tge_dp( x, y ) result(cmp)
    real(kind=dp), intent(in) :: x, y

    cmp = (x .tgt. y) .or. (x .teq. y)

end function tge_dp

! tfloor_dp --
!     Tolerant FLOOR Function
!
! Arguments:
!
!     x  -  is given as a double precision argument to be operated on.
!           it is assumed that X is represented with m mantissa bits.
!     ct -  is   given   as   a   Comparison   Tolerance   such   that
!           0. < CT <  3-Sqrt(5)/2. If the relative difference between
!           X and a whole number is  less  than  CT,  then  TFLOOR  is
!           returned   as   this   whole   number.   By  treating  the
!           floating-point numbers as a finite ordered set  note  that
!           the  heuristic  eps=2.**(-(m-1))   and   CT=3*eps   causes
!           arguments  of  TFLOOR/TCEIL to be treated as whole numbers
!           if they are  exactly  whole  numbers  or  are  immediately
!           adjacent to whole number representations.  Since EPS,  the
!           "distance"  between  floating-point  numbers  on  the unit
!           interval, and m, the number of bits in X's mantissa, exist
!           on  every  floating-point   computer,   TFLOOR/TCEIL   are
!           consistently definable on every floating-point computer.
!
!           For more information see the following references:
!
!       {1} P. E. Hagerty, "More on Fuzzy Floor and Ceiling," APL  QUOTE
!           QUAD 8(4):20-24, June 1978. Note that TFLOOR=FL5 took five
!           years of refereed evolution (publication).
!
!       {2} L. M. Breed, "Definitions for Fuzzy Floor and Ceiling",  APL
!           QUOTE QUAD 8(3):16-23, March 1978.
!
!     H. D. KNOBLE, Penn State University.
!
!           FLOOR(X) is the largest integer algegraically less than
!           or equal to X; that is, the unfuzzy Floor Function.
!
elemental function tfloor_dp( x, ct_opt )
    real(kind=dp), intent(in)           :: x
    real(kind=dp), intent(in), optional :: ct_opt
    real(kind=dp)                       :: tfloor_dp

    real(kind=dp)                       :: q, rmax, eps5, ct

    if ( present(ct_opt) ) then
        ct = ct_opt
    else
        ct = eps_dp3
    endif

!   Hagerty's FL5 Function follows...

    q=1.0_dp

    if( x < 0.0_dp ) q = 1.0_dp - ct

    rmax = q / (2.0_dp - ct)
    eps5 = ct / q
    tfloor_dp = floor( x + max( ct, min( rmax, eps5 * abs(1.0_dp + floor(x)) ) ) )

    if ( x <= 0.0_dp .or. (tfloor_dp - x) < rmax ) return

    tfloor_dp = tfloor_dp-1.0_dp

contains
elemental function dint( x )
    real(kind=dp), intent(in) :: x
    real(kind=dp)             :: dint

    dint = x - mod(x,1.0_dp)
end function dint

elemental function floor( x )
    real(kind=dp), intent(in) :: x
    real(kind=dp)             :: floor

      floor = dint(x) - mod( 2.0_dp + sign(1.0_dp,x), 3.0_dp )
end function floor
end function tfloor_dp

! tceil_dp --
!     Tolerant Ceiling Function
!
! Arguments:
!     See tfloor_dp
!
elemental function tceil_dp( x, ct_opt )
    real(kind=dp), intent(in)           :: x
    real(kind=dp), intent(in), optional :: ct_opt
    real(kind=dp)                       :: tceil_dp

    if ( present(ct_opt) ) then
        tceil_dp = -tfloor_dp(-x, ct_opt)
    else
        tceil_dp = -tfloor_dp(-x, eps_dp3)
    endif
end function tceil_dp

! tround_dp --
!     Tolerant Round Function
!
! Arguments:
!     See tfloor_dp
!
! Note:
!     See Knuth, Art of Computer Programming, Vol. 1, Problem 1.2.4-5.
!
elemental function tround_dp( x, ct_opt )
    real(kind=dp), intent(in)           :: x
    real(kind=dp), intent(in), optional :: ct_opt
    real(kind=dp)                       :: tround_dp

    if ( present(ct_opt) ) then
        tround_dp = tfloor_dp(x +0.5_dp, ct_opt)
    else
        tround_dp = tfloor_dp(x +0.5_dp, eps_dp3)
    endif

end function tround_dp

! tfloor_sp, tceil_sp, tround_sp --
!     Tolerant Floor, Ceiling and Rond Functions for single-precision
!
! Arguments:
!     See tfloor_dp
!
elemental function tfloor_sp( x, ct_opt )
    real(kind=sp), intent(in)           :: x
    real(kind=sp), intent(in), optional :: ct_opt
    real(kind=sp)                       :: tfloor_sp
    real(kind=sp)                       :: ct

    if ( present(ct_opt) ) then
        ct = ct_opt
    else
        ct = eps_sp3
    endif

    tfloor_sp = real( tfloor_dp(real(x,dp), real(ct,dp)), sp)

end function tfloor_sp

elemental function tceil_sp( x, ct_opt )
    real(kind=sp), intent(in)           :: x
    real(kind=sp), intent(in), optional :: ct_opt
    real(kind=sp)                       :: tceil_sp
    real(kind=sp)                       :: ct

    if ( present(ct_opt) ) then
        ct = ct_opt
    else
        ct = eps_sp3
    endif
    tceil_sp = -real( tfloor_dp(-real(x,dp), real(ct,dp)), sp)

end function tceil_sp

elemental function tround_sp( x, ct_opt )
    real(kind=sp), intent(in)           :: x
    real(kind=sp), intent(in), optional :: ct_opt
    real(kind=sp)                       :: tround_sp
    real(kind=sp)                       :: ct

    if ( present(ct_opt) ) then
        ct = ct_opt
    else
        ct = eps_sp3
    endif
    tround_sp = real( tfloor_dp(real(x,dp) + 0.5_dp, real(ct,dp)), sp)

end function tround_sp

end module stdlib_tolerant
