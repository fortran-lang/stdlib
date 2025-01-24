submodule(stdlib_lapack_base) stdlib_lapack_blas_like_scalar
  implicit none


  contains

     pure logical(lk) module function stdlib_sisnan( sin )
     !! SISNAN returns .TRUE. if its argument is NaN, and .FALSE.
     !! otherwise.  To be replaced by the Fortran 2003 intrinsic in the
     !! future.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: sin
        ! =====================================================================
        ! Executable Statements 
           stdlib_sisnan = stdlib_slaisnan(sin,sin)
           return
     end function stdlib_sisnan

     pure logical(lk) module function stdlib_disnan( din )
     !! DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
     !! otherwise.  To be replaced by the Fortran 2003 intrinsic in the
     !! future.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: din
        ! =====================================================================
        ! Executable Statements 
           stdlib_disnan = stdlib_dlaisnan(din,din)
           return
     end function stdlib_disnan




     pure logical(lk) module function stdlib_slaisnan( sin1, sin2 )
     !! This routine is not for general use.  It exists solely to avoid
     !! over-optimization in SISNAN.
     !! SLAISNAN checks for NaNs by comparing its two arguments for
     !! inequality.  NaN is the only floating-point value where NaN != NaN
     !! returns .TRUE.  To check for NaNs, pass the same variable as both
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
     !! arguments.
     !! A compiler must assume that the two arguments are
     !! not the same variable, and the test will not be optimized away.
     !! Interprocedural or whole-program optimization may delete this
     !! test.  The ISNAN functions will be replaced by the correct
     !! Fortran 03 intrinsic once the intrinsic is widely available.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: sin1, sin2
        ! =====================================================================
        ! Executable Statements 
           stdlib_slaisnan = (sin1/=sin2)
           return
     end function stdlib_slaisnan

     pure logical(lk) module function stdlib_dlaisnan( din1, din2 )
     !! This routine is not for general use.  It exists solely to avoid
     !! over-optimization in DISNAN.
     !! DLAISNAN checks for NaNs by comparing its two arguments for
     !! inequality.  NaN is the only floating-point value where NaN != NaN
     !! returns .TRUE.  To check for NaNs, pass the same variable as both
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
     !! arguments.
     !! A compiler must assume that the two arguments are
     !! not the same variable, and the test will not be optimized away.
     !! Interprocedural or whole-program optimization may delete this
     !! test.  The ISNAN functions will be replaced by the correct
     !! Fortran 03 intrinsic once the intrinsic is widely available.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: din1, din2
        ! =====================================================================
        ! Executable Statements 
           stdlib_dlaisnan = (din1/=din2)
           return
     end function stdlib_dlaisnan




     pure module subroutine stdlib_sladiv( a, b, c, d, p, q )
     !! SLADIV performs complex division in  real arithmetic
     !! a + i*b
     !! p + i*q = ---------
     !! c + i*d
     !! The algorithm is due to Michael Baudin and Robert L. Smith
     !! and can be found in the paper
     !! "A Robust Complex Division in Scilab"
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c, d
           real(sp), intent(out) :: p, q
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: bs = two
           
           
           
           ! Local Scalars 
           real(sp) :: aa, bb, cc, dd, ab, cd, s, ov, un, be, eps
           ! Intrinsic Functions 
           ! Executable Statements 
           aa = a
           bb = b
           cc = c
           dd = d
           ab = max( abs(a), abs(b) )
           cd = max( abs(c), abs(d) )
           s = one
           ov = stdlib_slamch( 'OVERFLOW THRESHOLD' )
           un = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'EPSILON' )
           be = bs / (eps*eps)
           if( ab >= half*ov ) then
              aa = half * aa
              bb = half * bb
              s  = two * s
           end if
           if( cd >= half*ov ) then
              cc = half * cc
              dd = half * dd
              s  = half * s
           end if
           if( ab <= un*bs/eps ) then
              aa = aa * be
              bb = bb * be
              s  = s / be
           end if
           if( cd <= un*bs/eps ) then
              cc = cc * be
              dd = dd * be
              s  = s * be
           end if
           if( abs( d )<=abs( c ) ) then
              call stdlib_sladiv1(aa, bb, cc, dd, p, q)
           else
              call stdlib_sladiv1(bb, aa, dd, cc, p, q)
              q = -q
           end if
           p = p * s
           q = q * s
           return
     end subroutine stdlib_sladiv

     pure module subroutine stdlib_dladiv( a, b, c, d, p, q )
     !! DLADIV performs complex division in  real arithmetic
     !! a + i*b
     !! p + i*q = ---------
     !! c + i*d
     !! The algorithm is due to Michael Baudin and Robert L. Smith
     !! and can be found in the paper
     !! "A Robust Complex Division in Scilab"
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c, d
           real(dp), intent(out) :: p, q
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: bs = two
           
           
           
           ! Local Scalars 
           real(dp) :: aa, bb, cc, dd, ab, cd, s, ov, un, be, eps
           ! Intrinsic Functions 
           ! Executable Statements 
           aa = a
           bb = b
           cc = c
           dd = d
           ab = max( abs(a), abs(b) )
           cd = max( abs(c), abs(d) )
           s = one
           ov = stdlib_dlamch( 'OVERFLOW THRESHOLD' )
           un = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'EPSILON' )
           be = bs / (eps*eps)
           if( ab >= half*ov ) then
              aa = half * aa
              bb = half * bb
              s  = two * s
           end if
           if( cd >= half*ov ) then
              cc = half * cc
              dd = half * dd
              s  = half * s
           end if
           if( ab <= un*bs/eps ) then
              aa = aa * be
              bb = bb * be
              s  = s / be
           end if
           if( cd <= un*bs/eps ) then
              cc = cc * be
              dd = dd * be
              s  = s * be
           end if
           if( abs( d )<=abs( c ) ) then
              call stdlib_dladiv1(aa, bb, cc, dd, p, q)
           else
              call stdlib_dladiv1(bb, aa, dd, cc, p, q)
              q = -q
           end if
           p = p * s
           q = q * s
           return
     end subroutine stdlib_dladiv


     pure complex(sp) module function stdlib_cladiv( x, y )
     !! CLADIV := X / Y, where X and Y are complex.  The computation of X / Y
     !! will not overflow on an intermediary step unless the results
     !! overflows.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(sp), intent(in) :: x, y
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: zi, zr
           ! Intrinsic Functions 
           ! Executable Statements 
           call stdlib_sladiv( real( x,KIND=sp), aimag( x ), real( y,KIND=sp), aimag( y ), zr,zi )
                     
           stdlib_cladiv = cmplx( zr, zi,KIND=sp)
           return
     end function stdlib_cladiv

     pure complex(dp)     module function stdlib_zladiv( x, y )
     !! ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
     !! will not overflow on an intermediary step unless the results
     !! overflows.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(dp), intent(in) :: x, y
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: zi, zr
           ! Intrinsic Functions 
           ! Executable Statements 
           call stdlib_dladiv( real( x,KIND=dp), aimag( x ), real( y,KIND=dp), aimag( y ), zr,zi )
                     
           stdlib_zladiv = cmplx( zr, zi,KIND=dp)
           return
     end function stdlib_zladiv




     pure real(sp) module function stdlib_slapy2( x, y )
     !! SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
     !! overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: x, y
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: w, xabs, yabs, z, hugeval
           logical(lk) :: x_is_nan, y_is_nan
           ! Intrinsic Functions 
           ! Executable Statements 
           x_is_nan = stdlib_sisnan( x )
           y_is_nan = stdlib_sisnan( y )
           if ( x_is_nan ) stdlib_slapy2 = x
           if ( y_is_nan ) stdlib_slapy2 = y
           hugeval = stdlib_slamch( 'OVERFLOW' )
           if ( .not.( x_is_nan.or.y_is_nan ) ) then
              xabs = abs( x )
              yabs = abs( y )
              w = max( xabs, yabs )
              z = min( xabs, yabs )
              if( z==zero .or. w>hugeval ) then
                 stdlib_slapy2 = w
              else
                 stdlib_slapy2 = w*sqrt( one+( z / w )**2_ilp )
              end if
           end if
           return
     end function stdlib_slapy2

     pure real(dp) module function stdlib_dlapy2( x, y )
     !! DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
     !! overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: x, y
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: w, xabs, yabs, z, hugeval
           logical(lk) :: x_is_nan, y_is_nan
           ! Intrinsic Functions 
           ! Executable Statements 
           x_is_nan = stdlib_disnan( x )
           y_is_nan = stdlib_disnan( y )
           if ( x_is_nan ) stdlib_dlapy2 = x
           if ( y_is_nan ) stdlib_dlapy2 = y
           hugeval = stdlib_dlamch( 'OVERFLOW' )
           if ( .not.( x_is_nan.or.y_is_nan ) ) then
              xabs = abs( x )
              yabs = abs( y )
              w = max( xabs, yabs )
              z = min( xabs, yabs )
              if( z==zero .or. w>hugeval ) then
                 stdlib_dlapy2 = w
              else
                 stdlib_dlapy2 = w*sqrt( one+( z / w )**2_ilp )
              end if
           end if
           return
     end function stdlib_dlapy2




     pure real(sp) module function stdlib_slapy3( x, y, z )
     !! SLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
     !! unnecessary overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: x, y, z
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: w, xabs, yabs, zabs, hugeval
           ! Intrinsic Functions 
           ! Executable Statements 
           hugeval = stdlib_slamch( 'OVERFLOW' )
           xabs = abs( x )
           yabs = abs( y )
           zabs = abs( z )
           w = max( xabs, yabs, zabs )
           if( w==zero .or. w>hugeval ) then
           ! w can be zero for max(0,nan,0)
           ! adding all three entries together will make sure
           ! nan will not disappear.
              stdlib_slapy3 =  xabs + yabs + zabs
           else
              stdlib_slapy3 = w*sqrt( ( xabs / w )**2_ilp+( yabs / w )**2_ilp+( zabs / w )**2_ilp )
           end if
           return
     end function stdlib_slapy3

     pure real(dp) module function stdlib_dlapy3( x, y, z )
     !! DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
     !! unnecessary overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: x, y, z
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: w, xabs, yabs, zabs, hugeval
           ! Intrinsic Functions 
           ! Executable Statements 
           hugeval = stdlib_dlamch( 'OVERFLOW' )
           xabs = abs( x )
           yabs = abs( y )
           zabs = abs( z )
           w = max( xabs, yabs, zabs )
           if( w==zero .or. w>hugeval ) then
           ! w can be zero for max(0,nan,0)
           ! adding all three entries together will make sure
           ! nan will not disappear.
              stdlib_dlapy3 =  xabs + yabs + zabs
           else
              stdlib_dlapy3 = w*sqrt( ( xabs / w )**2_ilp+( yabs / w )**2_ilp+( zabs / w )**2_ilp )
           end if
           return
     end function stdlib_dlapy3




     pure logical(lk) module function stdlib_I64_sisnan( sin )
     !! SISNAN returns .TRUE. if its argument is NaN, and .FALSE.
     !! otherwise.  To be replaced by the Fortran 2003 intrinsic in the
     !! future.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: sin
        ! =====================================================================
        ! Executable Statements 
           stdlib_I64_sisnan = stdlib_I64_slaisnan(sin,sin)
           return
     end function stdlib_I64_sisnan

     pure logical(lk) module function stdlib_I64_disnan( din )
     !! DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
     !! otherwise.  To be replaced by the Fortran 2003 intrinsic in the
     !! future.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: din
        ! =====================================================================
        ! Executable Statements 
           stdlib_I64_disnan = stdlib_I64_dlaisnan(din,din)
           return
     end function stdlib_I64_disnan




     pure logical(lk) module function stdlib_I64_slaisnan( sin1, sin2 )
     !! This routine is not for general use.  It exists solely to avoid
     !! over-optimization in SISNAN.
     !! SLAISNAN checks for NaNs by comparing its two arguments for
     !! inequality.  NaN is the only floating-point value where NaN != NaN
     !! returns .TRUE.  To check for NaNs, pass the same variable as both
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
     !! arguments.
     !! A compiler must assume that the two arguments are
     !! not the same variable, and the test will not be optimized away.
     !! Interprocedural or whole-program optimization may delete this
     !! test.  The ISNAN functions will be replaced by the correct
     !! Fortran 03 intrinsic once the intrinsic is widely available.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: sin1, sin2
        ! =====================================================================
        ! Executable Statements 
           stdlib_I64_slaisnan = (sin1/=sin2)
           return
     end function stdlib_I64_slaisnan

     pure logical(lk) module function stdlib_I64_dlaisnan( din1, din2 )
     !! This routine is not for general use.  It exists solely to avoid
     !! over-optimization in DISNAN.
     !! DLAISNAN checks for NaNs by comparing its two arguments for
     !! inequality.  NaN is the only floating-point value where NaN != NaN
     !! returns .TRUE.  To check for NaNs, pass the same variable as both
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
     !! arguments.
     !! A compiler must assume that the two arguments are
     !! not the same variable, and the test will not be optimized away.
     !! Interprocedural or whole-program optimization may delete this
     !! test.  The ISNAN functions will be replaced by the correct
     !! Fortran 03 intrinsic once the intrinsic is widely available.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: din1, din2
        ! =====================================================================
        ! Executable Statements 
           stdlib_I64_dlaisnan = (din1/=din2)
           return
     end function stdlib_I64_dlaisnan




     pure module subroutine stdlib_I64_sladiv( a, b, c, d, p, q )
     !! SLADIV performs complex division in  real arithmetic
     !! a + i*b
     !! p + i*q = ---------
     !! c + i*d
     !! The algorithm is due to Michael Baudin and Robert L. Smith
     !! and can be found in the paper
     !! "A Robust Complex Division in Scilab"
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c, d
           real(sp), intent(out) :: p, q
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: bs = two
           
           
           
           ! Local Scalars 
           real(sp) :: aa, bb, cc, dd, ab, cd, s, ov, un, be, eps
           ! Intrinsic Functions 
           ! Executable Statements 
           aa = a
           bb = b
           cc = c
           dd = d
           ab = max( abs(a), abs(b) )
           cd = max( abs(c), abs(d) )
           s = one
           ov = stdlib_I64_slamch( 'OVERFLOW THRESHOLD' )
           un = stdlib_I64_slamch( 'SAFE MINIMUM' )
           eps = stdlib_I64_slamch( 'EPSILON' )
           be = bs / (eps*eps)
           if( ab >= half*ov ) then
              aa = half * aa
              bb = half * bb
              s  = two * s
           end if
           if( cd >= half*ov ) then
              cc = half * cc
              dd = half * dd
              s  = half * s
           end if
           if( ab <= un*bs/eps ) then
              aa = aa * be
              bb = bb * be
              s  = s / be
           end if
           if( cd <= un*bs/eps ) then
              cc = cc * be
              dd = dd * be
              s  = s * be
           end if
           if( abs( d )<=abs( c ) ) then
              call stdlib_I64_sladiv1(aa, bb, cc, dd, p, q)
           else
              call stdlib_I64_sladiv1(bb, aa, dd, cc, p, q)
              q = -q
           end if
           p = p * s
           q = q * s
           return
     end subroutine stdlib_I64_sladiv

     pure module subroutine stdlib_I64_dladiv( a, b, c, d, p, q )
     !! DLADIV performs complex division in  real arithmetic
     !! a + i*b
     !! p + i*q = ---------
     !! c + i*d
     !! The algorithm is due to Michael Baudin and Robert L. Smith
     !! and can be found in the paper
     !! "A Robust Complex Division in Scilab"
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c, d
           real(dp), intent(out) :: p, q
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: bs = two
           
           
           
           ! Local Scalars 
           real(dp) :: aa, bb, cc, dd, ab, cd, s, ov, un, be, eps
           ! Intrinsic Functions 
           ! Executable Statements 
           aa = a
           bb = b
           cc = c
           dd = d
           ab = max( abs(a), abs(b) )
           cd = max( abs(c), abs(d) )
           s = one
           ov = stdlib_I64_dlamch( 'OVERFLOW THRESHOLD' )
           un = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_I64_dlamch( 'EPSILON' )
           be = bs / (eps*eps)
           if( ab >= half*ov ) then
              aa = half * aa
              bb = half * bb
              s  = two * s
           end if
           if( cd >= half*ov ) then
              cc = half * cc
              dd = half * dd
              s  = half * s
           end if
           if( ab <= un*bs/eps ) then
              aa = aa * be
              bb = bb * be
              s  = s / be
           end if
           if( cd <= un*bs/eps ) then
              cc = cc * be
              dd = dd * be
              s  = s * be
           end if
           if( abs( d )<=abs( c ) ) then
              call stdlib_I64_dladiv1(aa, bb, cc, dd, p, q)
           else
              call stdlib_I64_dladiv1(bb, aa, dd, cc, p, q)
              q = -q
           end if
           p = p * s
           q = q * s
           return
     end subroutine stdlib_I64_dladiv


     pure complex(sp) module function stdlib_I64_cladiv( x, y )
     !! CLADIV := X / Y, where X and Y are complex.  The computation of X / Y
     !! will not overflow on an intermediary step unless the results
     !! overflows.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(sp), intent(in) :: x, y
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: zi, zr
           ! Intrinsic Functions 
           ! Executable Statements 
           call stdlib_I64_sladiv( real( x,KIND=sp), aimag( x ), real( y,KIND=sp), aimag( y ), zr,zi )
                     
           stdlib_I64_cladiv = cmplx( zr, zi,KIND=sp)
           return
     end function stdlib_I64_cladiv

     pure complex(dp)     module function stdlib_I64_zladiv( x, y )
     !! ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
     !! will not overflow on an intermediary step unless the results
     !! overflows.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(dp), intent(in) :: x, y
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: zi, zr
           ! Intrinsic Functions 
           ! Executable Statements 
           call stdlib_I64_dladiv( real( x,KIND=dp), aimag( x ), real( y,KIND=dp), aimag( y ), zr,zi )
                     
           stdlib_I64_zladiv = cmplx( zr, zi,KIND=dp)
           return
     end function stdlib_I64_zladiv




     pure real(sp) module function stdlib_I64_slapy2( x, y )
     !! SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
     !! overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: x, y
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: w, xabs, yabs, z, hugeval
           logical(lk) :: x_is_nan, y_is_nan
           ! Intrinsic Functions 
           ! Executable Statements 
           x_is_nan = stdlib_I64_sisnan( x )
           y_is_nan = stdlib_I64_sisnan( y )
           if ( x_is_nan ) stdlib_I64_slapy2 = x
           if ( y_is_nan ) stdlib_I64_slapy2 = y
           hugeval = stdlib_I64_slamch( 'OVERFLOW' )
           if ( .not.( x_is_nan.or.y_is_nan ) ) then
              xabs = abs( x )
              yabs = abs( y )
              w = max( xabs, yabs )
              z = min( xabs, yabs )
              if( z==zero .or. w>hugeval ) then
                 stdlib_I64_slapy2 = w
              else
                 stdlib_I64_slapy2 = w*sqrt( one+( z / w )**2_ilp64 )
              end if
           end if
           return
     end function stdlib_I64_slapy2

     pure real(dp) module function stdlib_I64_dlapy2( x, y )
     !! DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
     !! overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: x, y
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: w, xabs, yabs, z, hugeval
           logical(lk) :: x_is_nan, y_is_nan
           ! Intrinsic Functions 
           ! Executable Statements 
           x_is_nan = stdlib_I64_disnan( x )
           y_is_nan = stdlib_I64_disnan( y )
           if ( x_is_nan ) stdlib_I64_dlapy2 = x
           if ( y_is_nan ) stdlib_I64_dlapy2 = y
           hugeval = stdlib_I64_dlamch( 'OVERFLOW' )
           if ( .not.( x_is_nan.or.y_is_nan ) ) then
              xabs = abs( x )
              yabs = abs( y )
              w = max( xabs, yabs )
              z = min( xabs, yabs )
              if( z==zero .or. w>hugeval ) then
                 stdlib_I64_dlapy2 = w
              else
                 stdlib_I64_dlapy2 = w*sqrt( one+( z / w )**2_ilp64 )
              end if
           end if
           return
     end function stdlib_I64_dlapy2




     pure real(sp) module function stdlib_I64_slapy3( x, y, z )
     !! SLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
     !! unnecessary overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: x, y, z
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: w, xabs, yabs, zabs, hugeval
           ! Intrinsic Functions 
           ! Executable Statements 
           hugeval = stdlib_I64_slamch( 'OVERFLOW' )
           xabs = abs( x )
           yabs = abs( y )
           zabs = abs( z )
           w = max( xabs, yabs, zabs )
           if( w==zero .or. w>hugeval ) then
           ! w can be zero for max(0,nan,0)
           ! adding all three entries together will make sure
           ! nan will not disappear.
              stdlib_I64_slapy3 =  xabs + yabs + zabs
           else
              stdlib_I64_slapy3 = w*sqrt( ( xabs / w )**2_ilp64+( yabs / w )**2_ilp64+( zabs / w )**2_ilp64 )
           end if
           return
     end function stdlib_I64_slapy3

     pure real(dp) module function stdlib_I64_dlapy3( x, y, z )
     !! DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
     !! unnecessary overflow and unnecessary underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: x, y, z
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: w, xabs, yabs, zabs, hugeval
           ! Intrinsic Functions 
           ! Executable Statements 
           hugeval = stdlib_I64_dlamch( 'OVERFLOW' )
           xabs = abs( x )
           yabs = abs( y )
           zabs = abs( z )
           w = max( xabs, yabs, zabs )
           if( w==zero .or. w>hugeval ) then
           ! w can be zero for max(0,nan,0)
           ! adding all three entries together will make sure
           ! nan will not disappear.
              stdlib_I64_dlapy3 =  xabs + yabs + zabs
           else
              stdlib_I64_dlapy3 = w*sqrt( ( xabs / w )**2_ilp64+( yabs / w )**2_ilp64+( zabs / w )**2_ilp64 )
           end if
           return
     end function stdlib_I64_dlapy3



end submodule stdlib_lapack_blas_like_scalar
