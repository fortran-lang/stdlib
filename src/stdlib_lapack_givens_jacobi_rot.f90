submodule(stdlib_lapack_base) stdlib_lapack_givens_jacobi_rot
  implicit none


  contains

     pure module subroutine stdlib_slartg( f, g, c, s, r )
     !! SLARTG generates a plane rotation so that
     !! [  C  S  ]  .  [ F ]  =  [ R ]
     !! [ -S  C  ]     [ G ]     [ 0 ]
     !! where C**2 + S**2 = 1.
     !! The mathematical formulas used for C and S are
     !! R = sign(F) * sqrt(F**2 + G**2)
     !! C = F / R
     !! S = G / R
     !! Hence C >= 0. The algorithm used to compute these quantities
     !! incorporates scaling to avoid overflow or underflow in computing the
     !! square root of the sum of squares.
     !! This version is discontinuous in R at F = 0 but it returns the same
     !! C and S as SLARTG for complex inputs (F,0) and (G,0).
     !! This is a more accurate version of the BLAS1 routine SROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0 and (G .ne. 0), then C=0 and S=sign(1,G) without doing any
     !! floating point operations (saves work in SBDSQR when
     !! there are zeros on the diagonal).
     !! If F exceeds G in magnitude, C will be positive.
     !! Below, wp=>sp stands for single precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_sp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(sp), intent(out) :: c, r, s
        real(sp), intent(in) :: f, g
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: d, f1, fs, g1, gs, p, u, uu
        ! Intrinsic Functions 
        ! Executable Statements 
        f1 = abs( f )
        g1 = abs( g )
        if( g == zero ) then
           c = one
           s = zero
           r = f
        else if( f == zero ) then
           c = zero
           s = sign( one, g )
           r = g1
     else if( f1 > rtmin .and. f1 < rtmax .and.            g1 > rtmin .and. g1 < rtmax ) &
               then
           d = sqrt( f*f + g*g )
           p = one / d
           c = f1*p
           s = g*sign( p, f )
           r = sign( d, f )
        else
           u = min( safmax, max( safmin, f1, g1 ) )
           uu = one / u
           fs = f*uu
           gs = g*uu
           d = sqrt( fs*fs + gs*gs )
           p = one / d
           c = abs( fs )*p
           s = gs*sign( p, f )
           r = sign( d, f )*u
        end if
        return
     end subroutine stdlib_slartg

     pure module subroutine stdlib_dlartg( f, g, c, s, r )
     !! DLARTG generates a plane rotation so that
     !! [  C  S  ]  .  [ F ]  =  [ R ]
     !! [ -S  C  ]     [ G ]     [ 0 ]
     !! where C**2 + S**2 = 1.
     !! The mathematical formulas used for C and S are
     !! R = sign(F) * sqrt(F**2 + G**2)
     !! C = F / R
     !! S = G / R
     !! Hence C >= 0. The algorithm used to compute these quantities
     !! incorporates scaling to avoid overflow or underflow in computing the
     !! square root of the sum of squares.
     !! This version is discontinuous in R at F = 0 but it returns the same
     !! C and S as ZLARTG for complex inputs (F,0) and (G,0).
     !! This is a more accurate version of the BLAS1 routine DROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0 and (G .ne. 0), then C=0 and S=sign(1,G) without doing any
     !! floating point operations (saves work in DBDSQR when
     !! there are zeros on the diagonal).
     !! If F exceeds G in magnitude, C will be positive.
     !! Below, wp=>dp stands for double precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_dp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(dp), intent(out) :: c, r, s
        real(dp), intent(in) :: f, g
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: d, f1, fs, g1, gs, p, u, uu
        ! Intrinsic Functions 
        ! Executable Statements 
        f1 = abs( f )
        g1 = abs( g )
        if( g == zero ) then
           c = one
           s = zero
           r = f
        else if( f == zero ) then
           c = zero
           s = sign( one, g )
           r = g1
     else if( f1 > rtmin .and. f1 < rtmax .and.            g1 > rtmin .and. g1 < rtmax ) &
               then
           d = sqrt( f*f + g*g )
           p = one / d
           c = f1*p
           s = g*sign( p, f )
           r = sign( d, f )
        else
           u = min( safmax, max( safmin, f1, g1 ) )
           uu = one / u
           fs = f*uu
           gs = g*uu
           d = sqrt( fs*fs + gs*gs )
           p = one / d
           c = abs( fs )*p
           s = gs*sign( p, f )
           r = sign( d, f )*u
        end if
        return
     end subroutine stdlib_dlartg


     pure module subroutine stdlib_clartg( f, g, c, s, r )
     !! CLARTG generates a plane rotation so that
     !! [  C         S  ] . [ F ]  =  [ R ]
     !! [ -conjg(S)  C  ]   [ G ]     [ 0 ]
     !! where C is real and C**2 + |S|**2 = 1.
     !! The mathematical formulas used for C and S are
     !! sgn(x) = {  x / |x|,   x != 0
     !! {  1,         x = 0
     !! R = sgn(F) * sqrt(|F|**2 + |G|**2)
     !! C = |F| / sqrt(|F|**2 + |G|**2)
     !! S = sgn(F) * conjg(G) / sqrt(|F|**2 + |G|**2)
     !! When F and G are real, the formulas simplify to C = F/R and
     !! S = G/R, and the returned values of C, S, and R should be
     !! identical to those returned by CLARTG.
     !! The algorithm used to compute these quantities incorporates scaling
     !! to avoid overflow or underflow in computing the square root of the
     !! sum of squares.
     !! This is a faster version of the BLAS1 routine CROTG, except for
     !! the following differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0, then C=0 and S is chosen so that R is real.
     !! Below, wp=>sp stands for single precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_sp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(sp), intent(out) :: c
        complex(sp), intent(in) :: f, g
        complex(sp), intent(out) :: r, s
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(sp) :: fs, gs, t
        ! Intrinsic Functions 
        ! Statement Functions 
        real(sp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=sp)**2_ilp + aimag( t )**2_ilp
        ! Executable Statements 
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=sp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2_ilp + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        return
     end subroutine stdlib_clartg

     pure module subroutine stdlib_zlartg( f, g, c, s, r )
     !! ZLARTG generates a plane rotation so that
     !! [  C         S  ] . [ F ]  =  [ R ]
     !! [ -conjg(S)  C  ]   [ G ]     [ 0 ]
     !! where C is real and C**2 + |S|**2 = 1.
     !! The mathematical formulas used for C and S are
     !! sgn(x) = {  x / |x|,   x != 0
     !! {  1,         x = 0
     !! R = sgn(F) * sqrt(|F|**2 + |G|**2)
     !! C = |F| / sqrt(|F|**2 + |G|**2)
     !! S = sgn(F) * conjg(G) / sqrt(|F|**2 + |G|**2)
     !! When F and G are real, the formulas simplify to C = F/R and
     !! S = G/R, and the returned values of C, S, and R should be
     !! identical to those returned by DLARTG.
     !! The algorithm used to compute these quantities incorporates scaling
     !! to avoid overflow or underflow in computing the square root of the
     !! sum of squares.
     !! This is a faster version of the BLAS1 routine ZROTG, except for
     !! the following differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0, then C=0 and S is chosen so that R is real.
     !! Below, wp=>dp stands for double precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_dp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(dp), intent(out) :: c
        complex(dp), intent(in) :: f, g
        complex(dp), intent(out) :: r, s
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(dp) :: fs, gs, t
        ! Intrinsic Functions 
        ! Statement Functions 
        real(dp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=dp)**2_ilp + aimag( t )**2_ilp
        ! Executable Statements 
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=dp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2_ilp + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        return
     end subroutine stdlib_zlartg




     pure module subroutine stdlib_slartgp( f, g, cs, sn, r )
     !! SLARTGP generates a plane rotation so that
     !! [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
     !! [ -SN  CS  ]     [ G ]     [ 0 ]
     !! This is a slower, more accurate version of the Level 1 BLAS routine SROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then CS=(+/-)1 and SN=0.
     !! If F=0 and (G .ne. 0), then CS=0 and SN=(+/-)1.
     !! The sign is chosen so that R >= 0.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(out) :: cs, r, sn
           real(sp), intent(in) :: f, g
        ! =====================================================================
           
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp) :: count, i
           real(sp) :: eps, f1, g1, safmin, safmn2, safmx2, scale
           ! Intrinsic Functions 
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Executable Statements 
           ! if( first ) then
              safmin = stdlib_slamch( 'S' )
              eps = stdlib_slamch( 'E' )
              safmn2 = stdlib_slamch( 'B' )**int( log( safmin / eps ) /log( stdlib_slamch( 'B' ) )&
                         / two,KIND=ilp)
              safmx2 = one / safmn2
              ! first = .false.
           ! end if
           if( g==zero ) then
              cs = sign( one, f )
              sn = zero
              r = abs( f )
           else if( f==zero ) then
              cs = zero
              sn = sign( one, g )
              r = abs( g )
           else
              f1 = f
              g1 = g
              scale = max( abs( f1 ), abs( g1 ) )
              if( scale>=safmx2 ) then
                 count = 0_ilp
                 10 continue
                 count = count + 1_ilp
                 f1 = f1*safmn2
                 g1 = g1*safmn2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale>=safmx2 .and. count < 20)go to 10
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmx2
                 end do
              else if( scale<=safmn2 ) then
                 count = 0_ilp
                 30 continue
                 count = count + 1_ilp
                 f1 = f1*safmx2
                 g1 = g1*safmx2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale<=safmn2 )go to 30
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmn2
                 end do
              else
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
              end if
              if( r<zero ) then
                 cs = -cs
                 sn = -sn
                 r = -r
              end if
           end if
           return
     end subroutine stdlib_slartgp

     pure module subroutine stdlib_dlartgp( f, g, cs, sn, r )
     !! DLARTGP generates a plane rotation so that
     !! [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
     !! [ -SN  CS  ]     [ G ]     [ 0 ]
     !! This is a slower, more accurate version of the Level 1 BLAS routine DROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then CS=(+/-)1 and SN=0.
     !! If F=0 and (G .ne. 0), then CS=0 and SN=(+/-)1.
     !! The sign is chosen so that R >= 0.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(out) :: cs, r, sn
           real(dp), intent(in) :: f, g
        ! =====================================================================
           
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp) :: count, i
           real(dp) :: eps, f1, g1, safmin, safmn2, safmx2, scale
           ! Intrinsic Functions 
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Executable Statements 
           ! if( first ) then
              safmin = stdlib_dlamch( 'S' )
              eps = stdlib_dlamch( 'E' )
              safmn2 = stdlib_dlamch( 'B' )**int( log( safmin / eps ) /log( stdlib_dlamch( 'B' ) )&
                         / two,KIND=ilp)
              safmx2 = one / safmn2
              ! first = .false.
           ! end if
           if( g==zero ) then
              cs = sign( one, f )
              sn = zero
              r = abs( f )
           else if( f==zero ) then
              cs = zero
              sn = sign( one, g )
              r = abs( g )
           else
              f1 = f
              g1 = g
              scale = max( abs( f1 ), abs( g1 ) )
              if( scale>=safmx2 ) then
                 count = 0_ilp
                 10 continue
                 count = count + 1_ilp
                 f1 = f1*safmn2
                 g1 = g1*safmn2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale>=safmx2 .and. count < 20 )go to 10
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmx2
                 end do
              else if( scale<=safmn2 ) then
                 count = 0_ilp
                 30 continue
                 count = count + 1_ilp
                 f1 = f1*safmx2
                 g1 = g1*safmx2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale<=safmn2 )go to 30
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmn2
                 end do
              else
                 r = sqrt( f1**2_ilp+g1**2_ilp )
                 cs = f1 / r
                 sn = g1 / r
              end if
              if( r<zero ) then
                 cs = -cs
                 sn = -sn
                 r = -r
              end if
           end if
           return
     end subroutine stdlib_dlartgp




     pure module subroutine stdlib_slasr( side, pivot, direct, m, n, c, s, a, lda )
     !! SLASR applies a sequence of plane rotations to a real matrix A,
     !! from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: c(*), s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           real(sp) :: ctemp, stemp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp
           else if( m<0_ilp ) then
              info = 4_ilp
           else if( n<0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_slasr

     pure module subroutine stdlib_dlasr( side, pivot, direct, m, n, c, s, a, lda )
     !! DLASR applies a sequence of plane rotations to a real matrix A,
     !! from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: c(*), s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           real(dp) :: ctemp, stemp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp
           else if( m<0_ilp ) then
              info = 4_ilp
           else if( n<0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dlasr


     pure module subroutine stdlib_clasr( side, pivot, direct, m, n, c, s, a, lda )
     !! CLASR applies a sequence of real plane rotations to a complex matrix
     !! A, from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           real(sp) :: ctemp, stemp
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp
           else if( m<0_ilp ) then
              info = 4_ilp
           else if( n<0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_clasr

     pure module subroutine stdlib_zlasr( side, pivot, direct, m, n, c, s, a, lda )
     !! ZLASR applies a sequence of real plane rotations to a complex matrix
     !! A, from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           real(dp) :: ctemp, stemp
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp
           else if( m<0_ilp ) then
              info = 4_ilp
           else if( n<0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp, i )
                             a( 1_ilp, i ) = stemp*temp + ctemp*a( 1_ilp, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp )
                             a( i, 1_ilp ) = stemp*temp + ctemp*a( i, 1_ilp )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_zlasr




     pure module subroutine stdlib_slargv( n, x, incx, y, incy, c, incc )
     !! SLARGV generates a vector of real plane rotations, determined by
     !! elements of the real vectors x and y. For i = 1,2,...,n
     !! (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
     !! ( -s(i)  c(i) ) ( y(i) ) = (   0  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(out) :: c(*)
           real(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           real(sp) :: f, g, t, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           loop_10: do i = 1, n
              f = x( ix )
              g = y( iy )
              if( g==zero ) then
                 c( ic ) = one
              else if( f==zero ) then
                 c( ic ) = zero
                 y( iy ) = one
                 x( ix ) = g
              else if( abs( f )>abs( g ) ) then
                 t = g / f
                 tt = sqrt( one+t*t )
                 c( ic ) = one / tt
                 y( iy ) = t*c( ic )
                 x( ix ) = f*tt
              else
                 t = f / g
                 tt = sqrt( one+t*t )
                 y( iy ) = one / tt
                 c( ic ) = t*y( iy )
                 x( ix ) = g*tt
              end if
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_10
           return
     end subroutine stdlib_slargv

     pure module subroutine stdlib_dlargv( n, x, incx, y, incy, c, incc )
     !! DLARGV generates a vector of real plane rotations, determined by
     !! elements of the real vectors x and y. For i = 1,2,...,n
     !! (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
     !! ( -s(i)  c(i) ) ( y(i) ) = (   0  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(out) :: c(*)
           real(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           real(dp) :: f, g, t, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           loop_10: do i = 1, n
              f = x( ix )
              g = y( iy )
              if( g==zero ) then
                 c( ic ) = one
              else if( f==zero ) then
                 c( ic ) = zero
                 y( iy ) = one
                 x( ix ) = g
              else if( abs( f )>abs( g ) ) then
                 t = g / f
                 tt = sqrt( one+t*t )
                 c( ic ) = one / tt
                 y( iy ) = t*c( ic )
                 x( ix ) = f*tt
              else
                 t = f / g
                 tt = sqrt( one+t*t )
                 y( iy ) = one / tt
                 c( ic ) = t*y( iy )
                 x( ix ) = g*tt
              end if
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_10
           return
     end subroutine stdlib_dlargv


     pure module subroutine stdlib_clargv( n, x, incx, y, incy, c, incc )
     !! CLARGV generates a vector of complex plane rotations with real
     !! cosines, determined by elements of the complex vectors x and y.
     !! For i = 1,2,...,n
     !! (        c(i)   s(i) ) ( x(i) ) = ( r(i) )
     !! ( -conjg(s(i))  c(i) ) ( y(i) ) = (   0  )
     !! where c(i)**2 + ABS(s(i))**2 = 1
     !! The following conventions are used (these are the same as in CLARTG,
     !! but differ from the BLAS1 routine CROTG):
     !! If y(i)=0, then c(i)=1 and s(i)=0.
     !! If x(i)=0, then c(i)=0 and s(i) is chosen so that r(i) is real.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(out) :: c(*)
           complex(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp) :: count, i, ic, ix, iy, j
           real(sp) :: cs, d, di, dr, eps, f2, f2s, g2, g2s, safmin, safmn2, safmx2, scale
           complex(sp) :: f, ff, fs, g, gs, r, sn
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1, abssq
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Statement Function Definitions 
           abs1( ff ) = max( abs( real( ff,KIND=sp) ), abs( aimag( ff ) ) )
           abssq( ff ) = real( ff,KIND=sp)**2_ilp + aimag( ff )**2_ilp
           ! Executable Statements 
           ! if( first ) then
              ! first = .false.
              safmin = stdlib_slamch( 'S' )
              eps = stdlib_slamch( 'E' )
              safmn2 = stdlib_slamch( 'B' )**int( log( safmin / eps ) /log( stdlib_slamch( 'B' ) )&
                         / two,KIND=ilp)
              safmx2 = one / safmn2
           ! end if
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           loop_60: do i = 1, n
              f = x( ix )
              g = y( iy )
              ! use identical algorithm as in stdlib_clartg
              scale = max( abs1( f ), abs1( g ) )
              fs = f
              gs = g
              count = 0_ilp
              if( scale>=safmx2 ) then
              10 continue
                 count = count + 1_ilp
                 fs = fs*safmn2
                 gs = gs*safmn2
                 scale = scale*safmn2
                 if( scale>=safmx2 .and. count < 20 )go to 10
              else if( scale<=safmn2 ) then
                 if( g==czero ) then
                    cs = one
                    sn = czero
                    r = f
                    go to 50
                 end if
                 20 continue
                 count = count - 1_ilp
                 fs = fs*safmx2
                 gs = gs*safmx2
                 scale = scale*safmx2
                 if( scale<=safmn2 )go to 20
              end if
              f2 = abssq( fs )
              g2 = abssq( gs )
              if( f2<=max( g2, one )*safmin ) then
                 ! this is a rare case: f is very small.
                 if( f==czero ) then
                    cs = zero
                    r = stdlib_slapy2( real( g,KIND=sp), aimag( g ) )
                    ! do complex/real division explicitly with two real
                    ! divisions
                    d = stdlib_slapy2( real( gs,KIND=sp), aimag( gs ) )
                    sn = cmplx( real( gs,KIND=sp) / d, -aimag( gs ) / d,KIND=sp)
                    go to 50
                 end if
                 f2s = stdlib_slapy2( real( fs,KIND=sp), aimag( fs ) )
                 ! g2 and g2s are accurate
                 ! g2 is at least safmin, and g2s is at least safmn2
                 g2s = sqrt( g2 )
                 ! error in cs from underflow in f2s is at most
                 ! unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
                 ! if max(g2,one)=g2, then f2 .lt. g2*safmin,
                 ! and so cs .lt. sqrt(safmin)
                 ! if max(g2,one)=one, then f2 .lt. safmin
                 ! and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
                 ! therefore, cs = f2s/g2s / sqrt( 1 + (f2s/g2s)**2 ) = f2s/g2s
                 cs = f2s / g2s
                 ! make sure abs(ff) = 1
                 ! do complex/real division explicitly with 2 real divisions
                 if( abs1( f )>one ) then
                    d = stdlib_slapy2( real( f,KIND=sp), aimag( f ) )
                    ff = cmplx( real( f,KIND=sp) / d, aimag( f ) / d,KIND=sp)
                 else
                    dr = safmx2*real( f,KIND=sp)
                    di = safmx2*aimag( f )
                    d = stdlib_slapy2( dr, di )
                    ff = cmplx( dr / d, di / d,KIND=sp)
                 end if
                 sn = ff*cmplx( real( gs,KIND=sp) / g2s, -aimag( gs ) / g2s,KIND=sp)
                 r = cs*f + sn*g
              else
                 ! this is the most common case.
                 ! neither f2 nor f2/g2 are less than safmin
                 ! f2s cannot overflow, and it is accurate
                 f2s = sqrt( one+g2 / f2 )
                 ! do the f2s(real)*fs(complex) multiply with two real
                 ! multiplies
                 r = cmplx( f2s*real( fs,KIND=sp), f2s*aimag( fs ),KIND=sp)
                 cs = one / f2s
                 d = f2 + g2
                 ! do complex/real division explicitly with two real divisions
                 sn = cmplx( real( r,KIND=sp) / d, aimag( r ) / d,KIND=sp)
                 sn = sn*conjg( gs )
                 if( count/=0_ilp ) then
                    if( count>0_ilp ) then
                       do j = 1, count
                          r = r*safmx2
                       end do
                    else
                       do j = 1, -count
                          r = r*safmn2
                       end do
                    end if
                 end if
              end if
              50 continue
              c( ic ) = cs
              y( iy ) = sn
              x( ix ) = r
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_60
           return
     end subroutine stdlib_clargv

     pure module subroutine stdlib_zlargv( n, x, incx, y, incy, c, incc )
     !! ZLARGV generates a vector of complex plane rotations with real
     !! cosines, determined by elements of the complex vectors x and y.
     !! For i = 1,2,...,n
     !! (        c(i)   s(i) ) ( x(i) ) = ( r(i) )
     !! ( -conjg(s(i))  c(i) ) ( y(i) ) = (   0  )
     !! where c(i)**2 + ABS(s(i))**2 = 1
     !! The following conventions are used (these are the same as in ZLARTG,
     !! but differ from the BLAS1 routine ZROTG):
     !! If y(i)=0, then c(i)=1 and s(i)=0.
     !! If x(i)=0, then c(i)=0 and s(i) is chosen so that r(i) is real.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(out) :: c(*)
           complex(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp) :: count, i, ic, ix, iy, j
           real(dp) :: cs, d, di, dr, eps, f2, f2s, g2, g2s, safmin, safmn2, safmx2, scale
           complex(dp) :: f, ff, fs, g, gs, r, sn
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1, abssq
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Statement Function Definitions 
           abs1( ff ) = max( abs( real( ff,KIND=dp) ), abs( aimag( ff ) ) )
           abssq( ff ) = real( ff,KIND=dp)**2_ilp + aimag( ff )**2_ilp
           ! Executable Statements 
           ! if( first ) then
              ! first = .false.
              safmin = stdlib_dlamch( 'S' )
              eps = stdlib_dlamch( 'E' )
              safmn2 = stdlib_dlamch( 'B' )**int( log( safmin / eps ) /log( stdlib_dlamch( 'B' ) )&
                         / two,KIND=ilp)
              safmx2 = one / safmn2
           ! end if
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           loop_60: do i = 1, n
              f = x( ix )
              g = y( iy )
              ! use identical algorithm as in stdlib_zlartg
              scale = max( abs1( f ), abs1( g ) )
              fs = f
              gs = g
              count = 0_ilp
              if( scale>=safmx2 ) then
              10 continue
                 count = count + 1_ilp
                 fs = fs*safmn2
                 gs = gs*safmn2
                 scale = scale*safmn2
                 if( scale>=safmx2 .and. count < 20 )go to 10
              else if( scale<=safmn2 ) then
                 if( g==czero ) then
                    cs = one
                    sn = czero
                    r = f
                    go to 50
                 end if
                 20 continue
                 count = count - 1_ilp
                 fs = fs*safmx2
                 gs = gs*safmx2
                 scale = scale*safmx2
                 if( scale<=safmn2 )go to 20
              end if
              f2 = abssq( fs )
              g2 = abssq( gs )
              if( f2<=max( g2, one )*safmin ) then
                 ! this is a rare case: f is very small.
                 if( f==czero ) then
                    cs = zero
                    r = stdlib_dlapy2( real( g,KIND=dp), aimag( g ) )
                    ! do complex/real division explicitly with two real
                    ! divisions
                    d = stdlib_dlapy2( real( gs,KIND=dp), aimag( gs ) )
                    sn = cmplx( real( gs,KIND=dp) / d, -aimag( gs ) / d,KIND=dp)
                    go to 50
                 end if
                 f2s = stdlib_dlapy2( real( fs,KIND=dp), aimag( fs ) )
                 ! g2 and g2s are accurate
                 ! g2 is at least safmin, and g2s is at least safmn2
                 g2s = sqrt( g2 )
                 ! error in cs from underflow in f2s is at most
                 ! unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
                 ! if max(g2,one)=g2, then f2 .lt. g2*safmin,
                 ! and so cs .lt. sqrt(safmin)
                 ! if max(g2,one)=one, then f2 .lt. safmin
                 ! and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
                 ! therefore, cs = f2s/g2s / sqrt( 1 + (f2s/g2s)**2 ) = f2s/g2s
                 cs = f2s / g2s
                 ! make sure abs(ff) = 1
                 ! do complex/real division explicitly with 2 real divisions
                 if( abs1( f )>one ) then
                    d = stdlib_dlapy2( real( f,KIND=dp), aimag( f ) )
                    ff = cmplx( real( f,KIND=dp) / d, aimag( f ) / d,KIND=dp)
                 else
                    dr = safmx2*real( f,KIND=dp)
                    di = safmx2*aimag( f )
                    d = stdlib_dlapy2( dr, di )
                    ff = cmplx( dr / d, di / d,KIND=dp)
                 end if
                 sn = ff*cmplx( real( gs,KIND=dp) / g2s, -aimag( gs ) / g2s,KIND=dp)
                 r = cs*f + sn*g
              else
                 ! this is the most common case.
                 ! neither f2 nor f2/g2 are less than safmin
                 ! f2s cannot overflow, and it is accurate
                 f2s = sqrt( one+g2 / f2 )
                 ! do the f2s(real)*fs(complex) multiply with two real
                 ! multiplies
                 r = cmplx( f2s*real( fs,KIND=dp), f2s*aimag( fs ),KIND=dp)
                 cs = one / f2s
                 d = f2 + g2
                 ! do complex/real division explicitly with two real divisions
                 sn = cmplx( real( r,KIND=dp) / d, aimag( r ) / d,KIND=dp)
                 sn = sn*conjg( gs )
                 if( count/=0_ilp ) then
                    if( count>0_ilp ) then
                       do j = 1, count
                          r = r*safmx2
                       end do
                    else
                       do j = 1, -count
                          r = r*safmn2
                       end do
                    end if
                 end if
              end if
              50 continue
              c( ic ) = cs
              y( iy ) = sn
              x( ix ) = r
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_60
           return
     end subroutine stdlib_zlargv




     pure module subroutine stdlib_slartv( n, x, incx, y, incy, c, s, incc )
     !! SLARTV applies a vector of real plane rotations to elements of the
     !! real vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
     !! ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           real(sp) :: xi, yi
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - s( ic )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_slartv

     pure module subroutine stdlib_dlartv( n, x, incx, y, incy, c, s, incc )
     !! DLARTV applies a vector of real plane rotations to elements of the
     !! real vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
     !! ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           real(dp) :: xi, yi
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - s( ic )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_dlartv


     pure module subroutine stdlib_clartv( n, x, incx, y, incy, c, s, incc )
     !! CLARTV applies a vector of complex plane rotations with real cosines
     !! to elements of the complex vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (        c(i)   s(i) ) ( x(i) )
     !! ( y(i) )    ( -conjg(s(i))  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           complex(sp) :: xi, yi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - conjg( s( ic ) )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_clartv

     pure module subroutine stdlib_zlartv( n, x, incx, y, incy, c, s, incc )
     !! ZLARTV applies a vector of complex plane rotations with real cosines
     !! to elements of the complex vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (        c(i)   s(i) ) ( x(i) )
     !! ( y(i) )    ( -conjg(s(i))  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix, iy
           complex(dp) :: xi, yi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           iy = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - conjg( s( ic ) )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_zlartv




     pure module subroutine stdlib_slar2v( n, x, y, z, incx, c, s, incc )
     !! SLAR2V applies a vector of real plane rotations from both sides to
     !! a sequence of 2-by-2 real symmetric matrices, defined by the elements
     !! of the vectors x, y and z. For i = 1,2,...,n
     !! ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
     !! ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix
           real(sp) :: ci, si, t1, t2, t3, t4, t5, t6, xi, yi, zi
           ! Executable Statements 
           ix = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( ix )
              zi = z( ix )
              ci = c( ic )
              si = s( ic )
              t1 = si*zi
              t2 = ci*zi
              t3 = t2 - si*xi
              t4 = t2 + si*yi
              t5 = ci*xi + t1
              t6 = ci*yi - t1
              x( ix ) = ci*t5 + si*t4
              y( ix ) = ci*t6 - si*t3
              z( ix ) = ci*t4 - si*t5
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_slar2v

     pure module subroutine stdlib_dlar2v( n, x, y, z, incx, c, s, incc )
     !! DLAR2V applies a vector of real plane rotations from both sides to
     !! a sequence of 2-by-2 real symmetric matrices, defined by the elements
     !! of the vectors x, y and z. For i = 1,2,...,n
     !! ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
     !! ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix
           real(dp) :: ci, si, t1, t2, t3, t4, t5, t6, xi, yi, zi
           ! Executable Statements 
           ix = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = x( ix )
              yi = y( ix )
              zi = z( ix )
              ci = c( ic )
              si = s( ic )
              t1 = si*zi
              t2 = ci*zi
              t3 = t2 - si*xi
              t4 = t2 + si*yi
              t5 = ci*xi + t1
              t6 = ci*yi - t1
              x( ix ) = ci*t5 + si*t4
              y( ix ) = ci*t6 - si*t3
              z( ix ) = ci*t4 - si*t5
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_dlar2v


     pure module subroutine stdlib_clar2v( n, x, y, z, incx, c, s, incc )
     !! CLAR2V applies a vector of complex plane rotations with real cosines
     !! from both sides to a sequence of 2-by-2 complex Hermitian matrices,
     !! defined by the elements of the vectors x, y and z. For i = 1,2,...,n
     !! (       x(i)  z(i) ) :=
     !! ( conjg(z(i)) y(i) )
     !! (  c(i) conjg(s(i)) ) (       x(i)  z(i) ) ( c(i) -conjg(s(i)) )
     !! ( -s(i)       c(i)  ) ( conjg(z(i)) y(i) ) ( s(i)        c(i)  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix
           real(sp) :: ci, sii, sir, t1i, t1r, t5, t6, xi, yi, zii, zir
           complex(sp) :: si, t2, t3, t4, zi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = real( x( ix ),KIND=sp)
              yi = real( y( ix ),KIND=sp)
              zi = z( ix )
              zir = real( zi,KIND=sp)
              zii = aimag( zi )
              ci = c( ic )
              si = s( ic )
              sir = real( si,KIND=sp)
              sii = aimag( si )
              t1r = sir*zir - sii*zii
              t1i = sir*zii + sii*zir
              t2 = ci*zi
              t3 = t2 - conjg( si )*xi
              t4 = conjg( t2 ) + si*yi
              t5 = ci*xi + t1r
              t6 = ci*yi - t1r
              x( ix ) = ci*t5 + ( sir*real( t4,KIND=sp)+sii*aimag( t4 ) )
              y( ix ) = ci*t6 - ( sir*real( t3,KIND=sp)-sii*aimag( t3 ) )
              z( ix ) = ci*t3 + conjg( si )*cmplx( t6, t1i,KIND=sp)
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_clar2v

     pure module subroutine stdlib_zlar2v( n, x, y, z, incx, c, s, incc )
     !! ZLAR2V applies a vector of complex plane rotations with real cosines
     !! from both sides to a sequence of 2-by-2 complex Hermitian matrices,
     !! defined by the elements of the vectors x, y and z. For i = 1,2,...,n
     !! (       x(i)  z(i) ) :=
     !! ( conjg(z(i)) y(i) )
     !! (  c(i) conjg(s(i)) ) (       x(i)  z(i) ) ( c(i) -conjg(s(i)) )
     !! ( -s(i)       c(i)  ) ( conjg(z(i)) y(i) ) ( s(i)        c(i)  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, ix
           real(dp) :: ci, sii, sir, t1i, t1r, t5, t6, xi, yi, zii, zir
           complex(dp) :: si, t2, t3, t4, zi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp
           ic = 1_ilp
           do i = 1, n
              xi = real( x( ix ),KIND=dp)
              yi = real( y( ix ),KIND=dp)
              zi = z( ix )
              zir = real( zi,KIND=dp)
              zii = aimag( zi )
              ci = c( ic )
              si = s( ic )
              sir = real( si,KIND=dp)
              sii = aimag( si )
              t1r = sir*zir - sii*zii
              t1i = sir*zii + sii*zir
              t2 = ci*zi
              t3 = t2 - conjg( si )*xi
              t4 = conjg( t2 ) + si*yi
              t5 = ci*xi + t1r
              t6 = ci*yi - t1r
              x( ix ) = ci*t5 + ( sir*real( t4,KIND=dp)+sii*aimag( t4 ) )
              y( ix ) = ci*t6 - ( sir*real( t3,KIND=dp)-sii*aimag( t3 ) )
              z( ix ) = ci*t3 + conjg( si )*cmplx( t6, t1i,KIND=dp)
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_zlar2v




     pure module subroutine stdlib_clacrt( n, cx, incx, cy, incy, c, s )
     !! CLACRT performs the operation
     !! (  c  s )( x )  ==> ( x )
     !! ( -s  c )( y )      ( y )
     !! where c and s are complex and the vectors x and y are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: c, s
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(sp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp
           iy = 1_ilp
           if( incx<0_ilp )ix = ( -n+1 )*incx + 1_ilp
           if( incy<0_ilp )iy = ( -n+1 )*incy + 1_ilp
           do i = 1, n
              ctemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - s*cx( ix )
              cx( ix ) = ctemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              ctemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - s*cx( i )
              cx( i ) = ctemp
           end do
           return
     end subroutine stdlib_clacrt

     pure module subroutine stdlib_zlacrt( n, cx, incx, cy, incy, c, s )
     !! ZLACRT performs the operation
     !! (  c  s )( x )  ==> ( x )
     !! ( -s  c )( y )      ( y )
     !! where c and s are complex and the vectors x and y are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: c, s
           ! Array Arguments 
           complex(dp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(dp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp
           iy = 1_ilp
           if( incx<0_ilp )ix = ( -n+1 )*incx + 1_ilp
           if( incy<0_ilp )iy = ( -n+1 )*incy + 1_ilp
           do i = 1, n
              ctemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - s*cx( ix )
              cx( ix ) = ctemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              ctemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - s*cx( i )
              cx( i ) = ctemp
           end do
           return
     end subroutine stdlib_zlacrt




     pure module subroutine stdlib_I64_slartg( f, g, c, s, r )
     !! SLARTG generates a plane rotation so that
     !! [  C  S  ]  .  [ F ]  =  [ R ]
     !! [ -S  C  ]     [ G ]     [ 0 ]
     !! where C**2 + S**2 = 1.
     !! The mathematical formulas used for C and S are
     !! R = sign(F) * sqrt(F**2 + G**2)
     !! C = F / R
     !! S = G / R
     !! Hence C >= 0. The algorithm used to compute these quantities
     !! incorporates scaling to avoid overflow or underflow in computing the
     !! square root of the sum of squares.
     !! This version is discontinuous in R at F = 0 but it returns the same
     !! C and S as SLARTG for complex inputs (F,0) and (G,0).
     !! This is a more accurate version of the BLAS1 routine SROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0 and (G .ne. 0), then C=0 and S=sign(1,G) without doing any
     !! floating point operations (saves work in SBDSQR when
     !! there are zeros on the diagonal).
     !! If F exceeds G in magnitude, C will be positive.
     !! Below, wp=>sp stands for single precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_sp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(sp), intent(out) :: c, r, s
        real(sp), intent(in) :: f, g
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: d, f1, fs, g1, gs, p, u, uu
        ! Intrinsic Functions 
        ! Executable Statements 
        f1 = abs( f )
        g1 = abs( g )
        if( g == zero ) then
           c = one
           s = zero
           r = f
        else if( f == zero ) then
           c = zero
           s = sign( one, g )
           r = g1
     else if( f1 > rtmin .and. f1 < rtmax .and.            g1 > rtmin .and. g1 < rtmax ) &
               then
           d = sqrt( f*f + g*g )
           p = one / d
           c = f1*p
           s = g*sign( p, f )
           r = sign( d, f )
        else
           u = min( safmax, max( safmin, f1, g1 ) )
           uu = one / u
           fs = f*uu
           gs = g*uu
           d = sqrt( fs*fs + gs*gs )
           p = one / d
           c = abs( fs )*p
           s = gs*sign( p, f )
           r = sign( d, f )*u
        end if
        return
     end subroutine stdlib_I64_slartg

     pure module subroutine stdlib_I64_dlartg( f, g, c, s, r )
     !! DLARTG generates a plane rotation so that
     !! [  C  S  ]  .  [ F ]  =  [ R ]
     !! [ -S  C  ]     [ G ]     [ 0 ]
     !! where C**2 + S**2 = 1.
     !! The mathematical formulas used for C and S are
     !! R = sign(F) * sqrt(F**2 + G**2)
     !! C = F / R
     !! S = G / R
     !! Hence C >= 0. The algorithm used to compute these quantities
     !! incorporates scaling to avoid overflow or underflow in computing the
     !! square root of the sum of squares.
     !! This version is discontinuous in R at F = 0 but it returns the same
     !! C and S as ZLARTG for complex inputs (F,0) and (G,0).
     !! This is a more accurate version of the BLAS1 routine DROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0 and (G .ne. 0), then C=0 and S=sign(1,G) without doing any
     !! floating point operations (saves work in DBDSQR when
     !! there are zeros on the diagonal).
     !! If F exceeds G in magnitude, C will be positive.
     !! Below, wp=>dp stands for double precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_dp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(dp), intent(out) :: c, r, s
        real(dp), intent(in) :: f, g
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: d, f1, fs, g1, gs, p, u, uu
        ! Intrinsic Functions 
        ! Executable Statements 
        f1 = abs( f )
        g1 = abs( g )
        if( g == zero ) then
           c = one
           s = zero
           r = f
        else if( f == zero ) then
           c = zero
           s = sign( one, g )
           r = g1
     else if( f1 > rtmin .and. f1 < rtmax .and.            g1 > rtmin .and. g1 < rtmax ) &
               then
           d = sqrt( f*f + g*g )
           p = one / d
           c = f1*p
           s = g*sign( p, f )
           r = sign( d, f )
        else
           u = min( safmax, max( safmin, f1, g1 ) )
           uu = one / u
           fs = f*uu
           gs = g*uu
           d = sqrt( fs*fs + gs*gs )
           p = one / d
           c = abs( fs )*p
           s = gs*sign( p, f )
           r = sign( d, f )*u
        end if
        return
     end subroutine stdlib_I64_dlartg


     pure module subroutine stdlib_I64_clartg( f, g, c, s, r )
     !! CLARTG generates a plane rotation so that
     !! [  C         S  ] . [ F ]  =  [ R ]
     !! [ -conjg(S)  C  ]   [ G ]     [ 0 ]
     !! where C is real and C**2 + |S|**2 = 1.
     !! The mathematical formulas used for C and S are
     !! sgn(x) = {  x / |x|,   x != 0
     !! {  1,         x = 0
     !! R = sgn(F) * sqrt(|F|**2 + |G|**2)
     !! C = |F| / sqrt(|F|**2 + |G|**2)
     !! S = sgn(F) * conjg(G) / sqrt(|F|**2 + |G|**2)
     !! When F and G are real, the formulas simplify to C = F/R and
     !! S = G/R, and the returned values of C, S, and R should be
     !! identical to those returned by CLARTG.
     !! The algorithm used to compute these quantities incorporates scaling
     !! to avoid overflow or underflow in computing the square root of the
     !! sum of squares.
     !! This is a faster version of the BLAS1 routine CROTG, except for
     !! the following differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0, then C=0 and S is chosen so that R is real.
     !! Below, wp=>sp stands for single precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_sp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(sp), intent(out) :: c
        complex(sp), intent(in) :: f, g
        complex(sp), intent(out) :: r, s
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(sp) :: fs, gs, t
        ! Intrinsic Functions 
        ! Statement Functions 
        real(sp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=sp)**2_ilp64 + aimag( t )**2_ilp64
        ! Executable Statements 
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=sp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp64 / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2_ilp64 + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp64 / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        return
     end subroutine stdlib_I64_clartg

     pure module subroutine stdlib_I64_zlartg( f, g, c, s, r )
     !! ZLARTG generates a plane rotation so that
     !! [  C         S  ] . [ F ]  =  [ R ]
     !! [ -conjg(S)  C  ]   [ G ]     [ 0 ]
     !! where C is real and C**2 + |S|**2 = 1.
     !! The mathematical formulas used for C and S are
     !! sgn(x) = {  x / |x|,   x != 0
     !! {  1,         x = 0
     !! R = sgn(F) * sqrt(|F|**2 + |G|**2)
     !! C = |F| / sqrt(|F|**2 + |G|**2)
     !! S = sgn(F) * conjg(G) / sqrt(|F|**2 + |G|**2)
     !! When F and G are real, the formulas simplify to C = F/R and
     !! S = G/R, and the returned values of C, S, and R should be
     !! identical to those returned by DLARTG.
     !! The algorithm used to compute these quantities incorporates scaling
     !! to avoid overflow or underflow in computing the square root of the
     !! sum of squares.
     !! This is a faster version of the BLAS1 routine ZROTG, except for
     !! the following differences:
     !! F and G are unchanged on return.
     !! If G=0, then C=1 and S=0.
     !! If F=0, then C=0 and S is chosen so that R is real.
     !! Below, wp=>dp stands for double precision from LA_CONSTANTS module.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! february 2021
           use stdlib_blas_constants_dp, only: zero, half, one, czero, safmax, safmin, rtmin, rtmax
        ! Scalar Arguments 
        real(dp), intent(out) :: c
        complex(dp), intent(in) :: f, g
        complex(dp), intent(out) :: r, s
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(dp) :: fs, gs, t
        ! Intrinsic Functions 
        ! Statement Functions 
        real(dp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=dp)**2_ilp64 + aimag( t )**2_ilp64
        ! Executable Statements 
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=dp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp64 / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2_ilp64 + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1_ilp64 / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        return
     end subroutine stdlib_I64_zlartg




     pure module subroutine stdlib_I64_slartgp( f, g, cs, sn, r )
     !! SLARTGP generates a plane rotation so that
     !! [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
     !! [ -SN  CS  ]     [ G ]     [ 0 ]
     !! This is a slower, more accurate version of the Level 1 BLAS routine SROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then CS=(+/-)1 and SN=0.
     !! If F=0 and (G .ne. 0), then CS=0 and SN=(+/-)1.
     !! The sign is chosen so that R >= 0.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(out) :: cs, r, sn
           real(sp), intent(in) :: f, g
        ! =====================================================================
           
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp64) :: count, i
           real(sp) :: eps, f1, g1, safmin, safmn2, safmx2, scale
           ! Intrinsic Functions 
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Executable Statements 
           ! if( first ) then
              safmin = stdlib_I64_slamch( 'S' )
              eps = stdlib_I64_slamch( 'E' )
              safmn2 = stdlib_I64_slamch( 'B' )**int( log( safmin / eps ) /log( stdlib_I64_slamch( 'B' ) )&
                         / two,KIND=ilp64)
              safmx2 = one / safmn2
              ! first = .false.
           ! end if
           if( g==zero ) then
              cs = sign( one, f )
              sn = zero
              r = abs( f )
           else if( f==zero ) then
              cs = zero
              sn = sign( one, g )
              r = abs( g )
           else
              f1 = f
              g1 = g
              scale = max( abs( f1 ), abs( g1 ) )
              if( scale>=safmx2 ) then
                 count = 0_ilp64
                 10 continue
                 count = count + 1_ilp64
                 f1 = f1*safmn2
                 g1 = g1*safmn2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale>=safmx2 .and. count < 20)go to 10
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmx2
                 end do
              else if( scale<=safmn2 ) then
                 count = 0_ilp64
                 30 continue
                 count = count + 1_ilp64
                 f1 = f1*safmx2
                 g1 = g1*safmx2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale<=safmn2 )go to 30
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmn2
                 end do
              else
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
              end if
              if( r<zero ) then
                 cs = -cs
                 sn = -sn
                 r = -r
              end if
           end if
           return
     end subroutine stdlib_I64_slartgp

     pure module subroutine stdlib_I64_dlartgp( f, g, cs, sn, r )
     !! DLARTGP generates a plane rotation so that
     !! [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
     !! [ -SN  CS  ]     [ G ]     [ 0 ]
     !! This is a slower, more accurate version of the Level 1 BLAS routine DROTG,
     !! with the following other differences:
     !! F and G are unchanged on return.
     !! If G=0, then CS=(+/-)1 and SN=0.
     !! If F=0 and (G .ne. 0), then CS=0 and SN=(+/-)1.
     !! The sign is chosen so that R >= 0.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(out) :: cs, r, sn
           real(dp), intent(in) :: f, g
        ! =====================================================================
           
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp64) :: count, i
           real(dp) :: eps, f1, g1, safmin, safmn2, safmx2, scale
           ! Intrinsic Functions 
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Executable Statements 
           ! if( first ) then
              safmin = stdlib_I64_dlamch( 'S' )
              eps = stdlib_I64_dlamch( 'E' )
              safmn2 = stdlib_I64_dlamch( 'B' )**int( log( safmin / eps ) /log( stdlib_I64_dlamch( 'B' ) )&
                         / two,KIND=ilp64)
              safmx2 = one / safmn2
              ! first = .false.
           ! end if
           if( g==zero ) then
              cs = sign( one, f )
              sn = zero
              r = abs( f )
           else if( f==zero ) then
              cs = zero
              sn = sign( one, g )
              r = abs( g )
           else
              f1 = f
              g1 = g
              scale = max( abs( f1 ), abs( g1 ) )
              if( scale>=safmx2 ) then
                 count = 0_ilp64
                 10 continue
                 count = count + 1_ilp64
                 f1 = f1*safmn2
                 g1 = g1*safmn2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale>=safmx2 .and. count < 20 )go to 10
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmx2
                 end do
              else if( scale<=safmn2 ) then
                 count = 0_ilp64
                 30 continue
                 count = count + 1_ilp64
                 f1 = f1*safmx2
                 g1 = g1*safmx2
                 scale = max( abs( f1 ), abs( g1 ) )
                 if( scale<=safmn2 )go to 30
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
                 do i = 1, count
                    r = r*safmn2
                 end do
              else
                 r = sqrt( f1**2_ilp64+g1**2_ilp64 )
                 cs = f1 / r
                 sn = g1 / r
              end if
              if( r<zero ) then
                 cs = -cs
                 sn = -sn
                 r = -r
              end if
           end if
           return
     end subroutine stdlib_I64_dlartgp




     pure module subroutine stdlib_I64_slasr( side, pivot, direct, m, n, c, s, a, lda )
     !! SLASR applies a sequence of plane rotations to a real matrix A,
     !! from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp64), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: c(*), s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           real(sp) :: ctemp, stemp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp64
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp64
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp64
           else if( m<0_ilp64 ) then
              info = 4_ilp64
           else if( n<0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_slasr

     pure module subroutine stdlib_I64_dlasr( side, pivot, direct, m, n, c, s, a, lda )
     !! DLASR applies a sequence of plane rotations to a real matrix A,
     !! from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp64), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: c(*), s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           real(dp) :: ctemp, stemp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp64
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp64
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp64
           else if( m<0_ilp64 ) then
              info = 4_ilp64
           else if( n<0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dlasr


     pure module subroutine stdlib_I64_clasr( side, pivot, direct, m, n, c, s, a, lda )
     !! CLASR applies a sequence of real plane rotations to a complex matrix
     !! A, from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp64), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           real(sp) :: ctemp, stemp
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp64
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp64
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp64
           else if( m<0_ilp64 ) then
              info = 4_ilp64
           else if( n<0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_clasr

     pure module subroutine stdlib_I64_zlasr( side, pivot, direct, m, n, c, s, a, lda )
     !! ZLASR applies a sequence of real plane rotations to a complex matrix
     !! A, from either the left or the right.
     !! When SIDE = 'L', the transformation takes the form
     !! A := P*A
     !! and when SIDE = 'R', the transformation takes the form
     !! A := A*P**T
     !! where P is an orthogonal matrix consisting of a sequence of z plane
     !! rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
     !! and P**T is the transpose of P.
     !! When DIRECT = 'F' (Forward sequence), then
     !! P = P(z-1) * ... * P(2) * P(1)
     !! and when DIRECT = 'B' (Backward sequence), then
     !! P = P(1) * P(2) * ... * P(z-1)
     !! where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
     !! R(k) = (  c(k)  s(k) )
     !! = ( -s(k)  c(k) ).
     !! When PIVOT = 'V' (Variable pivot), the rotation is performed
     !! for the plane (k,k+1), i.e., P(k) has the form
     !! P(k) = (  1                                            )
     !! (       ...                                     )
     !! (              1                                )
     !! (                   c(k)  s(k)                  )
     !! (                  -s(k)  c(k)                  )
     !! (                                1              )
     !! (                                     ...       )
     !! (                                            1  )
     !! where R(k) appears as a rank-2 modification to the identity matrix in
     !! rows and columns k and k+1.
     !! When PIVOT = 'T' (Top pivot), the rotation is performed for the
     !! plane (1,k+1), so P(k) has the form
     !! P(k) = (  c(k)                    s(k)                 )
     !! (         1                                     )
     !! (              ...                              )
     !! (                     1                         )
     !! ( -s(k)                    c(k)                 )
     !! (                                 1             )
     !! (                                      ...      )
     !! (                                             1 )
     !! where R(k) appears in rows and columns 1 and k+1.
     !! Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
     !! performed for the plane (k,z), giving P(k) the form
     !! P(k) = ( 1                                             )
     !! (      ...                                      )
     !! (             1                                 )
     !! (                  c(k)                    s(k) )
     !! (                         1                     )
     !! (                              ...              )
     !! (                                     1         )
     !! (                 -s(k)                    c(k) )
     !! where R(k) appears in rows and columns k and z.  The rotations are
     !! performed without ever forming P(k) explicitly.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, pivot, side
           integer(ilp64), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           real(dp) :: ctemp, stemp
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           if( .not.( stdlib_lsame( side, 'L' ) .or. stdlib_lsame( side, 'R' ) ) ) then
              info = 1_ilp64
           else if( .not.( stdlib_lsame( pivot, 'V' ) .or. stdlib_lsame( pivot,'T' ) .or. &
                     stdlib_lsame( pivot, 'B' ) ) ) then
              info = 2_ilp64
           else if( .not.( stdlib_lsame( direct, 'F' ) .or. stdlib_lsame( direct, 'B' ) ) )&
                     then
              info = 3_ilp64
           else if( m<0_ilp64 ) then
              info = 4_ilp64
           else if( n<0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLASR ', info )
              return
           end if
           ! quick return if possible
           if( ( m==0 ) .or. ( n==0 ) )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  p * a
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j+1, i )
                             a( j+1, i ) = ctemp*temp - stemp*a( j, i )
                             a( j, i ) = stemp*temp + ctemp*a( j, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, m
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = ctemp*temp - stemp*a( 1_ilp64, i )
                             a( 1_ilp64, i ) = stemp*temp + ctemp*a( 1_ilp64, i )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, m - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = m - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, n
                             temp = a( j, i )
                             a( j, i ) = stemp*a( m, i ) + ctemp*temp
                             a( m, i ) = ctemp*a( m, i ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form a * p**t
              if( stdlib_lsame( pivot, 'V' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j+1 )
                             a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
                             a( i, j ) = stemp*temp + ctemp*a( i, j )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'T' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 2, n
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n, 2, -1
                       ctemp = c( j-1 )
                       stemp = s( j-1 )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = ctemp*temp - stemp*a( i, 1_ilp64 )
                             a( i, 1_ilp64 ) = stemp*temp + ctemp*a( i, 1_ilp64 )
                          end do
                       end if
                    end do
                 end if
              else if( stdlib_lsame( pivot, 'B' ) ) then
                 if( stdlib_lsame( direct, 'F' ) ) then
                    do j = 1, n - 1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 else if( stdlib_lsame( direct, 'B' ) ) then
                    do j = n - 1, 1, -1
                       ctemp = c( j )
                       stemp = s( j )
                       if( ( ctemp/=one ) .or. ( stemp/=zero ) ) then
                          do i = 1, m
                             temp = a( i, j )
                             a( i, j ) = stemp*a( i, n ) + ctemp*temp
                             a( i, n ) = ctemp*a( i, n ) - stemp*temp
                          end do
                       end if
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_zlasr




     pure module subroutine stdlib_I64_slargv( n, x, incx, y, incy, c, incc )
     !! SLARGV generates a vector of real plane rotations, determined by
     !! elements of the real vectors x and y. For i = 1,2,...,n
     !! (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
     !! ( -s(i)  c(i) ) ( y(i) ) = (   0  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(out) :: c(*)
           real(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           real(sp) :: f, g, t, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           loop_10: do i = 1, n
              f = x( ix )
              g = y( iy )
              if( g==zero ) then
                 c( ic ) = one
              else if( f==zero ) then
                 c( ic ) = zero
                 y( iy ) = one
                 x( ix ) = g
              else if( abs( f )>abs( g ) ) then
                 t = g / f
                 tt = sqrt( one+t*t )
                 c( ic ) = one / tt
                 y( iy ) = t*c( ic )
                 x( ix ) = f*tt
              else
                 t = f / g
                 tt = sqrt( one+t*t )
                 y( iy ) = one / tt
                 c( ic ) = t*y( iy )
                 x( ix ) = g*tt
              end if
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_10
           return
     end subroutine stdlib_I64_slargv

     pure module subroutine stdlib_I64_dlargv( n, x, incx, y, incy, c, incc )
     !! DLARGV generates a vector of real plane rotations, determined by
     !! elements of the real vectors x and y. For i = 1,2,...,n
     !! (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
     !! ( -s(i)  c(i) ) ( y(i) ) = (   0  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(out) :: c(*)
           real(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           real(dp) :: f, g, t, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           loop_10: do i = 1, n
              f = x( ix )
              g = y( iy )
              if( g==zero ) then
                 c( ic ) = one
              else if( f==zero ) then
                 c( ic ) = zero
                 y( iy ) = one
                 x( ix ) = g
              else if( abs( f )>abs( g ) ) then
                 t = g / f
                 tt = sqrt( one+t*t )
                 c( ic ) = one / tt
                 y( iy ) = t*c( ic )
                 x( ix ) = f*tt
              else
                 t = f / g
                 tt = sqrt( one+t*t )
                 y( iy ) = one / tt
                 c( ic ) = t*y( iy )
                 x( ix ) = g*tt
              end if
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_10
           return
     end subroutine stdlib_I64_dlargv


     pure module subroutine stdlib_I64_clargv( n, x, incx, y, incy, c, incc )
     !! CLARGV generates a vector of complex plane rotations with real
     !! cosines, determined by elements of the complex vectors x and y.
     !! For i = 1,2,...,n
     !! (        c(i)   s(i) ) ( x(i) ) = ( r(i) )
     !! ( -conjg(s(i))  c(i) ) ( y(i) ) = (   0  )
     !! where c(i)**2 + ABS(s(i))**2 = 1
     !! The following conventions are used (these are the same as in CLARTG,
     !! but differ from the BLAS1 routine CROTG):
     !! If y(i)=0, then c(i)=1 and s(i)=0.
     !! If x(i)=0, then c(i)=0 and s(i) is chosen so that r(i) is real.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(out) :: c(*)
           complex(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp64) :: count, i, ic, ix, iy, j
           real(sp) :: cs, d, di, dr, eps, f2, f2s, g2, g2s, safmin, safmn2, safmx2, scale
           complex(sp) :: f, ff, fs, g, gs, r, sn
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1, abssq
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Statement Function Definitions 
           abs1( ff ) = max( abs( real( ff,KIND=sp) ), abs( aimag( ff ) ) )
           abssq( ff ) = real( ff,KIND=sp)**2_ilp64 + aimag( ff )**2_ilp64
           ! Executable Statements 
           ! if( first ) then
              ! first = .false.
              safmin = stdlib_I64_slamch( 'S' )
              eps = stdlib_I64_slamch( 'E' )
              safmn2 = stdlib_I64_slamch( 'B' )**int( log( safmin / eps ) /log( stdlib_I64_slamch( 'B' ) )&
                         / two,KIND=ilp64)
              safmx2 = one / safmn2
           ! end if
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           loop_60: do i = 1, n
              f = x( ix )
              g = y( iy )
              ! use identical algorithm as in stdlib_I64_clartg
              scale = max( abs1( f ), abs1( g ) )
              fs = f
              gs = g
              count = 0_ilp64
              if( scale>=safmx2 ) then
              10 continue
                 count = count + 1_ilp64
                 fs = fs*safmn2
                 gs = gs*safmn2
                 scale = scale*safmn2
                 if( scale>=safmx2 .and. count < 20 )go to 10
              else if( scale<=safmn2 ) then
                 if( g==czero ) then
                    cs = one
                    sn = czero
                    r = f
                    go to 50
                 end if
                 20 continue
                 count = count - 1_ilp64
                 fs = fs*safmx2
                 gs = gs*safmx2
                 scale = scale*safmx2
                 if( scale<=safmn2 )go to 20
              end if
              f2 = abssq( fs )
              g2 = abssq( gs )
              if( f2<=max( g2, one )*safmin ) then
                 ! this is a rare case: f is very small.
                 if( f==czero ) then
                    cs = zero
                    r = stdlib_I64_slapy2( real( g,KIND=sp), aimag( g ) )
                    ! do complex/real division explicitly with two real
                    ! divisions
                    d = stdlib_I64_slapy2( real( gs,KIND=sp), aimag( gs ) )
                    sn = cmplx( real( gs,KIND=sp) / d, -aimag( gs ) / d,KIND=sp)
                    go to 50
                 end if
                 f2s = stdlib_I64_slapy2( real( fs,KIND=sp), aimag( fs ) )
                 ! g2 and g2s are accurate
                 ! g2 is at least safmin, and g2s is at least safmn2
                 g2s = sqrt( g2 )
                 ! error in cs from underflow in f2s is at most
                 ! unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
                 ! if max(g2,one)=g2, then f2 .lt. g2*safmin,
                 ! and so cs .lt. sqrt(safmin)
                 ! if max(g2,one)=one, then f2 .lt. safmin
                 ! and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
                 ! therefore, cs = f2s/g2s / sqrt( 1 + (f2s/g2s)**2 ) = f2s/g2s
                 cs = f2s / g2s
                 ! make sure abs(ff) = 1
                 ! do complex/real division explicitly with 2 real divisions
                 if( abs1( f )>one ) then
                    d = stdlib_I64_slapy2( real( f,KIND=sp), aimag( f ) )
                    ff = cmplx( real( f,KIND=sp) / d, aimag( f ) / d,KIND=sp)
                 else
                    dr = safmx2*real( f,KIND=sp)
                    di = safmx2*aimag( f )
                    d = stdlib_I64_slapy2( dr, di )
                    ff = cmplx( dr / d, di / d,KIND=sp)
                 end if
                 sn = ff*cmplx( real( gs,KIND=sp) / g2s, -aimag( gs ) / g2s,KIND=sp)
                 r = cs*f + sn*g
              else
                 ! this is the most common case.
                 ! neither f2 nor f2/g2 are less than safmin
                 ! f2s cannot overflow, and it is accurate
                 f2s = sqrt( one+g2 / f2 )
                 ! do the f2s(real)*fs(complex) multiply with two real
                 ! multiplies
                 r = cmplx( f2s*real( fs,KIND=sp), f2s*aimag( fs ),KIND=sp)
                 cs = one / f2s
                 d = f2 + g2
                 ! do complex/real division explicitly with two real divisions
                 sn = cmplx( real( r,KIND=sp) / d, aimag( r ) / d,KIND=sp)
                 sn = sn*conjg( gs )
                 if( count/=0_ilp64 ) then
                    if( count>0_ilp64 ) then
                       do j = 1, count
                          r = r*safmx2
                       end do
                    else
                       do j = 1, -count
                          r = r*safmn2
                       end do
                    end if
                 end if
              end if
              50 continue
              c( ic ) = cs
              y( iy ) = sn
              x( ix ) = r
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_60
           return
     end subroutine stdlib_I64_clargv

     pure module subroutine stdlib_I64_zlargv( n, x, incx, y, incy, c, incc )
     !! ZLARGV generates a vector of complex plane rotations with real
     !! cosines, determined by elements of the complex vectors x and y.
     !! For i = 1,2,...,n
     !! (        c(i)   s(i) ) ( x(i) ) = ( r(i) )
     !! ( -conjg(s(i))  c(i) ) ( y(i) ) = (   0  )
     !! where c(i)**2 + ABS(s(i))**2 = 1
     !! The following conventions are used (these are the same as in ZLARTG,
     !! but differ from the BLAS1 routine ZROTG):
     !! If y(i)=0, then c(i)=1 and s(i)=0.
     !! If x(i)=0, then c(i)=0 and s(i) is chosen so that r(i) is real.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(out) :: c(*)
           complex(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           ! logical            first
           integer(ilp64) :: count, i, ic, ix, iy, j
           real(dp) :: cs, d, di, dr, eps, f2, f2s, g2, g2s, safmin, safmn2, safmx2, scale
           complex(dp) :: f, ff, fs, g, gs, r, sn
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1, abssq
           ! Save Statement 
           ! save               first, safmx2, safmin, safmn2
           ! Data Statements 
           ! data               first / .true. /
           ! Statement Function Definitions 
           abs1( ff ) = max( abs( real( ff,KIND=dp) ), abs( aimag( ff ) ) )
           abssq( ff ) = real( ff,KIND=dp)**2_ilp64 + aimag( ff )**2_ilp64
           ! Executable Statements 
           ! if( first ) then
              ! first = .false.
              safmin = stdlib_I64_dlamch( 'S' )
              eps = stdlib_I64_dlamch( 'E' )
              safmn2 = stdlib_I64_dlamch( 'B' )**int( log( safmin / eps ) /log( stdlib_I64_dlamch( 'B' ) )&
                         / two,KIND=ilp64)
              safmx2 = one / safmn2
           ! end if
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           loop_60: do i = 1, n
              f = x( ix )
              g = y( iy )
              ! use identical algorithm as in stdlib_I64_zlartg
              scale = max( abs1( f ), abs1( g ) )
              fs = f
              gs = g
              count = 0_ilp64
              if( scale>=safmx2 ) then
              10 continue
                 count = count + 1_ilp64
                 fs = fs*safmn2
                 gs = gs*safmn2
                 scale = scale*safmn2
                 if( scale>=safmx2 .and. count < 20 )go to 10
              else if( scale<=safmn2 ) then
                 if( g==czero ) then
                    cs = one
                    sn = czero
                    r = f
                    go to 50
                 end if
                 20 continue
                 count = count - 1_ilp64
                 fs = fs*safmx2
                 gs = gs*safmx2
                 scale = scale*safmx2
                 if( scale<=safmn2 )go to 20
              end if
              f2 = abssq( fs )
              g2 = abssq( gs )
              if( f2<=max( g2, one )*safmin ) then
                 ! this is a rare case: f is very small.
                 if( f==czero ) then
                    cs = zero
                    r = stdlib_I64_dlapy2( real( g,KIND=dp), aimag( g ) )
                    ! do complex/real division explicitly with two real
                    ! divisions
                    d = stdlib_I64_dlapy2( real( gs,KIND=dp), aimag( gs ) )
                    sn = cmplx( real( gs,KIND=dp) / d, -aimag( gs ) / d,KIND=dp)
                    go to 50
                 end if
                 f2s = stdlib_I64_dlapy2( real( fs,KIND=dp), aimag( fs ) )
                 ! g2 and g2s are accurate
                 ! g2 is at least safmin, and g2s is at least safmn2
                 g2s = sqrt( g2 )
                 ! error in cs from underflow in f2s is at most
                 ! unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
                 ! if max(g2,one)=g2, then f2 .lt. g2*safmin,
                 ! and so cs .lt. sqrt(safmin)
                 ! if max(g2,one)=one, then f2 .lt. safmin
                 ! and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
                 ! therefore, cs = f2s/g2s / sqrt( 1 + (f2s/g2s)**2 ) = f2s/g2s
                 cs = f2s / g2s
                 ! make sure abs(ff) = 1
                 ! do complex/real division explicitly with 2 real divisions
                 if( abs1( f )>one ) then
                    d = stdlib_I64_dlapy2( real( f,KIND=dp), aimag( f ) )
                    ff = cmplx( real( f,KIND=dp) / d, aimag( f ) / d,KIND=dp)
                 else
                    dr = safmx2*real( f,KIND=dp)
                    di = safmx2*aimag( f )
                    d = stdlib_I64_dlapy2( dr, di )
                    ff = cmplx( dr / d, di / d,KIND=dp)
                 end if
                 sn = ff*cmplx( real( gs,KIND=dp) / g2s, -aimag( gs ) / g2s,KIND=dp)
                 r = cs*f + sn*g
              else
                 ! this is the most common case.
                 ! neither f2 nor f2/g2 are less than safmin
                 ! f2s cannot overflow, and it is accurate
                 f2s = sqrt( one+g2 / f2 )
                 ! do the f2s(real)*fs(complex) multiply with two real
                 ! multiplies
                 r = cmplx( f2s*real( fs,KIND=dp), f2s*aimag( fs ),KIND=dp)
                 cs = one / f2s
                 d = f2 + g2
                 ! do complex/real division explicitly with two real divisions
                 sn = cmplx( real( r,KIND=dp) / d, aimag( r ) / d,KIND=dp)
                 sn = sn*conjg( gs )
                 if( count/=0_ilp64 ) then
                    if( count>0_ilp64 ) then
                       do j = 1, count
                          r = r*safmx2
                       end do
                    else
                       do j = 1, -count
                          r = r*safmn2
                       end do
                    end if
                 end if
              end if
              50 continue
              c( ic ) = cs
              y( iy ) = sn
              x( ix ) = r
              ic = ic + incc
              iy = iy + incy
              ix = ix + incx
           end do loop_60
           return
     end subroutine stdlib_I64_zlargv




     pure module subroutine stdlib_I64_slartv( n, x, incx, y, incy, c, s, incc )
     !! SLARTV applies a vector of real plane rotations to elements of the
     !! real vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
     !! ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           real(sp) :: xi, yi
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - s( ic )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_slartv

     pure module subroutine stdlib_I64_dlartv( n, x, incx, y, incy, c, s, incc )
     !! DLARTV applies a vector of real plane rotations to elements of the
     !! real vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
     !! ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           real(dp) :: xi, yi
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - s( ic )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_dlartv


     pure module subroutine stdlib_I64_clartv( n, x, incx, y, incy, c, s, incc )
     !! CLARTV applies a vector of complex plane rotations with real cosines
     !! to elements of the complex vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (        c(i)   s(i) ) ( x(i) )
     !! ( y(i) )    ( -conjg(s(i))  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           complex(sp) :: xi, yi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - conjg( s( ic ) )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_clartv

     pure module subroutine stdlib_I64_zlartv( n, x, incx, y, incy, c, s, incc )
     !! ZLARTV applies a vector of complex plane rotations with real cosines
     !! to elements of the complex vectors x and y. For i = 1,2,...,n
     !! ( x(i) ) := (        c(i)   s(i) ) ( x(i) )
     !! ( y(i) )    ( -conjg(s(i))  c(i) ) ( y(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix, iy
           complex(dp) :: xi, yi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           iy = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( iy )
              x( ix ) = c( ic )*xi + s( ic )*yi
              y( iy ) = c( ic )*yi - conjg( s( ic ) )*xi
              ix = ix + incx
              iy = iy + incy
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_zlartv




     pure module subroutine stdlib_I64_slar2v( n, x, y, z, incx, c, s, incc )
     !! SLAR2V applies a vector of real plane rotations from both sides to
     !! a sequence of 2-by-2 real symmetric matrices, defined by the elements
     !! of the vectors x, y and z. For i = 1,2,...,n
     !! ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
     !! ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix
           real(sp) :: ci, si, t1, t2, t3, t4, t5, t6, xi, yi, zi
           ! Executable Statements 
           ix = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( ix )
              zi = z( ix )
              ci = c( ic )
              si = s( ic )
              t1 = si*zi
              t2 = ci*zi
              t3 = t2 - si*xi
              t4 = t2 + si*yi
              t5 = ci*xi + t1
              t6 = ci*yi - t1
              x( ix ) = ci*t5 + si*t4
              y( ix ) = ci*t6 - si*t3
              z( ix ) = ci*t4 - si*t5
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_slar2v

     pure module subroutine stdlib_I64_dlar2v( n, x, y, z, incx, c, s, incc )
     !! DLAR2V applies a vector of real plane rotations from both sides to
     !! a sequence of 2-by-2 real symmetric matrices, defined by the elements
     !! of the vectors x, y and z. For i = 1,2,...,n
     !! ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
     !! ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix
           real(dp) :: ci, si, t1, t2, t3, t4, t5, t6, xi, yi, zi
           ! Executable Statements 
           ix = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = x( ix )
              yi = y( ix )
              zi = z( ix )
              ci = c( ic )
              si = s( ic )
              t1 = si*zi
              t2 = ci*zi
              t3 = t2 - si*xi
              t4 = t2 + si*yi
              t5 = ci*xi + t1
              t6 = ci*yi - t1
              x( ix ) = ci*t5 + si*t4
              y( ix ) = ci*t6 - si*t3
              z( ix ) = ci*t4 - si*t5
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_dlar2v


     pure module subroutine stdlib_I64_clar2v( n, x, y, z, incx, c, s, incc )
     !! CLAR2V applies a vector of complex plane rotations with real cosines
     !! from both sides to a sequence of 2-by-2 complex Hermitian matrices,
     !! defined by the elements of the vectors x, y and z. For i = 1,2,...,n
     !! (       x(i)  z(i) ) :=
     !! ( conjg(z(i)) y(i) )
     !! (  c(i) conjg(s(i)) ) (       x(i)  z(i) ) ( c(i) -conjg(s(i)) )
     !! ( -s(i)       c(i)  ) ( conjg(z(i)) y(i) ) ( s(i)        c(i)  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix
           real(sp) :: ci, sii, sir, t1i, t1r, t5, t6, xi, yi, zii, zir
           complex(sp) :: si, t2, t3, t4, zi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = real( x( ix ),KIND=sp)
              yi = real( y( ix ),KIND=sp)
              zi = z( ix )
              zir = real( zi,KIND=sp)
              zii = aimag( zi )
              ci = c( ic )
              si = s( ic )
              sir = real( si,KIND=sp)
              sii = aimag( si )
              t1r = sir*zir - sii*zii
              t1i = sir*zii + sii*zir
              t2 = ci*zi
              t3 = t2 - conjg( si )*xi
              t4 = conjg( t2 ) + si*yi
              t5 = ci*xi + t1r
              t6 = ci*yi - t1r
              x( ix ) = ci*t5 + ( sir*real( t4,KIND=sp)+sii*aimag( t4 ) )
              y( ix ) = ci*t6 - ( sir*real( t3,KIND=sp)-sii*aimag( t3 ) )
              z( ix ) = ci*t3 + conjg( si )*cmplx( t6, t1i,KIND=sp)
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_clar2v

     pure module subroutine stdlib_I64_zlar2v( n, x, y, z, incx, c, s, incc )
     !! ZLAR2V applies a vector of complex plane rotations with real cosines
     !! from both sides to a sequence of 2-by-2 complex Hermitian matrices,
     !! defined by the elements of the vectors x, y and z. For i = 1,2,...,n
     !! (       x(i)  z(i) ) :=
     !! ( conjg(z(i)) y(i) )
     !! (  c(i) conjg(s(i)) ) (       x(i)  z(i) ) ( c(i) -conjg(s(i)) )
     !! ( -s(i)       c(i)  ) ( conjg(z(i)) y(i) ) ( s(i)        c(i)  )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incc, incx, n
           ! Array Arguments 
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*), z(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, ix
           real(dp) :: ci, sii, sir, t1i, t1r, t5, t6, xi, yi, zii, zir
           complex(dp) :: si, t2, t3, t4, zi
           ! Intrinsic Functions 
           ! Executable Statements 
           ix = 1_ilp64
           ic = 1_ilp64
           do i = 1, n
              xi = real( x( ix ),KIND=dp)
              yi = real( y( ix ),KIND=dp)
              zi = z( ix )
              zir = real( zi,KIND=dp)
              zii = aimag( zi )
              ci = c( ic )
              si = s( ic )
              sir = real( si,KIND=dp)
              sii = aimag( si )
              t1r = sir*zir - sii*zii
              t1i = sir*zii + sii*zir
              t2 = ci*zi
              t3 = t2 - conjg( si )*xi
              t4 = conjg( t2 ) + si*yi
              t5 = ci*xi + t1r
              t6 = ci*yi - t1r
              x( ix ) = ci*t5 + ( sir*real( t4,KIND=dp)+sii*aimag( t4 ) )
              y( ix ) = ci*t6 - ( sir*real( t3,KIND=dp)-sii*aimag( t3 ) )
              z( ix ) = ci*t3 + conjg( si )*cmplx( t6, t1i,KIND=dp)
              ix = ix + incx
              ic = ic + incc
           end do
           return
     end subroutine stdlib_I64_zlar2v




     pure module subroutine stdlib_I64_clacrt( n, cx, incx, cy, incy, c, s )
     !! CLACRT performs the operation
     !! (  c  s )( x )  ==> ( x )
     !! ( -s  c )( y )      ( y )
     !! where c and s are complex and the vectors x and y are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: c, s
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ix, iy
           complex(sp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp64
           iy = 1_ilp64
           if( incx<0_ilp64 )ix = ( -n+1 )*incx + 1_ilp64
           if( incy<0_ilp64 )iy = ( -n+1 )*incy + 1_ilp64
           do i = 1, n
              ctemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - s*cx( ix )
              cx( ix ) = ctemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              ctemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - s*cx( i )
              cx( i ) = ctemp
           end do
           return
     end subroutine stdlib_I64_clacrt

     pure module subroutine stdlib_I64_zlacrt( n, cx, incx, cy, incy, c, s )
     !! ZLACRT performs the operation
     !! (  c  s )( x )  ==> ( x )
     !! ( -s  c )( y )      ( y )
     !! where c and s are complex and the vectors x and y are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: c, s
           ! Array Arguments 
           complex(dp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ix, iy
           complex(dp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp64
           iy = 1_ilp64
           if( incx<0_ilp64 )ix = ( -n+1 )*incx + 1_ilp64
           if( incy<0_ilp64 )iy = ( -n+1 )*incy + 1_ilp64
           do i = 1, n
              ctemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - s*cx( ix )
              cx( ix ) = ctemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              ctemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - s*cx( i )
              cx( i ) = ctemp
           end do
           return
     end subroutine stdlib_I64_zlacrt



end submodule stdlib_lapack_givens_jacobi_rot
