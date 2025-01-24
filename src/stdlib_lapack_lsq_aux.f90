submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_lsq_aux
  implicit none


  contains

     pure module subroutine stdlib_slaic1( job, j, x, sest, w, gamma, sestpr, s, c )
     !! SLAIC1 applies one step of incremental condition estimation in
     !! its simplest version:
     !! Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
     !! lower triangular matrix L, such that
     !! twonorm(L*x) = sest
     !! Then SLAIC1 computes sestpr, s, c such that
     !! the vector
     !! [ s*x ]
     !! xhat = [  c  ]
     !! is an approximate singular vector of
     !! [ L      0  ]
     !! Lhat = [ w**T gamma ]
     !! in the sense that
     !! twonorm(Lhat*xhat) = sestpr.
     !! Depending on JOB, an estimate for the largest or smallest singular
     !! value is computed.
     !! Note that [s c]**T and sestpr**2 is an eigenpair of the system
     !! diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
     !! [ gamma ]
     !! where  alpha =  x**T*w.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: j, job
           real(sp), intent(out) :: c, s, sestpr
           real(sp), intent(in) :: gamma, sest
           ! Array Arguments 
           real(sp), intent(in) :: w(j), x(j)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: absalp, absest, absgam, alpha, b, cosine, eps, norma, s1, s2, sine, t, &
                     test, tmp, zeta1, zeta2
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_slamch( 'EPSILON' )
           alpha = stdlib_sdot( j, x, 1_ilp, w, 1_ilp )
           absalp = abs( alpha )
           absgam = abs( gamma )
           absest = abs( sest )
           if( job==1_ilp ) then
              ! estimating largest singular value
              ! special cases
              if( sest==zero ) then
                 s1 = max( absgam, absalp )
                 if( s1==zero ) then
                    s = zero
                    c = one
                    sestpr = zero
                 else
                    s = alpha / s1
                    c = gamma / s1
                    tmp = sqrt( s*s+c*c )
                    s = s / tmp
                    c = c / tmp
                    sestpr = s1*tmp
                 end if
                 return
              else if( absgam<=eps*absest ) then
                 s = one
                 c = zero
                 tmp = max( absest, absalp )
                 s1 = absest / tmp
                 s2 = absalp / tmp
                 sestpr = tmp*sqrt( s1*s1+s2*s2 )
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = one
                    c = zero
                    sestpr = s2
                 else
                    s = zero
                    c = one
                    sestpr = s1
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    s = sqrt( one+tmp*tmp )
                    sestpr = s2*s
                    c = ( gamma / s2 ) / s
                    s = sign( one, alpha ) / s
                 else
                    tmp = s2 / s1
                    c = sqrt( one+tmp*tmp )
                    sestpr = s1*c
                    s = ( alpha / s1 ) / c
                    c = sign( one, gamma ) / c
                 end if
                 return
              else
                 ! normal case
                 zeta1 = alpha / absest
                 zeta2 = gamma / absest
                 b = ( one-zeta1*zeta1-zeta2*zeta2 )*half
                 c = zeta1*zeta1
                 if( b>zero ) then
                    t = c / ( b+sqrt( b*b+c ) )
                 else
                    t = sqrt( b*b+c ) - b
                 end if
                 sine = -zeta1 / t
                 cosine = -zeta2 / ( one+t )
                 tmp = sqrt( sine*sine+cosine*cosine )
                 s = sine / tmp
                 c = cosine / tmp
                 sestpr = sqrt( t+one )*absest
                 return
              end if
           else if( job==2_ilp ) then
              ! estimating smallest singular value
              ! special cases
              if( sest==zero ) then
                 sestpr = zero
                 if( max( absgam, absalp )==zero ) then
                    sine = one
                    cosine = zero
                 else
                    sine = -gamma
                    cosine = alpha
                 end if
                 s1 = max( abs( sine ), abs( cosine ) )
                 s = sine / s1
                 c = cosine / s1
                 tmp = sqrt( s*s+c*c )
                 s = s / tmp
                 c = c / tmp
                 return
              else if( absgam<=eps*absest ) then
                 s = zero
                 c = one
                 sestpr = absgam
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = zero
                    c = one
                    sestpr = s1
                 else
                    s = one
                    c = zero
                    sestpr = s2
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    c = sqrt( one+tmp*tmp )
                    sestpr = absest*( tmp / c )
                    s = -( gamma / s2 ) / c
                    c = sign( one, alpha ) / c
                 else
                    tmp = s2 / s1
                    s = sqrt( one+tmp*tmp )
                    sestpr = absest / s
                    c = ( alpha / s1 ) / s
                    s = -sign( one, gamma ) / s
                 end if
                 return
              else
                 ! normal case
                 zeta1 = alpha / absest
                 zeta2 = gamma / absest
                 norma = max( one+zeta1*zeta1+abs( zeta1*zeta2 ),abs( zeta1*zeta2 )+zeta2*zeta2 )
                           
                 ! see if root is closer to zero or to one
                 test = one + two*( zeta1-zeta2 )*( zeta1+zeta2 )
                 if( test>=zero ) then
                    ! root is close to zero, compute directly
                    b = ( zeta1*zeta1+zeta2*zeta2+one )*half
                    c = zeta2*zeta2
                    t = c / ( b+sqrt( abs( b*b-c ) ) )
                    sine = zeta1 / ( one-t )
                    cosine = -zeta2 / t
                    sestpr = sqrt( t+four*eps*eps*norma )*absest
                 else
                    ! root is closer to one, shift by that amount
                    b = ( zeta2*zeta2+zeta1*zeta1-one )*half
                    c = zeta1*zeta1
                    if( b>=zero ) then
                       t = -c / ( b+sqrt( b*b+c ) )
                    else
                       t = b - sqrt( b*b+c )
                    end if
                    sine = -zeta1 / t
                    cosine = -zeta2 / ( one+t )
                    sestpr = sqrt( one+t+four*eps*eps*norma )*absest
                 end if
                 tmp = sqrt( sine*sine+cosine*cosine )
                 s = sine / tmp
                 c = cosine / tmp
                 return
              end if
           end if
           return
     end subroutine stdlib_slaic1

     pure module subroutine stdlib_dlaic1( job, j, x, sest, w, gamma, sestpr, s, c )
     !! DLAIC1 applies one step of incremental condition estimation in
     !! its simplest version:
     !! Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
     !! lower triangular matrix L, such that
     !! twonorm(L*x) = sest
     !! Then DLAIC1 computes sestpr, s, c such that
     !! the vector
     !! [ s*x ]
     !! xhat = [  c  ]
     !! is an approximate singular vector of
     !! [ L       0  ]
     !! Lhat = [ w**T gamma ]
     !! in the sense that
     !! twonorm(Lhat*xhat) = sestpr.
     !! Depending on JOB, an estimate for the largest or smallest singular
     !! value is computed.
     !! Note that [s c]**T and sestpr**2 is an eigenpair of the system
     !! diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
     !! [ gamma ]
     !! where  alpha =  x**T*w.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: j, job
           real(dp), intent(out) :: c, s, sestpr
           real(dp), intent(in) :: gamma, sest
           ! Array Arguments 
           real(dp), intent(in) :: w(j), x(j)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: absalp, absest, absgam, alpha, b, cosine, eps, norma, s1, s2, sine, t, &
                     test, tmp, zeta1, zeta2
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_dlamch( 'EPSILON' )
           alpha = stdlib_ddot( j, x, 1_ilp, w, 1_ilp )
           absalp = abs( alpha )
           absgam = abs( gamma )
           absest = abs( sest )
           if( job==1_ilp ) then
              ! estimating largest singular value
              ! special cases
              if( sest==zero ) then
                 s1 = max( absgam, absalp )
                 if( s1==zero ) then
                    s = zero
                    c = one
                    sestpr = zero
                 else
                    s = alpha / s1
                    c = gamma / s1
                    tmp = sqrt( s*s+c*c )
                    s = s / tmp
                    c = c / tmp
                    sestpr = s1*tmp
                 end if
                 return
              else if( absgam<=eps*absest ) then
                 s = one
                 c = zero
                 tmp = max( absest, absalp )
                 s1 = absest / tmp
                 s2 = absalp / tmp
                 sestpr = tmp*sqrt( s1*s1+s2*s2 )
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = one
                    c = zero
                    sestpr = s2
                 else
                    s = zero
                    c = one
                    sestpr = s1
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    s = sqrt( one+tmp*tmp )
                    sestpr = s2*s
                    c = ( gamma / s2 ) / s
                    s = sign( one, alpha ) / s
                 else
                    tmp = s2 / s1
                    c = sqrt( one+tmp*tmp )
                    sestpr = s1*c
                    s = ( alpha / s1 ) / c
                    c = sign( one, gamma ) / c
                 end if
                 return
              else
                 ! normal case
                 zeta1 = alpha / absest
                 zeta2 = gamma / absest
                 b = ( one-zeta1*zeta1-zeta2*zeta2 )*half
                 c = zeta1*zeta1
                 if( b>zero ) then
                    t = c / ( b+sqrt( b*b+c ) )
                 else
                    t = sqrt( b*b+c ) - b
                 end if
                 sine = -zeta1 / t
                 cosine = -zeta2 / ( one+t )
                 tmp = sqrt( sine*sine+cosine*cosine )
                 s = sine / tmp
                 c = cosine / tmp
                 sestpr = sqrt( t+one )*absest
                 return
              end if
           else if( job==2_ilp ) then
              ! estimating smallest singular value
              ! special cases
              if( sest==zero ) then
                 sestpr = zero
                 if( max( absgam, absalp )==zero ) then
                    sine = one
                    cosine = zero
                 else
                    sine = -gamma
                    cosine = alpha
                 end if
                 s1 = max( abs( sine ), abs( cosine ) )
                 s = sine / s1
                 c = cosine / s1
                 tmp = sqrt( s*s+c*c )
                 s = s / tmp
                 c = c / tmp
                 return
              else if( absgam<=eps*absest ) then
                 s = zero
                 c = one
                 sestpr = absgam
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = zero
                    c = one
                    sestpr = s1
                 else
                    s = one
                    c = zero
                    sestpr = s2
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    c = sqrt( one+tmp*tmp )
                    sestpr = absest*( tmp / c )
                    s = -( gamma / s2 ) / c
                    c = sign( one, alpha ) / c
                 else
                    tmp = s2 / s1
                    s = sqrt( one+tmp*tmp )
                    sestpr = absest / s
                    c = ( alpha / s1 ) / s
                    s = -sign( one, gamma ) / s
                 end if
                 return
              else
                 ! normal case
                 zeta1 = alpha / absest
                 zeta2 = gamma / absest
                 norma = max( one+zeta1*zeta1+abs( zeta1*zeta2 ),abs( zeta1*zeta2 )+zeta2*zeta2 )
                           
                 ! see if root is closer to zero or to one
                 test = one + two*( zeta1-zeta2 )*( zeta1+zeta2 )
                 if( test>=zero ) then
                    ! root is close to zero, compute directly
                    b = ( zeta1*zeta1+zeta2*zeta2+one )*half
                    c = zeta2*zeta2
                    t = c / ( b+sqrt( abs( b*b-c ) ) )
                    sine = zeta1 / ( one-t )
                    cosine = -zeta2 / t
                    sestpr = sqrt( t+four*eps*eps*norma )*absest
                 else
                    ! root is closer to one, shift by that amount
                    b = ( zeta2*zeta2+zeta1*zeta1-one )*half
                    c = zeta1*zeta1
                    if( b>=zero ) then
                       t = -c / ( b+sqrt( b*b+c ) )
                    else
                       t = b - sqrt( b*b+c )
                    end if
                    sine = -zeta1 / t
                    cosine = -zeta2 / ( one+t )
                    sestpr = sqrt( one+t+four*eps*eps*norma )*absest
                 end if
                 tmp = sqrt( sine*sine+cosine*cosine )
                 s = sine / tmp
                 c = cosine / tmp
                 return
              end if
           end if
           return
     end subroutine stdlib_dlaic1


     pure module subroutine stdlib_claic1( job, j, x, sest, w, gamma, sestpr, s, c )
     !! CLAIC1 applies one step of incremental condition estimation in
     !! its simplest version:
     !! Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
     !! lower triangular matrix L, such that
     !! twonorm(L*x) = sest
     !! Then CLAIC1 computes sestpr, s, c such that
     !! the vector
     !! [ s*x ]
     !! xhat = [  c  ]
     !! is an approximate singular vector of
     !! [ L      0  ]
     !! Lhat = [ w**H gamma ]
     !! in the sense that
     !! twonorm(Lhat*xhat) = sestpr.
     !! Depending on JOB, an estimate for the largest or smallest singular
     !! value is computed.
     !! Note that [s c]**H and sestpr**2 is an eigenpair of the system
     !! diag(sest*sest, 0) + [alpha  gamma] * [ conjg(alpha) ]
     !! [ conjg(gamma) ]
     !! where  alpha =  x**H*w.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: j, job
           real(sp), intent(in) :: sest
           real(sp), intent(out) :: sestpr
           complex(sp), intent(out) :: c, s
           complex(sp), intent(in) :: gamma
           ! Array Arguments 
           complex(sp), intent(in) :: w(j), x(j)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: absalp, absest, absgam, b, eps, norma, s1, s2, scl, t, test, tmp, zeta1, &
                     zeta2
           complex(sp) :: alpha, cosine, sine
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_slamch( 'EPSILON' )
           alpha = stdlib_cdotc( j, x, 1_ilp, w, 1_ilp )
           absalp = abs( alpha )
           absgam = abs( gamma )
           absest = abs( sest )
           if( job==1_ilp ) then
              ! estimating largest singular value
              ! special cases
              if( sest==zero ) then
                 s1 = max( absgam, absalp )
                 if( s1==zero ) then
                    s = zero
                    c = one
                    sestpr = zero
                 else
                    s = alpha / s1
                    c = gamma / s1
                    tmp = real( sqrt( s*conjg( s )+c*conjg( c ) ),KIND=sp)
                    s = s / tmp
                    c = c / tmp
                    sestpr = s1*tmp
                 end if
                 return
              else if( absgam<=eps*absest ) then
                 s = one
                 c = zero
                 tmp = max( absest, absalp )
                 s1 = absest / tmp
                 s2 = absalp / tmp
                 sestpr = tmp*sqrt( s1*s1+s2*s2 )
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = one
                    c = zero
                    sestpr = s2
                 else
                    s = zero
                    c = one
                    sestpr = s1
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    scl = sqrt( one+tmp*tmp )
                    sestpr = s2*scl
                    s = ( alpha / s2 ) / scl
                    c = ( gamma / s2 ) / scl
                 else
                    tmp = s2 / s1
                    scl = sqrt( one+tmp*tmp )
                    sestpr = s1*scl
                    s = ( alpha / s1 ) / scl
                    c = ( gamma / s1 ) / scl
                 end if
                 return
              else
                 ! normal case
                 zeta1 = absalp / absest
                 zeta2 = absgam / absest
                 b = ( one-zeta1*zeta1-zeta2*zeta2 )*half
                 c = zeta1*zeta1
                 if( b>zero ) then
                    t = real( c / ( b+sqrt( b*b+c ) ),KIND=sp)
                 else
                    t = real( sqrt( b*b+c ) - b,KIND=sp)
                 end if
                 sine = -( alpha / absest ) / t
                 cosine = -( gamma / absest ) / ( one+t )
                 tmp = real( sqrt( sine * conjg( sine )+ cosine * conjg( cosine ) ),KIND=sp)
                           
                 s = sine / tmp
                 c = cosine / tmp
                 sestpr = sqrt( t+one )*absest
                 return
              end if
           else if( job==2_ilp ) then
              ! estimating smallest singular value
              ! special cases
              if( sest==zero ) then
                 sestpr = zero
                 if( max( absgam, absalp )==zero ) then
                    sine = one
                    cosine = zero
                 else
                    sine = -conjg( gamma )
                    cosine = conjg( alpha )
                 end if
                 s1 = max( abs( sine ), abs( cosine ) )
                 s = sine / s1
                 c = cosine / s1
                 tmp = real( sqrt( s*conjg( s )+c*conjg( c ) ),KIND=sp)
                 s = s / tmp
                 c = c / tmp
                 return
              else if( absgam<=eps*absest ) then
                 s = zero
                 c = one
                 sestpr = absgam
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = zero
                    c = one
                    sestpr = s1
                 else
                    s = one
                    c = zero
                    sestpr = s2
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    scl = sqrt( one+tmp*tmp )
                    sestpr = absest*( tmp / scl )
                    s = -( conjg( gamma ) / s2 ) / scl
                    c = ( conjg( alpha ) / s2 ) / scl
                 else
                    tmp = s2 / s1
                    scl = sqrt( one+tmp*tmp )
                    sestpr = absest / scl
                    s = -( conjg( gamma ) / s1 ) / scl
                    c = ( conjg( alpha ) / s1 ) / scl
                 end if
                 return
              else
                 ! normal case
                 zeta1 = absalp / absest
                 zeta2 = absgam / absest
                 norma = max( one+zeta1*zeta1+zeta1*zeta2,zeta1*zeta2+zeta2*zeta2 )
                 ! see if root is closer to zero or to one
                 test = one + two*( zeta1-zeta2 )*( zeta1+zeta2 )
                 if( test>=zero ) then
                    ! root is close to zero, compute directly
                    b = ( zeta1*zeta1+zeta2*zeta2+one )*half
                    c = zeta2*zeta2
                    t = real( c / ( b+sqrt( abs( b*b-c ) ) ),KIND=sp)
                    sine = ( alpha / absest ) / ( one-t )
                    cosine = -( gamma / absest ) / t
                    sestpr = sqrt( t+four*eps*eps*norma )*absest
                 else
                    ! root is closer to one, shift by that amount
                    b = ( zeta2*zeta2+zeta1*zeta1-one )*half
                    c = zeta1*zeta1
                    if( b>=zero ) then
                       t = real( -c / ( b+sqrt( b*b+c ) ),KIND=sp)
                    else
                       t = real( b - sqrt( b*b+c ),KIND=sp)
                    end if
                    sine = -( alpha / absest ) / t
                    cosine = -( gamma / absest ) / ( one+t )
                    sestpr = sqrt( one+t+four*eps*eps*norma )*absest
                 end if
                 tmp = real( sqrt( sine * conjg( sine )+ cosine * conjg( cosine ) ),KIND=sp)
                           
                 s = sine / tmp
                 c = cosine / tmp
                 return
              end if
           end if
           return
     end subroutine stdlib_claic1

     pure module subroutine stdlib_zlaic1( job, j, x, sest, w, gamma, sestpr, s, c )
     !! ZLAIC1 applies one step of incremental condition estimation in
     !! its simplest version:
     !! Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
     !! lower triangular matrix L, such that
     !! twonorm(L*x) = sest
     !! Then ZLAIC1 computes sestpr, s, c such that
     !! the vector
     !! [ s*x ]
     !! xhat = [  c  ]
     !! is an approximate singular vector of
     !! [ L       0  ]
     !! Lhat = [ w**H gamma ]
     !! in the sense that
     !! twonorm(Lhat*xhat) = sestpr.
     !! Depending on JOB, an estimate for the largest or smallest singular
     !! value is computed.
     !! Note that [s c]**H and sestpr**2 is an eigenpair of the system
     !! diag(sest*sest, 0) + [alpha  gamma] * [ conjg(alpha) ]
     !! [ conjg(gamma) ]
     !! where  alpha =  x**H * w.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: j, job
           real(dp), intent(in) :: sest
           real(dp), intent(out) :: sestpr
           complex(dp), intent(out) :: c, s
           complex(dp), intent(in) :: gamma
           ! Array Arguments 
           complex(dp), intent(in) :: w(j), x(j)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: absalp, absest, absgam, b, eps, norma, s1, s2, scl, t, test, tmp, zeta1, &
                     zeta2
           complex(dp) :: alpha, cosine, sine
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_dlamch( 'EPSILON' )
           alpha = stdlib_zdotc( j, x, 1_ilp, w, 1_ilp )
           absalp = abs( alpha )
           absgam = abs( gamma )
           absest = abs( sest )
           if( job==1_ilp ) then
              ! estimating largest singular value
              ! special cases
              if( sest==zero ) then
                 s1 = max( absgam, absalp )
                 if( s1==zero ) then
                    s = zero
                    c = one
                    sestpr = zero
                 else
                    s = alpha / s1
                    c = gamma / s1
                    tmp = real( sqrt( s*conjg( s )+c*conjg( c ) ),KIND=dp)
                    s = s / tmp
                    c = c / tmp
                    sestpr = s1*tmp
                 end if
                 return
              else if( absgam<=eps*absest ) then
                 s = one
                 c = zero
                 tmp = max( absest, absalp )
                 s1 = absest / tmp
                 s2 = absalp / tmp
                 sestpr = tmp*sqrt( s1*s1+s2*s2 )
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = one
                    c = zero
                    sestpr = s2
                 else
                    s = zero
                    c = one
                    sestpr = s1
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    scl = sqrt( one+tmp*tmp )
                    sestpr = s2*scl
                    s = ( alpha / s2 ) / scl
                    c = ( gamma / s2 ) / scl
                 else
                    tmp = s2 / s1
                    scl = sqrt( one+tmp*tmp )
                    sestpr = s1*scl
                    s = ( alpha / s1 ) / scl
                    c = ( gamma / s1 ) / scl
                 end if
                 return
              else
                 ! normal case
                 zeta1 = absalp / absest
                 zeta2 = absgam / absest
                 b = ( one-zeta1*zeta1-zeta2*zeta2 )*half
                 c = zeta1*zeta1
                 if( b>zero ) then
                    t = real( c / ( b+sqrt( b*b+c ) ),KIND=dp)
                 else
                    t = real( sqrt( b*b+c ) - b,KIND=dp)
                 end if
                 sine = -( alpha / absest ) / t
                 cosine = -( gamma / absest ) / ( one+t )
                 tmp = real( sqrt( sine * conjg( sine )+ cosine * conjg( cosine ) ),KIND=dp)
                           
                 s = sine / tmp
                 c = cosine / tmp
                 sestpr = sqrt( t+one )*absest
                 return
              end if
           else if( job==2_ilp ) then
              ! estimating smallest singular value
              ! special cases
              if( sest==zero ) then
                 sestpr = zero
                 if( max( absgam, absalp )==zero ) then
                    sine = one
                    cosine = zero
                 else
                    sine = -conjg( gamma )
                    cosine = conjg( alpha )
                 end if
                 s1 = max( abs( sine ), abs( cosine ) )
                 s = sine / s1
                 c = cosine / s1
                 tmp = real( sqrt( s*conjg( s )+c*conjg( c ) ),KIND=dp)
                 s = s / tmp
                 c = c / tmp
                 return
              else if( absgam<=eps*absest ) then
                 s = zero
                 c = one
                 sestpr = absgam
                 return
              else if( absalp<=eps*absest ) then
                 s1 = absgam
                 s2 = absest
                 if( s1<=s2 ) then
                    s = zero
                    c = one
                    sestpr = s1
                 else
                    s = one
                    c = zero
                    sestpr = s2
                 end if
                 return
              else if( absest<=eps*absalp .or. absest<=eps*absgam ) then
                 s1 = absgam
                 s2 = absalp
                 if( s1<=s2 ) then
                    tmp = s1 / s2
                    scl = sqrt( one+tmp*tmp )
                    sestpr = absest*( tmp / scl )
                    s = -( conjg( gamma ) / s2 ) / scl
                    c = ( conjg( alpha ) / s2 ) / scl
                 else
                    tmp = s2 / s1
                    scl = sqrt( one+tmp*tmp )
                    sestpr = absest / scl
                    s = -( conjg( gamma ) / s1 ) / scl
                    c = ( conjg( alpha ) / s1 ) / scl
                 end if
                 return
              else
                 ! normal case
                 zeta1 = absalp / absest
                 zeta2 = absgam / absest
                 norma = max( one+zeta1*zeta1+zeta1*zeta2,zeta1*zeta2+zeta2*zeta2 )
                 ! see if root is closer to zero or to one
                 test = one + two*( zeta1-zeta2 )*( zeta1+zeta2 )
                 if( test>=zero ) then
                    ! root is close to zero, compute directly
                    b = ( zeta1*zeta1+zeta2*zeta2+one )*half
                    c = zeta2*zeta2
                    t = real( c / ( b+sqrt( abs( b*b-c ) ) ),KIND=dp)
                    sine = ( alpha / absest ) / ( one-t )
                    cosine = -( gamma / absest ) / t
                    sestpr = sqrt( t+four*eps*eps*norma )*absest
                 else
                    ! root is closer to one, shift by that amount
                    b = ( zeta2*zeta2+zeta1*zeta1-one )*half
                    c = zeta1*zeta1
                    if( b>=zero ) then
                       t = -c / ( b+sqrt( b*b+c ) )
                    else
                       t = b - sqrt( b*b+c )
                    end if
                    sine = -( alpha / absest ) / t
                    cosine = -( gamma / absest ) / ( one+t )
                    sestpr = sqrt( one+t+four*eps*eps*norma )*absest
                 end if
                 tmp = real( sqrt( sine * conjg( sine )+ cosine * conjg( cosine ) ),KIND=dp)
                           
                 s = sine / tmp
                 c = cosine / tmp
                 return
              end if
           end if
           return
     end subroutine stdlib_zlaic1




     pure module subroutine stdlib_slals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
     !! SLALS0 applies back the multiplying factors of either the left or the
     !! right singular vector matrix of a diagonal matrix appended by a row
     !! to the right hand side matrix B in solving the least squares problem
     !! using the divide-and-conquer SVD approach.
     !! For the left singular vector matrix, three types of orthogonal
     !! matrices are involved:
     !! (1L) Givens rotations: the number of such rotations is GIVPTR; the
     !! pairs of columns/rows they were applied to are stored in GIVCOL;
     !! and the C- and S-values of these rotations are stored in GIVNUM.
     !! (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
     !! row, and for J=2:N, PERM(J)-th row of B is to be moved to the
     !! J-th row.
     !! (3L) The left singular vector matrix of the remaining matrix.
     !! For the right singular vector matrix, four types of orthogonal
     !! matrices are involved:
     !! (1R) The right singular vector matrix of the remaining matrix.
     !! (2R) If SQRE = 1, one extra Givens rotation to generate the right
     !! null space.
     !! (3R) The inverse transformation of (2L).
     !! (4R) The inverse transformation of (1L).
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: c, s
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(out) :: bx(ldbx,*), work(*)
           real(sp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, m, n, nlp1
           real(sp) :: diflj, difrj, dj, dsigj, dsigjp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( nrhs<1_ilp ) then
              info = -5_ilp
           else if( ldb<n ) then
              info = -7_ilp
           else if( ldbx<n ) then
              info = -9_ilp
           else if( givptr<0_ilp ) then
              info = -11_ilp
           else if( ldgcol<n ) then
              info = -13_ilp
           else if( ldgnum<n ) then
              info = -15_ilp
           else if( k<1_ilp ) then
              info = -20_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLALS0', -info )
              return
           end if
           m = n + sqre
           nlp1 = nl + 1_ilp
           if( icompq==0_ilp ) then
              ! apply back orthogonal transformations from the left.
              ! step (1l): apply back the givens rotations performed.
              do i = 1, givptr
                 call stdlib_srot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb, &
                           givnum( i, 2_ilp ),givnum( i, 1_ilp ) )
              end do
              ! step (2l): permute rows of b.
              call stdlib_scopy( nrhs, b( nlp1, 1_ilp ), ldb, bx( 1_ilp, 1_ilp ), ldbx )
              do i = 2, n
                 call stdlib_scopy( nrhs, b( perm( i ), 1_ilp ), ldb, bx( i, 1_ilp ), ldbx )
              end do
              ! step (3l): apply the inverse of the left singular vector
              ! matrix to bx.
              if( k==1_ilp ) then
                 call stdlib_scopy( nrhs, bx, ldbx, b, ldb )
                 if( z( 1_ilp )<zero ) then
                    call stdlib_sscal( nrhs, negone, b, ldb )
                 end if
              else
                 loop_50: do j = 1, k
                    diflj = difl( j )
                    dj = poles( j, 1_ilp )
                    dsigj = -poles( j, 2_ilp )
                    if( j<k ) then
                       difrj = -difr( j, 1_ilp )
                       dsigjp = -poles( j+1, 2_ilp )
                    end if
                    if( ( z( j )==zero ) .or. ( poles( j, 2_ilp )==zero ) )then
                       work( j ) = zero
                    else
                       work( j ) = -poles( j, 2_ilp )*z( j ) / diflj /( poles( j, 2_ilp )+dj )
                    end if
                    do i = 1, j - 1
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          work( i ) = zero
                       else
                          work( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_slamc3( poles( i, 2_ilp ), dsigj &
                                    )-diflj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    do i = j + 1, k
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          work( i ) = zero
                       else
                          work( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_slamc3( poles( i, 2_ilp ), &
                                    dsigjp )+difrj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    work( 1_ilp ) = negone
                    temp = stdlib_snrm2( k, work, 1_ilp )
                    call stdlib_sgemv( 'T', k, nrhs, one, bx, ldbx, work, 1_ilp, zero,b( j, 1_ilp ), ldb )
                              
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, temp, one, 1_ilp, nrhs, b( j, 1_ilp ),ldb, info )
                              
                 end do loop_50
              end if
              ! move the deflated rows of bx to b also.
              if( k<max( m, n ) )call stdlib_slacpy( 'A', n-k, nrhs, bx( k+1, 1_ilp ), ldbx,b( k+1, 1_ilp &
                        ), ldb )
           else
              ! apply back the right orthogonal transformations.
              ! step (1r): apply back the new right singular vector matrix
              ! to b.
              if( k==1_ilp ) then
                 call stdlib_scopy( nrhs, b, ldb, bx, ldbx )
              else
                 do j = 1, k
                    dsigj = poles( j, 2_ilp )
                    if( z( j )==zero ) then
                       work( j ) = zero
                    else
                       work( j ) = -z( j ) / difl( j ) /( dsigj+poles( j, 1_ilp ) ) / difr( j, 2_ilp )
                                 
                    end if
                    do i = 1, j - 1
                       if( z( j )==zero ) then
                          work( i ) = zero
                       else
                          work( i ) = z( j ) / ( stdlib_slamc3( dsigj, -poles( i+1,2_ilp ) )-difr( i, &
                                    1_ilp ) ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    do i = j + 1, k
                       if( z( j )==zero ) then
                          work( i ) = zero
                       else
                          work( i ) = z( j ) / ( stdlib_slamc3( dsigj, -poles( i,2_ilp ) )-difl( i ) )&
                                     /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    call stdlib_sgemv( 'T', k, nrhs, one, b, ldb, work, 1_ilp, zero,bx( j, 1_ilp ), ldbx )
                              
                 end do
              end if
              ! step (2r): if sqre = 1, apply back the rotation that is
              ! related to the right null space of the subproblem.
              if( sqre==1_ilp ) then
                 call stdlib_scopy( nrhs, b( m, 1_ilp ), ldb, bx( m, 1_ilp ), ldbx )
                 call stdlib_srot( nrhs, bx( 1_ilp, 1_ilp ), ldbx, bx( m, 1_ilp ), ldbx, c, s )
              end if
              if( k<max( m, n ) )call stdlib_slacpy( 'A', n-k, nrhs, b( k+1, 1_ilp ), ldb, bx( k+1, 1_ilp &
                        ),ldbx )
              ! step (3r): permute rows of b.
              call stdlib_scopy( nrhs, bx( 1_ilp, 1_ilp ), ldbx, b( nlp1, 1_ilp ), ldb )
              if( sqre==1_ilp ) then
                 call stdlib_scopy( nrhs, bx( m, 1_ilp ), ldbx, b( m, 1_ilp ), ldb )
              end if
              do i = 2, n
                 call stdlib_scopy( nrhs, bx( i, 1_ilp ), ldbx, b( perm( i ), 1_ilp ), ldb )
              end do
              ! step (4r): apply back the givens rotations performed.
              do i = givptr, 1, -1
                 call stdlib_srot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb, &
                           givnum( i, 2_ilp ),-givnum( i, 1_ilp ) )
              end do
           end if
           return
     end subroutine stdlib_slals0

     pure module subroutine stdlib_dlals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
     !! DLALS0 applies back the multiplying factors of either the left or the
     !! right singular vector matrix of a diagonal matrix appended by a row
     !! to the right hand side matrix B in solving the least squares problem
     !! using the divide-and-conquer SVD approach.
     !! For the left singular vector matrix, three types of orthogonal
     !! matrices are involved:
     !! (1L) Givens rotations: the number of such rotations is GIVPTR; the
     !! pairs of columns/rows they were applied to are stored in GIVCOL;
     !! and the C- and S-values of these rotations are stored in GIVNUM.
     !! (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
     !! row, and for J=2:N, PERM(J)-th row of B is to be moved to the
     !! J-th row.
     !! (3L) The left singular vector matrix of the remaining matrix.
     !! For the right singular vector matrix, four types of orthogonal
     !! matrices are involved:
     !! (1R) The right singular vector matrix of the remaining matrix.
     !! (2R) If SQRE = 1, one extra Givens rotation to generate the right
     !! null space.
     !! (3R) The inverse transformation of (2L).
     !! (4R) The inverse transformation of (1L).
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: c, s
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(out) :: bx(ldbx,*), work(*)
           real(dp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, m, n, nlp1
           real(dp) :: diflj, difrj, dj, dsigj, dsigjp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( nrhs<1_ilp ) then
              info = -5_ilp
           else if( ldb<n ) then
              info = -7_ilp
           else if( ldbx<n ) then
              info = -9_ilp
           else if( givptr<0_ilp ) then
              info = -11_ilp
           else if( ldgcol<n ) then
              info = -13_ilp
           else if( ldgnum<n ) then
              info = -15_ilp
           else if( k<1_ilp ) then
              info = -20_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLALS0', -info )
              return
           end if
           m = n + sqre
           nlp1 = nl + 1_ilp
           if( icompq==0_ilp ) then
              ! apply back orthogonal transformations from the left.
              ! step (1l): apply back the givens rotations performed.
              do i = 1, givptr
                 call stdlib_drot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb, &
                           givnum( i, 2_ilp ),givnum( i, 1_ilp ) )
              end do
              ! step (2l): permute rows of b.
              call stdlib_dcopy( nrhs, b( nlp1, 1_ilp ), ldb, bx( 1_ilp, 1_ilp ), ldbx )
              do i = 2, n
                 call stdlib_dcopy( nrhs, b( perm( i ), 1_ilp ), ldb, bx( i, 1_ilp ), ldbx )
              end do
              ! step (3l): apply the inverse of the left singular vector
              ! matrix to bx.
              if( k==1_ilp ) then
                 call stdlib_dcopy( nrhs, bx, ldbx, b, ldb )
                 if( z( 1_ilp )<zero ) then
                    call stdlib_dscal( nrhs, negone, b, ldb )
                 end if
              else
                 loop_50: do j = 1, k
                    diflj = difl( j )
                    dj = poles( j, 1_ilp )
                    dsigj = -poles( j, 2_ilp )
                    if( j<k ) then
                       difrj = -difr( j, 1_ilp )
                       dsigjp = -poles( j+1, 2_ilp )
                    end if
                    if( ( z( j )==zero ) .or. ( poles( j, 2_ilp )==zero ) )then
                       work( j ) = zero
                    else
                       work( j ) = -poles( j, 2_ilp )*z( j ) / diflj /( poles( j, 2_ilp )+dj )
                    end if
                    do i = 1, j - 1
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          work( i ) = zero
                       else
                          work( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_dlamc3( poles( i, 2_ilp ), dsigj &
                                    )-diflj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    do i = j + 1, k
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          work( i ) = zero
                       else
                          work( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_dlamc3( poles( i, 2_ilp ), &
                                    dsigjp )+difrj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    work( 1_ilp ) = negone
                    temp = stdlib_dnrm2( k, work, 1_ilp )
                    call stdlib_dgemv( 'T', k, nrhs, one, bx, ldbx, work, 1_ilp, zero,b( j, 1_ilp ), ldb )
                              
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, temp, one, 1_ilp, nrhs, b( j, 1_ilp ),ldb, info )
                              
                 end do loop_50
              end if
              ! move the deflated rows of bx to b also.
              if( k<max( m, n ) )call stdlib_dlacpy( 'A', n-k, nrhs, bx( k+1, 1_ilp ), ldbx,b( k+1, 1_ilp &
                        ), ldb )
           else
              ! apply back the right orthogonal transformations.
              ! step (1r): apply back the new right singular vector matrix
              ! to b.
              if( k==1_ilp ) then
                 call stdlib_dcopy( nrhs, b, ldb, bx, ldbx )
              else
                 do j = 1, k
                    dsigj = poles( j, 2_ilp )
                    if( z( j )==zero ) then
                       work( j ) = zero
                    else
                       work( j ) = -z( j ) / difl( j ) /( dsigj+poles( j, 1_ilp ) ) / difr( j, 2_ilp )
                                 
                    end if
                    do i = 1, j - 1
                       if( z( j )==zero ) then
                          work( i ) = zero
                       else
                          work( i ) = z( j ) / ( stdlib_dlamc3( dsigj, -poles( i+1,2_ilp ) )-difr( i, &
                                    1_ilp ) ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    do i = j + 1, k
                       if( z( j )==zero ) then
                          work( i ) = zero
                       else
                          work( i ) = z( j ) / ( stdlib_dlamc3( dsigj, -poles( i,2_ilp ) )-difl( i ) )&
                                     /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    call stdlib_dgemv( 'T', k, nrhs, one, b, ldb, work, 1_ilp, zero,bx( j, 1_ilp ), ldbx )
                              
                 end do
              end if
              ! step (2r): if sqre = 1, apply back the rotation that is
              ! related to the right null space of the subproblem.
              if( sqre==1_ilp ) then
                 call stdlib_dcopy( nrhs, b( m, 1_ilp ), ldb, bx( m, 1_ilp ), ldbx )
                 call stdlib_drot( nrhs, bx( 1_ilp, 1_ilp ), ldbx, bx( m, 1_ilp ), ldbx, c, s )
              end if
              if( k<max( m, n ) )call stdlib_dlacpy( 'A', n-k, nrhs, b( k+1, 1_ilp ), ldb, bx( k+1, 1_ilp &
                        ),ldbx )
              ! step (3r): permute rows of b.
              call stdlib_dcopy( nrhs, bx( 1_ilp, 1_ilp ), ldbx, b( nlp1, 1_ilp ), ldb )
              if( sqre==1_ilp ) then
                 call stdlib_dcopy( nrhs, bx( m, 1_ilp ), ldbx, b( m, 1_ilp ), ldb )
              end if
              do i = 2, n
                 call stdlib_dcopy( nrhs, bx( i, 1_ilp ), ldbx, b( perm( i ), 1_ilp ), ldb )
              end do
              ! step (4r): apply back the givens rotations performed.
              do i = givptr, 1, -1
                 call stdlib_drot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb, &
                           givnum( i, 2_ilp ),-givnum( i, 1_ilp ) )
              end do
           end if
           return
     end subroutine stdlib_dlals0


     pure module subroutine stdlib_clals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
     !! CLALS0 applies back the multiplying factors of either the left or the
     !! right singular vector matrix of a diagonal matrix appended by a row
     !! to the right hand side matrix B in solving the least squares problem
     !! using the divide-and-conquer SVD approach.
     !! For the left singular vector matrix, three types of orthogonal
     !! matrices are involved:
     !! (1L) Givens rotations: the number of such rotations is GIVPTR; the
     !! pairs of columns/rows they were applied to are stored in GIVCOL;
     !! and the C- and S-values of these rotations are stored in GIVNUM.
     !! (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
     !! row, and for J=2:N, PERM(J)-th row of B is to be moved to the
     !! J-th row.
     !! (3L) The left singular vector matrix of the remaining matrix.
     !! For the right singular vector matrix, four types of orthogonal
     !! matrices are involved:
     !! (1R) The right singular vector matrix of the remaining matrix.
     !! (2R) If SQRE = 1, one extra Givens rotation to generate the right
     !! null space.
     !! (3R) The inverse transformation of (2L).
     !! (4R) The inverse transformation of (1L).
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: c, s
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(sp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: bx(ldbx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jcol, jrow, m, n, nlp1
           real(sp) :: diflj, difrj, dj, dsigj, dsigjp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( nrhs<1_ilp ) then
              info = -5_ilp
           else if( ldb<n ) then
              info = -7_ilp
           else if( ldbx<n ) then
              info = -9_ilp
           else if( givptr<0_ilp ) then
              info = -11_ilp
           else if( ldgcol<n ) then
              info = -13_ilp
           else if( ldgnum<n ) then
              info = -15_ilp
           else if( k<1_ilp ) then
              info = -20_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLALS0', -info )
              return
           end if
           m = n + sqre
           nlp1 = nl + 1_ilp
           if( icompq==0_ilp ) then
              ! apply back orthogonal transformations from the left.
              ! step (1l): apply back the givens rotations performed.
              do i = 1, givptr
                 call stdlib_csrot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb,&
                            givnum( i, 2_ilp ),givnum( i, 1_ilp ) )
              end do
              ! step (2l): permute rows of b.
              call stdlib_ccopy( nrhs, b( nlp1, 1_ilp ), ldb, bx( 1_ilp, 1_ilp ), ldbx )
              do i = 2, n
                 call stdlib_ccopy( nrhs, b( perm( i ), 1_ilp ), ldb, bx( i, 1_ilp ), ldbx )
              end do
              ! step (3l): apply the inverse of the left singular vector
              ! matrix to bx.
              if( k==1_ilp ) then
                 call stdlib_ccopy( nrhs, bx, ldbx, b, ldb )
                 if( z( 1_ilp )<zero ) then
                    call stdlib_csscal( nrhs, negone, b, ldb )
                 end if
              else
                 loop_100: do j = 1, k
                    diflj = difl( j )
                    dj = poles( j, 1_ilp )
                    dsigj = -poles( j, 2_ilp )
                    if( j<k ) then
                       difrj = -difr( j, 1_ilp )
                       dsigjp = -poles( j+1, 2_ilp )
                    end if
                    if( ( z( j )==zero ) .or. ( poles( j, 2_ilp )==zero ) )then
                       rwork( j ) = zero
                    else
                       rwork( j ) = -poles( j, 2_ilp )*z( j ) / diflj /( poles( j, 2_ilp )+dj )
                    end if
                    do i = 1, j - 1
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_slamc3( poles( i, 2_ilp ), &
                                    dsigj )-diflj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    do i = j + 1, k
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_slamc3( poles( i, 2_ilp ), &
                                    dsigjp )+difrj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    rwork( 1_ilp ) = negone
                    temp = stdlib_snrm2( k, rwork, 1_ilp )
                    ! since b and bx are complex, the following call to stdlib_sgemv
                    ! is performed in two steps (real and imaginary parts).
                    ! call stdlib_sgemv( 't', k, nrhs, one, bx, ldbx, work, 1, zero,
          ! $                     b( j, 1 ), ldb )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = real( bx( jrow, jcol ),KIND=sp)
                       end do
                    end do
                    call stdlib_sgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k ), 1_ilp )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = aimag( bx( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_sgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k+nrhs ), 1_ilp )
                    do jcol = 1, nrhs
                       b( j, jcol ) = cmplx( rwork( jcol+k ),rwork( jcol+k+nrhs ),KIND=sp)
                    end do
                    call stdlib_clascl( 'G', 0_ilp, 0_ilp, temp, one, 1_ilp, nrhs, b( j, 1_ilp ),ldb, info )
                              
                 end do loop_100
              end if
              ! move the deflated rows of bx to b also.
              if( k<max( m, n ) )call stdlib_clacpy( 'A', n-k, nrhs, bx( k+1, 1_ilp ), ldbx,b( k+1, 1_ilp &
                        ), ldb )
           else
              ! apply back the right orthogonal transformations.
              ! step (1r): apply back the new right singular vector matrix
              ! to b.
              if( k==1_ilp ) then
                 call stdlib_ccopy( nrhs, b, ldb, bx, ldbx )
              else
                 loop_180: do j = 1, k
                    dsigj = poles( j, 2_ilp )
                    if( z( j )==zero ) then
                       rwork( j ) = zero
                    else
                       rwork( j ) = -z( j ) / difl( j ) /( dsigj+poles( j, 1_ilp ) ) / difr( j, 2_ilp )
                                 
                    end if
                    do i = 1, j - 1
                       if( z( j )==zero ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = z( j ) / ( stdlib_slamc3( dsigj, -poles( i+1,2_ilp ) )-difr( i,&
                                     1_ilp ) ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    do i = j + 1, k
                       if( z( j )==zero ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = z( j ) / ( stdlib_slamc3( dsigj, -poles( i,2_ilp ) )-difl( i ) &
                                    ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    ! since b and bx are complex, the following call to stdlib_sgemv
                    ! is performed in two steps (real and imaginary parts).
                    ! call stdlib_sgemv( 't', k, nrhs, one, b, ldb, work, 1, zero,
          ! $                     bx( j, 1 ), ldbx )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = real( b( jrow, jcol ),KIND=sp)
                       end do
                    end do
                    call stdlib_sgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k ), 1_ilp )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = aimag( b( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_sgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k+nrhs ), 1_ilp )
                    do jcol = 1, nrhs
                       bx( j, jcol ) = cmplx( rwork( jcol+k ),rwork( jcol+k+nrhs ),KIND=sp)
                                 
                    end do
                 end do loop_180
              end if
              ! step (2r): if sqre = 1, apply back the rotation that is
              ! related to the right null space of the subproblem.
              if( sqre==1_ilp ) then
                 call stdlib_ccopy( nrhs, b( m, 1_ilp ), ldb, bx( m, 1_ilp ), ldbx )
                 call stdlib_csrot( nrhs, bx( 1_ilp, 1_ilp ), ldbx, bx( m, 1_ilp ), ldbx, c, s )
              end if
              if( k<max( m, n ) )call stdlib_clacpy( 'A', n-k, nrhs, b( k+1, 1_ilp ), ldb,bx( k+1, 1_ilp )&
                        , ldbx )
              ! step (3r): permute rows of b.
              call stdlib_ccopy( nrhs, bx( 1_ilp, 1_ilp ), ldbx, b( nlp1, 1_ilp ), ldb )
              if( sqre==1_ilp ) then
                 call stdlib_ccopy( nrhs, bx( m, 1_ilp ), ldbx, b( m, 1_ilp ), ldb )
              end if
              do i = 2, n
                 call stdlib_ccopy( nrhs, bx( i, 1_ilp ), ldbx, b( perm( i ), 1_ilp ), ldb )
              end do
              ! step (4r): apply back the givens rotations performed.
              do i = givptr, 1, -1
                 call stdlib_csrot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb,&
                            givnum( i, 2_ilp ),-givnum( i, 1_ilp ) )
              end do
           end if
           return
     end subroutine stdlib_clals0

     pure module subroutine stdlib_zlals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
     !! ZLALS0 applies back the multiplying factors of either the left or the
     !! right singular vector matrix of a diagonal matrix appended by a row
     !! to the right hand side matrix B in solving the least squares problem
     !! using the divide-and-conquer SVD approach.
     !! For the left singular vector matrix, three types of orthogonal
     !! matrices are involved:
     !! (1L) Givens rotations: the number of such rotations is GIVPTR; the
     !! pairs of columns/rows they were applied to are stored in GIVCOL;
     !! and the C- and S-values of these rotations are stored in GIVNUM.
     !! (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
     !! row, and for J=2:N, PERM(J)-th row of B is to be moved to the
     !! J-th row.
     !! (3L) The left singular vector matrix of the remaining matrix.
     !! For the right singular vector matrix, four types of orthogonal
     !! matrices are involved:
     !! (1R) The right singular vector matrix of the remaining matrix.
     !! (2R) If SQRE = 1, one extra Givens rotation to generate the right
     !! null space.
     !! (3R) The inverse transformation of (2L).
     !! (4R) The inverse transformation of (1L).
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: c, s
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(dp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: bx(ldbx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jcol, jrow, m, n, nlp1
           real(dp) :: diflj, difrj, dj, dsigj, dsigjp, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( nrhs<1_ilp ) then
              info = -5_ilp
           else if( ldb<n ) then
              info = -7_ilp
           else if( ldbx<n ) then
              info = -9_ilp
           else if( givptr<0_ilp ) then
              info = -11_ilp
           else if( ldgcol<n ) then
              info = -13_ilp
           else if( ldgnum<n ) then
              info = -15_ilp
           else if( k<1_ilp ) then
              info = -20_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLALS0', -info )
              return
           end if
           m = n + sqre
           nlp1 = nl + 1_ilp
           if( icompq==0_ilp ) then
              ! apply back orthogonal transformations from the left.
              ! step (1l): apply back the givens rotations performed.
              do i = 1, givptr
                 call stdlib_zdrot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb,&
                            givnum( i, 2_ilp ),givnum( i, 1_ilp ) )
              end do
              ! step (2l): permute rows of b.
              call stdlib_zcopy( nrhs, b( nlp1, 1_ilp ), ldb, bx( 1_ilp, 1_ilp ), ldbx )
              do i = 2, n
                 call stdlib_zcopy( nrhs, b( perm( i ), 1_ilp ), ldb, bx( i, 1_ilp ), ldbx )
              end do
              ! step (3l): apply the inverse of the left singular vector
              ! matrix to bx.
              if( k==1_ilp ) then
                 call stdlib_zcopy( nrhs, bx, ldbx, b, ldb )
                 if( z( 1_ilp )<zero ) then
                    call stdlib_zdscal( nrhs, negone, b, ldb )
                 end if
              else
                 loop_100: do j = 1, k
                    diflj = difl( j )
                    dj = poles( j, 1_ilp )
                    dsigj = -poles( j, 2_ilp )
                    if( j<k ) then
                       difrj = -difr( j, 1_ilp )
                       dsigjp = -poles( j+1, 2_ilp )
                    end if
                    if( ( z( j )==zero ) .or. ( poles( j, 2_ilp )==zero ) )then
                       rwork( j ) = zero
                    else
                       rwork( j ) = -poles( j, 2_ilp )*z( j ) / diflj /( poles( j, 2_ilp )+dj )
                    end if
                    do i = 1, j - 1
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_dlamc3( poles( i, 2_ilp ), &
                                    dsigj )-diflj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    do i = j + 1, k
                       if( ( z( i )==zero ) .or.( poles( i, 2_ilp )==zero ) ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = poles( i, 2_ilp )*z( i ) /( stdlib_dlamc3( poles( i, 2_ilp ), &
                                    dsigjp )+difrj ) / ( poles( i, 2_ilp )+dj )
                       end if
                    end do
                    rwork( 1_ilp ) = negone
                    temp = stdlib_dnrm2( k, rwork, 1_ilp )
                    ! since b and bx are complex, the following call to stdlib_dgemv
                    ! is performed in two steps (real and imaginary parts).
                    ! call stdlib_dgemv( 't', k, nrhs, one, bx, ldbx, work, 1, zero,
          ! $                     b( j, 1 ), ldb )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = real( bx( jrow, jcol ),KIND=dp)
                       end do
                    end do
                    call stdlib_dgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k ), 1_ilp )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = aimag( bx( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_dgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k+nrhs ), 1_ilp )
                    do jcol = 1, nrhs
                       b( j, jcol ) = cmplx( rwork( jcol+k ),rwork( jcol+k+nrhs ),KIND=dp)
                    end do
                    call stdlib_zlascl( 'G', 0_ilp, 0_ilp, temp, one, 1_ilp, nrhs, b( j, 1_ilp ),ldb, info )
                              
                 end do loop_100
              end if
              ! move the deflated rows of bx to b also.
              if( k<max( m, n ) )call stdlib_zlacpy( 'A', n-k, nrhs, bx( k+1, 1_ilp ), ldbx,b( k+1, 1_ilp &
                        ), ldb )
           else
              ! apply back the right orthogonal transformations.
              ! step (1r): apply back the new right singular vector matrix
              ! to b.
              if( k==1_ilp ) then
                 call stdlib_zcopy( nrhs, b, ldb, bx, ldbx )
              else
                 loop_180: do j = 1, k
                    dsigj = poles( j, 2_ilp )
                    if( z( j )==zero ) then
                       rwork( j ) = zero
                    else
                       rwork( j ) = -z( j ) / difl( j ) /( dsigj+poles( j, 1_ilp ) ) / difr( j, 2_ilp )
                                 
                    end if
                    do i = 1, j - 1
                       if( z( j )==zero ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = z( j ) / ( stdlib_dlamc3( dsigj, -poles( i+1,2_ilp ) )-difr( i,&
                                     1_ilp ) ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    do i = j + 1, k
                       if( z( j )==zero ) then
                          rwork( i ) = zero
                       else
                          rwork( i ) = z( j ) / ( stdlib_dlamc3( dsigj, -poles( i,2_ilp ) )-difl( i ) &
                                    ) /( dsigj+poles( i, 1_ilp ) ) / difr( i, 2_ilp )
                       end if
                    end do
                    ! since b and bx are complex, the following call to stdlib_dgemv
                    ! is performed in two steps (real and imaginary parts).
                    ! call stdlib_dgemv( 't', k, nrhs, one, b, ldb, work, 1, zero,
          ! $                     bx( j, 1 ), ldbx )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = real( b( jrow, jcol ),KIND=dp)
                       end do
                    end do
                    call stdlib_dgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k ), 1_ilp )
                    i = k + nrhs*2_ilp
                    do jcol = 1, nrhs
                       do jrow = 1, k
                          i = i + 1_ilp
                          rwork( i ) = aimag( b( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_dgemv( 'T', k, nrhs, one, rwork( 1_ilp+k+nrhs*2_ilp ), k,rwork( 1_ilp ), 1_ilp, &
                              zero, rwork( 1_ilp+k+nrhs ), 1_ilp )
                    do jcol = 1, nrhs
                       bx( j, jcol ) = cmplx( rwork( jcol+k ),rwork( jcol+k+nrhs ),KIND=dp)
                                 
                    end do
                 end do loop_180
              end if
              ! step (2r): if sqre = 1, apply back the rotation that is
              ! related to the right null space of the subproblem.
              if( sqre==1_ilp ) then
                 call stdlib_zcopy( nrhs, b( m, 1_ilp ), ldb, bx( m, 1_ilp ), ldbx )
                 call stdlib_zdrot( nrhs, bx( 1_ilp, 1_ilp ), ldbx, bx( m, 1_ilp ), ldbx, c, s )
              end if
              if( k<max( m, n ) )call stdlib_zlacpy( 'A', n-k, nrhs, b( k+1, 1_ilp ), ldb, bx( k+1, 1_ilp &
                        ),ldbx )
              ! step (3r): permute rows of b.
              call stdlib_zcopy( nrhs, bx( 1_ilp, 1_ilp ), ldbx, b( nlp1, 1_ilp ), ldb )
              if( sqre==1_ilp ) then
                 call stdlib_zcopy( nrhs, bx( m, 1_ilp ), ldbx, b( m, 1_ilp ), ldb )
              end if
              do i = 2, n
                 call stdlib_zcopy( nrhs, bx( i, 1_ilp ), ldbx, b( perm( i ), 1_ilp ), ldb )
              end do
              ! step (4r): apply back the givens rotations performed.
              do i = givptr, 1, -1
                 call stdlib_zdrot( nrhs, b( givcol( i, 2_ilp ), 1_ilp ), ldb,b( givcol( i, 1_ilp ), 1_ilp ), ldb,&
                            givnum( i, 2_ilp ),-givnum( i, 1_ilp ) )
              end do
           end if
           return
     end subroutine stdlib_zlals0




     pure module subroutine stdlib_slalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
     !! SLALSA is an itermediate step in solving the least squares problem
     !! by computing the SVD of the coefficient matrix in compact form (The
     !! singular vectors are computed as products of simple orthorgonal
     !! matrices.).
     !! If ICOMPQ = 0, SLALSA applies the inverse of the left singular vector
     !! matrix of an upper bidiagonal matrix to the right hand side; and if
     !! ICOMPQ = 1, SLALSA applies the right singular vector matrix to the
     !! right hand side. The singular vector matrices were generated in
     !! compact form by SLALSA.
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, work,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(out) :: bx(ldbx,*), work(*)
           real(sp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, im1, inode, j, lf, ll, lvl, lvl2, nd, ndb1, ndiml, ndimr, &
                     nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqre
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<smlsiz ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ldb<n ) then
              info = -6_ilp
           else if( ldbx<n ) then
              info = -8_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldgcol<n ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLALSA', -info )
              return
           end if
           ! book-keeping and  setting up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           call stdlib_slasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! the following code applies back the left singular vector factors.
           ! for applying back the right singular vector factors, go to 50.
           if( icompq==1_ilp ) then
              go to 50
           end if
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_slasdq. the corresponding left and right singular vector
           ! matrices are in explicit form. first apply back the left
           ! singular vector matrices.
           ndb1 = ( nd+1 ) / 2_ilp
           do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              call stdlib_sgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,b( nlf, 1_ilp ), ldb, &
                        zero, bx( nlf, 1_ilp ), ldbx )
              call stdlib_sgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,b( nrf, 1_ilp ), ldb, &
                        zero, bx( nrf, 1_ilp ), ldbx )
           end do
           ! next copy the rows of b that correspond to unchanged rows
           ! in the bidiagonal matrix to bx.
           do i = 1, nd
              ic = iwork( inode+i-1 )
              call stdlib_scopy( nrhs, b( ic, 1_ilp ), ldb, bx( ic, 1_ilp ), ldbx )
           end do
           ! finally go through the left singular vector matrices of all
           ! the other subproblems bottom-up on the tree.
           j = 2_ilp**nlvl
           sqre = 0_ilp
           do lvl = nlvl, 1, -1
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 j = j - 1_ilp
                 call stdlib_slals0( icompq, nl, nr, sqre, nrhs, bx( nlf, 1_ilp ), ldbx,b( nlf, 1_ilp ), &
                 ldb, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, lvl2 &
                 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl ), k( &
                           j ), c( j ), s( j ), work,info )
              end do
           end do
           go to 90
           ! icompq = 1: applying back the right singular vector factors.
           50 continue
           ! first now go through the right singular vector matrices of all
           ! the tree nodes top-down.
           j = 0_ilp
           loop_70: do lvl = 1, nlvl
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = ll, lf, -1
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqre = 0_ilp
                 else
                    sqre = 1_ilp
                 end if
                 j = j + 1_ilp
                 call stdlib_slals0( icompq, nl, nr, sqre, nrhs, b( nlf, 1_ilp ), ldb,bx( nlf, 1_ilp ), &
                 ldbx, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, &
                 lvl2 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl )&
                           , k( j ), c( j ), s( j ), work,info )
              end do
           end do loop_70
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_slasdq. the corresponding right singular vector
           ! matrices are in explicit form. apply them back.
           ndb1 = ( nd+1 ) / 2_ilp
           do i = ndb1, nd
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlp1 = nl + 1_ilp
              if( i==nd ) then
                 nrp1 = nr
              else
                 nrp1 = nr + 1_ilp
              end if
              nlf = ic - nl
              nrf = ic + 1_ilp
              call stdlib_sgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,b( nlf, 1_ilp ), &
                        ldb, zero, bx( nlf, 1_ilp ), ldbx )
              call stdlib_sgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,b( nrf, 1_ilp ), &
                        ldb, zero, bx( nrf, 1_ilp ), ldbx )
           end do
           90 continue
           return
     end subroutine stdlib_slalsa

     pure module subroutine stdlib_dlalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
     !! DLALSA is an itermediate step in solving the least squares problem
     !! by computing the SVD of the coefficient matrix in compact form (The
     !! singular vectors are computed as products of simple orthorgonal
     !! matrices.).
     !! If ICOMPQ = 0, DLALSA applies the inverse of the left singular vector
     !! matrix of an upper bidiagonal matrix to the right hand side; and if
     !! ICOMPQ = 1, DLALSA applies the right singular vector matrix to the
     !! right hand side. The singular vector matrices were generated in
     !! compact form by DLALSA.
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, work,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(out) :: bx(ldbx,*), work(*)
           real(dp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, im1, inode, j, lf, ll, lvl, lvl2, nd, ndb1, ndiml, ndimr, &
                     nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqre
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<smlsiz ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ldb<n ) then
              info = -6_ilp
           else if( ldbx<n ) then
              info = -8_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldgcol<n ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLALSA', -info )
              return
           end if
           ! book-keeping and  setting up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           call stdlib_dlasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! the following code applies back the left singular vector factors.
           ! for applying back the right singular vector factors, go to 50.
           if( icompq==1_ilp ) then
              go to 50
           end if
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_dlasdq. the corresponding left and right singular vector
           ! matrices are in explicit form. first apply back the left
           ! singular vector matrices.
           ndb1 = ( nd+1 ) / 2_ilp
           do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              call stdlib_dgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,b( nlf, 1_ilp ), ldb, &
                        zero, bx( nlf, 1_ilp ), ldbx )
              call stdlib_dgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,b( nrf, 1_ilp ), ldb, &
                        zero, bx( nrf, 1_ilp ), ldbx )
           end do
           ! next copy the rows of b that correspond to unchanged rows
           ! in the bidiagonal matrix to bx.
           do i = 1, nd
              ic = iwork( inode+i-1 )
              call stdlib_dcopy( nrhs, b( ic, 1_ilp ), ldb, bx( ic, 1_ilp ), ldbx )
           end do
           ! finally go through the left singular vector matrices of all
           ! the other subproblems bottom-up on the tree.
           j = 2_ilp**nlvl
           sqre = 0_ilp
           do lvl = nlvl, 1, -1
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 j = j - 1_ilp
                 call stdlib_dlals0( icompq, nl, nr, sqre, nrhs, bx( nlf, 1_ilp ), ldbx,b( nlf, 1_ilp ), &
                 ldb, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, lvl2 &
                 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl ), k( &
                           j ), c( j ), s( j ), work,info )
              end do
           end do
           go to 90
           ! icompq = 1: applying back the right singular vector factors.
           50 continue
           ! first now go through the right singular vector matrices of all
           ! the tree nodes top-down.
           j = 0_ilp
           loop_70: do lvl = 1, nlvl
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = ll, lf, -1
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqre = 0_ilp
                 else
                    sqre = 1_ilp
                 end if
                 j = j + 1_ilp
                 call stdlib_dlals0( icompq, nl, nr, sqre, nrhs, b( nlf, 1_ilp ), ldb,bx( nlf, 1_ilp ), &
                 ldbx, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, &
                 lvl2 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl )&
                           , k( j ), c( j ), s( j ), work,info )
              end do
           end do loop_70
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_dlasdq. the corresponding right singular vector
           ! matrices are in explicit form. apply them back.
           ndb1 = ( nd+1 ) / 2_ilp
           do i = ndb1, nd
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlp1 = nl + 1_ilp
              if( i==nd ) then
                 nrp1 = nr
              else
                 nrp1 = nr + 1_ilp
              end if
              nlf = ic - nl
              nrf = ic + 1_ilp
              call stdlib_dgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,b( nlf, 1_ilp ), &
                        ldb, zero, bx( nlf, 1_ilp ), ldbx )
              call stdlib_dgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,b( nrf, 1_ilp ), &
                        ldb, zero, bx( nrf, 1_ilp ), ldbx )
           end do
           90 continue
           return
     end subroutine stdlib_dlalsa


     pure module subroutine stdlib_clalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
     !! CLALSA is an itermediate step in solving the least squares problem
     !! by computing the SVD of the coefficient matrix in compact form (The
     !! singular vectors are computed as products of simple orthorgonal
     !! matrices.).
     !! If ICOMPQ = 0, CLALSA applies the inverse of the left singular vector
     !! matrix of an upper bidiagonal matrix to the right hand side; and if
     !! ICOMPQ = 1, CLALSA applies the right singular vector matrix to the
     !! right hand side. The singular vector matrices were generated in
     !! compact form by CLALSA.
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, rwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: bx(ldbx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, im1, inode, j, jcol, jimag, jreal, jrow, lf, ll, lvl, lvl2, &
                     nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqre
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<smlsiz ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ldb<n ) then
              info = -6_ilp
           else if( ldbx<n ) then
              info = -8_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldgcol<n ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLALSA', -info )
              return
           end if
           ! book-keeping and  setting up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           call stdlib_slasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! the following code applies back the left singular vector factors.
           ! for applying back the right singular vector factors, go to 170.
           if( icompq==1_ilp ) then
              go to 170
           end if
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_slasdq. the corresponding left and right singular vector
           ! matrices are in explicit form. first apply back the left
           ! singular vector matrices.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_130: do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              ! since b and bx are complex, the following call to stdlib_sgemm
              ! is performed in two steps (real and imaginary parts).
              ! call stdlib_sgemm( 't', 'n', nl, nrhs, nl, one, u( nlf, 1 ), ldu,
           ! $               b( nlf, 1 ), ldb, zero, bx( nlf, 1 ), ldbx )
              j = nl*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,rwork( 1_ilp+nl*nrhs*2_ilp &
                        ), nl, zero, rwork( 1_ilp ), nl )
              j = nl*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,rwork( 1_ilp+nl*nrhs*2_ilp &
                        ), nl, zero, rwork( 1_ilp+nl*nrhs ),nl )
              jreal = 0_ilp
              jimag = nl*nrhs
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                 end do
              end do
              ! since b and bx are complex, the following call to stdlib_sgemm
              ! is performed in two steps (real and imaginary parts).
              ! call stdlib_sgemm( 't', 'n', nr, nrhs, nr, one, u( nrf, 1 ), ldu,
          ! $               b( nrf, 1 ), ldb, zero, bx( nrf, 1 ), ldbx )
              j = nr*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,rwork( 1_ilp+nr*nrhs*2_ilp &
                        ), nr, zero, rwork( 1_ilp ), nr )
              j = nr*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,rwork( 1_ilp+nr*nrhs*2_ilp &
                        ), nr, zero, rwork( 1_ilp+nr*nrhs ),nr )
              jreal = 0_ilp
              jimag = nr*nrhs
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                 end do
              end do
           end do loop_130
           ! next copy the rows of b that correspond to unchanged rows
           ! in the bidiagonal matrix to bx.
           do i = 1, nd
              ic = iwork( inode+i-1 )
              call stdlib_ccopy( nrhs, b( ic, 1_ilp ), ldb, bx( ic, 1_ilp ), ldbx )
           end do
           ! finally go through the left singular vector matrices of all
           ! the other subproblems bottom-up on the tree.
           j = 2_ilp**nlvl
           sqre = 0_ilp
           do lvl = nlvl, 1, -1
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 j = j - 1_ilp
                 call stdlib_clals0( icompq, nl, nr, sqre, nrhs, bx( nlf, 1_ilp ), ldbx,b( nlf, 1_ilp ), &
                 ldb, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, lvl2 &
                 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl ), k( &
                           j ), c( j ), s( j ), rwork,info )
              end do
           end do
           go to 330
           ! icompq = 1: applying back the right singular vector factors.
           170 continue
           ! first now go through the right singular vector matrices of all
           ! the tree nodes top-down.
           j = 0_ilp
           loop_190: do lvl = 1, nlvl
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = ll, lf, -1
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqre = 0_ilp
                 else
                    sqre = 1_ilp
                 end if
                 j = j + 1_ilp
                 call stdlib_clals0( icompq, nl, nr, sqre, nrhs, b( nlf, 1_ilp ), ldb,bx( nlf, 1_ilp ), &
                 ldbx, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, &
                 lvl2 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl )&
                           , k( j ), c( j ), s( j ), rwork,info )
              end do
           end do loop_190
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_slasdq. the corresponding right singular vector
           ! matrices are in explicit form. apply them back.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_320: do i = ndb1, nd
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlp1 = nl + 1_ilp
              if( i==nd ) then
                 nrp1 = nr
              else
                 nrp1 = nr + 1_ilp
              end if
              nlf = ic - nl
              nrf = ic + 1_ilp
              ! since b and bx are complex, the following call to stdlib_sgemm is
              ! performed in two steps (real and imaginary parts).
              ! call stdlib_sgemm( 't', 'n', nlp1, nrhs, nlp1, one, vt( nlf, 1 ), ldu,
          ! $               b( nlf, 1 ), ldb, zero, bx( nlf, 1 ), ldbx )
              j = nlp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nlp1*nrhs*2_ilp ), nlp1, zero, rwork( 1_ilp ),nlp1 )
              j = nlp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nlp1*nrhs*2_ilp ), nlp1, zero,rwork( 1_ilp+nlp1*nrhs ), nlp1 )
              jreal = 0_ilp
              jimag = nlp1*nrhs
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                 end do
              end do
              ! since b and bx are complex, the following call to stdlib_sgemm is
              ! performed in two steps (real and imaginary parts).
              ! call stdlib_sgemm( 't', 'n', nrp1, nrhs, nrp1, one, vt( nrf, 1 ), ldu,
          ! $               b( nrf, 1 ), ldb, zero, bx( nrf, 1 ), ldbx )
              j = nrp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nrp1*nrhs*2_ilp ), nrp1, zero, rwork( 1_ilp ),nrp1 )
              j = nrp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nrp1*nrhs*2_ilp ), nrp1, zero,rwork( 1_ilp+nrp1*nrhs ), nrp1 )
              jreal = 0_ilp
              jimag = nrp1*nrhs
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                 end do
              end do
           end do loop_320
           330 continue
           return
     end subroutine stdlib_clalsa

     pure module subroutine stdlib_zlalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
     !! ZLALSA is an itermediate step in solving the least squares problem
     !! by computing the SVD of the coefficient matrix in compact form (The
     !! singular vectors are computed as products of simple orthorgonal
     !! matrices.).
     !! If ICOMPQ = 0, ZLALSA applies the inverse of the left singular vector
     !! matrix of an upper bidiagonal matrix to the right hand side; and if
     !! ICOMPQ = 1, ZLALSA applies the right singular vector matrix to the
     !! right hand side. The singular vector matrices were generated in
     !! compact form by ZLALSA.
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, rwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: bx(ldbx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, im1, inode, j, jcol, jimag, jreal, jrow, lf, ll, lvl, lvl2, &
                     nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqre
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<smlsiz ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ldb<n ) then
              info = -6_ilp
           else if( ldbx<n ) then
              info = -8_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldgcol<n ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLALSA', -info )
              return
           end if
           ! book-keeping and  setting up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           call stdlib_dlasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! the following code applies back the left singular vector factors.
           ! for applying back the right singular vector factors, go to 170.
           if( icompq==1_ilp ) then
              go to 170
           end if
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_dlasdq. the corresponding left and right singular vector
           ! matrices are in explicit form. first apply back the left
           ! singular vector matrices.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_130: do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              ! since b and bx are complex, the following call to stdlib_dgemm
              ! is performed in two steps (real and imaginary parts).
              ! call stdlib_dgemm( 't', 'n', nl, nrhs, nl, one, u( nlf, 1 ), ldu,
           ! $               b( nlf, 1 ), ldb, zero, bx( nlf, 1 ), ldbx )
              j = nl*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,rwork( 1_ilp+nl*nrhs*2_ilp &
                        ), nl, zero, rwork( 1_ilp ), nl )
              j = nl*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nl, nrhs, nl, one, u( nlf, 1_ilp ), ldu,rwork( 1_ilp+nl*nrhs*2_ilp &
                        ), nl, zero, rwork( 1_ilp+nl*nrhs ),nl )
              jreal = 0_ilp
              jimag = nl*nrhs
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nl - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
              ! since b and bx are complex, the following call to stdlib_dgemm
              ! is performed in two steps (real and imaginary parts).
              ! call stdlib_dgemm( 't', 'n', nr, nrhs, nr, one, u( nrf, 1 ), ldu,
          ! $               b( nrf, 1 ), ldb, zero, bx( nrf, 1 ), ldbx )
              j = nr*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,rwork( 1_ilp+nr*nrhs*2_ilp &
                        ), nr, zero, rwork( 1_ilp ), nr )
              j = nr*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nr, nrhs, nr, one, u( nrf, 1_ilp ), ldu,rwork( 1_ilp+nr*nrhs*2_ilp &
                        ), nr, zero, rwork( 1_ilp+nr*nrhs ),nr )
              jreal = 0_ilp
              jimag = nr*nrhs
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nr - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
           end do loop_130
           ! next copy the rows of b that correspond to unchanged rows
           ! in the bidiagonal matrix to bx.
           do i = 1, nd
              ic = iwork( inode+i-1 )
              call stdlib_zcopy( nrhs, b( ic, 1_ilp ), ldb, bx( ic, 1_ilp ), ldbx )
           end do
           ! finally go through the left singular vector matrices of all
           ! the other subproblems bottom-up on the tree.
           j = 2_ilp**nlvl
           sqre = 0_ilp
           do lvl = nlvl, 1, -1
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 j = j - 1_ilp
                 call stdlib_zlals0( icompq, nl, nr, sqre, nrhs, bx( nlf, 1_ilp ), ldbx,b( nlf, 1_ilp ), &
                 ldb, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, lvl2 &
                 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl ), k( &
                           j ), c( j ), s( j ), rwork,info )
              end do
           end do
           go to 330
           ! icompq = 1: applying back the right singular vector factors.
           170 continue
           ! first now go through the right singular vector matrices of all
           ! the tree nodes top-down.
           j = 0_ilp
           loop_190: do lvl = 1, nlvl
              lvl2 = 2_ilp*lvl - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = ll, lf, -1
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqre = 0_ilp
                 else
                    sqre = 1_ilp
                 end if
                 j = j + 1_ilp
                 call stdlib_zlals0( icompq, nl, nr, sqre, nrhs, b( nlf, 1_ilp ), ldb,bx( nlf, 1_ilp ), &
                 ldbx, perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ), ldgcol,givnum( nlf, &
                 lvl2 ), ldu, poles( nlf, lvl2 ),difl( nlf, lvl ), difr( nlf, lvl2 ),z( nlf, lvl )&
                           , k( j ), c( j ), s( j ), rwork,info )
              end do
           end do loop_190
           ! the nodes on the bottom level of the tree were solved
           ! by stdlib_dlasdq. the corresponding right singular vector
           ! matrices are in explicit form. apply them back.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_320: do i = ndb1, nd
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nr = iwork( ndimr+i1 )
              nlp1 = nl + 1_ilp
              if( i==nd ) then
                 nrp1 = nr
              else
                 nrp1 = nr + 1_ilp
              end if
              nlf = ic - nl
              nrf = ic + 1_ilp
              ! since b and bx are complex, the following call to stdlib_dgemm is
              ! performed in two steps (real and imaginary parts).
              ! call stdlib_dgemm( 't', 'n', nlp1, nrhs, nlp1, one, vt( nlf, 1 ), ldu,
          ! $               b( nlf, 1 ), ldb, zero, bx( nlf, 1 ), ldbx )
              j = nlp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nlp1*nrhs*2_ilp ), nlp1, zero, rwork( 1_ilp ),nlp1 )
              j = nlp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nlp1, nrhs, nlp1, one, vt( nlf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nlp1*nrhs*2_ilp ), nlp1, zero,rwork( 1_ilp+nlp1*nrhs ), nlp1 )
              jreal = 0_ilp
              jimag = nlp1*nrhs
              do jcol = 1, nrhs
                 do jrow = nlf, nlf + nlp1 - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
              ! since b and bx are complex, the following call to stdlib_dgemm is
              ! performed in two steps (real and imaginary parts).
              ! call stdlib_dgemm( 't', 'n', nrp1, nrhs, nrp1, one, vt( nrf, 1 ), ldu,
          ! $               b( nrf, 1 ), ldb, zero, bx( nrf, 1 ), ldbx )
              j = nrp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nrp1*nrhs*2_ilp ), nrp1, zero, rwork( 1_ilp ),nrp1 )
              j = nrp1*nrhs*2_ilp
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', nrp1, nrhs, nrp1, one, vt( nrf, 1_ilp ), ldu,rwork( 1_ilp+&
                        nrp1*nrhs*2_ilp ), nrp1, zero,rwork( 1_ilp+nrp1*nrhs ), nrp1 )
              jreal = 0_ilp
              jimag = nrp1*nrhs
              do jcol = 1, nrhs
                 do jrow = nrf, nrf + nrp1 - 1
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    bx( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
           end do loop_320
           330 continue
           return
     end subroutine stdlib_zlalsa




     pure module subroutine stdlib_slalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, iwork, &
     !! SLALSD uses the singular value decomposition of A to solve the least
     !! squares problem of finding X to minimize the Euclidean norm of each
     !! column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
     !! are N-by-NRHS. The solution X overwrites B.
     !! The singular values of A smaller than RCOND times the largest
     !! singular value are treated as zero in solving the least squares
     !! problem; in this case a minimum norm solution is returned.
     !! The actual singular values are returned in D in ascending order.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: b(ldb,*), d(*), e(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: bx, bxst, c, difl, difr, givcol, givnum, givptr, i, icmpq1, icmpq2, &
           iwk, j, k, nlvl, nm1, nsize, nsub, nwork, perm, poles, s, sizei, smlszp, sqre, st, st1,&
                      u, vt, z
           real(sp) :: cs, eps, orgnrm, r, rcnd, sn, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ( ldb<1_ilp ) .or. ( ldb<n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLALSD', -info )
              return
           end if
           eps = stdlib_slamch( 'EPSILON' )
           ! set up the tolerance.
           if( ( rcond<=zero ) .or. ( rcond>=one ) ) then
              rcnd = eps
           else
              rcnd = rcond
           end if
           rank = 0_ilp
           ! quick return if possible.
           if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              if( d( 1_ilp )==zero ) then
                 call stdlib_slaset( 'A', 1_ilp, nrhs, zero, zero, b, ldb )
              else
                 rank = 1_ilp
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, d( 1_ilp ), one, 1_ilp, nrhs, b, ldb, info )
                 d( 1_ilp ) = abs( d( 1_ilp ) )
              end if
              return
           end if
           ! rotate the matrix if it is lower bidiagonal.
           if( uplo=='L' ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( nrhs==1_ilp ) then
                    call stdlib_srot( 1_ilp, b( i, 1_ilp ), 1_ilp, b( i+1, 1_ilp ), 1_ilp, cs, sn )
                 else
                    work( i*2_ilp-1 ) = cs
                    work( i*2_ilp ) = sn
                 end if
              end do
              if( nrhs>1_ilp ) then
                 do i = 1, nrhs
                    do j = 1, n - 1
                       cs = work( j*2_ilp-1 )
                       sn = work( j*2_ilp )
                       call stdlib_srot( 1_ilp, b( j, i ), 1_ilp, b( j+1, i ), 1_ilp, cs, sn )
                    end do
                 end do
              end if
           end if
           ! scale.
           nm1 = n - 1_ilp
           orgnrm = stdlib_slanst( 'M', n, d, e )
           if( orgnrm==zero ) then
              call stdlib_slaset( 'A', n, nrhs, zero, zero, b, ldb )
              return
           end if
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, info )
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              nwork = 1_ilp + n*n
              call stdlib_slaset( 'A', n, n, zero, one, work, n )
              call stdlib_slasdq( 'U', 0_ilp, n, n, 0_ilp, nrhs, d, e, work, n, work, n, b,ldb, work( &
                        nwork ), info )
              if( info/=0_ilp ) then
                 return
              end if
              tol = rcnd*abs( d( stdlib_isamax( n, d, 1_ilp ) ) )
              do i = 1, n
                 if( d( i )<=tol ) then
                    call stdlib_slaset( 'A', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 else
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs, b( i, 1_ilp ),ldb, info )
                              
                    rank = rank + 1_ilp
                 end if
              end do
              call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, work, n, b, ldb, zero,work( nwork ), &
                        n )
              call stdlib_slacpy( 'A', n, nrhs, work( nwork ), n, b, ldb )
              ! unscale.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
              call stdlib_slasrt( 'D', n, d, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
              return
           end if
           ! book-keeping and setting up some constants.
           nlvl = int( log( real( n,KIND=sp) / real( smlsiz+1,KIND=sp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           u = 1_ilp
           vt = 1_ilp + smlsiz*n
           difl = vt + smlszp*n
           difr = difl + nlvl*n
           z = difr + nlvl*n*2_ilp
           c = z + nlvl*n
           s = c + n
           poles = s + n
           givnum = poles + 2_ilp*nlvl*n
           bx = givnum + 2_ilp*nlvl*n
           nwork = bx + n*nrhs
           sizei = 1_ilp + n
           k = sizei + n
           givptr = k + n
           perm = givptr + n
           givcol = perm + nlvl*n
           iwk = givcol + nlvl*n*2_ilp
           st = 1_ilp
           sqre = 0_ilp
           icmpq1 = 1_ilp
           icmpq2 = 0_ilp
           nsub = 0_ilp
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           loop_60: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 nsub = nsub + 1_ilp
                 iwork( nsub ) = st
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n), which is not solved
                    ! explicitly.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                    nsub = nsub + 1_ilp
                    iwork( nsub ) = n
                    iwork( sizei+nsub-1 ) = 1_ilp
                    call stdlib_scopy( nrhs, b( n, 1_ilp ), ldb, work( bx+nm1 ), n )
                 end if
                 st1 = st - 1_ilp
                 if( nsize==1_ilp ) then
                    ! this is a 1-by-1 subproblem and is not solved
                    ! explicitly.
                    call stdlib_scopy( nrhs, b( st, 1_ilp ), ldb, work( bx+st1 ), n )
                 else if( nsize<=smlsiz ) then
                    ! this is a small subproblem and is solved by stdlib_slasdq.
                    call stdlib_slaset( 'A', nsize, nsize, zero, one,work( vt+st1 ), n )
                    call stdlib_slasdq( 'U', 0_ilp, nsize, nsize, 0_ilp, nrhs, d( st ),e( st ), work( vt+&
                              st1 ), n, work( nwork ),n, b( st, 1_ilp ), ldb, work( nwork ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    call stdlib_slacpy( 'A', nsize, nrhs, b( st, 1_ilp ), ldb,work( bx+st1 ), n )
                              
                 else
                    ! a large problem. solve it using divide and conquer.
                    call stdlib_slasda( icmpq1, smlsiz, nsize, sqre, d( st ),e( st ), work( u+st1 &
                    ), n, work( vt+st1 ),iwork( k+st1 ), work( difl+st1 ),work( difr+st1 ), work( &
                    z+st1 ),work( poles+st1 ), iwork( givptr+st1 ),iwork( givcol+st1 ), n, iwork( &
                    perm+st1 ),work( givnum+st1 ), work( c+st1 ),work( s+st1 ), work( nwork ), &
                              iwork( iwk ),info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    bxst = bx + st1
                    call stdlib_slalsa( icmpq2, smlsiz, nsize, nrhs, b( st, 1_ilp ),ldb, work( bxst ),&
                     n, work( u+st1 ), n,work( vt+st1 ), iwork( k+st1 ),work( difl+st1 ), work( &
                     difr+st1 ),work( z+st1 ), work( poles+st1 ),iwork( givptr+st1 ), iwork( &
                     givcol+st1 ), n,iwork( perm+st1 ), work( givnum+st1 ),work( c+st1 ), work( s+&
                               st1 ), work( nwork ),iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                 end if
                 st = i + 1_ilp
              end if
           end do loop_60
           ! apply the singular values and treat the tiny ones as zero.
           tol = rcnd*abs( d( stdlib_isamax( n, d, 1_ilp ) ) )
           do i = 1, n
              ! some of the elements in d can be negative because 1-by-1
              ! subproblems were not solved explicitly.
              if( abs( d( i ) )<=tol ) then
                 call stdlib_slaset( 'A', 1_ilp, nrhs, zero, zero, work( bx+i-1 ), n )
              else
                 rank = rank + 1_ilp
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs,work( bx+i-1 ), n, info )
                           
              end if
              d( i ) = abs( d( i ) )
           end do
           ! now apply back the right singular vectors.
           icmpq2 = 1_ilp
           do i = 1, nsub
              st = iwork( i )
              st1 = st - 1_ilp
              nsize = iwork( sizei+i-1 )
              bxst = bx + st1
              if( nsize==1_ilp ) then
                 call stdlib_scopy( nrhs, work( bxst ), n, b( st, 1_ilp ), ldb )
              else if( nsize<=smlsiz ) then
                 call stdlib_sgemm( 'T', 'N', nsize, nrhs, nsize, one,work( vt+st1 ), n, work( &
                           bxst ), n, zero,b( st, 1_ilp ), ldb )
              else
                 call stdlib_slalsa( icmpq2, smlsiz, nsize, nrhs, work( bxst ), n,b( st, 1_ilp ), ldb,&
                  work( u+st1 ), n,work( vt+st1 ), iwork( k+st1 ),work( difl+st1 ), work( difr+&
                  st1 ),work( z+st1 ), work( poles+st1 ),iwork( givptr+st1 ), iwork( givcol+st1 ),&
                   n,iwork( perm+st1 ), work( givnum+st1 ),work( c+st1 ), work( s+st1 ), work( &
                             nwork ),iwork( iwk ), info )
                 if( info/=0_ilp ) then
                    return
                 end if
              end if
           end do
           ! unscale and sort the singular values.
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           call stdlib_slasrt( 'D', n, d, info )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
           return
     end subroutine stdlib_slalsd

     pure module subroutine stdlib_dlalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, iwork, &
     !! DLALSD uses the singular value decomposition of A to solve the least
     !! squares problem of finding X to minimize the Euclidean norm of each
     !! column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
     !! are N-by-NRHS. The solution X overwrites B.
     !! The singular values of A smaller than RCOND times the largest
     !! singular value are treated as zero in solving the least squares
     !! problem; in this case a minimum norm solution is returned.
     !! The actual singular values are returned in D in ascending order.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: b(ldb,*), d(*), e(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: bx, bxst, c, difl, difr, givcol, givnum, givptr, i, icmpq1, icmpq2, &
           iwk, j, k, nlvl, nm1, nsize, nsub, nwork, perm, poles, s, sizei, smlszp, sqre, st, st1,&
                      u, vt, z
           real(dp) :: cs, eps, orgnrm, r, rcnd, sn, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ( ldb<1_ilp ) .or. ( ldb<n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLALSD', -info )
              return
           end if
           eps = stdlib_dlamch( 'EPSILON' )
           ! set up the tolerance.
           if( ( rcond<=zero ) .or. ( rcond>=one ) ) then
              rcnd = eps
           else
              rcnd = rcond
           end if
           rank = 0_ilp
           ! quick return if possible.
           if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              if( d( 1_ilp )==zero ) then
                 call stdlib_dlaset( 'A', 1_ilp, nrhs, zero, zero, b, ldb )
              else
                 rank = 1_ilp
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, d( 1_ilp ), one, 1_ilp, nrhs, b, ldb, info )
                 d( 1_ilp ) = abs( d( 1_ilp ) )
              end if
              return
           end if
           ! rotate the matrix if it is lower bidiagonal.
           if( uplo=='L' ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( nrhs==1_ilp ) then
                    call stdlib_drot( 1_ilp, b( i, 1_ilp ), 1_ilp, b( i+1, 1_ilp ), 1_ilp, cs, sn )
                 else
                    work( i*2_ilp-1 ) = cs
                    work( i*2_ilp ) = sn
                 end if
              end do
              if( nrhs>1_ilp ) then
                 do i = 1, nrhs
                    do j = 1, n - 1
                       cs = work( j*2_ilp-1 )
                       sn = work( j*2_ilp )
                       call stdlib_drot( 1_ilp, b( j, i ), 1_ilp, b( j+1, i ), 1_ilp, cs, sn )
                    end do
                 end do
              end if
           end if
           ! scale.
           nm1 = n - 1_ilp
           orgnrm = stdlib_dlanst( 'M', n, d, e )
           if( orgnrm==zero ) then
              call stdlib_dlaset( 'A', n, nrhs, zero, zero, b, ldb )
              return
           end if
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, info )
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              nwork = 1_ilp + n*n
              call stdlib_dlaset( 'A', n, n, zero, one, work, n )
              call stdlib_dlasdq( 'U', 0_ilp, n, n, 0_ilp, nrhs, d, e, work, n, work, n, b,ldb, work( &
                        nwork ), info )
              if( info/=0_ilp ) then
                 return
              end if
              tol = rcnd*abs( d( stdlib_idamax( n, d, 1_ilp ) ) )
              do i = 1, n
                 if( d( i )<=tol ) then
                    call stdlib_dlaset( 'A', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 else
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs, b( i, 1_ilp ),ldb, info )
                              
                    rank = rank + 1_ilp
                 end if
              end do
              call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, work, n, b, ldb, zero,work( nwork ), &
                        n )
              call stdlib_dlacpy( 'A', n, nrhs, work( nwork ), n, b, ldb )
              ! unscale.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
              call stdlib_dlasrt( 'D', n, d, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
              return
           end if
           ! book-keeping and setting up some constants.
           nlvl = int( log( real( n,KIND=dp) / real( smlsiz+1,KIND=dp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           u = 1_ilp
           vt = 1_ilp + smlsiz*n
           difl = vt + smlszp*n
           difr = difl + nlvl*n
           z = difr + nlvl*n*2_ilp
           c = z + nlvl*n
           s = c + n
           poles = s + n
           givnum = poles + 2_ilp*nlvl*n
           bx = givnum + 2_ilp*nlvl*n
           nwork = bx + n*nrhs
           sizei = 1_ilp + n
           k = sizei + n
           givptr = k + n
           perm = givptr + n
           givcol = perm + nlvl*n
           iwk = givcol + nlvl*n*2_ilp
           st = 1_ilp
           sqre = 0_ilp
           icmpq1 = 1_ilp
           icmpq2 = 0_ilp
           nsub = 0_ilp
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           loop_60: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 nsub = nsub + 1_ilp
                 iwork( nsub ) = st
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n), which is not solved
                    ! explicitly.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                    nsub = nsub + 1_ilp
                    iwork( nsub ) = n
                    iwork( sizei+nsub-1 ) = 1_ilp
                    call stdlib_dcopy( nrhs, b( n, 1_ilp ), ldb, work( bx+nm1 ), n )
                 end if
                 st1 = st - 1_ilp
                 if( nsize==1_ilp ) then
                    ! this is a 1-by-1 subproblem and is not solved
                    ! explicitly.
                    call stdlib_dcopy( nrhs, b( st, 1_ilp ), ldb, work( bx+st1 ), n )
                 else if( nsize<=smlsiz ) then
                    ! this is a small subproblem and is solved by stdlib_dlasdq.
                    call stdlib_dlaset( 'A', nsize, nsize, zero, one,work( vt+st1 ), n )
                    call stdlib_dlasdq( 'U', 0_ilp, nsize, nsize, 0_ilp, nrhs, d( st ),e( st ), work( vt+&
                              st1 ), n, work( nwork ),n, b( st, 1_ilp ), ldb, work( nwork ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    call stdlib_dlacpy( 'A', nsize, nrhs, b( st, 1_ilp ), ldb,work( bx+st1 ), n )
                              
                 else
                    ! a large problem. solve it using divide and conquer.
                    call stdlib_dlasda( icmpq1, smlsiz, nsize, sqre, d( st ),e( st ), work( u+st1 &
                    ), n, work( vt+st1 ),iwork( k+st1 ), work( difl+st1 ),work( difr+st1 ), work( &
                    z+st1 ),work( poles+st1 ), iwork( givptr+st1 ),iwork( givcol+st1 ), n, iwork( &
                    perm+st1 ),work( givnum+st1 ), work( c+st1 ),work( s+st1 ), work( nwork ), &
                              iwork( iwk ),info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    bxst = bx + st1
                    call stdlib_dlalsa( icmpq2, smlsiz, nsize, nrhs, b( st, 1_ilp ),ldb, work( bxst ),&
                     n, work( u+st1 ), n,work( vt+st1 ), iwork( k+st1 ),work( difl+st1 ), work( &
                     difr+st1 ),work( z+st1 ), work( poles+st1 ),iwork( givptr+st1 ), iwork( &
                     givcol+st1 ), n,iwork( perm+st1 ), work( givnum+st1 ),work( c+st1 ), work( s+&
                               st1 ), work( nwork ),iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                 end if
                 st = i + 1_ilp
              end if
           end do loop_60
           ! apply the singular values and treat the tiny ones as zero.
           tol = rcnd*abs( d( stdlib_idamax( n, d, 1_ilp ) ) )
           do i = 1, n
              ! some of the elements in d can be negative because 1-by-1
              ! subproblems were not solved explicitly.
              if( abs( d( i ) )<=tol ) then
                 call stdlib_dlaset( 'A', 1_ilp, nrhs, zero, zero, work( bx+i-1 ), n )
              else
                 rank = rank + 1_ilp
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs,work( bx+i-1 ), n, info )
                           
              end if
              d( i ) = abs( d( i ) )
           end do
           ! now apply back the right singular vectors.
           icmpq2 = 1_ilp
           do i = 1, nsub
              st = iwork( i )
              st1 = st - 1_ilp
              nsize = iwork( sizei+i-1 )
              bxst = bx + st1
              if( nsize==1_ilp ) then
                 call stdlib_dcopy( nrhs, work( bxst ), n, b( st, 1_ilp ), ldb )
              else if( nsize<=smlsiz ) then
                 call stdlib_dgemm( 'T', 'N', nsize, nrhs, nsize, one,work( vt+st1 ), n, work( &
                           bxst ), n, zero,b( st, 1_ilp ), ldb )
              else
                 call stdlib_dlalsa( icmpq2, smlsiz, nsize, nrhs, work( bxst ), n,b( st, 1_ilp ), ldb,&
                  work( u+st1 ), n,work( vt+st1 ), iwork( k+st1 ),work( difl+st1 ), work( difr+&
                  st1 ),work( z+st1 ), work( poles+st1 ),iwork( givptr+st1 ), iwork( givcol+st1 ),&
                   n,iwork( perm+st1 ), work( givnum+st1 ),work( c+st1 ), work( s+st1 ), work( &
                             nwork ),iwork( iwk ), info )
                 if( info/=0_ilp ) then
                    return
                 end if
              end if
           end do
           ! unscale and sort the singular values.
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           call stdlib_dlasrt( 'D', n, d, info )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
           return
     end subroutine stdlib_dlalsd


     pure module subroutine stdlib_clalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, rwork, &
     !! CLALSD uses the singular value decomposition of A to solve the least
     !! squares problem of finding X to minimize the Euclidean norm of each
     !! column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
     !! are N-by-NRHS. The solution X overwrites B.
     !! The singular values of A smaller than RCOND times the largest
     !! singular value are treated as zero in solving the least squares
     !! problem; in this case a minimum norm solution is returned.
     !! The actual singular values are returned in D in ascending order.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: bx, bxst, c, difl, difr, givcol, givnum, givptr, i, icmpq1, icmpq2, &
           irwb, irwib, irwrb, irwu, irwvt, irwwrk, iwk, j, jcol, jimag, jreal, jrow, k, nlvl, &
           nm1, nrwork, nsize, nsub, perm, poles, s, sizei, smlszp, sqre, st, st1, u, vt, &
                     z
           real(sp) :: cs, eps, orgnrm, r, rcnd, sn, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ( ldb<1_ilp ) .or. ( ldb<n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLALSD', -info )
              return
           end if
           eps = stdlib_slamch( 'EPSILON' )
           ! set up the tolerance.
           if( ( rcond<=zero ) .or. ( rcond>=one ) ) then
              rcnd = eps
           else
              rcnd = rcond
           end if
           rank = 0_ilp
           ! quick return if possible.
           if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              if( d( 1_ilp )==zero ) then
                 call stdlib_claset( 'A', 1_ilp, nrhs, czero, czero, b, ldb )
              else
                 rank = 1_ilp
                 call stdlib_clascl( 'G', 0_ilp, 0_ilp, d( 1_ilp ), one, 1_ilp, nrhs, b, ldb, info )
                 d( 1_ilp ) = abs( d( 1_ilp ) )
              end if
              return
           end if
           ! rotate the matrix if it is lower bidiagonal.
           if( uplo=='L' ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( nrhs==1_ilp ) then
                    call stdlib_csrot( 1_ilp, b( i, 1_ilp ), 1_ilp, b( i+1, 1_ilp ), 1_ilp, cs, sn )
                 else
                    rwork( i*2_ilp-1 ) = cs
                    rwork( i*2_ilp ) = sn
                 end if
              end do
              if( nrhs>1_ilp ) then
                 do i = 1, nrhs
                    do j = 1, n - 1
                       cs = rwork( j*2_ilp-1 )
                       sn = rwork( j*2_ilp )
                       call stdlib_csrot( 1_ilp, b( j, i ), 1_ilp, b( j+1, i ), 1_ilp, cs, sn )
                    end do
                 end do
              end if
           end if
           ! scale.
           nm1 = n - 1_ilp
           orgnrm = stdlib_slanst( 'M', n, d, e )
           if( orgnrm==zero ) then
              call stdlib_claset( 'A', n, nrhs, czero, czero, b, ldb )
              return
           end if
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, info )
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              irwu = 1_ilp
              irwvt = irwu + n*n
              irwwrk = irwvt + n*n
              irwrb = irwwrk
              irwib = irwrb + n*nrhs
              irwb = irwib + n*nrhs
              call stdlib_slaset( 'A', n, n, zero, one, rwork( irwu ), n )
              call stdlib_slaset( 'A', n, n, zero, one, rwork( irwvt ), n )
              call stdlib_slasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e, rwork( irwvt ), n,rwork( irwu ), n, &
                        rwork( irwwrk ), 1_ilp,rwork( irwwrk ), info )
              if( info/=0_ilp ) then
                 return
              end if
              ! in the real version, b is passed to stdlib_slasdq and multiplied
              ! internally by q**h. here b is complex and that product is
              ! computed below in two steps (real and imaginary parts).
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, rwork( irwu ), n,rwork( irwb ), n, &
                        zero, rwork( irwrb ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, rwork( irwu ), n,rwork( irwb ), n, &
                        zero, rwork( irwib ), n )
              jreal = irwrb - 1_ilp
              jimag = irwib - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    b( jrow, jcol ) = cmplx( rwork( jreal ), rwork( jimag ),KIND=sp)
                 end do
              end do
              tol = rcnd*abs( d( stdlib_isamax( n, d, 1_ilp ) ) )
              do i = 1, n
                 if( d( i )<=tol ) then
                    call stdlib_claset( 'A', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 else
                    call stdlib_clascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs, b( i, 1_ilp ),ldb, info )
                              
                    rank = rank + 1_ilp
                 end if
              end do
              ! since b is complex, the following call to stdlib_sgemm is performed
              ! in two steps (real and imaginary parts). that is for v * b
              ! (in the real version of the code v**h is stored in work).
              ! call stdlib_sgemm( 't', 'n', n, nrhs, n, one, work, n, b, ldb, zero,
          ! $               work( nwork ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, rwork( irwvt ), n,rwork( irwb ), n, &
                        zero, rwork( irwrb ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, rwork( irwvt ), n,rwork( irwb ), n, &
                        zero, rwork( irwib ), n )
              jreal = irwrb - 1_ilp
              jimag = irwib - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    b( jrow, jcol ) = cmplx( rwork( jreal ), rwork( jimag ),KIND=sp)
                 end do
              end do
              ! unscale.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
              call stdlib_slasrt( 'D', n, d, info )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
              return
           end if
           ! book-keeping and setting up some constants.
           nlvl = int( log( real( n,KIND=sp) / real( smlsiz+1,KIND=sp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           u = 1_ilp
           vt = 1_ilp + smlsiz*n
           difl = vt + smlszp*n
           difr = difl + nlvl*n
           z = difr + nlvl*n*2_ilp
           c = z + nlvl*n
           s = c + n
           poles = s + n
           givnum = poles + 2_ilp*nlvl*n
           nrwork = givnum + 2_ilp*nlvl*n
           bx = 1_ilp
           irwrb = nrwork
           irwib = irwrb + smlsiz*nrhs
           irwb = irwib + smlsiz*nrhs
           sizei = 1_ilp + n
           k = sizei + n
           givptr = k + n
           perm = givptr + n
           givcol = perm + nlvl*n
           iwk = givcol + nlvl*n*2_ilp
           st = 1_ilp
           sqre = 0_ilp
           icmpq1 = 1_ilp
           icmpq2 = 0_ilp
           nsub = 0_ilp
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           loop_240: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 nsub = nsub + 1_ilp
                 iwork( nsub ) = st
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n), which is not solved
                    ! explicitly.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                    nsub = nsub + 1_ilp
                    iwork( nsub ) = n
                    iwork( sizei+nsub-1 ) = 1_ilp
                    call stdlib_ccopy( nrhs, b( n, 1_ilp ), ldb, work( bx+nm1 ), n )
                 end if
                 st1 = st - 1_ilp
                 if( nsize==1_ilp ) then
                    ! this is a 1-by-1 subproblem and is not solved
                    ! explicitly.
                    call stdlib_ccopy( nrhs, b( st, 1_ilp ), ldb, work( bx+st1 ), n )
                 else if( nsize<=smlsiz ) then
                    ! this is a small subproblem and is solved by stdlib_slasdq.
                    call stdlib_slaset( 'A', nsize, nsize, zero, one,rwork( vt+st1 ), n )
                    call stdlib_slaset( 'A', nsize, nsize, zero, one,rwork( u+st1 ), n )
                    call stdlib_slasdq( 'U', 0_ilp, nsize, nsize, nsize, 0_ilp, d( st ),e( st ), rwork( &
                    vt+st1 ), n, rwork( u+st1 ),n, rwork( nrwork ), 1_ilp, rwork( nrwork ),info )
                              
                    if( info/=0_ilp ) then
                       return
                    end if
                    ! in the real version, b is passed to stdlib_slasdq and multiplied
                    ! internally by q**h. here b is complex and that product is
                    ! computed below in two steps (real and imaginary parts).
                    j = irwb - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          j = j + 1_ilp
                          rwork( j ) = real( b( jrow, jcol ),KIND=sp)
                       end do
                    end do
                    call stdlib_sgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( u+st1 ), n, rwork(&
                               irwb ), nsize,zero, rwork( irwrb ), nsize )
                    j = irwb - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          j = j + 1_ilp
                          rwork( j ) = aimag( b( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_sgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( u+st1 ), n, rwork(&
                               irwb ), nsize,zero, rwork( irwib ), nsize )
                    jreal = irwrb - 1_ilp
                    jimag = irwib - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          jreal = jreal + 1_ilp
                          jimag = jimag + 1_ilp
                          b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                       end do
                    end do
                    call stdlib_clacpy( 'A', nsize, nrhs, b( st, 1_ilp ), ldb,work( bx+st1 ), n )
                              
                 else
                    ! a large problem. solve it using divide and conquer.
                    call stdlib_slasda( icmpq1, smlsiz, nsize, sqre, d( st ),e( st ), rwork( u+&
                    st1 ), n, rwork( vt+st1 ),iwork( k+st1 ), rwork( difl+st1 ),rwork( difr+st1 ),&
                     rwork( z+st1 ),rwork( poles+st1 ), iwork( givptr+st1 ),iwork( givcol+st1 ), &
                     n, iwork( perm+st1 ),rwork( givnum+st1 ), rwork( c+st1 ),rwork( s+st1 ), &
                               rwork( nrwork ),iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    bxst = bx + st1
                    call stdlib_clalsa( icmpq2, smlsiz, nsize, nrhs, b( st, 1_ilp ),ldb, work( bxst ),&
                     n, rwork( u+st1 ), n,rwork( vt+st1 ), iwork( k+st1 ),rwork( difl+st1 ), &
                     rwork( difr+st1 ),rwork( z+st1 ), rwork( poles+st1 ),iwork( givptr+st1 ), &
                     iwork( givcol+st1 ), n,iwork( perm+st1 ), rwork( givnum+st1 ),rwork( c+st1 ),&
                                rwork( s+st1 ),rwork( nrwork ), iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                 end if
                 st = i + 1_ilp
              end if
           end do loop_240
           ! apply the singular values and treat the tiny ones as zero.
           tol = rcnd*abs( d( stdlib_isamax( n, d, 1_ilp ) ) )
           do i = 1, n
              ! some of the elements in d can be negative because 1-by-1
              ! subproblems were not solved explicitly.
              if( abs( d( i ) )<=tol ) then
                 call stdlib_claset( 'A', 1_ilp, nrhs, czero, czero, work( bx+i-1 ), n )
              else
                 rank = rank + 1_ilp
                 call stdlib_clascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs,work( bx+i-1 ), n, info )
                           
              end if
              d( i ) = abs( d( i ) )
           end do
           ! now apply back the right singular vectors.
           icmpq2 = 1_ilp
           loop_320: do i = 1, nsub
              st = iwork( i )
              st1 = st - 1_ilp
              nsize = iwork( sizei+i-1 )
              bxst = bx + st1
              if( nsize==1_ilp ) then
                 call stdlib_ccopy( nrhs, work( bxst ), n, b( st, 1_ilp ), ldb )
              else if( nsize<=smlsiz ) then
                 ! since b and bx are complex, the following call to stdlib_sgemm
                 ! is performed in two steps (real and imaginary parts).
                 ! call stdlib_sgemm( 't', 'n', nsize, nrhs, nsize, one,
          ! $                  rwork( vt+st1 ), n, rwork( bxst ), n, zero,
          ! $                  b( st, 1 ), ldb )
                 j = bxst - n - 1_ilp
                 jreal = irwb - 1_ilp
                 do jcol = 1, nrhs
                    j = j + n
                    do jrow = 1, nsize
                       jreal = jreal + 1_ilp
                       rwork( jreal ) = real( work( j+jrow ),KIND=sp)
                    end do
                 end do
                 call stdlib_sgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( vt+st1 ), n, rwork( &
                           irwb ), nsize, zero,rwork( irwrb ), nsize )
                 j = bxst - n - 1_ilp
                 jimag = irwb - 1_ilp
                 do jcol = 1, nrhs
                    j = j + n
                    do jrow = 1, nsize
                       jimag = jimag + 1_ilp
                       rwork( jimag ) = aimag( work( j+jrow ) )
                    end do
                 end do
                 call stdlib_sgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( vt+st1 ), n, rwork( &
                           irwb ), nsize, zero,rwork( irwib ), nsize )
                 jreal = irwrb - 1_ilp
                 jimag = irwib - 1_ilp
                 do jcol = 1, nrhs
                    do jrow = st, st + nsize - 1
                       jreal = jreal + 1_ilp
                       jimag = jimag + 1_ilp
                       b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=sp)
                    end do
                 end do
              else
                 call stdlib_clalsa( icmpq2, smlsiz, nsize, nrhs, work( bxst ), n,b( st, 1_ilp ), ldb,&
                  rwork( u+st1 ), n,rwork( vt+st1 ), iwork( k+st1 ),rwork( difl+st1 ), rwork( &
                  difr+st1 ),rwork( z+st1 ), rwork( poles+st1 ),iwork( givptr+st1 ), iwork( &
                  givcol+st1 ), n,iwork( perm+st1 ), rwork( givnum+st1 ),rwork( c+st1 ), rwork( s+&
                            st1 ),rwork( nrwork ), iwork( iwk ), info )
                 if( info/=0_ilp ) then
                    return
                 end if
              end if
           end do loop_320
           ! unscale and sort the singular values.
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           call stdlib_slasrt( 'D', n, d, info )
           call stdlib_clascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
           return
     end subroutine stdlib_clalsd

     pure module subroutine stdlib_zlalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, rwork, &
     !! ZLALSD uses the singular value decomposition of A to solve the least
     !! squares problem of finding X to minimize the Euclidean norm of each
     !! column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
     !! are N-by-NRHS. The solution X overwrites B.
     !! The singular values of A smaller than RCOND times the largest
     !! singular value are treated as zero in solving the least squares
     !! problem; in this case a minimum norm solution is returned.
     !! The actual singular values are returned in D in ascending order.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: bx, bxst, c, difl, difr, givcol, givnum, givptr, i, icmpq1, icmpq2, &
           irwb, irwib, irwrb, irwu, irwvt, irwwrk, iwk, j, jcol, jimag, jreal, jrow, k, nlvl, &
           nm1, nrwork, nsize, nsub, perm, poles, s, sizei, smlszp, sqre, st, st1, u, vt, &
                     z
           real(dp) :: cs, eps, orgnrm, rcnd, r, sn, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<1_ilp ) then
              info = -4_ilp
           else if( ( ldb<1_ilp ) .or. ( ldb<n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLALSD', -info )
              return
           end if
           eps = stdlib_dlamch( 'EPSILON' )
           ! set up the tolerance.
           if( ( rcond<=zero ) .or. ( rcond>=one ) ) then
              rcnd = eps
           else
              rcnd = rcond
           end if
           rank = 0_ilp
           ! quick return if possible.
           if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              if( d( 1_ilp )==zero ) then
                 call stdlib_zlaset( 'A', 1_ilp, nrhs, czero, czero, b, ldb )
              else
                 rank = 1_ilp
                 call stdlib_zlascl( 'G', 0_ilp, 0_ilp, d( 1_ilp ), one, 1_ilp, nrhs, b, ldb, info )
                 d( 1_ilp ) = abs( d( 1_ilp ) )
              end if
              return
           end if
           ! rotate the matrix if it is lower bidiagonal.
           if( uplo=='L' ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( nrhs==1_ilp ) then
                    call stdlib_zdrot( 1_ilp, b( i, 1_ilp ), 1_ilp, b( i+1, 1_ilp ), 1_ilp, cs, sn )
                 else
                    rwork( i*2_ilp-1 ) = cs
                    rwork( i*2_ilp ) = sn
                 end if
              end do
              if( nrhs>1_ilp ) then
                 do i = 1, nrhs
                    do j = 1, n - 1
                       cs = rwork( j*2_ilp-1 )
                       sn = rwork( j*2_ilp )
                       call stdlib_zdrot( 1_ilp, b( j, i ), 1_ilp, b( j+1, i ), 1_ilp, cs, sn )
                    end do
                 end do
              end if
           end if
           ! scale.
           nm1 = n - 1_ilp
           orgnrm = stdlib_dlanst( 'M', n, d, e )
           if( orgnrm==zero ) then
              call stdlib_zlaset( 'A', n, nrhs, czero, czero, b, ldb )
              return
           end if
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, info )
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              irwu = 1_ilp
              irwvt = irwu + n*n
              irwwrk = irwvt + n*n
              irwrb = irwwrk
              irwib = irwrb + n*nrhs
              irwb = irwib + n*nrhs
              call stdlib_dlaset( 'A', n, n, zero, one, rwork( irwu ), n )
              call stdlib_dlaset( 'A', n, n, zero, one, rwork( irwvt ), n )
              call stdlib_dlasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e, rwork( irwvt ), n,rwork( irwu ), n, &
                        rwork( irwwrk ), 1_ilp,rwork( irwwrk ), info )
              if( info/=0_ilp ) then
                 return
              end if
              ! in the real version, b is passed to stdlib_dlasdq and multiplied
              ! internally by q**h. here b is complex and that product is
              ! computed below in two steps (real and imaginary parts).
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, rwork( irwu ), n,rwork( irwb ), n, &
                        zero, rwork( irwrb ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, rwork( irwu ), n,rwork( irwb ), n, &
                        zero, rwork( irwib ), n )
              jreal = irwrb - 1_ilp
              jimag = irwib - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
              tol = rcnd*abs( d( stdlib_idamax( n, d, 1_ilp ) ) )
              do i = 1, n
                 if( d( i )<=tol ) then
                    call stdlib_zlaset( 'A', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 else
                    call stdlib_zlascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs, b( i, 1_ilp ),ldb, info )
                              
                    rank = rank + 1_ilp
                 end if
              end do
              ! since b is complex, the following call to stdlib_dgemm is performed
              ! in two steps (real and imaginary parts). that is for v * b
              ! (in the real version of the code v**h is stored in work).
              ! call stdlib_dgemm( 't', 'n', n, nrhs, n, one, work, n, b, ldb, zero,
          ! $               work( nwork ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, rwork( irwvt ), n,rwork( irwb ), n, &
                        zero, rwork( irwrb ), n )
              j = irwb - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    j = j + 1_ilp
                    rwork( j ) = aimag( b( jrow, jcol ) )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, rwork( irwvt ), n,rwork( irwb ), n, &
                        zero, rwork( irwib ), n )
              jreal = irwrb - 1_ilp
              jimag = irwib - 1_ilp
              do jcol = 1, nrhs
                 do jrow = 1, n
                    jreal = jreal + 1_ilp
                    jimag = jimag + 1_ilp
                    b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                 end do
              end do
              ! unscale.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
              call stdlib_dlasrt( 'D', n, d, info )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
              return
           end if
           ! book-keeping and setting up some constants.
           nlvl = int( log( real( n,KIND=dp) / real( smlsiz+1,KIND=dp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           u = 1_ilp
           vt = 1_ilp + smlsiz*n
           difl = vt + smlszp*n
           difr = difl + nlvl*n
           z = difr + nlvl*n*2_ilp
           c = z + nlvl*n
           s = c + n
           poles = s + n
           givnum = poles + 2_ilp*nlvl*n
           nrwork = givnum + 2_ilp*nlvl*n
           bx = 1_ilp
           irwrb = nrwork
           irwib = irwrb + smlsiz*nrhs
           irwb = irwib + smlsiz*nrhs
           sizei = 1_ilp + n
           k = sizei + n
           givptr = k + n
           perm = givptr + n
           givcol = perm + nlvl*n
           iwk = givcol + nlvl*n*2_ilp
           st = 1_ilp
           sqre = 0_ilp
           icmpq1 = 1_ilp
           icmpq2 = 0_ilp
           nsub = 0_ilp
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           loop_240: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 nsub = nsub + 1_ilp
                 iwork( nsub ) = st
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n), which is not solved
                    ! explicitly.
                    nsize = i - st + 1_ilp
                    iwork( sizei+nsub-1 ) = nsize
                    nsub = nsub + 1_ilp
                    iwork( nsub ) = n
                    iwork( sizei+nsub-1 ) = 1_ilp
                    call stdlib_zcopy( nrhs, b( n, 1_ilp ), ldb, work( bx+nm1 ), n )
                 end if
                 st1 = st - 1_ilp
                 if( nsize==1_ilp ) then
                    ! this is a 1-by-1 subproblem and is not solved
                    ! explicitly.
                    call stdlib_zcopy( nrhs, b( st, 1_ilp ), ldb, work( bx+st1 ), n )
                 else if( nsize<=smlsiz ) then
                    ! this is a small subproblem and is solved by stdlib_dlasdq.
                    call stdlib_dlaset( 'A', nsize, nsize, zero, one,rwork( vt+st1 ), n )
                    call stdlib_dlaset( 'A', nsize, nsize, zero, one,rwork( u+st1 ), n )
                    call stdlib_dlasdq( 'U', 0_ilp, nsize, nsize, nsize, 0_ilp, d( st ),e( st ), rwork( &
                    vt+st1 ), n, rwork( u+st1 ),n, rwork( nrwork ), 1_ilp, rwork( nrwork ),info )
                              
                    if( info/=0_ilp ) then
                       return
                    end if
                    ! in the real version, b is passed to stdlib_dlasdq and multiplied
                    ! internally by q**h. here b is complex and that product is
                    ! computed below in two steps (real and imaginary parts).
                    j = irwb - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          j = j + 1_ilp
                          rwork( j ) = real( b( jrow, jcol ),KIND=dp)
                       end do
                    end do
                    call stdlib_dgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( u+st1 ), n, rwork(&
                               irwb ), nsize,zero, rwork( irwrb ), nsize )
                    j = irwb - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          j = j + 1_ilp
                          rwork( j ) = aimag( b( jrow, jcol ) )
                       end do
                    end do
                    call stdlib_dgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( u+st1 ), n, rwork(&
                               irwb ), nsize,zero, rwork( irwib ), nsize )
                    jreal = irwrb - 1_ilp
                    jimag = irwib - 1_ilp
                    do jcol = 1, nrhs
                       do jrow = st, st + nsize - 1
                          jreal = jreal + 1_ilp
                          jimag = jimag + 1_ilp
                          b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                       end do
                    end do
                    call stdlib_zlacpy( 'A', nsize, nrhs, b( st, 1_ilp ), ldb,work( bx+st1 ), n )
                              
                 else
                    ! a large problem. solve it using divide and conquer.
                    call stdlib_dlasda( icmpq1, smlsiz, nsize, sqre, d( st ),e( st ), rwork( u+&
                    st1 ), n, rwork( vt+st1 ),iwork( k+st1 ), rwork( difl+st1 ),rwork( difr+st1 ),&
                     rwork( z+st1 ),rwork( poles+st1 ), iwork( givptr+st1 ),iwork( givcol+st1 ), &
                     n, iwork( perm+st1 ),rwork( givnum+st1 ), rwork( c+st1 ),rwork( s+st1 ), &
                               rwork( nrwork ),iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                    bxst = bx + st1
                    call stdlib_zlalsa( icmpq2, smlsiz, nsize, nrhs, b( st, 1_ilp ),ldb, work( bxst ),&
                     n, rwork( u+st1 ), n,rwork( vt+st1 ), iwork( k+st1 ),rwork( difl+st1 ), &
                     rwork( difr+st1 ),rwork( z+st1 ), rwork( poles+st1 ),iwork( givptr+st1 ), &
                     iwork( givcol+st1 ), n,iwork( perm+st1 ), rwork( givnum+st1 ),rwork( c+st1 ),&
                                rwork( s+st1 ),rwork( nrwork ), iwork( iwk ), info )
                    if( info/=0_ilp ) then
                       return
                    end if
                 end if
                 st = i + 1_ilp
              end if
           end do loop_240
           ! apply the singular values and treat the tiny ones as zero.
           tol = rcnd*abs( d( stdlib_idamax( n, d, 1_ilp ) ) )
           do i = 1, n
              ! some of the elements in d can be negative because 1-by-1
              ! subproblems were not solved explicitly.
              if( abs( d( i ) )<=tol ) then
                 call stdlib_zlaset( 'A', 1_ilp, nrhs, czero, czero, work( bx+i-1 ), n )
              else
                 rank = rank + 1_ilp
                 call stdlib_zlascl( 'G', 0_ilp, 0_ilp, d( i ), one, 1_ilp, nrhs,work( bx+i-1 ), n, info )
                           
              end if
              d( i ) = abs( d( i ) )
           end do
           ! now apply back the right singular vectors.
           icmpq2 = 1_ilp
           loop_320: do i = 1, nsub
              st = iwork( i )
              st1 = st - 1_ilp
              nsize = iwork( sizei+i-1 )
              bxst = bx + st1
              if( nsize==1_ilp ) then
                 call stdlib_zcopy( nrhs, work( bxst ), n, b( st, 1_ilp ), ldb )
              else if( nsize<=smlsiz ) then
                 ! since b and bx are complex, the following call to stdlib_dgemm
                 ! is performed in two steps (real and imaginary parts).
                 ! call stdlib_dgemm( 't', 'n', nsize, nrhs, nsize, one,
          ! $                  rwork( vt+st1 ), n, rwork( bxst ), n, zero,
          ! $                  b( st, 1 ), ldb )
                 j = bxst - n - 1_ilp
                 jreal = irwb - 1_ilp
                 do jcol = 1, nrhs
                    j = j + n
                    do jrow = 1, nsize
                       jreal = jreal + 1_ilp
                       rwork( jreal ) = real( work( j+jrow ),KIND=dp)
                    end do
                 end do
                 call stdlib_dgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( vt+st1 ), n, rwork( &
                           irwb ), nsize, zero,rwork( irwrb ), nsize )
                 j = bxst - n - 1_ilp
                 jimag = irwb - 1_ilp
                 do jcol = 1, nrhs
                    j = j + n
                    do jrow = 1, nsize
                       jimag = jimag + 1_ilp
                       rwork( jimag ) = aimag( work( j+jrow ) )
                    end do
                 end do
                 call stdlib_dgemm( 'T', 'N', nsize, nrhs, nsize, one,rwork( vt+st1 ), n, rwork( &
                           irwb ), nsize, zero,rwork( irwib ), nsize )
                 jreal = irwrb - 1_ilp
                 jimag = irwib - 1_ilp
                 do jcol = 1, nrhs
                    do jrow = st, st + nsize - 1
                       jreal = jreal + 1_ilp
                       jimag = jimag + 1_ilp
                       b( jrow, jcol ) = cmplx( rwork( jreal ),rwork( jimag ),KIND=dp)
                    end do
                 end do
              else
                 call stdlib_zlalsa( icmpq2, smlsiz, nsize, nrhs, work( bxst ), n,b( st, 1_ilp ), ldb,&
                  rwork( u+st1 ), n,rwork( vt+st1 ), iwork( k+st1 ),rwork( difl+st1 ), rwork( &
                  difr+st1 ),rwork( z+st1 ), rwork( poles+st1 ),iwork( givptr+st1 ), iwork( &
                  givcol+st1 ), n,iwork( perm+st1 ), rwork( givnum+st1 ),rwork( c+st1 ), rwork( s+&
                            st1 ),rwork( nrwork ), iwork( iwk ), info )
                 if( info/=0_ilp ) then
                    return
                 end if
              end if
           end do loop_320
           ! unscale and sort the singular values.
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           call stdlib_dlasrt( 'D', n, d, info )
           call stdlib_zlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, nrhs, b, ldb, info )
           return
     end subroutine stdlib_zlalsd



end submodule stdlib_lapack_lsq_aux
