submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_cosine_sine
  implicit none


  contains

     pure module subroutine stdlib_sbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     !! SBBCSD computes the CS decomposition of an orthogonal matrix in
     !! bidiagonal-block form,
     !! [ B11 | B12 0  0 ]
     !! [  0  |  0 -I  0 ]
     !! X = [----------------]
     !! [ B21 | B22 0  0 ]
     !! [  0  |  0  0  I ]
     !! [  C | -S  0  0 ]
     !! [ U1 |    ] [  0 |  0 -I  0 ] [ V1 |    ]**T
     !! = [---------] [---------------] [---------]   .
     !! [    | U2 ] [  S |  C  0  0 ] [    | V2 ]
     !! [  0 |  0  0  I ]
     !! X is M-by-M, its top-left block is P-by-Q, and Q must be no larger
     !! than P, M-P, or M-Q. (If Q is not the smallest index, then X must be
     !! transposed and/or permuted. This can be done in constant time using
     !! the TRANS and SIGNS options. See SORCSD for details.)
     !! The bidiagonal matrices B11, B12, B21, and B22 are represented
     !! implicitly by angles THETA(1:Q) and PHI(1:Q-1).
     !! The orthogonal matrices U1, U2, V1T, and V2T are input/output.
     !! The input matrices are pre- or post-multiplied by the appropriate
     !! singular vector matrices.
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, work, &
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, one, ten, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lwork, m, p, q
           ! Array Arguments 
           real(sp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), work(*)
           real(sp), intent(inout) :: phi(*), theta(*)
           real(sp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
        ! ===================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 6_ilp
           real(sp), parameter :: hundred = 100.0_sp
           real(sp), parameter :: meighth = -0.125_sp
           real(sp), parameter :: piover2 = 1.57079632679489661923132169163975144210_sp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery, restart11, restart12, restart21, restart22, wantu1, &
                     wantu2, wantv1t, wantv2t
           integer(ilp) :: i, imin, imax, iter, iu1cs, iu1sn, iu2cs, iu2sn, iv1tcs, iv1tsn, &
                     iv2tcs, iv2tsn, j, lworkmin, lworkopt, maxit, mini
           real(sp) :: b11bulge, b12bulge, b21bulge, b22bulge, dummy, eps, mu, nu, r, sigma11, &
                     sigma21, temp, thetamax, thetamin, thresh, tol, tolmul, unfl, x1, x2, y1, y2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( m < 0_ilp ) then
              info = -6_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -7_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -8_ilp
           else if( q > p .or. q > m-p .or. q > m-q ) then
              info = -8_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -12_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -14_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -16_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -18_ilp
           end if
           ! quick return if q = 0
           if( info == 0_ilp .and. q == 0_ilp ) then
              lworkmin = 1_ilp
              work(1_ilp) = lworkmin
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iu1cs = 1_ilp
              iu1sn = iu1cs + q
              iu2cs = iu1sn + q
              iu2sn = iu2cs + q
              iv1tcs = iu2sn + q
              iv1tsn = iv1tcs + q
              iv2tcs = iv1tsn + q
              iv2tsn = iv2tcs + q
              lworkopt = iv2tsn + q - 1_ilp
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -28_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SBBCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'EPSILON' )
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           tolmul = max( ten, min( hundred, eps**meighth ) )
           tol = tolmul*eps
           thresh = max( tol, maxitr*q*q*unfl )
           ! test for negligible sines or cosines
           do i = 1, q
              if( theta(i) < thresh ) then
                 theta(i) = zero
              else if( theta(i) > piover2-thresh ) then
                 theta(i) = piover2
              end if
           end do
           do i = 1, q-1
              if( phi(i) < thresh ) then
                 phi(i) = zero
              else if( phi(i) > piover2-thresh ) then
                 phi(i) = piover2
              end if
           end do
           ! initial deflation
           imax = q
           do while( imax > 1 )
              if( phi(imax-1) /= zero ) then
                 exit
              end if
              imax = imax - 1_ilp
           end do
           imin = imax - 1_ilp
           if  ( imin > 1_ilp ) then
              do while( phi(imin-1) /= zero )
                 imin = imin - 1_ilp
                 if  ( imin <= 1 ) exit
              end do
           end if
           ! initialize iteration counter
           maxit = maxitr*q*q
           iter = 0_ilp
           ! begin main iteration loop
           do while( imax > 1 )
              ! compute the matrix entries
              b11d(imin) = cos( theta(imin) )
              b21d(imin) = -sin( theta(imin) )
              do i = imin, imax - 1
                 b11e(i) = -sin( theta(i) ) * sin( phi(i) )
                 b11d(i+1) = cos( theta(i+1) ) * cos( phi(i) )
                 b12d(i) = sin( theta(i) ) * cos( phi(i) )
                 b12e(i) = cos( theta(i+1) ) * sin( phi(i) )
                 b21e(i) = -cos( theta(i) ) * sin( phi(i) )
                 b21d(i+1) = -sin( theta(i+1) ) * cos( phi(i) )
                 b22d(i) = cos( theta(i) ) * cos( phi(i) )
                 b22e(i) = -sin( theta(i+1) ) * sin( phi(i) )
              end do
              b12d(imax) = sin( theta(imax) )
              b22d(imax) = cos( theta(imax) )
              ! abort if not converging; otherwise, increment iter
              if( iter > maxit ) then
                 info = 0_ilp
                 do i = 1, q
                    if( phi(i) /= zero )info = info + 1_ilp
                 end do
                 return
              end if
              iter = iter + imax - imin
              ! compute shifts
              thetamax = theta(imin)
              thetamin = theta(imin)
              do i = imin+1, imax
                 if( theta(i) > thetamax )thetamax = theta(i)
                 if( theta(i) < thetamin )thetamin = theta(i)
              end do
              if( thetamax > piover2 - thresh ) then
                 ! zero on diagonals of b11 and b22; induce deflation with a
                 ! zero shift
                 mu = zero
                 nu = one
              else if( thetamin < thresh ) then
                 ! zero on diagonals of b12 and b22; induce deflation with a
                 ! zero shift
                 mu = one
                 nu = zero
              else
                 ! compute shifts for b11 and b21 and use the lesser
                 call stdlib_slas2( b11d(imax-1), b11e(imax-1), b11d(imax), sigma11,dummy )
                           
                 call stdlib_slas2( b21d(imax-1), b21e(imax-1), b21d(imax), sigma21,dummy )
                           
                 if( sigma11 <= sigma21 ) then
                    mu = sigma11
                    nu = sqrt( one - mu**2_ilp )
                    if( mu < thresh ) then
                       mu = zero
                       nu = one
                    end if
                 else
                    nu = sigma21
                    mu = sqrt( one - nu**2_ilp )
                    if( nu < thresh ) then
                       mu = one
                       nu = zero
                    end if
                 end if
              end if
              ! rotate to produce bulges in b11 and b21
              if( mu <= nu ) then
                 call stdlib_slartgs( b11d(imin), b11e(imin), mu,work(iv1tcs+imin-1), work(iv1tsn+&
                           imin-1) )
              else
                 call stdlib_slartgs( b21d(imin), b21e(imin), nu,work(iv1tcs+imin-1), work(iv1tsn+&
                           imin-1) )
              end if
              temp = work(iv1tcs+imin-1)*b11d(imin) +work(iv1tsn+imin-1)*b11e(imin)
              b11e(imin) = work(iv1tcs+imin-1)*b11e(imin) -work(iv1tsn+imin-1)*b11d(imin)
              b11d(imin) = temp
              b11bulge = work(iv1tsn+imin-1)*b11d(imin+1)
              b11d(imin+1) = work(iv1tcs+imin-1)*b11d(imin+1)
              temp = work(iv1tcs+imin-1)*b21d(imin) +work(iv1tsn+imin-1)*b21e(imin)
              b21e(imin) = work(iv1tcs+imin-1)*b21e(imin) -work(iv1tsn+imin-1)*b21d(imin)
              b21d(imin) = temp
              b21bulge = work(iv1tsn+imin-1)*b21d(imin+1)
              b21d(imin+1) = work(iv1tcs+imin-1)*b21d(imin+1)
              ! compute theta(imin)
              theta( imin ) = atan2( sqrt( b21d(imin)**2_ilp+b21bulge**2_ilp ),sqrt( b11d(imin)**2_ilp+&
                        b11bulge**2_ilp ) )
              ! chase the bulges in b11(imin+1,imin) and b21(imin+1,imin)
              if( b11d(imin)**2_ilp+b11bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_slartgp( b11bulge, b11d(imin), work(iu1sn+imin-1),work(iu1cs+imin-1),&
                            r )
              else if( mu <= nu ) then
                 call stdlib_slartgs( b11e( imin ), b11d( imin + 1_ilp ), mu,work(iu1cs+imin-1), work(&
                           iu1sn+imin-1) )
              else
                 call stdlib_slartgs( b12d( imin ), b12e( imin ), nu,work(iu1cs+imin-1), work(&
                           iu1sn+imin-1) )
              end if
              if( b21d(imin)**2_ilp+b21bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_slartgp( b21bulge, b21d(imin), work(iu2sn+imin-1),work(iu2cs+imin-1),&
                            r )
              else if( nu < mu ) then
                 call stdlib_slartgs( b21e( imin ), b21d( imin + 1_ilp ), nu,work(iu2cs+imin-1), work(&
                           iu2sn+imin-1) )
              else
                 call stdlib_slartgs( b22d(imin), b22e(imin), mu,work(iu2cs+imin-1), work(iu2sn+&
                           imin-1) )
              end if
              work(iu2cs+imin-1) = -work(iu2cs+imin-1)
              work(iu2sn+imin-1) = -work(iu2sn+imin-1)
              temp = work(iu1cs+imin-1)*b11e(imin) +work(iu1sn+imin-1)*b11d(imin+1)
              b11d(imin+1) = work(iu1cs+imin-1)*b11d(imin+1) -work(iu1sn+imin-1)*b11e(imin)
                        
              b11e(imin) = temp
              if( imax > imin+1 ) then
                 b11bulge = work(iu1sn+imin-1)*b11e(imin+1)
                 b11e(imin+1) = work(iu1cs+imin-1)*b11e(imin+1)
              end if
              temp = work(iu1cs+imin-1)*b12d(imin) +work(iu1sn+imin-1)*b12e(imin)
              b12e(imin) = work(iu1cs+imin-1)*b12e(imin) -work(iu1sn+imin-1)*b12d(imin)
              b12d(imin) = temp
              b12bulge = work(iu1sn+imin-1)*b12d(imin+1)
              b12d(imin+1) = work(iu1cs+imin-1)*b12d(imin+1)
              temp = work(iu2cs+imin-1)*b21e(imin) +work(iu2sn+imin-1)*b21d(imin+1)
              b21d(imin+1) = work(iu2cs+imin-1)*b21d(imin+1) -work(iu2sn+imin-1)*b21e(imin)
                        
              b21e(imin) = temp
              if( imax > imin+1 ) then
                 b21bulge = work(iu2sn+imin-1)*b21e(imin+1)
                 b21e(imin+1) = work(iu2cs+imin-1)*b21e(imin+1)
              end if
              temp = work(iu2cs+imin-1)*b22d(imin) +work(iu2sn+imin-1)*b22e(imin)
              b22e(imin) = work(iu2cs+imin-1)*b22e(imin) -work(iu2sn+imin-1)*b22d(imin)
              b22d(imin) = temp
              b22bulge = work(iu2sn+imin-1)*b22d(imin+1)
              b22d(imin+1) = work(iu2cs+imin-1)*b22d(imin+1)
              ! inner loop: chase bulges from b11(imin,imin+2),
              ! b12(imin,imin+1), b21(imin,imin+2), and b22(imin,imin+1) to
              ! bottom-right
              do i = imin+1, imax-1
                 ! compute phi(i-1)
                 x1 = sin(theta(i-1))*b11e(i-1) + cos(theta(i-1))*b21e(i-1)
                 x2 = sin(theta(i-1))*b11bulge + cos(theta(i-1))*b21bulge
                 y1 = sin(theta(i-1))*b12d(i-1) + cos(theta(i-1))*b22d(i-1)
                 y2 = sin(theta(i-1))*b12bulge + cos(theta(i-1))*b22bulge
                 phi(i-1) = atan2( sqrt(x1**2_ilp+x2**2_ilp), sqrt(y1**2_ilp+y2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 = b11e(i-1)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart21 = b21e(i-1)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12d(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22d(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i-1,i+1), b12(i-1,i),
                 ! b21(i-1,i+1), and b22(i-1,i). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart21 ) then
                    call stdlib_slartgp( x2, x1, work(iv1tsn+i-1), work(iv1tcs+i-1),r )
                 else if( .not. restart11 .and. restart21 ) then
                    call stdlib_slartgp( b11bulge, b11e(i-1), work(iv1tsn+i-1),work(iv1tcs+i-1), &
                              r )
                 else if( restart11 .and. .not. restart21 ) then
                    call stdlib_slartgp( b21bulge, b21e(i-1), work(iv1tsn+i-1),work(iv1tcs+i-1), &
                              r )
                 else if( mu <= nu ) then
                    call stdlib_slartgs( b11d(i), b11e(i), mu, work(iv1tcs+i-1),work(iv1tsn+i-1) )
                              
                 else
                    call stdlib_slartgs( b21d(i), b21e(i), nu, work(iv1tcs+i-1),work(iv1tsn+i-1) )
                              
                 end if
                 work(iv1tcs+i-1) = -work(iv1tcs+i-1)
                 work(iv1tsn+i-1) = -work(iv1tsn+i-1)
                 if( .not. restart12 .and. .not. restart22 ) then
                    call stdlib_slartgp( y2, y1, work(iv2tsn+i-1-1),work(iv2tcs+i-1-1), r )
                              
                 else if( .not. restart12 .and. restart22 ) then
                    call stdlib_slartgp( b12bulge, b12d(i-1), work(iv2tsn+i-1-1),work(iv2tcs+i-1-&
                              1_ilp), r )
                 else if( restart12 .and. .not. restart22 ) then
                    call stdlib_slartgp( b22bulge, b22d(i-1), work(iv2tsn+i-1-1),work(iv2tcs+i-1-&
                              1_ilp), r )
                 else if( nu < mu ) then
                    call stdlib_slartgs( b12e(i-1), b12d(i), nu, work(iv2tcs+i-1-1),work(iv2tsn+i-&
                              1_ilp-1) )
                 else
                    call stdlib_slartgs( b22e(i-1), b22d(i), mu, work(iv2tcs+i-1-1),work(iv2tsn+i-&
                              1_ilp-1) )
                 end if
                 temp = work(iv1tcs+i-1)*b11d(i) + work(iv1tsn+i-1)*b11e(i)
                 b11e(i) = work(iv1tcs+i-1)*b11e(i) -work(iv1tsn+i-1)*b11d(i)
                 b11d(i) = temp
                 b11bulge = work(iv1tsn+i-1)*b11d(i+1)
                 b11d(i+1) = work(iv1tcs+i-1)*b11d(i+1)
                 temp = work(iv1tcs+i-1)*b21d(i) + work(iv1tsn+i-1)*b21e(i)
                 b21e(i) = work(iv1tcs+i-1)*b21e(i) -work(iv1tsn+i-1)*b21d(i)
                 b21d(i) = temp
                 b21bulge = work(iv1tsn+i-1)*b21d(i+1)
                 b21d(i+1) = work(iv1tcs+i-1)*b21d(i+1)
                 temp = work(iv2tcs+i-1-1)*b12e(i-1) +work(iv2tsn+i-1-1)*b12d(i)
                 b12d(i) = work(iv2tcs+i-1-1)*b12d(i) -work(iv2tsn+i-1-1)*b12e(i-1)
                 b12e(i-1) = temp
                 b12bulge = work(iv2tsn+i-1-1)*b12e(i)
                 b12e(i) = work(iv2tcs+i-1-1)*b12e(i)
                 temp = work(iv2tcs+i-1-1)*b22e(i-1) +work(iv2tsn+i-1-1)*b22d(i)
                 b22d(i) = work(iv2tcs+i-1-1)*b22d(i) -work(iv2tsn+i-1-1)*b22e(i-1)
                 b22e(i-1) = temp
                 b22bulge = work(iv2tsn+i-1-1)*b22e(i)
                 b22e(i) = work(iv2tcs+i-1-1)*b22e(i)
                 ! compute theta(i)
                 x1 = cos(phi(i-1))*b11d(i) + sin(phi(i-1))*b12e(i-1)
                 x2 = cos(phi(i-1))*b11bulge + sin(phi(i-1))*b12bulge
                 y1 = cos(phi(i-1))*b21d(i) + sin(phi(i-1))*b22e(i-1)
                 y2 = cos(phi(i-1))*b21bulge + sin(phi(i-1))*b22bulge
                 theta(i) = atan2( sqrt(y1**2_ilp+y2**2_ilp), sqrt(x1**2_ilp+x2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 =   b11d(i)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12e(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart21 =   b21d(i)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22e(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i+1,i), b12(i+1,i-1),
                 ! b21(i+1,i), and b22(i+1,i-1). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart12 ) then
                    call stdlib_slartgp( x2, x1, work(iu1sn+i-1), work(iu1cs+i-1),r )
                 else if( .not. restart11 .and. restart12 ) then
                    call stdlib_slartgp( b11bulge, b11d(i), work(iu1sn+i-1),work(iu1cs+i-1), r )
                              
                 else if( restart11 .and. .not. restart12 ) then
                    call stdlib_slartgp( b12bulge, b12e(i-1), work(iu1sn+i-1),work(iu1cs+i-1), r )
                              
                 else if( mu <= nu ) then
                    call stdlib_slartgs( b11e(i), b11d(i+1), mu, work(iu1cs+i-1),work(iu1sn+i-1) )
                              
                 else
                    call stdlib_slartgs( b12d(i), b12e(i), nu, work(iu1cs+i-1),work(iu1sn+i-1) )
                              
                 end if
                 if( .not. restart21 .and. .not. restart22 ) then
                    call stdlib_slartgp( y2, y1, work(iu2sn+i-1), work(iu2cs+i-1),r )
                 else if( .not. restart21 .and. restart22 ) then
                    call stdlib_slartgp( b21bulge, b21d(i), work(iu2sn+i-1),work(iu2cs+i-1), r )
                              
                 else if( restart21 .and. .not. restart22 ) then
                    call stdlib_slartgp( b22bulge, b22e(i-1), work(iu2sn+i-1),work(iu2cs+i-1), r )
                              
                 else if( nu < mu ) then
                    call stdlib_slartgs( b21e(i), b21e(i+1), nu, work(iu2cs+i-1),work(iu2sn+i-1) )
                              
                 else
                    call stdlib_slartgs( b22d(i), b22e(i), mu, work(iu2cs+i-1),work(iu2sn+i-1) )
                              
                 end if
                 work(iu2cs+i-1) = -work(iu2cs+i-1)
                 work(iu2sn+i-1) = -work(iu2sn+i-1)
                 temp = work(iu1cs+i-1)*b11e(i) + work(iu1sn+i-1)*b11d(i+1)
                 b11d(i+1) = work(iu1cs+i-1)*b11d(i+1) -work(iu1sn+i-1)*b11e(i)
                 b11e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b11bulge = work(iu1sn+i-1)*b11e(i+1)
                    b11e(i+1) = work(iu1cs+i-1)*b11e(i+1)
                 end if
                 temp = work(iu2cs+i-1)*b21e(i) + work(iu2sn+i-1)*b21d(i+1)
                 b21d(i+1) = work(iu2cs+i-1)*b21d(i+1) -work(iu2sn+i-1)*b21e(i)
                 b21e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b21bulge = work(iu2sn+i-1)*b21e(i+1)
                    b21e(i+1) = work(iu2cs+i-1)*b21e(i+1)
                 end if
                 temp = work(iu1cs+i-1)*b12d(i) + work(iu1sn+i-1)*b12e(i)
                 b12e(i) = work(iu1cs+i-1)*b12e(i) - work(iu1sn+i-1)*b12d(i)
                 b12d(i) = temp
                 b12bulge = work(iu1sn+i-1)*b12d(i+1)
                 b12d(i+1) = work(iu1cs+i-1)*b12d(i+1)
                 temp = work(iu2cs+i-1)*b22d(i) + work(iu2sn+i-1)*b22e(i)
                 b22e(i) = work(iu2cs+i-1)*b22e(i) - work(iu2sn+i-1)*b22d(i)
                 b22d(i) = temp
                 b22bulge = work(iu2sn+i-1)*b22d(i+1)
                 b22d(i+1) = work(iu2cs+i-1)*b22d(i+1)
              end do
              ! compute phi(imax-1)
              x1 = sin(theta(imax-1))*b11e(imax-1) +cos(theta(imax-1))*b21e(imax-1)
              y1 = sin(theta(imax-1))*b12d(imax-1) +cos(theta(imax-1))*b22d(imax-1)
              y2 = sin(theta(imax-1))*b12bulge + cos(theta(imax-1))*b22bulge
              phi(imax-1) = atan2( abs(x1), sqrt(y1**2_ilp+y2**2_ilp) )
              ! chase bulges from b12(imax-1,imax) and b22(imax-1,imax)
              restart12 = b12d(imax-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
              restart22 = b22d(imax-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
              if( .not. restart12 .and. .not. restart22 ) then
                 call stdlib_slartgp( y2, y1, work(iv2tsn+imax-1-1),work(iv2tcs+imax-1-1), r )
                           
              else if( .not. restart12 .and. restart22 ) then
                 call stdlib_slartgp( b12bulge, b12d(imax-1), work(iv2tsn+imax-1-1),work(iv2tcs+&
                           imax-1-1), r )
              else if( restart12 .and. .not. restart22 ) then
                 call stdlib_slartgp( b22bulge, b22d(imax-1), work(iv2tsn+imax-1-1),work(iv2tcs+&
                           imax-1-1), r )
              else if( nu < mu ) then
                 call stdlib_slartgs( b12e(imax-1), b12d(imax), nu,work(iv2tcs+imax-1-1), work(&
                           iv2tsn+imax-1-1) )
              else
                 call stdlib_slartgs( b22e(imax-1), b22d(imax), mu,work(iv2tcs+imax-1-1), work(&
                           iv2tsn+imax-1-1) )
              end if
              temp = work(iv2tcs+imax-1-1)*b12e(imax-1) +work(iv2tsn+imax-1-1)*b12d(imax)
              b12d(imax) = work(iv2tcs+imax-1-1)*b12d(imax) -work(iv2tsn+imax-1-1)*b12e(imax-1)
                        
              b12e(imax-1) = temp
              temp = work(iv2tcs+imax-1-1)*b22e(imax-1) +work(iv2tsn+imax-1-1)*b22d(imax)
              b22d(imax) = work(iv2tcs+imax-1-1)*b22d(imax) -work(iv2tsn+imax-1-1)*b22e(imax-1)
                        
              b22e(imax-1) = temp
              ! update singular vectors
              if( wantu1 ) then
                 if( colmajor ) then
                    call stdlib_slasr( 'R', 'V', 'F', p, imax-imin+1,work(iu1cs+imin-1), work(&
                              iu1sn+imin-1),u1(1_ilp,imin), ldu1 )
                 else
                    call stdlib_slasr( 'L', 'V', 'F', imax-imin+1, p,work(iu1cs+imin-1), work(&
                              iu1sn+imin-1),u1(imin,1_ilp), ldu1 )
                 end if
              end if
              if( wantu2 ) then
                 if( colmajor ) then
                    call stdlib_slasr( 'R', 'V', 'F', m-p, imax-imin+1,work(iu2cs+imin-1), work(&
                              iu2sn+imin-1),u2(1_ilp,imin), ldu2 )
                 else
                    call stdlib_slasr( 'L', 'V', 'F', imax-imin+1, m-p,work(iu2cs+imin-1), work(&
                              iu2sn+imin-1),u2(imin,1_ilp), ldu2 )
                 end if
              end if
              if( wantv1t ) then
                 if( colmajor ) then
                    call stdlib_slasr( 'L', 'V', 'F', imax-imin+1, q,work(iv1tcs+imin-1), work(&
                              iv1tsn+imin-1),v1t(imin,1_ilp), ldv1t )
                 else
                    call stdlib_slasr( 'R', 'V', 'F', q, imax-imin+1,work(iv1tcs+imin-1), work(&
                              iv1tsn+imin-1),v1t(1_ilp,imin), ldv1t )
                 end if
              end if
              if( wantv2t ) then
                 if( colmajor ) then
                    call stdlib_slasr( 'L', 'V', 'F', imax-imin+1, m-q,work(iv2tcs+imin-1), work(&
                              iv2tsn+imin-1),v2t(imin,1_ilp), ldv2t )
                 else
                    call stdlib_slasr( 'R', 'V', 'F', m-q, imax-imin+1,work(iv2tcs+imin-1), work(&
                              iv2tsn+imin-1),v2t(1_ilp,imin), ldv2t )
                 end if
              end if
              ! fix signs on b11(imax-1,imax) and b21(imax-1,imax)
              if( b11e(imax-1)+b21e(imax-1) > 0_ilp ) then
                 b11d(imax) = -b11d(imax)
                 b21d(imax) = -b21d(imax)
                 if( wantv1t ) then
                    if( colmajor ) then
                       call stdlib_sscal( q, negone, v1t(imax,1_ilp), ldv1t )
                    else
                       call stdlib_sscal( q, negone, v1t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! compute theta(imax)
              x1 = cos(phi(imax-1))*b11d(imax) +sin(phi(imax-1))*b12e(imax-1)
              y1 = cos(phi(imax-1))*b21d(imax) +sin(phi(imax-1))*b22e(imax-1)
              theta(imax) = atan2( abs(y1), abs(x1) )
              ! fix signs on b11(imax,imax), b12(imax,imax-1), b21(imax,imax),
              ! and b22(imax,imax-1)
              if( b11d(imax)+b12e(imax-1) < 0_ilp ) then
                 b12d(imax) = -b12d(imax)
                 if( wantu1 ) then
                    if( colmajor ) then
                       call stdlib_sscal( p, negone, u1(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_sscal( p, negone, u1(imax,1_ilp), ldu1 )
                    end if
                 end if
              end if
              if( b21d(imax)+b22e(imax-1) > 0_ilp ) then
                 b22d(imax) = -b22d(imax)
                 if( wantu2 ) then
                    if( colmajor ) then
                       call stdlib_sscal( m-p, negone, u2(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_sscal( m-p, negone, u2(imax,1_ilp), ldu2 )
                    end if
                 end if
              end if
              ! fix signs on b12(imax,imax) and b22(imax,imax)
              if( b12d(imax)+b22d(imax) < 0_ilp ) then
                 if( wantv2t ) then
                    if( colmajor ) then
                       call stdlib_sscal( m-q, negone, v2t(imax,1_ilp), ldv2t )
                    else
                       call stdlib_sscal( m-q, negone, v2t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! test for negligible sines or cosines
              do i = imin, imax
                 if( theta(i) < thresh ) then
                    theta(i) = zero
                 else if( theta(i) > piover2-thresh ) then
                    theta(i) = piover2
                 end if
              end do
              do i = imin, imax-1
                 if( phi(i) < thresh ) then
                    phi(i) = zero
                 else if( phi(i) > piover2-thresh ) then
                    phi(i) = piover2
                 end if
              end do
              ! deflate
              if (imax > 1_ilp) then
                 do while( phi(imax-1) == zero )
                    imax = imax - 1_ilp
                    if (imax <= 1) exit
                 end do
              end if
              if( imin > imax - 1_ilp )imin = imax - 1_ilp
              if (imin > 1_ilp) then
                 do while (phi(imin-1) /= zero)
                     imin = imin - 1_ilp
                     if (imin <= 1) exit
                 end do
              end if
              ! repeat main iteration loop
           end do
           ! postprocessing: order theta from least to greatest
           do i = 1, q
              mini = i
              thetamin = theta(i)
              do j = i+1, q
                 if( theta(j) < thetamin ) then
                    mini = j
                    thetamin = theta(j)
                 end if
              end do
              if( mini /= i ) then
                 theta(mini) = theta(i)
                 theta(i) = thetamin
                 if( colmajor ) then
                    if( wantu1 )call stdlib_sswap( p, u1(1_ilp,i), 1_ilp, u1(1_ilp,mini), 1_ilp )
                    if( wantu2 )call stdlib_sswap( m-p, u2(1_ilp,i), 1_ilp, u2(1_ilp,mini), 1_ilp )
                    if( wantv1t )call stdlib_sswap( q, v1t(i,1_ilp), ldv1t, v1t(mini,1_ilp), ldv1t )
                              
                    if( wantv2t )call stdlib_sswap( m-q, v2t(i,1_ilp), ldv2t, v2t(mini,1_ilp),ldv2t )
                              
                 else
                    if( wantu1 )call stdlib_sswap( p, u1(i,1_ilp), ldu1, u1(mini,1_ilp), ldu1 )
                    if( wantu2 )call stdlib_sswap( m-p, u2(i,1_ilp), ldu2, u2(mini,1_ilp), ldu2 )
                    if( wantv1t )call stdlib_sswap( q, v1t(1_ilp,i), 1_ilp, v1t(1_ilp,mini), 1_ilp )
                    if( wantv2t )call stdlib_sswap( m-q, v2t(1_ilp,i), 1_ilp, v2t(1_ilp,mini), 1_ilp )
                 end if
              end if
           end do
           return
     end subroutine stdlib_sbbcsd

     pure module subroutine stdlib_dbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     !! DBBCSD computes the CS decomposition of an orthogonal matrix in
     !! bidiagonal-block form,
     !! [ B11 | B12 0  0 ]
     !! [  0  |  0 -I  0 ]
     !! X = [----------------]
     !! [ B21 | B22 0  0 ]
     !! [  0  |  0  0  I ]
     !! [  C | -S  0  0 ]
     !! [ U1 |    ] [  0 |  0 -I  0 ] [ V1 |    ]**T
     !! = [---------] [---------------] [---------]   .
     !! [    | U2 ] [  S |  C  0  0 ] [    | V2 ]
     !! [  0 |  0  0  I ]
     !! X is M-by-M, its top-left block is P-by-Q, and Q must be no larger
     !! than P, M-P, or M-Q. (If Q is not the smallest index, then X must be
     !! transposed and/or permuted. This can be done in constant time using
     !! the TRANS and SIGNS options. See DORCSD for details.)
     !! The bidiagonal matrices B11, B12, B21, and B22 are represented
     !! implicitly by angles THETA(1:Q) and PHI(1:Q-1).
     !! The orthogonal matrices U1, U2, V1T, and V2T are input/output.
     !! The input matrices are pre- or post-multiplied by the appropriate
     !! singular vector matrices.
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, work, &
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, one, ten, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lwork, m, p, q
           ! Array Arguments 
           real(dp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), work(*)
           real(dp), intent(inout) :: phi(*), theta(*)
           real(dp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
        ! ===================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 6_ilp
           real(dp), parameter :: hundred = 100.0_dp
           real(dp), parameter :: meighth = -0.125_dp
           real(dp), parameter :: piover2 = 1.57079632679489661923132169163975144210_dp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery, restart11, restart12, restart21, restart22, wantu1, &
                     wantu2, wantv1t, wantv2t
           integer(ilp) :: i, imin, imax, iter, iu1cs, iu1sn, iu2cs, iu2sn, iv1tcs, iv1tsn, &
                     iv2tcs, iv2tsn, j, lworkmin, lworkopt, maxit, mini
           real(dp) :: b11bulge, b12bulge, b21bulge, b22bulge, dummy, eps, mu, nu, r, sigma11, &
                     sigma21, temp, thetamax, thetamin, thresh, tol, tolmul, unfl, x1, x2, y1, y2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( m < 0_ilp ) then
              info = -6_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -7_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -8_ilp
           else if( q > p .or. q > m-p .or. q > m-q ) then
              info = -8_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -12_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -14_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -16_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -18_ilp
           end if
           ! quick return if q = 0
           if( info == 0_ilp .and. q == 0_ilp ) then
              lworkmin = 1_ilp
              work(1_ilp) = lworkmin
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iu1cs = 1_ilp
              iu1sn = iu1cs + q
              iu2cs = iu1sn + q
              iu2sn = iu2cs + q
              iv1tcs = iu2sn + q
              iv1tsn = iv1tcs + q
              iv2tcs = iv1tsn + q
              iv2tsn = iv2tcs + q
              lworkopt = iv2tsn + q - 1_ilp
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -28_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DBBCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'EPSILON' )
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           tolmul = max( ten, min( hundred, eps**meighth ) )
           tol = tolmul*eps
           thresh = max( tol, maxitr*q*q*unfl )
           ! test for negligible sines or cosines
           do i = 1, q
              if( theta(i) < thresh ) then
                 theta(i) = zero
              else if( theta(i) > piover2-thresh ) then
                 theta(i) = piover2
              end if
           end do
           do i = 1, q-1
              if( phi(i) < thresh ) then
                 phi(i) = zero
              else if( phi(i) > piover2-thresh ) then
                 phi(i) = piover2
              end if
           end do
           ! initial deflation
           imax = q
           do while( imax > 1 )
              if( phi(imax-1) /= zero ) then
                 exit
              end if
              imax = imax - 1_ilp
           end do
           imin = imax - 1_ilp
           if  ( imin > 1_ilp ) then
              do while( phi(imin-1) /= zero )
                 imin = imin - 1_ilp
                 if  ( imin <= 1 ) exit
              end do
           end if
           ! initialize iteration counter
           maxit = maxitr*q*q
           iter = 0_ilp
           ! begin main iteration loop
           do while( imax > 1 )
              ! compute the matrix entries
              b11d(imin) = cos( theta(imin) )
              b21d(imin) = -sin( theta(imin) )
              do i = imin, imax - 1
                 b11e(i) = -sin( theta(i) ) * sin( phi(i) )
                 b11d(i+1) = cos( theta(i+1) ) * cos( phi(i) )
                 b12d(i) = sin( theta(i) ) * cos( phi(i) )
                 b12e(i) = cos( theta(i+1) ) * sin( phi(i) )
                 b21e(i) = -cos( theta(i) ) * sin( phi(i) )
                 b21d(i+1) = -sin( theta(i+1) ) * cos( phi(i) )
                 b22d(i) = cos( theta(i) ) * cos( phi(i) )
                 b22e(i) = -sin( theta(i+1) ) * sin( phi(i) )
              end do
              b12d(imax) = sin( theta(imax) )
              b22d(imax) = cos( theta(imax) )
              ! abort if not converging; otherwise, increment iter
              if( iter > maxit ) then
                 info = 0_ilp
                 do i = 1, q
                    if( phi(i) /= zero )info = info + 1_ilp
                 end do
                 return
              end if
              iter = iter + imax - imin
              ! compute shifts
              thetamax = theta(imin)
              thetamin = theta(imin)
              do i = imin+1, imax
                 if( theta(i) > thetamax )thetamax = theta(i)
                 if( theta(i) < thetamin )thetamin = theta(i)
              end do
              if( thetamax > piover2 - thresh ) then
                 ! zero on diagonals of b11 and b22; induce deflation with a
                 ! zero shift
                 mu = zero
                 nu = one
              else if( thetamin < thresh ) then
                 ! zero on diagonals of b12 and b22; induce deflation with a
                 ! zero shift
                 mu = one
                 nu = zero
              else
                 ! compute shifts for b11 and b21 and use the lesser
                 call stdlib_dlas2( b11d(imax-1), b11e(imax-1), b11d(imax), sigma11,dummy )
                           
                 call stdlib_dlas2( b21d(imax-1), b21e(imax-1), b21d(imax), sigma21,dummy )
                           
                 if( sigma11 <= sigma21 ) then
                    mu = sigma11
                    nu = sqrt( one - mu**2_ilp )
                    if( mu < thresh ) then
                       mu = zero
                       nu = one
                    end if
                 else
                    nu = sigma21
                    mu = sqrt( one - nu**2_ilp )
                    if( nu < thresh ) then
                       mu = one
                       nu = zero
                    end if
                 end if
              end if
              ! rotate to produce bulges in b11 and b21
              if( mu <= nu ) then
                 call stdlib_dlartgs( b11d(imin), b11e(imin), mu,work(iv1tcs+imin-1), work(iv1tsn+&
                           imin-1) )
              else
                 call stdlib_dlartgs( b21d(imin), b21e(imin), nu,work(iv1tcs+imin-1), work(iv1tsn+&
                           imin-1) )
              end if
              temp = work(iv1tcs+imin-1)*b11d(imin) +work(iv1tsn+imin-1)*b11e(imin)
              b11e(imin) = work(iv1tcs+imin-1)*b11e(imin) -work(iv1tsn+imin-1)*b11d(imin)
              b11d(imin) = temp
              b11bulge = work(iv1tsn+imin-1)*b11d(imin+1)
              b11d(imin+1) = work(iv1tcs+imin-1)*b11d(imin+1)
              temp = work(iv1tcs+imin-1)*b21d(imin) +work(iv1tsn+imin-1)*b21e(imin)
              b21e(imin) = work(iv1tcs+imin-1)*b21e(imin) -work(iv1tsn+imin-1)*b21d(imin)
              b21d(imin) = temp
              b21bulge = work(iv1tsn+imin-1)*b21d(imin+1)
              b21d(imin+1) = work(iv1tcs+imin-1)*b21d(imin+1)
              ! compute theta(imin)
              theta( imin ) = atan2( sqrt( b21d(imin)**2_ilp+b21bulge**2_ilp ),sqrt( b11d(imin)**2_ilp+&
                        b11bulge**2_ilp ) )
              ! chase the bulges in b11(imin+1,imin) and b21(imin+1,imin)
              if( b11d(imin)**2_ilp+b11bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_dlartgp( b11bulge, b11d(imin), work(iu1sn+imin-1),work(iu1cs+imin-1),&
                            r )
              else if( mu <= nu ) then
                 call stdlib_dlartgs( b11e( imin ), b11d( imin + 1_ilp ), mu,work(iu1cs+imin-1), work(&
                           iu1sn+imin-1) )
              else
                 call stdlib_dlartgs( b12d( imin ), b12e( imin ), nu,work(iu1cs+imin-1), work(&
                           iu1sn+imin-1) )
              end if
              if( b21d(imin)**2_ilp+b21bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_dlartgp( b21bulge, b21d(imin), work(iu2sn+imin-1),work(iu2cs+imin-1),&
                            r )
              else if( nu < mu ) then
                 call stdlib_dlartgs( b21e( imin ), b21d( imin + 1_ilp ), nu,work(iu2cs+imin-1), work(&
                           iu2sn+imin-1) )
              else
                 call stdlib_dlartgs( b22d(imin), b22e(imin), mu,work(iu2cs+imin-1), work(iu2sn+&
                           imin-1) )
              end if
              work(iu2cs+imin-1) = -work(iu2cs+imin-1)
              work(iu2sn+imin-1) = -work(iu2sn+imin-1)
              temp = work(iu1cs+imin-1)*b11e(imin) +work(iu1sn+imin-1)*b11d(imin+1)
              b11d(imin+1) = work(iu1cs+imin-1)*b11d(imin+1) -work(iu1sn+imin-1)*b11e(imin)
                        
              b11e(imin) = temp
              if( imax > imin+1 ) then
                 b11bulge = work(iu1sn+imin-1)*b11e(imin+1)
                 b11e(imin+1) = work(iu1cs+imin-1)*b11e(imin+1)
              end if
              temp = work(iu1cs+imin-1)*b12d(imin) +work(iu1sn+imin-1)*b12e(imin)
              b12e(imin) = work(iu1cs+imin-1)*b12e(imin) -work(iu1sn+imin-1)*b12d(imin)
              b12d(imin) = temp
              b12bulge = work(iu1sn+imin-1)*b12d(imin+1)
              b12d(imin+1) = work(iu1cs+imin-1)*b12d(imin+1)
              temp = work(iu2cs+imin-1)*b21e(imin) +work(iu2sn+imin-1)*b21d(imin+1)
              b21d(imin+1) = work(iu2cs+imin-1)*b21d(imin+1) -work(iu2sn+imin-1)*b21e(imin)
                        
              b21e(imin) = temp
              if( imax > imin+1 ) then
                 b21bulge = work(iu2sn+imin-1)*b21e(imin+1)
                 b21e(imin+1) = work(iu2cs+imin-1)*b21e(imin+1)
              end if
              temp = work(iu2cs+imin-1)*b22d(imin) +work(iu2sn+imin-1)*b22e(imin)
              b22e(imin) = work(iu2cs+imin-1)*b22e(imin) -work(iu2sn+imin-1)*b22d(imin)
              b22d(imin) = temp
              b22bulge = work(iu2sn+imin-1)*b22d(imin+1)
              b22d(imin+1) = work(iu2cs+imin-1)*b22d(imin+1)
              ! inner loop: chase bulges from b11(imin,imin+2),
              ! b12(imin,imin+1), b21(imin,imin+2), and b22(imin,imin+1) to
              ! bottom-right
              do i = imin+1, imax-1
                 ! compute phi(i-1)
                 x1 = sin(theta(i-1))*b11e(i-1) + cos(theta(i-1))*b21e(i-1)
                 x2 = sin(theta(i-1))*b11bulge + cos(theta(i-1))*b21bulge
                 y1 = sin(theta(i-1))*b12d(i-1) + cos(theta(i-1))*b22d(i-1)
                 y2 = sin(theta(i-1))*b12bulge + cos(theta(i-1))*b22bulge
                 phi(i-1) = atan2( sqrt(x1**2_ilp+x2**2_ilp), sqrt(y1**2_ilp+y2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 = b11e(i-1)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart21 = b21e(i-1)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12d(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22d(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i-1,i+1), b12(i-1,i),
                 ! b21(i-1,i+1), and b22(i-1,i). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart21 ) then
                    call stdlib_dlartgp( x2, x1, work(iv1tsn+i-1), work(iv1tcs+i-1),r )
                 else if( .not. restart11 .and. restart21 ) then
                    call stdlib_dlartgp( b11bulge, b11e(i-1), work(iv1tsn+i-1),work(iv1tcs+i-1), &
                              r )
                 else if( restart11 .and. .not. restart21 ) then
                    call stdlib_dlartgp( b21bulge, b21e(i-1), work(iv1tsn+i-1),work(iv1tcs+i-1), &
                              r )
                 else if( mu <= nu ) then
                    call stdlib_dlartgs( b11d(i), b11e(i), mu, work(iv1tcs+i-1),work(iv1tsn+i-1) )
                              
                 else
                    call stdlib_dlartgs( b21d(i), b21e(i), nu, work(iv1tcs+i-1),work(iv1tsn+i-1) )
                              
                 end if
                 work(iv1tcs+i-1) = -work(iv1tcs+i-1)
                 work(iv1tsn+i-1) = -work(iv1tsn+i-1)
                 if( .not. restart12 .and. .not. restart22 ) then
                    call stdlib_dlartgp( y2, y1, work(iv2tsn+i-1-1),work(iv2tcs+i-1-1), r )
                              
                 else if( .not. restart12 .and. restart22 ) then
                    call stdlib_dlartgp( b12bulge, b12d(i-1), work(iv2tsn+i-1-1),work(iv2tcs+i-1-&
                              1_ilp), r )
                 else if( restart12 .and. .not. restart22 ) then
                    call stdlib_dlartgp( b22bulge, b22d(i-1), work(iv2tsn+i-1-1),work(iv2tcs+i-1-&
                              1_ilp), r )
                 else if( nu < mu ) then
                    call stdlib_dlartgs( b12e(i-1), b12d(i), nu, work(iv2tcs+i-1-1),work(iv2tsn+i-&
                              1_ilp-1) )
                 else
                    call stdlib_dlartgs( b22e(i-1), b22d(i), mu, work(iv2tcs+i-1-1),work(iv2tsn+i-&
                              1_ilp-1) )
                 end if
                 temp = work(iv1tcs+i-1)*b11d(i) + work(iv1tsn+i-1)*b11e(i)
                 b11e(i) = work(iv1tcs+i-1)*b11e(i) -work(iv1tsn+i-1)*b11d(i)
                 b11d(i) = temp
                 b11bulge = work(iv1tsn+i-1)*b11d(i+1)
                 b11d(i+1) = work(iv1tcs+i-1)*b11d(i+1)
                 temp = work(iv1tcs+i-1)*b21d(i) + work(iv1tsn+i-1)*b21e(i)
                 b21e(i) = work(iv1tcs+i-1)*b21e(i) -work(iv1tsn+i-1)*b21d(i)
                 b21d(i) = temp
                 b21bulge = work(iv1tsn+i-1)*b21d(i+1)
                 b21d(i+1) = work(iv1tcs+i-1)*b21d(i+1)
                 temp = work(iv2tcs+i-1-1)*b12e(i-1) +work(iv2tsn+i-1-1)*b12d(i)
                 b12d(i) = work(iv2tcs+i-1-1)*b12d(i) -work(iv2tsn+i-1-1)*b12e(i-1)
                 b12e(i-1) = temp
                 b12bulge = work(iv2tsn+i-1-1)*b12e(i)
                 b12e(i) = work(iv2tcs+i-1-1)*b12e(i)
                 temp = work(iv2tcs+i-1-1)*b22e(i-1) +work(iv2tsn+i-1-1)*b22d(i)
                 b22d(i) = work(iv2tcs+i-1-1)*b22d(i) -work(iv2tsn+i-1-1)*b22e(i-1)
                 b22e(i-1) = temp
                 b22bulge = work(iv2tsn+i-1-1)*b22e(i)
                 b22e(i) = work(iv2tcs+i-1-1)*b22e(i)
                 ! compute theta(i)
                 x1 = cos(phi(i-1))*b11d(i) + sin(phi(i-1))*b12e(i-1)
                 x2 = cos(phi(i-1))*b11bulge + sin(phi(i-1))*b12bulge
                 y1 = cos(phi(i-1))*b21d(i) + sin(phi(i-1))*b22e(i-1)
                 y2 = cos(phi(i-1))*b21bulge + sin(phi(i-1))*b22bulge
                 theta(i) = atan2( sqrt(y1**2_ilp+y2**2_ilp), sqrt(x1**2_ilp+x2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 =   b11d(i)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12e(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart21 =   b21d(i)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22e(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i+1,i), b12(i+1,i-1),
                 ! b21(i+1,i), and b22(i+1,i-1). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart12 ) then
                    call stdlib_dlartgp( x2, x1, work(iu1sn+i-1), work(iu1cs+i-1),r )
                 else if( .not. restart11 .and. restart12 ) then
                    call stdlib_dlartgp( b11bulge, b11d(i), work(iu1sn+i-1),work(iu1cs+i-1), r )
                              
                 else if( restart11 .and. .not. restart12 ) then
                    call stdlib_dlartgp( b12bulge, b12e(i-1), work(iu1sn+i-1),work(iu1cs+i-1), r )
                              
                 else if( mu <= nu ) then
                    call stdlib_dlartgs( b11e(i), b11d(i+1), mu, work(iu1cs+i-1),work(iu1sn+i-1) )
                              
                 else
                    call stdlib_dlartgs( b12d(i), b12e(i), nu, work(iu1cs+i-1),work(iu1sn+i-1) )
                              
                 end if
                 if( .not. restart21 .and. .not. restart22 ) then
                    call stdlib_dlartgp( y2, y1, work(iu2sn+i-1), work(iu2cs+i-1),r )
                 else if( .not. restart21 .and. restart22 ) then
                    call stdlib_dlartgp( b21bulge, b21d(i), work(iu2sn+i-1),work(iu2cs+i-1), r )
                              
                 else if( restart21 .and. .not. restart22 ) then
                    call stdlib_dlartgp( b22bulge, b22e(i-1), work(iu2sn+i-1),work(iu2cs+i-1), r )
                              
                 else if( nu < mu ) then
                    call stdlib_dlartgs( b21e(i), b21e(i+1), nu, work(iu2cs+i-1),work(iu2sn+i-1) )
                              
                 else
                    call stdlib_dlartgs( b22d(i), b22e(i), mu, work(iu2cs+i-1),work(iu2sn+i-1) )
                              
                 end if
                 work(iu2cs+i-1) = -work(iu2cs+i-1)
                 work(iu2sn+i-1) = -work(iu2sn+i-1)
                 temp = work(iu1cs+i-1)*b11e(i) + work(iu1sn+i-1)*b11d(i+1)
                 b11d(i+1) = work(iu1cs+i-1)*b11d(i+1) -work(iu1sn+i-1)*b11e(i)
                 b11e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b11bulge = work(iu1sn+i-1)*b11e(i+1)
                    b11e(i+1) = work(iu1cs+i-1)*b11e(i+1)
                 end if
                 temp = work(iu2cs+i-1)*b21e(i) + work(iu2sn+i-1)*b21d(i+1)
                 b21d(i+1) = work(iu2cs+i-1)*b21d(i+1) -work(iu2sn+i-1)*b21e(i)
                 b21e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b21bulge = work(iu2sn+i-1)*b21e(i+1)
                    b21e(i+1) = work(iu2cs+i-1)*b21e(i+1)
                 end if
                 temp = work(iu1cs+i-1)*b12d(i) + work(iu1sn+i-1)*b12e(i)
                 b12e(i) = work(iu1cs+i-1)*b12e(i) - work(iu1sn+i-1)*b12d(i)
                 b12d(i) = temp
                 b12bulge = work(iu1sn+i-1)*b12d(i+1)
                 b12d(i+1) = work(iu1cs+i-1)*b12d(i+1)
                 temp = work(iu2cs+i-1)*b22d(i) + work(iu2sn+i-1)*b22e(i)
                 b22e(i) = work(iu2cs+i-1)*b22e(i) - work(iu2sn+i-1)*b22d(i)
                 b22d(i) = temp
                 b22bulge = work(iu2sn+i-1)*b22d(i+1)
                 b22d(i+1) = work(iu2cs+i-1)*b22d(i+1)
              end do
              ! compute phi(imax-1)
              x1 = sin(theta(imax-1))*b11e(imax-1) +cos(theta(imax-1))*b21e(imax-1)
              y1 = sin(theta(imax-1))*b12d(imax-1) +cos(theta(imax-1))*b22d(imax-1)
              y2 = sin(theta(imax-1))*b12bulge + cos(theta(imax-1))*b22bulge
              phi(imax-1) = atan2( abs(x1), sqrt(y1**2_ilp+y2**2_ilp) )
              ! chase bulges from b12(imax-1,imax) and b22(imax-1,imax)
              restart12 = b12d(imax-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
              restart22 = b22d(imax-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
              if( .not. restart12 .and. .not. restart22 ) then
                 call stdlib_dlartgp( y2, y1, work(iv2tsn+imax-1-1),work(iv2tcs+imax-1-1), r )
                           
              else if( .not. restart12 .and. restart22 ) then
                 call stdlib_dlartgp( b12bulge, b12d(imax-1), work(iv2tsn+imax-1-1),work(iv2tcs+&
                           imax-1-1), r )
              else if( restart12 .and. .not. restart22 ) then
                 call stdlib_dlartgp( b22bulge, b22d(imax-1), work(iv2tsn+imax-1-1),work(iv2tcs+&
                           imax-1-1), r )
              else if( nu < mu ) then
                 call stdlib_dlartgs( b12e(imax-1), b12d(imax), nu,work(iv2tcs+imax-1-1), work(&
                           iv2tsn+imax-1-1) )
              else
                 call stdlib_dlartgs( b22e(imax-1), b22d(imax), mu,work(iv2tcs+imax-1-1), work(&
                           iv2tsn+imax-1-1) )
              end if
              temp = work(iv2tcs+imax-1-1)*b12e(imax-1) +work(iv2tsn+imax-1-1)*b12d(imax)
              b12d(imax) = work(iv2tcs+imax-1-1)*b12d(imax) -work(iv2tsn+imax-1-1)*b12e(imax-1)
                        
              b12e(imax-1) = temp
              temp = work(iv2tcs+imax-1-1)*b22e(imax-1) +work(iv2tsn+imax-1-1)*b22d(imax)
              b22d(imax) = work(iv2tcs+imax-1-1)*b22d(imax) -work(iv2tsn+imax-1-1)*b22e(imax-1)
                        
              b22e(imax-1) = temp
              ! update singular vectors
              if( wantu1 ) then
                 if( colmajor ) then
                    call stdlib_dlasr( 'R', 'V', 'F', p, imax-imin+1,work(iu1cs+imin-1), work(&
                              iu1sn+imin-1),u1(1_ilp,imin), ldu1 )
                 else
                    call stdlib_dlasr( 'L', 'V', 'F', imax-imin+1, p,work(iu1cs+imin-1), work(&
                              iu1sn+imin-1),u1(imin,1_ilp), ldu1 )
                 end if
              end if
              if( wantu2 ) then
                 if( colmajor ) then
                    call stdlib_dlasr( 'R', 'V', 'F', m-p, imax-imin+1,work(iu2cs+imin-1), work(&
                              iu2sn+imin-1),u2(1_ilp,imin), ldu2 )
                 else
                    call stdlib_dlasr( 'L', 'V', 'F', imax-imin+1, m-p,work(iu2cs+imin-1), work(&
                              iu2sn+imin-1),u2(imin,1_ilp), ldu2 )
                 end if
              end if
              if( wantv1t ) then
                 if( colmajor ) then
                    call stdlib_dlasr( 'L', 'V', 'F', imax-imin+1, q,work(iv1tcs+imin-1), work(&
                              iv1tsn+imin-1),v1t(imin,1_ilp), ldv1t )
                 else
                    call stdlib_dlasr( 'R', 'V', 'F', q, imax-imin+1,work(iv1tcs+imin-1), work(&
                              iv1tsn+imin-1),v1t(1_ilp,imin), ldv1t )
                 end if
              end if
              if( wantv2t ) then
                 if( colmajor ) then
                    call stdlib_dlasr( 'L', 'V', 'F', imax-imin+1, m-q,work(iv2tcs+imin-1), work(&
                              iv2tsn+imin-1),v2t(imin,1_ilp), ldv2t )
                 else
                    call stdlib_dlasr( 'R', 'V', 'F', m-q, imax-imin+1,work(iv2tcs+imin-1), work(&
                              iv2tsn+imin-1),v2t(1_ilp,imin), ldv2t )
                 end if
              end if
              ! fix signs on b11(imax-1,imax) and b21(imax-1,imax)
              if( b11e(imax-1)+b21e(imax-1) > 0_ilp ) then
                 b11d(imax) = -b11d(imax)
                 b21d(imax) = -b21d(imax)
                 if( wantv1t ) then
                    if( colmajor ) then
                       call stdlib_dscal( q, negone, v1t(imax,1_ilp), ldv1t )
                    else
                       call stdlib_dscal( q, negone, v1t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! compute theta(imax)
              x1 = cos(phi(imax-1))*b11d(imax) +sin(phi(imax-1))*b12e(imax-1)
              y1 = cos(phi(imax-1))*b21d(imax) +sin(phi(imax-1))*b22e(imax-1)
              theta(imax) = atan2( abs(y1), abs(x1) )
              ! fix signs on b11(imax,imax), b12(imax,imax-1), b21(imax,imax),
              ! and b22(imax,imax-1)
              if( b11d(imax)+b12e(imax-1) < 0_ilp ) then
                 b12d(imax) = -b12d(imax)
                 if( wantu1 ) then
                    if( colmajor ) then
                       call stdlib_dscal( p, negone, u1(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_dscal( p, negone, u1(imax,1_ilp), ldu1 )
                    end if
                 end if
              end if
              if( b21d(imax)+b22e(imax-1) > 0_ilp ) then
                 b22d(imax) = -b22d(imax)
                 if( wantu2 ) then
                    if( colmajor ) then
                       call stdlib_dscal( m-p, negone, u2(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_dscal( m-p, negone, u2(imax,1_ilp), ldu2 )
                    end if
                 end if
              end if
              ! fix signs on b12(imax,imax) and b22(imax,imax)
              if( b12d(imax)+b22d(imax) < 0_ilp ) then
                 if( wantv2t ) then
                    if( colmajor ) then
                       call stdlib_dscal( m-q, negone, v2t(imax,1_ilp), ldv2t )
                    else
                       call stdlib_dscal( m-q, negone, v2t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! test for negligible sines or cosines
              do i = imin, imax
                 if( theta(i) < thresh ) then
                    theta(i) = zero
                 else if( theta(i) > piover2-thresh ) then
                    theta(i) = piover2
                 end if
              end do
              do i = imin, imax-1
                 if( phi(i) < thresh ) then
                    phi(i) = zero
                 else if( phi(i) > piover2-thresh ) then
                    phi(i) = piover2
                 end if
              end do
              ! deflate
              if (imax > 1_ilp) then
                 do while( phi(imax-1) == zero )
                    imax = imax - 1_ilp
                    if (imax <= 1) exit
                 end do
              end if
              if( imin > imax - 1_ilp )imin = imax - 1_ilp
              if (imin > 1_ilp) then
                 do while (phi(imin-1) /= zero)
                     imin = imin - 1_ilp
                     if (imin <= 1) exit
                 end do
              end if
              ! repeat main iteration loop
           end do
           ! postprocessing: order theta from least to greatest
           do i = 1, q
              mini = i
              thetamin = theta(i)
              do j = i+1, q
                 if( theta(j) < thetamin ) then
                    mini = j
                    thetamin = theta(j)
                 end if
              end do
              if( mini /= i ) then
                 theta(mini) = theta(i)
                 theta(i) = thetamin
                 if( colmajor ) then
                    if( wantu1 )call stdlib_dswap( p, u1(1_ilp,i), 1_ilp, u1(1_ilp,mini), 1_ilp )
                    if( wantu2 )call stdlib_dswap( m-p, u2(1_ilp,i), 1_ilp, u2(1_ilp,mini), 1_ilp )
                    if( wantv1t )call stdlib_dswap( q, v1t(i,1_ilp), ldv1t, v1t(mini,1_ilp), ldv1t )
                              
                    if( wantv2t )call stdlib_dswap( m-q, v2t(i,1_ilp), ldv2t, v2t(mini,1_ilp),ldv2t )
                              
                 else
                    if( wantu1 )call stdlib_dswap( p, u1(i,1_ilp), ldu1, u1(mini,1_ilp), ldu1 )
                    if( wantu2 )call stdlib_dswap( m-p, u2(i,1_ilp), ldu2, u2(mini,1_ilp), ldu2 )
                    if( wantv1t )call stdlib_dswap( q, v1t(1_ilp,i), 1_ilp, v1t(1_ilp,mini), 1_ilp )
                    if( wantv2t )call stdlib_dswap( m-q, v2t(1_ilp,i), 1_ilp, v2t(1_ilp,mini), 1_ilp )
                 end if
              end if
           end do
           return
     end subroutine stdlib_dbbcsd


     pure module subroutine stdlib_cbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, rwork, &
               lrwork, info )
     !! CBBCSD computes the CS decomposition of a unitary matrix in
     !! bidiagonal-block form,
     !! [ B11 | B12 0  0 ]
     !! [  0  |  0 -I  0 ]
     !! X = [----------------]
     !! [ B21 | B22 0  0 ]
     !! [  0  |  0  0  I ]
     !! [  C | -S  0  0 ]
     !! [ U1 |    ] [  0 |  0 -I  0 ] [ V1 |    ]**H
     !! = [---------] [---------------] [---------]   .
     !! [    | U2 ] [  S |  C  0  0 ] [    | V2 ]
     !! [  0 |  0  0  I ]
     !! X is M-by-M, its top-left block is P-by-Q, and Q must be no larger
     !! than P, M-P, or M-Q. (If Q is not the smallest index, then X must be
     !! transposed and/or permuted. This can be done in constant time using
     !! the TRANS and SIGNS options. See CUNCSD for details.)
     !! The bidiagonal matrices B11, B12, B21, and B22 are represented
     !! implicitly by angles THETA(1:Q) and PHI(1:Q-1).
     !! The unitary matrices U1, U2, V1T, and V2T are input/output.
     !! The input matrices are pre- or post-multiplied by the appropriate
     !! singular vector matrices.
     
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, one, ten, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lrwork, m, p, q
           ! Array Arguments 
           real(sp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), rwork(*)
           real(sp), intent(inout) :: phi(*), theta(*)
           complex(sp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
                     
        ! ===================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 6_ilp
           real(sp), parameter :: hundred = 100.0_sp
           real(sp), parameter :: meighth = -0.125_sp
           real(sp), parameter :: piover2 = 1.57079632679489661923132169163975144210_sp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery, restart11, restart12, restart21, restart22, wantu1, &
                     wantu2, wantv1t, wantv2t
           integer(ilp) :: i, imin, imax, iter, iu1cs, iu1sn, iu2cs, iu2sn, iv1tcs, iv1tsn, &
                     iv2tcs, iv2tsn, j, lrworkmin, lrworkopt, maxit, mini
           real(sp) :: b11bulge, b12bulge, b21bulge, b22bulge, dummy, eps, mu, nu, r, sigma11, &
                     sigma21, temp, thetamax, thetamin, thresh, tol, tolmul, unfl, x1, x2, y1, y2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lrwork == -1_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( m < 0_ilp ) then
              info = -6_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -7_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -8_ilp
           else if( q > p .or. q > m-p .or. q > m-q ) then
              info = -8_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -12_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -14_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -16_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -18_ilp
           end if
           ! quick return if q = 0
           if( info == 0_ilp .and. q == 0_ilp ) then
              lrworkmin = 1_ilp
              rwork(1_ilp) = lrworkmin
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iu1cs = 1_ilp
              iu1sn = iu1cs + q
              iu2cs = iu1sn + q
              iu2sn = iu2cs + q
              iv1tcs = iu2sn + q
              iv1tsn = iv1tcs + q
              iv2tcs = iv1tsn + q
              iv2tsn = iv2tcs + q
              lrworkopt = iv2tsn + q - 1_ilp
              lrworkmin = lrworkopt
              rwork(1_ilp) = lrworkopt
              if( lrwork < lrworkmin .and. .not. lquery ) then
                 info = -28_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CBBCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'EPSILON' )
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           tolmul = max( ten, min( hundred, eps**meighth ) )
           tol = tolmul*eps
           thresh = max( tol, maxitr*q*q*unfl )
           ! test for negligible sines or cosines
           do i = 1, q
              if( theta(i) < thresh ) then
                 theta(i) = zero
              else if( theta(i) > piover2-thresh ) then
                 theta(i) = piover2
              end if
           end do
           do i = 1, q-1
              if( phi(i) < thresh ) then
                 phi(i) = zero
              else if( phi(i) > piover2-thresh ) then
                 phi(i) = piover2
              end if
           end do
           ! initial deflation
           imax = q
           do while( imax > 1 )
              if( phi(imax-1) /= zero ) then
                 exit
              end if
              imax = imax - 1_ilp
           end do
           imin = imax - 1_ilp
           if  ( imin > 1_ilp ) then
              do while( phi(imin-1) /= zero )
                 imin = imin - 1_ilp
                 if  ( imin <= 1 ) exit
              end do
           end if
           ! initialize iteration counter
           maxit = maxitr*q*q
           iter = 0_ilp
           ! begin main iteration loop
           do while( imax > 1 )
              ! compute the matrix entries
              b11d(imin) = cos( theta(imin) )
              b21d(imin) = -sin( theta(imin) )
              do i = imin, imax - 1
                 b11e(i) = -sin( theta(i) ) * sin( phi(i) )
                 b11d(i+1) = cos( theta(i+1) ) * cos( phi(i) )
                 b12d(i) = sin( theta(i) ) * cos( phi(i) )
                 b12e(i) = cos( theta(i+1) ) * sin( phi(i) )
                 b21e(i) = -cos( theta(i) ) * sin( phi(i) )
                 b21d(i+1) = -sin( theta(i+1) ) * cos( phi(i) )
                 b22d(i) = cos( theta(i) ) * cos( phi(i) )
                 b22e(i) = -sin( theta(i+1) ) * sin( phi(i) )
              end do
              b12d(imax) = sin( theta(imax) )
              b22d(imax) = cos( theta(imax) )
              ! abort if not converging; otherwise, increment iter
              if( iter > maxit ) then
                 info = 0_ilp
                 do i = 1, q
                    if( phi(i) /= zero )info = info + 1_ilp
                 end do
                 return
              end if
              iter = iter + imax - imin
              ! compute shifts
              thetamax = theta(imin)
              thetamin = theta(imin)
              do i = imin+1, imax
                 if( theta(i) > thetamax )thetamax = theta(i)
                 if( theta(i) < thetamin )thetamin = theta(i)
              end do
              if( thetamax > piover2 - thresh ) then
                 ! zero on diagonals of b11 and b22; induce deflation with a
                 ! zero shift
                 mu = zero
                 nu = one
              else if( thetamin < thresh ) then
                 ! zero on diagonals of b12 and b22; induce deflation with a
                 ! zero shift
                 mu = one
                 nu = zero
              else
                 ! compute shifts for b11 and b21 and use the lesser
                 call stdlib_slas2( b11d(imax-1), b11e(imax-1), b11d(imax), sigma11,dummy )
                           
                 call stdlib_slas2( b21d(imax-1), b21e(imax-1), b21d(imax), sigma21,dummy )
                           
                 if( sigma11 <= sigma21 ) then
                    mu = sigma11
                    nu = sqrt( one - mu**2_ilp )
                    if( mu < thresh ) then
                       mu = zero
                       nu = one
                    end if
                 else
                    nu = sigma21
                    mu = sqrt( one - nu**2_ilp )
                    if( nu < thresh ) then
                       mu = one
                       nu = zero
                    end if
                 end if
              end if
              ! rotate to produce bulges in b11 and b21
              if( mu <= nu ) then
                 call stdlib_slartgs( b11d(imin), b11e(imin), mu,rwork(iv1tcs+imin-1), rwork(&
                           iv1tsn+imin-1) )
              else
                 call stdlib_slartgs( b21d(imin), b21e(imin), nu,rwork(iv1tcs+imin-1), rwork(&
                           iv1tsn+imin-1) )
              end if
              temp = rwork(iv1tcs+imin-1)*b11d(imin) +rwork(iv1tsn+imin-1)*b11e(imin)
              b11e(imin) = rwork(iv1tcs+imin-1)*b11e(imin) -rwork(iv1tsn+imin-1)*b11d(imin)
                        
              b11d(imin) = temp
              b11bulge = rwork(iv1tsn+imin-1)*b11d(imin+1)
              b11d(imin+1) = rwork(iv1tcs+imin-1)*b11d(imin+1)
              temp = rwork(iv1tcs+imin-1)*b21d(imin) +rwork(iv1tsn+imin-1)*b21e(imin)
              b21e(imin) = rwork(iv1tcs+imin-1)*b21e(imin) -rwork(iv1tsn+imin-1)*b21d(imin)
                        
              b21d(imin) = temp
              b21bulge = rwork(iv1tsn+imin-1)*b21d(imin+1)
              b21d(imin+1) = rwork(iv1tcs+imin-1)*b21d(imin+1)
              ! compute theta(imin)
              theta( imin ) = atan2( sqrt( b21d(imin)**2_ilp+b21bulge**2_ilp ),sqrt( b11d(imin)**2_ilp+&
                        b11bulge**2_ilp ) )
              ! chase the bulges in b11(imin+1,imin) and b21(imin+1,imin)
              if( b11d(imin)**2_ilp+b11bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_slartgp( b11bulge, b11d(imin), rwork(iu1sn+imin-1),rwork(iu1cs+imin-&
                           1_ilp), r )
              else if( mu <= nu ) then
                 call stdlib_slartgs( b11e( imin ), b11d( imin + 1_ilp ), mu,rwork(iu1cs+imin-1), &
                           rwork(iu1sn+imin-1) )
              else
                 call stdlib_slartgs( b12d( imin ), b12e( imin ), nu,rwork(iu1cs+imin-1), rwork(&
                           iu1sn+imin-1) )
              end if
              if( b21d(imin)**2_ilp+b21bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_slartgp( b21bulge, b21d(imin), rwork(iu2sn+imin-1),rwork(iu2cs+imin-&
                           1_ilp), r )
              else if( nu < mu ) then
                 call stdlib_slartgs( b21e( imin ), b21d( imin + 1_ilp ), nu,rwork(iu2cs+imin-1), &
                           rwork(iu2sn+imin-1) )
              else
                 call stdlib_slartgs( b22d(imin), b22e(imin), mu,rwork(iu2cs+imin-1), rwork(iu2sn+&
                           imin-1) )
              end if
              rwork(iu2cs+imin-1) = -rwork(iu2cs+imin-1)
              rwork(iu2sn+imin-1) = -rwork(iu2sn+imin-1)
              temp = rwork(iu1cs+imin-1)*b11e(imin) +rwork(iu1sn+imin-1)*b11d(imin+1)
              b11d(imin+1) = rwork(iu1cs+imin-1)*b11d(imin+1) -rwork(iu1sn+imin-1)*b11e(imin)
                        
              b11e(imin) = temp
              if( imax > imin+1 ) then
                 b11bulge = rwork(iu1sn+imin-1)*b11e(imin+1)
                 b11e(imin+1) = rwork(iu1cs+imin-1)*b11e(imin+1)
              end if
              temp = rwork(iu1cs+imin-1)*b12d(imin) +rwork(iu1sn+imin-1)*b12e(imin)
              b12e(imin) = rwork(iu1cs+imin-1)*b12e(imin) -rwork(iu1sn+imin-1)*b12d(imin)
              b12d(imin) = temp
              b12bulge = rwork(iu1sn+imin-1)*b12d(imin+1)
              b12d(imin+1) = rwork(iu1cs+imin-1)*b12d(imin+1)
              temp = rwork(iu2cs+imin-1)*b21e(imin) +rwork(iu2sn+imin-1)*b21d(imin+1)
              b21d(imin+1) = rwork(iu2cs+imin-1)*b21d(imin+1) -rwork(iu2sn+imin-1)*b21e(imin)
                        
              b21e(imin) = temp
              if( imax > imin+1 ) then
                 b21bulge = rwork(iu2sn+imin-1)*b21e(imin+1)
                 b21e(imin+1) = rwork(iu2cs+imin-1)*b21e(imin+1)
              end if
              temp = rwork(iu2cs+imin-1)*b22d(imin) +rwork(iu2sn+imin-1)*b22e(imin)
              b22e(imin) = rwork(iu2cs+imin-1)*b22e(imin) -rwork(iu2sn+imin-1)*b22d(imin)
              b22d(imin) = temp
              b22bulge = rwork(iu2sn+imin-1)*b22d(imin+1)
              b22d(imin+1) = rwork(iu2cs+imin-1)*b22d(imin+1)
              ! inner loop: chase bulges from b11(imin,imin+2),
              ! b12(imin,imin+1), b21(imin,imin+2), and b22(imin,imin+1) to
              ! bottom-right
              do i = imin+1, imax-1
                 ! compute phi(i-1)
                 x1 = sin(theta(i-1))*b11e(i-1) + cos(theta(i-1))*b21e(i-1)
                 x2 = sin(theta(i-1))*b11bulge + cos(theta(i-1))*b21bulge
                 y1 = sin(theta(i-1))*b12d(i-1) + cos(theta(i-1))*b22d(i-1)
                 y2 = sin(theta(i-1))*b12bulge + cos(theta(i-1))*b22bulge
                 phi(i-1) = atan2( sqrt(x1**2_ilp+x2**2_ilp), sqrt(y1**2_ilp+y2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 = b11e(i-1)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart21 = b21e(i-1)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12d(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22d(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i-1,i+1), b12(i-1,i),
                 ! b21(i-1,i+1), and b22(i-1,i). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart21 ) then
                    call stdlib_slartgp( x2, x1, rwork(iv1tsn+i-1),rwork(iv1tcs+i-1), r )
                 else if( .not. restart11 .and. restart21 ) then
                    call stdlib_slartgp( b11bulge, b11e(i-1), rwork(iv1tsn+i-1),rwork(iv1tcs+i-1),&
                               r )
                 else if( restart11 .and. .not. restart21 ) then
                    call stdlib_slartgp( b21bulge, b21e(i-1), rwork(iv1tsn+i-1),rwork(iv1tcs+i-1),&
                               r )
                 else if( mu <= nu ) then
                    call stdlib_slartgs( b11d(i), b11e(i), mu, rwork(iv1tcs+i-1),rwork(iv1tsn+i-1)&
                               )
                 else
                    call stdlib_slartgs( b21d(i), b21e(i), nu, rwork(iv1tcs+i-1),rwork(iv1tsn+i-1)&
                               )
                 end if
                 rwork(iv1tcs+i-1) = -rwork(iv1tcs+i-1)
                 rwork(iv1tsn+i-1) = -rwork(iv1tsn+i-1)
                 if( .not. restart12 .and. .not. restart22 ) then
                    call stdlib_slartgp( y2, y1, rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-1-1), r )
                              
                 else if( .not. restart12 .and. restart22 ) then
                    call stdlib_slartgp( b12bulge, b12d(i-1), rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-&
                              1_ilp-1), r )
                 else if( restart12 .and. .not. restart22 ) then
                    call stdlib_slartgp( b22bulge, b22d(i-1), rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-&
                              1_ilp-1), r )
                 else if( nu < mu ) then
                    call stdlib_slartgs( b12e(i-1), b12d(i), nu,rwork(iv2tcs+i-1-1), rwork(iv2tsn+&
                              i-1-1) )
                 else
                    call stdlib_slartgs( b22e(i-1), b22d(i), mu,rwork(iv2tcs+i-1-1), rwork(iv2tsn+&
                              i-1-1) )
                 end if
                 temp = rwork(iv1tcs+i-1)*b11d(i) + rwork(iv1tsn+i-1)*b11e(i)
                 b11e(i) = rwork(iv1tcs+i-1)*b11e(i) -rwork(iv1tsn+i-1)*b11d(i)
                 b11d(i) = temp
                 b11bulge = rwork(iv1tsn+i-1)*b11d(i+1)
                 b11d(i+1) = rwork(iv1tcs+i-1)*b11d(i+1)
                 temp = rwork(iv1tcs+i-1)*b21d(i) + rwork(iv1tsn+i-1)*b21e(i)
                 b21e(i) = rwork(iv1tcs+i-1)*b21e(i) -rwork(iv1tsn+i-1)*b21d(i)
                 b21d(i) = temp
                 b21bulge = rwork(iv1tsn+i-1)*b21d(i+1)
                 b21d(i+1) = rwork(iv1tcs+i-1)*b21d(i+1)
                 temp = rwork(iv2tcs+i-1-1)*b12e(i-1) +rwork(iv2tsn+i-1-1)*b12d(i)
                 b12d(i) = rwork(iv2tcs+i-1-1)*b12d(i) -rwork(iv2tsn+i-1-1)*b12e(i-1)
                 b12e(i-1) = temp
                 b12bulge = rwork(iv2tsn+i-1-1)*b12e(i)
                 b12e(i) = rwork(iv2tcs+i-1-1)*b12e(i)
                 temp = rwork(iv2tcs+i-1-1)*b22e(i-1) +rwork(iv2tsn+i-1-1)*b22d(i)
                 b22d(i) = rwork(iv2tcs+i-1-1)*b22d(i) -rwork(iv2tsn+i-1-1)*b22e(i-1)
                 b22e(i-1) = temp
                 b22bulge = rwork(iv2tsn+i-1-1)*b22e(i)
                 b22e(i) = rwork(iv2tcs+i-1-1)*b22e(i)
                 ! compute theta(i)
                 x1 = cos(phi(i-1))*b11d(i) + sin(phi(i-1))*b12e(i-1)
                 x2 = cos(phi(i-1))*b11bulge + sin(phi(i-1))*b12bulge
                 y1 = cos(phi(i-1))*b21d(i) + sin(phi(i-1))*b22e(i-1)
                 y2 = cos(phi(i-1))*b21bulge + sin(phi(i-1))*b22bulge
                 theta(i) = atan2( sqrt(y1**2_ilp+y2**2_ilp), sqrt(x1**2_ilp+x2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 =   b11d(i)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12e(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart21 =   b21d(i)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22e(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i+1,i), b12(i+1,i-1),
                 ! b21(i+1,i), and b22(i+1,i-1). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart12 ) then
                    call stdlib_slartgp( x2, x1, rwork(iu1sn+i-1), rwork(iu1cs+i-1),r )
                 else if( .not. restart11 .and. restart12 ) then
                    call stdlib_slartgp( b11bulge, b11d(i), rwork(iu1sn+i-1),rwork(iu1cs+i-1), r )
                              
                 else if( restart11 .and. .not. restart12 ) then
                    call stdlib_slartgp( b12bulge, b12e(i-1), rwork(iu1sn+i-1),rwork(iu1cs+i-1), &
                              r )
                 else if( mu <= nu ) then
                    call stdlib_slartgs( b11e(i), b11d(i+1), mu, rwork(iu1cs+i-1),rwork(iu1sn+i-1)&
                               )
                 else
                    call stdlib_slartgs( b12d(i), b12e(i), nu, rwork(iu1cs+i-1),rwork(iu1sn+i-1) )
                              
                 end if
                 if( .not. restart21 .and. .not. restart22 ) then
                    call stdlib_slartgp( y2, y1, rwork(iu2sn+i-1), rwork(iu2cs+i-1),r )
                 else if( .not. restart21 .and. restart22 ) then
                    call stdlib_slartgp( b21bulge, b21d(i), rwork(iu2sn+i-1),rwork(iu2cs+i-1), r )
                              
                 else if( restart21 .and. .not. restart22 ) then
                    call stdlib_slartgp( b22bulge, b22e(i-1), rwork(iu2sn+i-1),rwork(iu2cs+i-1), &
                              r )
                 else if( nu < mu ) then
                    call stdlib_slartgs( b21e(i), b21e(i+1), nu, rwork(iu2cs+i-1),rwork(iu2sn+i-1)&
                               )
                 else
                    call stdlib_slartgs( b22d(i), b22e(i), mu, rwork(iu2cs+i-1),rwork(iu2sn+i-1) )
                              
                 end if
                 rwork(iu2cs+i-1) = -rwork(iu2cs+i-1)
                 rwork(iu2sn+i-1) = -rwork(iu2sn+i-1)
                 temp = rwork(iu1cs+i-1)*b11e(i) + rwork(iu1sn+i-1)*b11d(i+1)
                 b11d(i+1) = rwork(iu1cs+i-1)*b11d(i+1) -rwork(iu1sn+i-1)*b11e(i)
                 b11e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b11bulge = rwork(iu1sn+i-1)*b11e(i+1)
                    b11e(i+1) = rwork(iu1cs+i-1)*b11e(i+1)
                 end if
                 temp = rwork(iu2cs+i-1)*b21e(i) + rwork(iu2sn+i-1)*b21d(i+1)
                 b21d(i+1) = rwork(iu2cs+i-1)*b21d(i+1) -rwork(iu2sn+i-1)*b21e(i)
                 b21e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b21bulge = rwork(iu2sn+i-1)*b21e(i+1)
                    b21e(i+1) = rwork(iu2cs+i-1)*b21e(i+1)
                 end if
                 temp = rwork(iu1cs+i-1)*b12d(i) + rwork(iu1sn+i-1)*b12e(i)
                 b12e(i) = rwork(iu1cs+i-1)*b12e(i) -rwork(iu1sn+i-1)*b12d(i)
                 b12d(i) = temp
                 b12bulge = rwork(iu1sn+i-1)*b12d(i+1)
                 b12d(i+1) = rwork(iu1cs+i-1)*b12d(i+1)
                 temp = rwork(iu2cs+i-1)*b22d(i) + rwork(iu2sn+i-1)*b22e(i)
                 b22e(i) = rwork(iu2cs+i-1)*b22e(i) -rwork(iu2sn+i-1)*b22d(i)
                 b22d(i) = temp
                 b22bulge = rwork(iu2sn+i-1)*b22d(i+1)
                 b22d(i+1) = rwork(iu2cs+i-1)*b22d(i+1)
              end do
              ! compute phi(imax-1)
              x1 = sin(theta(imax-1))*b11e(imax-1) +cos(theta(imax-1))*b21e(imax-1)
              y1 = sin(theta(imax-1))*b12d(imax-1) +cos(theta(imax-1))*b22d(imax-1)
              y2 = sin(theta(imax-1))*b12bulge + cos(theta(imax-1))*b22bulge
              phi(imax-1) = atan2( abs(x1), sqrt(y1**2_ilp+y2**2_ilp) )
              ! chase bulges from b12(imax-1,imax) and b22(imax-1,imax)
              restart12 = b12d(imax-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
              restart22 = b22d(imax-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
              if( .not. restart12 .and. .not. restart22 ) then
                 call stdlib_slartgp( y2, y1, rwork(iv2tsn+imax-1-1),rwork(iv2tcs+imax-1-1), r )
                           
              else if( .not. restart12 .and. restart22 ) then
                 call stdlib_slartgp( b12bulge, b12d(imax-1),rwork(iv2tsn+imax-1-1),rwork(iv2tcs+&
                           imax-1-1), r )
              else if( restart12 .and. .not. restart22 ) then
                 call stdlib_slartgp( b22bulge, b22d(imax-1),rwork(iv2tsn+imax-1-1),rwork(iv2tcs+&
                           imax-1-1), r )
              else if( nu < mu ) then
                 call stdlib_slartgs( b12e(imax-1), b12d(imax), nu,rwork(iv2tcs+imax-1-1),rwork(&
                           iv2tsn+imax-1-1) )
              else
                 call stdlib_slartgs( b22e(imax-1), b22d(imax), mu,rwork(iv2tcs+imax-1-1),rwork(&
                           iv2tsn+imax-1-1) )
              end if
              temp = rwork(iv2tcs+imax-1-1)*b12e(imax-1) +rwork(iv2tsn+imax-1-1)*b12d(imax)
                        
              b12d(imax) = rwork(iv2tcs+imax-1-1)*b12d(imax) -rwork(iv2tsn+imax-1-1)*b12e(imax-1)
                        
              b12e(imax-1) = temp
              temp = rwork(iv2tcs+imax-1-1)*b22e(imax-1) +rwork(iv2tsn+imax-1-1)*b22d(imax)
                        
              b22d(imax) = rwork(iv2tcs+imax-1-1)*b22d(imax) -rwork(iv2tsn+imax-1-1)*b22e(imax-1)
                        
              b22e(imax-1) = temp
              ! update singular vectors
              if( wantu1 ) then
                 if( colmajor ) then
                    call stdlib_clasr( 'R', 'V', 'F', p, imax-imin+1,rwork(iu1cs+imin-1), rwork(&
                              iu1sn+imin-1),u1(1_ilp,imin), ldu1 )
                 else
                    call stdlib_clasr( 'L', 'V', 'F', imax-imin+1, p,rwork(iu1cs+imin-1), rwork(&
                              iu1sn+imin-1),u1(imin,1_ilp), ldu1 )
                 end if
              end if
              if( wantu2 ) then
                 if( colmajor ) then
                    call stdlib_clasr( 'R', 'V', 'F', m-p, imax-imin+1,rwork(iu2cs+imin-1), rwork(&
                              iu2sn+imin-1),u2(1_ilp,imin), ldu2 )
                 else
                    call stdlib_clasr( 'L', 'V', 'F', imax-imin+1, m-p,rwork(iu2cs+imin-1), rwork(&
                              iu2sn+imin-1),u2(imin,1_ilp), ldu2 )
                 end if
              end if
              if( wantv1t ) then
                 if( colmajor ) then
                    call stdlib_clasr( 'L', 'V', 'F', imax-imin+1, q,rwork(iv1tcs+imin-1), rwork(&
                              iv1tsn+imin-1),v1t(imin,1_ilp), ldv1t )
                 else
                    call stdlib_clasr( 'R', 'V', 'F', q, imax-imin+1,rwork(iv1tcs+imin-1), rwork(&
                              iv1tsn+imin-1),v1t(1_ilp,imin), ldv1t )
                 end if
              end if
              if( wantv2t ) then
                 if( colmajor ) then
                    call stdlib_clasr( 'L', 'V', 'F', imax-imin+1, m-q,rwork(iv2tcs+imin-1), &
                              rwork(iv2tsn+imin-1),v2t(imin,1_ilp), ldv2t )
                 else
                    call stdlib_clasr( 'R', 'V', 'F', m-q, imax-imin+1,rwork(iv2tcs+imin-1), &
                              rwork(iv2tsn+imin-1),v2t(1_ilp,imin), ldv2t )
                 end if
              end if
              ! fix signs on b11(imax-1,imax) and b21(imax-1,imax)
              if( b11e(imax-1)+b21e(imax-1) > 0_ilp ) then
                 b11d(imax) = -b11d(imax)
                 b21d(imax) = -b21d(imax)
                 if( wantv1t ) then
                    if( colmajor ) then
                       call stdlib_cscal( q, cnegone, v1t(imax,1_ilp), ldv1t )
                    else
                       call stdlib_cscal( q, cnegone, v1t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! compute theta(imax)
              x1 = cos(phi(imax-1))*b11d(imax) +sin(phi(imax-1))*b12e(imax-1)
              y1 = cos(phi(imax-1))*b21d(imax) +sin(phi(imax-1))*b22e(imax-1)
              theta(imax) = atan2( abs(y1), abs(x1) )
              ! fix signs on b11(imax,imax), b12(imax,imax-1), b21(imax,imax),
              ! and b22(imax,imax-1)
              if( b11d(imax)+b12e(imax-1) < 0_ilp ) then
                 b12d(imax) = -b12d(imax)
                 if( wantu1 ) then
                    if( colmajor ) then
                       call stdlib_cscal( p, cnegone, u1(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_cscal( p, cnegone, u1(imax,1_ilp), ldu1 )
                    end if
                 end if
              end if
              if( b21d(imax)+b22e(imax-1) > 0_ilp ) then
                 b22d(imax) = -b22d(imax)
                 if( wantu2 ) then
                    if( colmajor ) then
                       call stdlib_cscal( m-p, cnegone, u2(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_cscal( m-p, cnegone, u2(imax,1_ilp), ldu2 )
                    end if
                 end if
              end if
              ! fix signs on b12(imax,imax) and b22(imax,imax)
              if( b12d(imax)+b22d(imax) < 0_ilp ) then
                 if( wantv2t ) then
                    if( colmajor ) then
                       call stdlib_cscal( m-q, cnegone, v2t(imax,1_ilp), ldv2t )
                    else
                       call stdlib_cscal( m-q, cnegone, v2t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! test for negligible sines or cosines
              do i = imin, imax
                 if( theta(i) < thresh ) then
                    theta(i) = zero
                 else if( theta(i) > piover2-thresh ) then
                    theta(i) = piover2
                 end if
              end do
              do i = imin, imax-1
                 if( phi(i) < thresh ) then
                    phi(i) = zero
                 else if( phi(i) > piover2-thresh ) then
                    phi(i) = piover2
                 end if
              end do
              ! deflate
              if (imax > 1_ilp) then
                 do while( phi(imax-1) == zero )
                    imax = imax - 1_ilp
                    if (imax <= 1) exit
                 end do
              end if
              if( imin > imax - 1_ilp )imin = imax - 1_ilp
              if (imin > 1_ilp) then
                 do while (phi(imin-1) /= zero)
                     imin = imin - 1_ilp
                     if (imin <= 1) exit
                 end do
              end if
              ! repeat main iteration loop
           end do
           ! postprocessing: order theta from least to greatest
           do i = 1, q
              mini = i
              thetamin = theta(i)
              do j = i+1, q
                 if( theta(j) < thetamin ) then
                    mini = j
                    thetamin = theta(j)
                 end if
              end do
              if( mini /= i ) then
                 theta(mini) = theta(i)
                 theta(i) = thetamin
                 if( colmajor ) then
                    if( wantu1 )call stdlib_cswap( p, u1(1_ilp,i), 1_ilp, u1(1_ilp,mini), 1_ilp )
                    if( wantu2 )call stdlib_cswap( m-p, u2(1_ilp,i), 1_ilp, u2(1_ilp,mini), 1_ilp )
                    if( wantv1t )call stdlib_cswap( q, v1t(i,1_ilp), ldv1t, v1t(mini,1_ilp), ldv1t )
                              
                    if( wantv2t )call stdlib_cswap( m-q, v2t(i,1_ilp), ldv2t, v2t(mini,1_ilp),ldv2t )
                              
                 else
                    if( wantu1 )call stdlib_cswap( p, u1(i,1_ilp), ldu1, u1(mini,1_ilp), ldu1 )
                    if( wantu2 )call stdlib_cswap( m-p, u2(i,1_ilp), ldu2, u2(mini,1_ilp), ldu2 )
                    if( wantv1t )call stdlib_cswap( q, v1t(1_ilp,i), 1_ilp, v1t(1_ilp,mini), 1_ilp )
                    if( wantv2t )call stdlib_cswap( m-q, v2t(1_ilp,i), 1_ilp, v2t(1_ilp,mini), 1_ilp )
                 end if
              end if
           end do
           return
     end subroutine stdlib_cbbcsd

     pure module subroutine stdlib_zbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     !! ZBBCSD computes the CS decomposition of a unitary matrix in
     !! bidiagonal-block form,
     !! [ B11 | B12 0  0 ]
     !! [  0  |  0 -I  0 ]
     !! X = [----------------]
     !! [ B21 | B22 0  0 ]
     !! [  0  |  0  0  I ]
     !! [  C | -S  0  0 ]
     !! [ U1 |    ] [  0 |  0 -I  0 ] [ V1 |    ]**H
     !! = [---------] [---------------] [---------]   .
     !! [    | U2 ] [  S |  C  0  0 ] [    | V2 ]
     !! [  0 |  0  0  I ]
     !! X is M-by-M, its top-left block is P-by-Q, and Q must be no larger
     !! than P, M-P, or M-Q. (If Q is not the smallest index, then X must be
     !! transposed and/or permuted. This can be done in constant time using
     !! the TRANS and SIGNS options. See ZUNCSD for details.)
     !! The bidiagonal matrices B11, B12, B21, and B22 are represented
     !! implicitly by angles THETA(1:Q) and PHI(1:Q-1).
     !! The unitary matrices U1, U2, V1T, and V2T are input/output.
     !! The input matrices are pre- or post-multiplied by the appropriate
     !! singular vector matrices.
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, rwork, &
               lrwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, one, ten, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lrwork, m, p, q
           ! Array Arguments 
           real(dp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), rwork(*)
           real(dp), intent(inout) :: phi(*), theta(*)
           complex(dp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
                     
        ! ===================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 6_ilp
           real(dp), parameter :: hundred = 100.0_dp
           real(dp), parameter :: meighth = -0.125_dp
           real(dp), parameter :: piover2 = 1.57079632679489661923132169163975144210_dp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery, restart11, restart12, restart21, restart22, wantu1, &
                     wantu2, wantv1t, wantv2t
           integer(ilp) :: i, imin, imax, iter, iu1cs, iu1sn, iu2cs, iu2sn, iv1tcs, iv1tsn, &
                     iv2tcs, iv2tsn, j, lrworkmin, lrworkopt, maxit, mini
           real(dp) :: b11bulge, b12bulge, b21bulge, b22bulge, dummy, eps, mu, nu, r, sigma11, &
                     sigma21, temp, thetamax, thetamin, thresh, tol, tolmul, unfl, x1, x2, y1, y2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lrwork == -1_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( m < 0_ilp ) then
              info = -6_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -7_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -8_ilp
           else if( q > p .or. q > m-p .or. q > m-q ) then
              info = -8_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -12_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -14_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -16_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -18_ilp
           end if
           ! quick return if q = 0
           if( info == 0_ilp .and. q == 0_ilp ) then
              lrworkmin = 1_ilp
              rwork(1_ilp) = lrworkmin
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iu1cs = 1_ilp
              iu1sn = iu1cs + q
              iu2cs = iu1sn + q
              iu2sn = iu2cs + q
              iv1tcs = iu2sn + q
              iv1tsn = iv1tcs + q
              iv2tcs = iv1tsn + q
              iv2tsn = iv2tcs + q
              lrworkopt = iv2tsn + q - 1_ilp
              lrworkmin = lrworkopt
              rwork(1_ilp) = lrworkopt
              if( lrwork < lrworkmin .and. .not. lquery ) then
                 info = -28_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZBBCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'EPSILON' )
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           tolmul = max( ten, min( hundred, eps**meighth ) )
           tol = tolmul*eps
           thresh = max( tol, maxitr*q*q*unfl )
           ! test for negligible sines or cosines
           do i = 1, q
              if( theta(i) < thresh ) then
                 theta(i) = zero
              else if( theta(i) > piover2-thresh ) then
                 theta(i) = piover2
              end if
           end do
           do i = 1, q-1
              if( phi(i) < thresh ) then
                 phi(i) = zero
              else if( phi(i) > piover2-thresh ) then
                 phi(i) = piover2
              end if
           end do
           ! initial deflation
           imax = q
           do while( imax > 1 )
              if( phi(imax-1) /= zero ) then
                 exit
              end if
              imax = imax - 1_ilp
           end do
           imin = imax - 1_ilp
           if  ( imin > 1_ilp ) then
              do while( phi(imin-1) /= zero )
                 imin = imin - 1_ilp
                 if  ( imin <= 1 ) exit
              end do
           end if
           ! initialize iteration counter
           maxit = maxitr*q*q
           iter = 0_ilp
           ! begin main iteration loop
           do while( imax > 1 )
              ! compute the matrix entries
              b11d(imin) = cos( theta(imin) )
              b21d(imin) = -sin( theta(imin) )
              do i = imin, imax - 1
                 b11e(i) = -sin( theta(i) ) * sin( phi(i) )
                 b11d(i+1) = cos( theta(i+1) ) * cos( phi(i) )
                 b12d(i) = sin( theta(i) ) * cos( phi(i) )
                 b12e(i) = cos( theta(i+1) ) * sin( phi(i) )
                 b21e(i) = -cos( theta(i) ) * sin( phi(i) )
                 b21d(i+1) = -sin( theta(i+1) ) * cos( phi(i) )
                 b22d(i) = cos( theta(i) ) * cos( phi(i) )
                 b22e(i) = -sin( theta(i+1) ) * sin( phi(i) )
              end do
              b12d(imax) = sin( theta(imax) )
              b22d(imax) = cos( theta(imax) )
              ! abort if not converging; otherwise, increment iter
              if( iter > maxit ) then
                 info = 0_ilp
                 do i = 1, q
                    if( phi(i) /= zero )info = info + 1_ilp
                 end do
                 return
              end if
              iter = iter + imax - imin
              ! compute shifts
              thetamax = theta(imin)
              thetamin = theta(imin)
              do i = imin+1, imax
                 if( theta(i) > thetamax )thetamax = theta(i)
                 if( theta(i) < thetamin )thetamin = theta(i)
              end do
              if( thetamax > piover2 - thresh ) then
                 ! zero on diagonals of b11 and b22; induce deflation with a
                 ! zero shift
                 mu = zero
                 nu = one
              else if( thetamin < thresh ) then
                 ! zero on diagonals of b12 and b22; induce deflation with a
                 ! zero shift
                 mu = one
                 nu = zero
              else
                 ! compute shifts for b11 and b21 and use the lesser
                 call stdlib_dlas2( b11d(imax-1), b11e(imax-1), b11d(imax), sigma11,dummy )
                           
                 call stdlib_dlas2( b21d(imax-1), b21e(imax-1), b21d(imax), sigma21,dummy )
                           
                 if( sigma11 <= sigma21 ) then
                    mu = sigma11
                    nu = sqrt( one - mu**2_ilp )
                    if( mu < thresh ) then
                       mu = zero
                       nu = one
                    end if
                 else
                    nu = sigma21
                    mu = sqrt( one - nu**2_ilp )
                    if( nu < thresh ) then
                       mu = one
                       nu = zero
                    end if
                 end if
              end if
              ! rotate to produce bulges in b11 and b21
              if( mu <= nu ) then
                 call stdlib_dlartgs( b11d(imin), b11e(imin), mu,rwork(iv1tcs+imin-1), rwork(&
                           iv1tsn+imin-1) )
              else
                 call stdlib_dlartgs( b21d(imin), b21e(imin), nu,rwork(iv1tcs+imin-1), rwork(&
                           iv1tsn+imin-1) )
              end if
              temp = rwork(iv1tcs+imin-1)*b11d(imin) +rwork(iv1tsn+imin-1)*b11e(imin)
              b11e(imin) = rwork(iv1tcs+imin-1)*b11e(imin) -rwork(iv1tsn+imin-1)*b11d(imin)
                        
              b11d(imin) = temp
              b11bulge = rwork(iv1tsn+imin-1)*b11d(imin+1)
              b11d(imin+1) = rwork(iv1tcs+imin-1)*b11d(imin+1)
              temp = rwork(iv1tcs+imin-1)*b21d(imin) +rwork(iv1tsn+imin-1)*b21e(imin)
              b21e(imin) = rwork(iv1tcs+imin-1)*b21e(imin) -rwork(iv1tsn+imin-1)*b21d(imin)
                        
              b21d(imin) = temp
              b21bulge = rwork(iv1tsn+imin-1)*b21d(imin+1)
              b21d(imin+1) = rwork(iv1tcs+imin-1)*b21d(imin+1)
              ! compute theta(imin)
              theta( imin ) = atan2( sqrt( b21d(imin)**2_ilp+b21bulge**2_ilp ),sqrt( b11d(imin)**2_ilp+&
                        b11bulge**2_ilp ) )
              ! chase the bulges in b11(imin+1,imin) and b21(imin+1,imin)
              if( b11d(imin)**2_ilp+b11bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_dlartgp( b11bulge, b11d(imin), rwork(iu1sn+imin-1),rwork(iu1cs+imin-&
                           1_ilp), r )
              else if( mu <= nu ) then
                 call stdlib_dlartgs( b11e( imin ), b11d( imin + 1_ilp ), mu,rwork(iu1cs+imin-1), &
                           rwork(iu1sn+imin-1) )
              else
                 call stdlib_dlartgs( b12d( imin ), b12e( imin ), nu,rwork(iu1cs+imin-1), rwork(&
                           iu1sn+imin-1) )
              end if
              if( b21d(imin)**2_ilp+b21bulge**2_ilp > thresh**2_ilp ) then
                 call stdlib_dlartgp( b21bulge, b21d(imin), rwork(iu2sn+imin-1),rwork(iu2cs+imin-&
                           1_ilp), r )
              else if( nu < mu ) then
                 call stdlib_dlartgs( b21e( imin ), b21d( imin + 1_ilp ), nu,rwork(iu2cs+imin-1), &
                           rwork(iu2sn+imin-1) )
              else
                 call stdlib_dlartgs( b22d(imin), b22e(imin), mu,rwork(iu2cs+imin-1), rwork(iu2sn+&
                           imin-1) )
              end if
              rwork(iu2cs+imin-1) = -rwork(iu2cs+imin-1)
              rwork(iu2sn+imin-1) = -rwork(iu2sn+imin-1)
              temp = rwork(iu1cs+imin-1)*b11e(imin) +rwork(iu1sn+imin-1)*b11d(imin+1)
              b11d(imin+1) = rwork(iu1cs+imin-1)*b11d(imin+1) -rwork(iu1sn+imin-1)*b11e(imin)
                        
              b11e(imin) = temp
              if( imax > imin+1 ) then
                 b11bulge = rwork(iu1sn+imin-1)*b11e(imin+1)
                 b11e(imin+1) = rwork(iu1cs+imin-1)*b11e(imin+1)
              end if
              temp = rwork(iu1cs+imin-1)*b12d(imin) +rwork(iu1sn+imin-1)*b12e(imin)
              b12e(imin) = rwork(iu1cs+imin-1)*b12e(imin) -rwork(iu1sn+imin-1)*b12d(imin)
              b12d(imin) = temp
              b12bulge = rwork(iu1sn+imin-1)*b12d(imin+1)
              b12d(imin+1) = rwork(iu1cs+imin-1)*b12d(imin+1)
              temp = rwork(iu2cs+imin-1)*b21e(imin) +rwork(iu2sn+imin-1)*b21d(imin+1)
              b21d(imin+1) = rwork(iu2cs+imin-1)*b21d(imin+1) -rwork(iu2sn+imin-1)*b21e(imin)
                        
              b21e(imin) = temp
              if( imax > imin+1 ) then
                 b21bulge = rwork(iu2sn+imin-1)*b21e(imin+1)
                 b21e(imin+1) = rwork(iu2cs+imin-1)*b21e(imin+1)
              end if
              temp = rwork(iu2cs+imin-1)*b22d(imin) +rwork(iu2sn+imin-1)*b22e(imin)
              b22e(imin) = rwork(iu2cs+imin-1)*b22e(imin) -rwork(iu2sn+imin-1)*b22d(imin)
              b22d(imin) = temp
              b22bulge = rwork(iu2sn+imin-1)*b22d(imin+1)
              b22d(imin+1) = rwork(iu2cs+imin-1)*b22d(imin+1)
              ! inner loop: chase bulges from b11(imin,imin+2),
              ! b12(imin,imin+1), b21(imin,imin+2), and b22(imin,imin+1) to
              ! bottom-right
              do i = imin+1, imax-1
                 ! compute phi(i-1)
                 x1 = sin(theta(i-1))*b11e(i-1) + cos(theta(i-1))*b21e(i-1)
                 x2 = sin(theta(i-1))*b11bulge + cos(theta(i-1))*b21bulge
                 y1 = sin(theta(i-1))*b12d(i-1) + cos(theta(i-1))*b22d(i-1)
                 y2 = sin(theta(i-1))*b12bulge + cos(theta(i-1))*b22bulge
                 phi(i-1) = atan2( sqrt(x1**2_ilp+x2**2_ilp), sqrt(y1**2_ilp+y2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 = b11e(i-1)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart21 = b21e(i-1)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12d(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22d(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i-1,i+1), b12(i-1,i),
                 ! b21(i-1,i+1), and b22(i-1,i). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart21 ) then
                    call stdlib_dlartgp( x2, x1, rwork(iv1tsn+i-1),rwork(iv1tcs+i-1), r )
                 else if( .not. restart11 .and. restart21 ) then
                    call stdlib_dlartgp( b11bulge, b11e(i-1), rwork(iv1tsn+i-1),rwork(iv1tcs+i-1),&
                               r )
                 else if( restart11 .and. .not. restart21 ) then
                    call stdlib_dlartgp( b21bulge, b21e(i-1), rwork(iv1tsn+i-1),rwork(iv1tcs+i-1),&
                               r )
                 else if( mu <= nu ) then
                    call stdlib_dlartgs( b11d(i), b11e(i), mu, rwork(iv1tcs+i-1),rwork(iv1tsn+i-1)&
                               )
                 else
                    call stdlib_dlartgs( b21d(i), b21e(i), nu, rwork(iv1tcs+i-1),rwork(iv1tsn+i-1)&
                               )
                 end if
                 rwork(iv1tcs+i-1) = -rwork(iv1tcs+i-1)
                 rwork(iv1tsn+i-1) = -rwork(iv1tsn+i-1)
                 if( .not. restart12 .and. .not. restart22 ) then
                    call stdlib_dlartgp( y2, y1, rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-1-1), r )
                              
                 else if( .not. restart12 .and. restart22 ) then
                    call stdlib_dlartgp( b12bulge, b12d(i-1), rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-&
                              1_ilp-1), r )
                 else if( restart12 .and. .not. restart22 ) then
                    call stdlib_dlartgp( b22bulge, b22d(i-1), rwork(iv2tsn+i-1-1),rwork(iv2tcs+i-&
                              1_ilp-1), r )
                 else if( nu < mu ) then
                    call stdlib_dlartgs( b12e(i-1), b12d(i), nu,rwork(iv2tcs+i-1-1), rwork(iv2tsn+&
                              i-1-1) )
                 else
                    call stdlib_dlartgs( b22e(i-1), b22d(i), mu,rwork(iv2tcs+i-1-1), rwork(iv2tsn+&
                              i-1-1) )
                 end if
                 temp = rwork(iv1tcs+i-1)*b11d(i) + rwork(iv1tsn+i-1)*b11e(i)
                 b11e(i) = rwork(iv1tcs+i-1)*b11e(i) -rwork(iv1tsn+i-1)*b11d(i)
                 b11d(i) = temp
                 b11bulge = rwork(iv1tsn+i-1)*b11d(i+1)
                 b11d(i+1) = rwork(iv1tcs+i-1)*b11d(i+1)
                 temp = rwork(iv1tcs+i-1)*b21d(i) + rwork(iv1tsn+i-1)*b21e(i)
                 b21e(i) = rwork(iv1tcs+i-1)*b21e(i) -rwork(iv1tsn+i-1)*b21d(i)
                 b21d(i) = temp
                 b21bulge = rwork(iv1tsn+i-1)*b21d(i+1)
                 b21d(i+1) = rwork(iv1tcs+i-1)*b21d(i+1)
                 temp = rwork(iv2tcs+i-1-1)*b12e(i-1) +rwork(iv2tsn+i-1-1)*b12d(i)
                 b12d(i) = rwork(iv2tcs+i-1-1)*b12d(i) -rwork(iv2tsn+i-1-1)*b12e(i-1)
                 b12e(i-1) = temp
                 b12bulge = rwork(iv2tsn+i-1-1)*b12e(i)
                 b12e(i) = rwork(iv2tcs+i-1-1)*b12e(i)
                 temp = rwork(iv2tcs+i-1-1)*b22e(i-1) +rwork(iv2tsn+i-1-1)*b22d(i)
                 b22d(i) = rwork(iv2tcs+i-1-1)*b22d(i) -rwork(iv2tsn+i-1-1)*b22e(i-1)
                 b22e(i-1) = temp
                 b22bulge = rwork(iv2tsn+i-1-1)*b22e(i)
                 b22e(i) = rwork(iv2tcs+i-1-1)*b22e(i)
                 ! compute theta(i)
                 x1 = cos(phi(i-1))*b11d(i) + sin(phi(i-1))*b12e(i-1)
                 x2 = cos(phi(i-1))*b11bulge + sin(phi(i-1))*b12bulge
                 y1 = cos(phi(i-1))*b21d(i) + sin(phi(i-1))*b22e(i-1)
                 y2 = cos(phi(i-1))*b21bulge + sin(phi(i-1))*b22bulge
                 theta(i) = atan2( sqrt(y1**2_ilp+y2**2_ilp), sqrt(x1**2_ilp+x2**2_ilp) )
                 ! determine if there are bulges to chase or if a new direct
                 ! summand has been reached
                 restart11 =   b11d(i)**2_ilp + b11bulge**2_ilp <= thresh**2_ilp
                 restart12 = b12e(i-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
                 restart21 =   b21d(i)**2_ilp + b21bulge**2_ilp <= thresh**2_ilp
                 restart22 = b22e(i-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
                 ! if possible, chase bulges from b11(i+1,i), b12(i+1,i-1),
                 ! b21(i+1,i), and b22(i+1,i-1). if necessary, restart bulge-
                 ! chasing by applying the original shift again.
                 if( .not. restart11 .and. .not. restart12 ) then
                    call stdlib_dlartgp( x2, x1, rwork(iu1sn+i-1), rwork(iu1cs+i-1),r )
                 else if( .not. restart11 .and. restart12 ) then
                    call stdlib_dlartgp( b11bulge, b11d(i), rwork(iu1sn+i-1),rwork(iu1cs+i-1), r )
                              
                 else if( restart11 .and. .not. restart12 ) then
                    call stdlib_dlartgp( b12bulge, b12e(i-1), rwork(iu1sn+i-1),rwork(iu1cs+i-1), &
                              r )
                 else if( mu <= nu ) then
                    call stdlib_dlartgs( b11e(i), b11d(i+1), mu, rwork(iu1cs+i-1),rwork(iu1sn+i-1)&
                               )
                 else
                    call stdlib_dlartgs( b12d(i), b12e(i), nu, rwork(iu1cs+i-1),rwork(iu1sn+i-1) )
                              
                 end if
                 if( .not. restart21 .and. .not. restart22 ) then
                    call stdlib_dlartgp( y2, y1, rwork(iu2sn+i-1), rwork(iu2cs+i-1),r )
                 else if( .not. restart21 .and. restart22 ) then
                    call stdlib_dlartgp( b21bulge, b21d(i), rwork(iu2sn+i-1),rwork(iu2cs+i-1), r )
                              
                 else if( restart21 .and. .not. restart22 ) then
                    call stdlib_dlartgp( b22bulge, b22e(i-1), rwork(iu2sn+i-1),rwork(iu2cs+i-1), &
                              r )
                 else if( nu < mu ) then
                    call stdlib_dlartgs( b21e(i), b21e(i+1), nu, rwork(iu2cs+i-1),rwork(iu2sn+i-1)&
                               )
                 else
                    call stdlib_dlartgs( b22d(i), b22e(i), mu, rwork(iu2cs+i-1),rwork(iu2sn+i-1) )
                              
                 end if
                 rwork(iu2cs+i-1) = -rwork(iu2cs+i-1)
                 rwork(iu2sn+i-1) = -rwork(iu2sn+i-1)
                 temp = rwork(iu1cs+i-1)*b11e(i) + rwork(iu1sn+i-1)*b11d(i+1)
                 b11d(i+1) = rwork(iu1cs+i-1)*b11d(i+1) -rwork(iu1sn+i-1)*b11e(i)
                 b11e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b11bulge = rwork(iu1sn+i-1)*b11e(i+1)
                    b11e(i+1) = rwork(iu1cs+i-1)*b11e(i+1)
                 end if
                 temp = rwork(iu2cs+i-1)*b21e(i) + rwork(iu2sn+i-1)*b21d(i+1)
                 b21d(i+1) = rwork(iu2cs+i-1)*b21d(i+1) -rwork(iu2sn+i-1)*b21e(i)
                 b21e(i) = temp
                 if( i < imax - 1_ilp ) then
                    b21bulge = rwork(iu2sn+i-1)*b21e(i+1)
                    b21e(i+1) = rwork(iu2cs+i-1)*b21e(i+1)
                 end if
                 temp = rwork(iu1cs+i-1)*b12d(i) + rwork(iu1sn+i-1)*b12e(i)
                 b12e(i) = rwork(iu1cs+i-1)*b12e(i) -rwork(iu1sn+i-1)*b12d(i)
                 b12d(i) = temp
                 b12bulge = rwork(iu1sn+i-1)*b12d(i+1)
                 b12d(i+1) = rwork(iu1cs+i-1)*b12d(i+1)
                 temp = rwork(iu2cs+i-1)*b22d(i) + rwork(iu2sn+i-1)*b22e(i)
                 b22e(i) = rwork(iu2cs+i-1)*b22e(i) -rwork(iu2sn+i-1)*b22d(i)
                 b22d(i) = temp
                 b22bulge = rwork(iu2sn+i-1)*b22d(i+1)
                 b22d(i+1) = rwork(iu2cs+i-1)*b22d(i+1)
              end do
              ! compute phi(imax-1)
              x1 = sin(theta(imax-1))*b11e(imax-1) +cos(theta(imax-1))*b21e(imax-1)
              y1 = sin(theta(imax-1))*b12d(imax-1) +cos(theta(imax-1))*b22d(imax-1)
              y2 = sin(theta(imax-1))*b12bulge + cos(theta(imax-1))*b22bulge
              phi(imax-1) = atan2( abs(x1), sqrt(y1**2_ilp+y2**2_ilp) )
              ! chase bulges from b12(imax-1,imax) and b22(imax-1,imax)
              restart12 = b12d(imax-1)**2_ilp + b12bulge**2_ilp <= thresh**2_ilp
              restart22 = b22d(imax-1)**2_ilp + b22bulge**2_ilp <= thresh**2_ilp
              if( .not. restart12 .and. .not. restart22 ) then
                 call stdlib_dlartgp( y2, y1, rwork(iv2tsn+imax-1-1),rwork(iv2tcs+imax-1-1), r )
                           
              else if( .not. restart12 .and. restart22 ) then
                 call stdlib_dlartgp( b12bulge, b12d(imax-1),rwork(iv2tsn+imax-1-1),rwork(iv2tcs+&
                           imax-1-1), r )
              else if( restart12 .and. .not. restart22 ) then
                 call stdlib_dlartgp( b22bulge, b22d(imax-1),rwork(iv2tsn+imax-1-1),rwork(iv2tcs+&
                           imax-1-1), r )
              else if( nu < mu ) then
                 call stdlib_dlartgs( b12e(imax-1), b12d(imax), nu,rwork(iv2tcs+imax-1-1),rwork(&
                           iv2tsn+imax-1-1) )
              else
                 call stdlib_dlartgs( b22e(imax-1), b22d(imax), mu,rwork(iv2tcs+imax-1-1),rwork(&
                           iv2tsn+imax-1-1) )
              end if
              temp = rwork(iv2tcs+imax-1-1)*b12e(imax-1) +rwork(iv2tsn+imax-1-1)*b12d(imax)
                        
              b12d(imax) = rwork(iv2tcs+imax-1-1)*b12d(imax) -rwork(iv2tsn+imax-1-1)*b12e(imax-1)
                        
              b12e(imax-1) = temp
              temp = rwork(iv2tcs+imax-1-1)*b22e(imax-1) +rwork(iv2tsn+imax-1-1)*b22d(imax)
                        
              b22d(imax) = rwork(iv2tcs+imax-1-1)*b22d(imax) -rwork(iv2tsn+imax-1-1)*b22e(imax-1)
                        
              b22e(imax-1) = temp
              ! update singular vectors
              if( wantu1 ) then
                 if( colmajor ) then
                    call stdlib_zlasr( 'R', 'V', 'F', p, imax-imin+1,rwork(iu1cs+imin-1), rwork(&
                              iu1sn+imin-1),u1(1_ilp,imin), ldu1 )
                 else
                    call stdlib_zlasr( 'L', 'V', 'F', imax-imin+1, p,rwork(iu1cs+imin-1), rwork(&
                              iu1sn+imin-1),u1(imin,1_ilp), ldu1 )
                 end if
              end if
              if( wantu2 ) then
                 if( colmajor ) then
                    call stdlib_zlasr( 'R', 'V', 'F', m-p, imax-imin+1,rwork(iu2cs+imin-1), rwork(&
                              iu2sn+imin-1),u2(1_ilp,imin), ldu2 )
                 else
                    call stdlib_zlasr( 'L', 'V', 'F', imax-imin+1, m-p,rwork(iu2cs+imin-1), rwork(&
                              iu2sn+imin-1),u2(imin,1_ilp), ldu2 )
                 end if
              end if
              if( wantv1t ) then
                 if( colmajor ) then
                    call stdlib_zlasr( 'L', 'V', 'F', imax-imin+1, q,rwork(iv1tcs+imin-1), rwork(&
                              iv1tsn+imin-1),v1t(imin,1_ilp), ldv1t )
                 else
                    call stdlib_zlasr( 'R', 'V', 'F', q, imax-imin+1,rwork(iv1tcs+imin-1), rwork(&
                              iv1tsn+imin-1),v1t(1_ilp,imin), ldv1t )
                 end if
              end if
              if( wantv2t ) then
                 if( colmajor ) then
                    call stdlib_zlasr( 'L', 'V', 'F', imax-imin+1, m-q,rwork(iv2tcs+imin-1), &
                              rwork(iv2tsn+imin-1),v2t(imin,1_ilp), ldv2t )
                 else
                    call stdlib_zlasr( 'R', 'V', 'F', m-q, imax-imin+1,rwork(iv2tcs+imin-1), &
                              rwork(iv2tsn+imin-1),v2t(1_ilp,imin), ldv2t )
                 end if
              end if
              ! fix signs on b11(imax-1,imax) and b21(imax-1,imax)
              if( b11e(imax-1)+b21e(imax-1) > 0_ilp ) then
                 b11d(imax) = -b11d(imax)
                 b21d(imax) = -b21d(imax)
                 if( wantv1t ) then
                    if( colmajor ) then
                       call stdlib_zscal( q, cnegone, v1t(imax,1_ilp), ldv1t )
                    else
                       call stdlib_zscal( q, cnegone, v1t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! compute theta(imax)
              x1 = cos(phi(imax-1))*b11d(imax) +sin(phi(imax-1))*b12e(imax-1)
              y1 = cos(phi(imax-1))*b21d(imax) +sin(phi(imax-1))*b22e(imax-1)
              theta(imax) = atan2( abs(y1), abs(x1) )
              ! fix signs on b11(imax,imax), b12(imax,imax-1), b21(imax,imax),
              ! and b22(imax,imax-1)
              if( b11d(imax)+b12e(imax-1) < 0_ilp ) then
                 b12d(imax) = -b12d(imax)
                 if( wantu1 ) then
                    if( colmajor ) then
                       call stdlib_zscal( p, cnegone, u1(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_zscal( p, cnegone, u1(imax,1_ilp), ldu1 )
                    end if
                 end if
              end if
              if( b21d(imax)+b22e(imax-1) > 0_ilp ) then
                 b22d(imax) = -b22d(imax)
                 if( wantu2 ) then
                    if( colmajor ) then
                       call stdlib_zscal( m-p, cnegone, u2(1_ilp,imax), 1_ilp )
                    else
                       call stdlib_zscal( m-p, cnegone, u2(imax,1_ilp), ldu2 )
                    end if
                 end if
              end if
              ! fix signs on b12(imax,imax) and b22(imax,imax)
              if( b12d(imax)+b22d(imax) < 0_ilp ) then
                 if( wantv2t ) then
                    if( colmajor ) then
                       call stdlib_zscal( m-q, cnegone, v2t(imax,1_ilp), ldv2t )
                    else
                       call stdlib_zscal( m-q, cnegone, v2t(1_ilp,imax), 1_ilp )
                    end if
                 end if
              end if
              ! test for negligible sines or cosines
              do i = imin, imax
                 if( theta(i) < thresh ) then
                    theta(i) = zero
                 else if( theta(i) > piover2-thresh ) then
                    theta(i) = piover2
                 end if
              end do
              do i = imin, imax-1
                 if( phi(i) < thresh ) then
                    phi(i) = zero
                 else if( phi(i) > piover2-thresh ) then
                    phi(i) = piover2
                 end if
              end do
              ! deflate
              if (imax > 1_ilp) then
                 do while( phi(imax-1) == zero )
                    imax = imax - 1_ilp
                    if (imax <= 1) exit
                 end do
              end if
              if( imin > imax - 1_ilp )imin = imax - 1_ilp
              if (imin > 1_ilp) then
                 do while (phi(imin-1) /= zero)
                     imin = imin - 1_ilp
                     if (imin <= 1) exit
                 end do
              end if
              ! repeat main iteration loop
           end do
           ! postprocessing: order theta from least to greatest
           do i = 1, q
              mini = i
              thetamin = theta(i)
              do j = i+1, q
                 if( theta(j) < thetamin ) then
                    mini = j
                    thetamin = theta(j)
                 end if
              end do
              if( mini /= i ) then
                 theta(mini) = theta(i)
                 theta(i) = thetamin
                 if( colmajor ) then
                    if( wantu1 )call stdlib_zswap( p, u1(1_ilp,i), 1_ilp, u1(1_ilp,mini), 1_ilp )
                    if( wantu2 )call stdlib_zswap( m-p, u2(1_ilp,i), 1_ilp, u2(1_ilp,mini), 1_ilp )
                    if( wantv1t )call stdlib_zswap( q, v1t(i,1_ilp), ldv1t, v1t(mini,1_ilp), ldv1t )
                              
                    if( wantv2t )call stdlib_zswap( m-q, v2t(i,1_ilp), ldv2t, v2t(mini,1_ilp),ldv2t )
                              
                 else
                    if( wantu1 )call stdlib_zswap( p, u1(i,1_ilp), ldu1, u1(mini,1_ilp), ldu1 )
                    if( wantu2 )call stdlib_zswap( m-p, u2(i,1_ilp), ldu2, u2(mini,1_ilp), ldu2 )
                    if( wantv1t )call stdlib_zswap( q, v1t(1_ilp,i), 1_ilp, v1t(1_ilp,mini), 1_ilp )
                    if( wantv2t )call stdlib_zswap( m-q, v2t(1_ilp,i), 1_ilp, v2t(1_ilp,mini), 1_ilp )
                 end if
              end if
           end do
           return
     end subroutine stdlib_zbbcsd




     recursive module subroutine stdlib_cuncsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     !! CUNCSD computes the CS decomposition of an M-by-M partitioned
     !! unitary matrix X:
     !! [  I  0  0 |  0  0  0 ]
     !! [  0  C  0 |  0 -S  0 ]
     !! [ X11 | X12 ]   [ U1 |    ] [  0  0  0 |  0  0 -I ] [ V1 |    ]**H
     !! X = [-----------] = [---------] [---------------------] [---------]   .
     !! [ X21 | X22 ]   [    | U2 ] [  0  0  0 |  I  0  0 ] [    | V2 ]
     !! [  0  S  0 |  0  C  0 ]
     !! [  0  0  I |  0  0  0 ]
     !! X11 is P-by-Q. The unitary matrices U1, U2, V1, and V2 are P-by-P,
     !! (M-P)-by-(M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. C and S are
     !! R-by-R nonnegative diagonal matrices satisfying C^2 + S^2 = I, in
     !! which R = MIN(P,M-P,Q,M-Q).
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, rwork, lrwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lrwork, lwork, m, p, q
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           complex(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ===================================================================
           
           ! Local Scalars 
           character :: transt, signst
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, itauq2, j, lbbcsdwork, &
           lbbcsdworkmin, lbbcsdworkopt, lorbdbwork, lorbdbworkmin, lorbdbworkopt, lorglqwork, &
           lorglqworkmin, lorglqworkopt, lorgqrwork, lorgqrworkmin, lorgqrworkopt, lworkmin, &
                     lworkopt, p1, q1
           logical(lk) :: colmajor, defaultsigns, lquery, wantu1, wantu2, wantv1t, wantv2t
           integer(ilp) :: lrworkmin, lrworkopt
           logical(lk) :: lrquery
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           defaultsigns = .not. stdlib_lsame( signs, 'O' )
           lquery = lwork == -1_ilp
           lrquery = lrwork == -1_ilp
           if( m < 0_ilp ) then
              info = -7_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -8_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -9_ilp
           else if ( colmajor .and.  ldx11 < max( 1_ilp, p ) ) then
             info = -11_ilp
           else if (.not. colmajor .and. ldx11 < max( 1_ilp, q ) ) then
             info = -11_ilp
           else if (colmajor .and. ldx12 < max( 1_ilp, p ) ) then
             info = -13_ilp
           else if (.not. colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
             info = -13_ilp
           else if (colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
             info = -15_ilp
           else if (.not. colmajor .and. ldx21 < max( 1_ilp, q ) ) then
             info = -15_ilp
           else if (colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
             info = -17_ilp
           else if (.not. colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
             info = -17_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -20_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -22_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -24_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -26_ilp
           end if
           ! work with transpose if convenient
           if( info == 0_ilp .and. min( p, m-p ) < min( q, m-q ) ) then
              if( colmajor ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_cuncsd( jobv1t, jobv2t, jobu1, jobu2, transt, signst, m,q, p, x11, &
              ldx11, x21, ldx21, x12, ldx12, x22,ldx22, theta, v1t, ldv1t, v2t, ldv2t, u1, ldu1,&
                        u2, ldu2, work, lwork, rwork, lrwork, iwork,info )
              return
           end if
           ! work with permutation [ 0 i; i 0 ] * x * [ 0 i; i 0 ] if
           ! convenient
           if( info == 0_ilp .and. m-q < q ) then
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_cuncsd( jobu2, jobu1, jobv2t, jobv1t, trans, signst, m,m-p, m-q, x22, &
              ldx22, x21, ldx21, x12, ldx12, x11,ldx11, theta, u2, ldu2, u1, ldu1, v2t, ldv2t, &
                        v1t,ldv1t, work, lwork, rwork, lrwork, iwork, info )
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ! real workspace
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, q - 1_ilp )
              ib11e = ib11d + max( 1_ilp, q )
              ib12d = ib11e + max( 1_ilp, q - 1_ilp )
              ib12e = ib12d + max( 1_ilp, q )
              ib21d = ib12e + max( 1_ilp, q - 1_ilp )
              ib21e = ib21d + max( 1_ilp, q )
              ib22d = ib21e + max( 1_ilp, q - 1_ilp )
              ib22e = ib22d + max( 1_ilp, q )
              ibbcsd = ib22e + max( 1_ilp, q - 1_ilp )
              call stdlib_cbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, theta, u1, &
              ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, theta, theta, theta, theta, theta,theta, &
                        theta, theta, rwork, -1_ilp, childinfo )
              lbbcsdworkopt = int( rwork(1_ilp),KIND=ilp)
              lbbcsdworkmin = lbbcsdworkopt
              lrworkopt = ibbcsd + lbbcsdworkopt - 1_ilp
              lrworkmin = ibbcsd + lbbcsdworkmin - 1_ilp
              rwork(1_ilp) = lrworkopt
              ! complex workspace
              itaup1 = 2_ilp
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m - p )
              itauq2 = itauq1 + max( 1_ilp, q )
              iorgqr = itauq2 + max( 1_ilp, m - q )
              call stdlib_cungqr( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorgqrworkopt = int( work(1_ilp),KIND=ilp)
              lorgqrworkmin = max( 1_ilp, m - q )
              iorglq = itauq2 + max( 1_ilp, m - q )
              call stdlib_cunglq( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorglqworkopt = int( work(1_ilp),KIND=ilp)
              lorglqworkmin = max( 1_ilp, m - q )
              iorbdb = itauq2 + max( 1_ilp, m - q )
              call stdlib_cunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
                        ldx22, theta, theta, u1, u2,v1t, v2t, work, -1_ilp, childinfo )
              lorbdbworkopt = int( work(1_ilp),KIND=ilp)
              lorbdbworkmin = lorbdbworkopt
              lworkopt = max( iorgqr + lorgqrworkopt, iorglq + lorglqworkopt,iorbdb + &
                        lorbdbworkopt ) - 1_ilp
              lworkmin = max( iorgqr + lorgqrworkmin, iorglq + lorglqworkmin,iorbdb + &
                        lorbdbworkmin ) - 1_ilp
              work(1_ilp) = max(lworkopt,lworkmin)
              if( lwork < lworkmin.and. .not. ( lquery .or. lrquery ) ) then
                 info = -22_ilp
              else if( lrwork < lrworkmin.and. .not. ( lquery .or. lrquery ) ) then
                 info = -24_ilp
              else
                 lorgqrwork = lwork - iorgqr + 1_ilp
                 lorglqwork = lwork - iorglq + 1_ilp
                 lorbdbwork = lwork - iorbdb + 1_ilp
                 lbbcsdwork = lrwork - ibbcsd + 1_ilp
              end if
           end if
           ! abort if any illegal arguments
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNCSD', -info )
              return
           else if( lquery .or. lrquery ) then
              return
           end if
           ! transform to bidiagonal block form
           call stdlib_cunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12, x21,ldx21, x22, &
           ldx22, theta, rwork(iphi), work(itaup1),work(itaup2), work(itauq1), work(itauq2),work(&
                     iorbdb), lorbdbwork, childinfo )
           ! accumulate householder reflectors
           if( colmajor ) then
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_clacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_cungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqrwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_clacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_cungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqrwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_clacpy( 'U', q-1, q-1, x11(1_ilp,2_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_cunglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglqwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_clacpy( 'U', p, m-q, x12, ldx12, v2t, ldv2t )
                 if( m-p > q ) then
                    call stdlib_clacpy( 'U', m-p-q, m-p-q, x22(q+1,p+1), ldx22,v2t(p+1,p+1), &
                              ldv2t )
                 end if
                 if( m > q ) then
                    call stdlib_cunglq( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorglq), &
                              lorglqwork, info )
                 end if
              end if
           else
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_clacpy( 'U', q, p, x11, ldx11, u1, ldu1 )
                 call stdlib_cunglq( p, p, q, u1, ldu1, work(itaup1), work(iorglq),lorglqwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_clacpy( 'U', q, m-p, x21, ldx21, u2, ldu2 )
                 call stdlib_cunglq( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorglq), lorglqwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_clacpy( 'L', q-1, q-1, x11(2_ilp,1_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_cungqr( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorgqr), &
                           lorgqrwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 p1 = min( p+1, m )
                 q1 = min( q+1, m )
                 call stdlib_clacpy( 'L', m-q, p, x12, ldx12, v2t, ldv2t )
                 if ( m > p+q ) then
                    call stdlib_clacpy( 'L', m-p-q, m-p-q, x22(p1,q1), ldx22,v2t(p+1,p+1), ldv2t )
                              
                 end if
                 call stdlib_cungqr( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorgqr), &
                           lorgqrwork, info )
              end if
           end if
           ! compute the csd of the matrix in bidiagonal-block form
           call stdlib_cbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q, theta,rwork(iphi), &
           u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, rwork(ib11d), rwork(ib11e), rwork(ib12d),&
           rwork(ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd),&
                     lbbcsdwork, info )
           ! permute rows and columns to place identity submatrices in top-
           ! left corner of (1,1)-block and/or bottom-right corner of (1,2)-
           ! block and/or bottom-right corner of (2,1)-block and/or top-left
           ! corner of (2,2)-block
           if( q > 0_ilp .and. wantu2 ) then
              do i = 1, q
                 iwork(i) = m - p - q + i
              end do
              do i = q + 1, m - p
                 iwork(i) = i - q
              end do
              if( colmajor ) then
                 call stdlib_clapmt( .false., m-p, m-p, u2, ldu2, iwork )
              else
                 call stdlib_clapmr( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           end if
           if( m > 0_ilp .and. wantv2t ) then
              do i = 1, p
                 iwork(i) = m - p - q + i
              end do
              do i = p + 1, m - q
                 iwork(i) = i - p
              end do
              if( .not. colmajor ) then
                 call stdlib_clapmt( .false., m-q, m-q, v2t, ldv2t, iwork )
              else
                 call stdlib_clapmr( .false., m-q, m-q, v2t, ldv2t, iwork )
              end if
           end if
           return
           ! end stdlib_cuncsd
     end subroutine stdlib_cuncsd

     recursive module subroutine stdlib_zuncsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     !! ZUNCSD computes the CS decomposition of an M-by-M partitioned
     !! unitary matrix X:
     !! [  I  0  0 |  0  0  0 ]
     !! [  0  C  0 |  0 -S  0 ]
     !! [ X11 | X12 ]   [ U1 |    ] [  0  0  0 |  0  0 -I ] [ V1 |    ]**H
     !! X = [-----------] = [---------] [---------------------] [---------]   .
     !! [ X21 | X22 ]   [    | U2 ] [  0  0  0 |  I  0  0 ] [    | V2 ]
     !! [  0  S  0 |  0  C  0 ]
     !! [  0  0  I |  0  0  0 ]
     !! X11 is P-by-Q. The unitary matrices U1, U2, V1, and V2 are P-by-P,
     !! (M-P)-by-(M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. C and S are
     !! R-by-R nonnegative diagonal matrices satisfying C^2 + S^2 = I, in
     !! which R = MIN(P,M-P,Q,M-Q).
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, rwork, lrwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lrwork, lwork, m, p, q
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           complex(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ===================================================================
           
           ! Local Scalars 
           character :: transt, signst
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, itauq2, j, lbbcsdwork, &
           lbbcsdworkmin, lbbcsdworkopt, lorbdbwork, lorbdbworkmin, lorbdbworkopt, lorglqwork, &
           lorglqworkmin, lorglqworkopt, lorgqrwork, lorgqrworkmin, lorgqrworkopt, lworkmin, &
                     lworkopt, p1, q1
           logical(lk) :: colmajor, defaultsigns, lquery, wantu1, wantu2, wantv1t, wantv2t
           integer(ilp) :: lrworkmin, lrworkopt
           logical(lk) :: lrquery
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           defaultsigns = .not. stdlib_lsame( signs, 'O' )
           lquery = lwork == -1_ilp
           lrquery = lrwork == -1_ilp
           if( m < 0_ilp ) then
              info = -7_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -8_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -9_ilp
           else if ( colmajor .and.  ldx11 < max( 1_ilp, p ) ) then
             info = -11_ilp
           else if (.not. colmajor .and. ldx11 < max( 1_ilp, q ) ) then
             info = -11_ilp
           else if (colmajor .and. ldx12 < max( 1_ilp, p ) ) then
             info = -13_ilp
           else if (.not. colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
             info = -13_ilp
           else if (colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
             info = -15_ilp
           else if (.not. colmajor .and. ldx21 < max( 1_ilp, q ) ) then
             info = -15_ilp
           else if (colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
             info = -17_ilp
           else if (.not. colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
             info = -17_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -20_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -22_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -24_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -26_ilp
           end if
           ! work with transpose if convenient
           if( info == 0_ilp .and. min( p, m-p ) < min( q, m-q ) ) then
              if( colmajor ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_zuncsd( jobv1t, jobv2t, jobu1, jobu2, transt, signst, m,q, p, x11, &
              ldx11, x21, ldx21, x12, ldx12, x22,ldx22, theta, v1t, ldv1t, v2t, ldv2t, u1, ldu1,&
                        u2, ldu2, work, lwork, rwork, lrwork, iwork,info )
              return
           end if
           ! work with permutation [ 0 i; i 0 ] * x * [ 0 i; i 0 ] if
           ! convenient
           if( info == 0_ilp .and. m-q < q ) then
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_zuncsd( jobu2, jobu1, jobv2t, jobv1t, trans, signst, m,m-p, m-q, x22, &
              ldx22, x21, ldx21, x12, ldx12, x11,ldx11, theta, u2, ldu2, u1, ldu1, v2t, ldv2t, &
                        v1t,ldv1t, work, lwork, rwork, lrwork, iwork, info )
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ! real workspace
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, q - 1_ilp )
              ib11e = ib11d + max( 1_ilp, q )
              ib12d = ib11e + max( 1_ilp, q - 1_ilp )
              ib12e = ib12d + max( 1_ilp, q )
              ib21d = ib12e + max( 1_ilp, q - 1_ilp )
              ib21e = ib21d + max( 1_ilp, q )
              ib22d = ib21e + max( 1_ilp, q - 1_ilp )
              ib22e = ib22d + max( 1_ilp, q )
              ibbcsd = ib22e + max( 1_ilp, q - 1_ilp )
              call stdlib_zbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, theta, u1, &
              ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, theta, theta, theta, theta, theta,theta, &
                        theta, theta, rwork, -1_ilp, childinfo )
              lbbcsdworkopt = int( rwork(1_ilp),KIND=ilp)
              lbbcsdworkmin = lbbcsdworkopt
              lrworkopt = ibbcsd + lbbcsdworkopt - 1_ilp
              lrworkmin = ibbcsd + lbbcsdworkmin - 1_ilp
              rwork(1_ilp) = lrworkopt
              ! complex workspace
              itaup1 = 2_ilp
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m - p )
              itauq2 = itauq1 + max( 1_ilp, q )
              iorgqr = itauq2 + max( 1_ilp, m - q )
              call stdlib_zungqr( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorgqrworkopt = int( work(1_ilp),KIND=ilp)
              lorgqrworkmin = max( 1_ilp, m - q )
              iorglq = itauq2 + max( 1_ilp, m - q )
              call stdlib_zunglq( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorglqworkopt = int( work(1_ilp),KIND=ilp)
              lorglqworkmin = max( 1_ilp, m - q )
              iorbdb = itauq2 + max( 1_ilp, m - q )
              call stdlib_zunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
                        ldx22, theta, theta, u1, u2,v1t, v2t, work, -1_ilp, childinfo )
              lorbdbworkopt = int( work(1_ilp),KIND=ilp)
              lorbdbworkmin = lorbdbworkopt
              lworkopt = max( iorgqr + lorgqrworkopt, iorglq + lorglqworkopt,iorbdb + &
                        lorbdbworkopt ) - 1_ilp
              lworkmin = max( iorgqr + lorgqrworkmin, iorglq + lorglqworkmin,iorbdb + &
                        lorbdbworkmin ) - 1_ilp
              work(1_ilp) = max(lworkopt,lworkmin)
              if( lwork < lworkmin.and. .not. ( lquery .or. lrquery ) ) then
                 info = -22_ilp
              else if( lrwork < lrworkmin.and. .not. ( lquery .or. lrquery ) ) then
                 info = -24_ilp
              else
                 lorgqrwork = lwork - iorgqr + 1_ilp
                 lorglqwork = lwork - iorglq + 1_ilp
                 lorbdbwork = lwork - iorbdb + 1_ilp
                 lbbcsdwork = lrwork - ibbcsd + 1_ilp
              end if
           end if
           ! abort if any illegal arguments
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNCSD', -info )
              return
           else if( lquery .or. lrquery ) then
              return
           end if
           ! transform to bidiagonal block form
           call stdlib_zunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12, x21,ldx21, x22, &
           ldx22, theta, rwork(iphi), work(itaup1),work(itaup2), work(itauq1), work(itauq2),work(&
                     iorbdb), lorbdbwork, childinfo )
           ! accumulate householder reflectors
           if( colmajor ) then
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_zungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqrwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_zungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqrwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_zlacpy( 'U', q-1, q-1, x11(1_ilp,2_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_zunglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglqwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_zlacpy( 'U', p, m-q, x12, ldx12, v2t, ldv2t )
                 if( m-p > q) then
                    call stdlib_zlacpy( 'U', m-p-q, m-p-q, x22(q+1,p+1), ldx22,v2t(p+1,p+1), &
                              ldv2t )
                 end if
                 if( m > q ) then
                    call stdlib_zunglq( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorglq), &
                              lorglqwork, info )
                 end if
              end if
           else
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_zlacpy( 'U', q, p, x11, ldx11, u1, ldu1 )
                 call stdlib_zunglq( p, p, q, u1, ldu1, work(itaup1), work(iorglq),lorglqwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_zlacpy( 'U', q, m-p, x21, ldx21, u2, ldu2 )
                 call stdlib_zunglq( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorglq), lorglqwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_zlacpy( 'L', q-1, q-1, x11(2_ilp,1_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_zungqr( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorgqr), &
                           lorgqrwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 p1 = min( p+1, m )
                 q1 = min( q+1, m )
                 call stdlib_zlacpy( 'L', m-q, p, x12, ldx12, v2t, ldv2t )
                 if( m > p+q ) then
                    call stdlib_zlacpy( 'L', m-p-q, m-p-q, x22(p1,q1), ldx22,v2t(p+1,p+1), ldv2t )
                              
                 end if
                 call stdlib_zungqr( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorgqr), &
                           lorgqrwork, info )
              end if
           end if
           ! compute the csd of the matrix in bidiagonal-block form
           call stdlib_zbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q, theta,rwork(iphi), &
           u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, rwork(ib11d), rwork(ib11e), rwork(ib12d),&
           rwork(ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd),&
                     lbbcsdwork, info )
           ! permute rows and columns to place identity submatrices in top-
           ! left corner of (1,1)-block and/or bottom-right corner of (1,2)-
           ! block and/or bottom-right corner of (2,1)-block and/or top-left
           ! corner of (2,2)-block
           if( q > 0_ilp .and. wantu2 ) then
              do i = 1, q
                 iwork(i) = m - p - q + i
              end do
              do i = q + 1, m - p
                 iwork(i) = i - q
              end do
              if( colmajor ) then
                 call stdlib_zlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              else
                 call stdlib_zlapmr( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           end if
           if( m > 0_ilp .and. wantv2t ) then
              do i = 1, p
                 iwork(i) = m - p - q + i
              end do
              do i = p + 1, m - q
                 iwork(i) = i - p
              end do
              if( .not. colmajor ) then
                 call stdlib_zlapmt( .false., m-q, m-q, v2t, ldv2t, iwork )
              else
                 call stdlib_zlapmr( .false., m-q, m-q, v2t, ldv2t, iwork )
              end if
           end if
           return
           ! end stdlib_zuncsd
     end subroutine stdlib_zuncsd




     module subroutine stdlib_cuncsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
     !! CUNCSD2BY1 computes the CS decomposition of an M-by-Q matrix X with
     !! orthonormal columns that has been partitioned into a 2-by-1 block
     !! structure:
     !! [  I1 0  0 ]
     !! [  0  C  0 ]
     !! [ X11 ]   [ U1 |    ] [  0  0  0 ]
     !! X = [-----] = [---------] [----------] V1**T .
     !! [ X21 ]   [    | U2 ] [  0  0  0 ]
     !! [  0  S  0 ]
     !! [  0  0  I2]
     !! X11 is P-by-Q. The unitary matrices U1, U2, and V1 are P-by-P,
     !! (M-P)-by-(M-P), and Q-by-Q, respectively. C and S are R-by-R
     !! nonnegative diagonal matrices satisfying C^2 + S^2 = I, in which
     !! R = MIN(P,M-P,Q,M-Q). I1 is a K1-by-K1 identity matrix and I2 is a
     !! K2-by-K2 identity matrix, where K1 = MAX(Q+P-M,0), K2 = MAX(Q-P,0).
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, rwork, lrwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           integer(ilp), intent(in) :: lrwork
           integer(ilp) :: lrworkmin, lrworkopt
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           real(sp), intent(out) :: theta(*)
           complex(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, j, lbbcsd, lorbdb, &
           lorglq, lorglqmin, lorglqopt, lorgqr, lorgqrmin, lorgqropt, lworkmin, lworkopt, &
                     r
           logical(lk) :: lquery, wantu1, wantu2, wantv1t
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           complex(sp) :: cdum(1_ilp,1_ilp)
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           lquery = ( lwork==-1_ilp ) .or. ( lrwork==-1_ilp )
           if( m < 0_ilp ) then
              info = -4_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -5_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -6_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -10_ilp
           else if( wantu1 .and. ldu1 < max( 1_ilp, p ) ) then
              info = -13_ilp
           else if( wantu2 .and. ldu2 < max( 1_ilp, m - p ) ) then
              info = -15_ilp
           else if( wantv1t .and. ldv1t < max( 1_ilp, q ) ) then
              info = -17_ilp
           end if
           r = min( p, m-p, q, m-q )
           ! compute workspace
             ! work layout:
           ! |-----------------------------------------|
           ! | lworkopt (1)                            |
           ! |-----------------------------------------|
           ! | taup1 (max(1,p))                        |
           ! | taup2 (max(1,m-p))                      |
           ! | tauq1 (max(1,q))                        |
           ! |-----------------------------------------|
           ! | stdlib_cunbdb work | stdlib_cungqr work | stdlib_cunglq work |
           ! |             |             |             |
           ! |             |             |             |
           ! |             |             |             |
           ! |             |             |             |
           ! |-----------------------------------------|
             ! rwork layout:
           ! |------------------|
           ! | lrworkopt (1)    |
           ! |------------------|
           ! | phi (max(1,r-1)) |
           ! |------------------|
           ! | b11d (r)         |
           ! | b11e (r-1)       |
           ! | b12d (r)         |
           ! | b12e (r-1)       |
           ! | b21d (r)         |
           ! | b21e (r-1)       |
           ! | b22d (r)         |
           ! | b22e (r-1)       |
           ! | stdlib_cbbcsd rwork     |
           ! |------------------|
           if( info == 0_ilp ) then
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, r-1 )
              ib11e = ib11d + max( 1_ilp, r )
              ib12d = ib11e + max( 1_ilp, r - 1_ilp )
              ib12e = ib12d + max( 1_ilp, r )
              ib21d = ib12e + max( 1_ilp, r - 1_ilp )
              ib21e = ib21d + max( 1_ilp, r )
              ib22d = ib21e + max( 1_ilp, r - 1_ilp )
              ib22e = ib22d + max( 1_ilp, r )
              ibbcsd = ib22e + max( 1_ilp, r - 1_ilp )
              itaup1 = 2_ilp
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m-p )
              iorbdb = itauq1 + max( 1_ilp, q )
              iorgqr = itauq1 + max( 1_ilp, q )
              iorglq = itauq1 + max( 1_ilp, q )
              lorgqrmin = 1_ilp
              lorgqropt = 1_ilp
              lorglqmin = 1_ilp
              lorglqopt = 1_ilp
              if( r == q ) then
                 call stdlib_cunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,dum, cdum, cdum, &
                           cdum, work, -1_ilp,childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_cungqr( p, p, q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 endif
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_cungqr( m-p, m-p, q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_cunglq( q-1, q-1, q-1, v1t, ldv1t,cdum, work(1_ilp), -1_ilp, childinfo )
                              
                    lorglqmin = max( lorglqmin, q-1 )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_cbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,dum(1_ilp), u1, &
                 ldu1, u2, ldu2, v1t, ldv1t, cdum,1_ilp, dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                           1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else if( r == p ) then
                 call stdlib_cunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_cungqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, cdum, work(1_ilp),-1_ilp, childinfo &
                              )
                    lorgqrmin = max( lorgqrmin, p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_cungqr( m-p, m-p, q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_cunglq( q, q, r, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_cbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,dum, v1t, &
                 ldv1t, cdum, 1_ilp, u1, ldu1, u2, ldu2,dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                           1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else if( r == m-p ) then
                 call stdlib_cunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_cungqr( p, p, q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_cungqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2, cdum,work(1_ilp), -1_ilp, &
                              childinfo )
                    lorgqrmin = max( lorgqrmin, m-p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_cunglq( q, q, r, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_cbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, dum, cdum,&
                  1_ilp, v1t, ldv1t, u2, ldu2, u1,ldu1, dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                            1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else
                 call stdlib_cunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, cdum, work(1_ilp), -1_ilp, childinfo)
                 lorbdb = m + int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_cungqr( p, p, m-q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_cungqr( m-p, m-p, m-q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_cunglq( q, q, q, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_cbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, dum, u2, &
                 ldu2, u1, ldu1, cdum, 1_ilp, v1t,ldv1t, dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                           1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              end if
              lrworkmin = ibbcsd+lbbcsd-1
              lrworkopt = lrworkmin
              rwork(1_ilp) = lrworkopt
              lworkmin = max( iorbdb+lorbdb-1,iorgqr+lorgqrmin-1,iorglq+lorglqmin-1 )
              lworkopt = max( iorbdb+lorbdb-1,iorgqr+lorgqropt-1,iorglq+lorglqopt-1 )
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
              if( lrwork < lrworkmin .and. .not.lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNCSD2BY1', -info )
              return
           else if( lquery ) then
              return
           end if
           lorgqr = lwork-iorgqr+1
           lorglq = lwork-iorglq+1
           ! handle four cases separately: r = q, r = p, r = m-p, and r = m-q,
           ! in which r = min(p,m-p,q,m-q)
           if( r == q ) then
              ! case 1: r = q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_cunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_clacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_cungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_clacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_cungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 v1t(1_ilp,1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_clacpy( 'U', q-1, q-1, x21(1_ilp,2_ilp), ldx21, v1t(2_ilp,2_ilp),ldv1t )
                 call stdlib_cunglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglq, childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_cbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,rwork(iphi), u1, &
              ldu1, u2, ldu2, v1t, ldv1t, cdum,1_ilp, rwork(ib11d), rwork(ib11e), rwork(ib12d),rwork(&
              ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd),lrwork-&
                        ibbcsd+1, childinfo )
              ! permute rows and columns to place czero submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_clapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == p ) then
              ! case 2: r = p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_cunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 u1(1_ilp,1_ilp) = cone
                 do j = 2, p
                    u1(1_ilp,j) = czero
                    u1(j,1_ilp) = czero
                 end do
                 call stdlib_clacpy( 'L', p-1, p-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp), ldu1 )
                 call stdlib_cungqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, work(itaup1),work(iorgqr), &
                           lorgqr, childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_clacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_cungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_clacpy( 'U', p, q, x11, ldx11, v1t, ldv1t )
                 call stdlib_cunglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_cbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,rwork(iphi), v1t,&
               ldv1t, cdum, 1_ilp, u1, ldu1, u2,ldu2, rwork(ib11d), rwork(ib11e), rwork(ib12d),rwork(&
               ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd), &
                         lbbcsd,childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_clapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == m-p ) then
              ! case 3: r = m-p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_cunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_clacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_cungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 u2(1_ilp,1_ilp) = cone
                 do j = 2, m-p
                    u2(1_ilp,j) = czero
                    u2(j,1_ilp) = czero
                 end do
                 call stdlib_clacpy( 'L', m-p-1, m-p-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_cungqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2,work(itaup2), work(iorgqr)&
                           , lorgqr, childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_clacpy( 'U', m-p, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_cunglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_cbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, rwork(iphi), &
              cdum, 1_ilp, v1t, ldv1t, u2, ldu2,u1, ldu1, rwork(ib11d), rwork(ib11e),rwork(ib12d), &
              rwork(ib12e), rwork(ib21d),rwork(ib21e), rwork(ib22d), rwork(ib22e),rwork(ibbcsd), &
                        lbbcsd, childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > r ) then
                 do i = 1, r
                    iwork(i) = q - r + i
                 end do
                 do i = r + 1, q
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_clapmt( .false., p, q, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_clapmr( .false., q, q, v1t, ldv1t, iwork )
                 end if
              end if
           else
              ! case 4: r = m-q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_cunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
              itaup1), work(itaup2),work(itauq1), work(iorbdb), work(iorbdb+m),lorbdb-m, &
                        childinfo )
              ! accumulate householder reflectors
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_ccopy( m-p, work(iorbdb+p), 1_ilp, u2, 1_ilp )
              end if
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_ccopy( p, work(iorbdb), 1_ilp, u1, 1_ilp )
                 do j = 2, p
                    u1(1_ilp,j) = czero
                 end do
                 call stdlib_clacpy( 'L', p-1, m-q-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp),ldu1 )
                 call stdlib_cungqr( p, p, m-q, u1, ldu1, work(itaup1),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 do j = 2, m-p
                    u2(1_ilp,j) = czero
                 end do
                 call stdlib_clacpy( 'L', m-p-1, m-q-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_cungqr( m-p, m-p, m-q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_clacpy( 'U', m-q, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_clacpy( 'U', p-(m-q), q-(m-q), x11(m-q+1,m-q+1), ldx11,v1t(m-q+1,m-q+&
                           1_ilp), ldv1t )
                 call stdlib_clacpy( 'U', -p+q, q-p, x21(m-q+1,p+1), ldx21,v1t(p+1,p+1), ldv1t )
                           
                 call stdlib_cunglq( q, q, q, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_cbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, rwork(iphi), &
              u2, ldu2, u1, ldu1, cdum, 1_ilp,v1t, ldv1t, rwork(ib11d), rwork(ib11e),rwork(ib12d), &
              rwork(ib12e), rwork(ib21d),rwork(ib21e), rwork(ib22d), rwork(ib22e),rwork(ibbcsd), &
                        lbbcsd, childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( p > r ) then
                 do i = 1, r
                    iwork(i) = p - r + i
                 end do
                 do i = r + 1, p
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_clapmt( .false., p, p, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_clapmr( .false., p, q, v1t, ldv1t, iwork )
                 end if
              end if
           end if
           return
     end subroutine stdlib_cuncsd2by1

     module subroutine stdlib_zuncsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
     !! ZUNCSD2BY1 computes the CS decomposition of an M-by-Q matrix X with
     !! orthonormal columns that has been partitioned into a 2-by-1 block
     !! structure:
     !! [  I1 0  0 ]
     !! [  0  C  0 ]
     !! [ X11 ]   [ U1 |    ] [  0  0  0 ]
     !! X = [-----] = [---------] [----------] V1**T .
     !! [ X21 ]   [    | U2 ] [  0  0  0 ]
     !! [  0  S  0 ]
     !! [  0  0  I2]
     !! X11 is P-by-Q. The unitary matrices U1, U2, and V1 are P-by-P,
     !! (M-P)-by-(M-P), and Q-by-Q, respectively. C and S are R-by-R
     !! nonnegative diagonal matrices satisfying C^2 + S^2 = I, in which
     !! R = MIN(P,M-P,Q,M-Q). I1 is a K1-by-K1 identity matrix and I2 is a
     !! K2-by-K2 identity matrix, where K1 = MAX(Q+P-M,0), K2 = MAX(Q-P,0).
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, rwork, lrwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           integer(ilp), intent(in) :: lrwork
           integer(ilp) :: lrworkmin, lrworkopt
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           real(dp), intent(out) :: theta(*)
           complex(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, j, lbbcsd, lorbdb, &
           lorglq, lorglqmin, lorglqopt, lorgqr, lorgqrmin, lorgqropt, lworkmin, lworkopt, &
                     r
           logical(lk) :: lquery, wantu1, wantu2, wantv1t
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           complex(dp) :: cdum(1_ilp,1_ilp)
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           lquery = ( lwork==-1_ilp ) .or. ( lrwork==-1_ilp )
           if( m < 0_ilp ) then
              info = -4_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -5_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -6_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -10_ilp
           else if( wantu1 .and. ldu1 < max( 1_ilp, p ) ) then
              info = -13_ilp
           else if( wantu2 .and. ldu2 < max( 1_ilp, m - p ) ) then
              info = -15_ilp
           else if( wantv1t .and. ldv1t < max( 1_ilp, q ) ) then
              info = -17_ilp
           end if
           r = min( p, m-p, q, m-q )
           ! compute workspace
             ! work layout:
           ! |-----------------------------------------|
           ! | lworkopt (1)                            |
           ! |-----------------------------------------|
           ! | taup1 (max(1,p))                        |
           ! | taup2 (max(1,m-p))                      |
           ! | tauq1 (max(1,q))                        |
           ! |-----------------------------------------|
           ! | stdlib_zunbdb work | stdlib_zungqr work | stdlib_zunglq work |
           ! |             |             |             |
           ! |             |             |             |
           ! |             |             |             |
           ! |             |             |             |
           ! |-----------------------------------------|
             ! rwork layout:
           ! |------------------|
           ! | lrworkopt (1)    |
           ! |------------------|
           ! | phi (max(1,r-1)) |
           ! |------------------|
           ! | b11d (r)         |
           ! | b11e (r-1)       |
           ! | b12d (r)         |
           ! | b12e (r-1)       |
           ! | b21d (r)         |
           ! | b21e (r-1)       |
           ! | b22d (r)         |
           ! | b22e (r-1)       |
           ! | stdlib_zbbcsd rwork     |
           ! |------------------|
           if( info == 0_ilp ) then
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, r-1 )
              ib11e = ib11d + max( 1_ilp, r )
              ib12d = ib11e + max( 1_ilp, r - 1_ilp )
              ib12e = ib12d + max( 1_ilp, r )
              ib21d = ib12e + max( 1_ilp, r - 1_ilp )
              ib21e = ib21d + max( 1_ilp, r )
              ib22d = ib21e + max( 1_ilp, r - 1_ilp )
              ib22e = ib22d + max( 1_ilp, r )
              ibbcsd = ib22e + max( 1_ilp, r - 1_ilp )
              itaup1 = 2_ilp
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m-p )
              iorbdb = itauq1 + max( 1_ilp, q )
              iorgqr = itauq1 + max( 1_ilp, q )
              iorglq = itauq1 + max( 1_ilp, q )
              lorgqrmin = 1_ilp
              lorgqropt = 1_ilp
              lorglqmin = 1_ilp
              lorglqopt = 1_ilp
              if( r == q ) then
                 call stdlib_zunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, work, -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_zungqr( p, p, q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 endif
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_zungqr( m-p, m-p, q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_zunglq( q-1, q-1, q-1, v1t, ldv1t,cdum, work(1_ilp), -1_ilp, childinfo )
                              
                    lorglqmin = max( lorglqmin, q-1 )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_zbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,dum, u1, ldu1,&
                  u2, ldu2, v1t, ldv1t, cdum, 1_ilp,dum, dum, dum, dum, dum, dum, dum, dum,rwork(1_ilp), -&
                            1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else if( r == p ) then
                 call stdlib_zunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_zungqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, cdum, work(1_ilp),-1_ilp, childinfo &
                              )
                    lorgqrmin = max( lorgqrmin, p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_zungqr( m-p, m-p, q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_zunglq( q, q, r, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_zbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,dum, v1t, &
                 ldv1t, cdum, 1_ilp, u1, ldu1, u2, ldu2,dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                           1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else if( r == m-p ) then
                 call stdlib_zunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_zungqr( p, p, q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_zungqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2, cdum,work(1_ilp), -1_ilp, &
                              childinfo )
                    lorgqrmin = max( lorgqrmin, m-p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_zunglq( q, q, r, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_zbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, dum, cdum,&
                  1_ilp, v1t, ldv1t, u2, ldu2, u1,ldu1, dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                            1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              else
                 call stdlib_zunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, dum,cdum, cdum, &
                           cdum, cdum, work(1_ilp), -1_ilp, childinfo)
                 lorbdb = m + int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_zungqr( p, p, m-q, u1, ldu1, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_zungqr( m-p, m-p, m-q, u2, ldu2, cdum, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_zunglq( q, q, q, v1t, ldv1t, cdum, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_zbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, dum, u2, &
                 ldu2, u1, ldu1, cdum, 1_ilp, v1t,ldv1t, dum, dum, dum, dum, dum, dum, dum, dum,rwork(&
                           1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( rwork(1_ilp),KIND=ilp)
              end if
              lrworkmin = ibbcsd+lbbcsd-1
              lrworkopt = lrworkmin
              rwork(1_ilp) = lrworkopt
              lworkmin = max( iorbdb+lorbdb-1,iorgqr+lorgqrmin-1,iorglq+lorglqmin-1 )
              lworkopt = max( iorbdb+lorbdb-1,iorgqr+lorgqropt-1,iorglq+lorglqopt-1 )
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
              if( lrwork < lrworkmin .and. .not.lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNCSD2BY1', -info )
              return
           else if( lquery ) then
              return
           end if
           lorgqr = lwork-iorgqr+1
           lorglq = lwork-iorglq+1
           ! handle four cases separately: r = q, r = p, r = m-p, and r = m-q,
           ! in which r = min(p,m-p,q,m-q)
           if( r == q ) then
              ! case 1: r = q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_zunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_zungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_zungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 v1t(1_ilp,1_ilp) = cone
                 do j = 2, q
                    v1t(1_ilp,j) = czero
                    v1t(j,1_ilp) = czero
                 end do
                 call stdlib_zlacpy( 'U', q-1, q-1, x21(1_ilp,2_ilp), ldx21, v1t(2_ilp,2_ilp),ldv1t )
                 call stdlib_zunglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglq, childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_zbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,rwork(iphi), u1, &
              ldu1, u2, ldu2, v1t, ldv1t, cdum,1_ilp, rwork(ib11d), rwork(ib11e), rwork(ib12d),rwork(&
              ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd),lrwork-&
                        ibbcsd+1, childinfo )
              ! permute rows and columns to place czero submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_zlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == p ) then
              ! case 2: r = p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_zunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 u1(1_ilp,1_ilp) = cone
                 do j = 2, p
                    u1(1_ilp,j) = czero
                    u1(j,1_ilp) = czero
                 end do
                 call stdlib_zlacpy( 'L', p-1, p-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp), ldu1 )
                 call stdlib_zungqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, work(itaup1),work(iorgqr), &
                           lorgqr, childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_zungqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_zlacpy( 'U', p, q, x11, ldx11, v1t, ldv1t )
                 call stdlib_zunglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_zbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,rwork(iphi), v1t,&
               ldv1t, cdum, 1_ilp, u1, ldu1, u2,ldu2, rwork(ib11d), rwork(ib11e), rwork(ib12d),rwork(&
               ib12e), rwork(ib21d), rwork(ib21e),rwork(ib22d), rwork(ib22e), rwork(ibbcsd), &
                         lbbcsd,childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_zlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == m-p ) then
              ! case 3: r = m-p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_zunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
                        itaup1), work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_zlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_zungqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 u2(1_ilp,1_ilp) = cone
                 do j = 2, m-p
                    u2(1_ilp,j) = czero
                    u2(j,1_ilp) = czero
                 end do
                 call stdlib_zlacpy( 'L', m-p-1, m-p-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_zungqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2,work(itaup2), work(iorgqr)&
                           , lorgqr, childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_zlacpy( 'U', m-p, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_zunglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_zbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, rwork(iphi), &
              cdum, 1_ilp, v1t, ldv1t, u2, ldu2,u1, ldu1, rwork(ib11d), rwork(ib11e),rwork(ib12d), &
              rwork(ib12e), rwork(ib21d),rwork(ib21e), rwork(ib22d), rwork(ib22e),rwork(ibbcsd), &
                        lbbcsd, childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > r ) then
                 do i = 1, r
                    iwork(i) = q - r + i
                 end do
                 do i = r + 1, q
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_zlapmt( .false., p, q, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_zlapmr( .false., q, q, v1t, ldv1t, iwork )
                 end if
              end if
           else
              ! case 4: r = m-q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_zunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,rwork(iphi), work(&
              itaup1), work(itaup2),work(itauq1), work(iorbdb), work(iorbdb+m),lorbdb-m, &
                        childinfo )
              ! accumulate householder reflectors
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_zcopy( m-p, work(iorbdb+p), 1_ilp, u2, 1_ilp )
              end if
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_zcopy( p, work(iorbdb), 1_ilp, u1, 1_ilp )
                 do j = 2, p
                    u1(1_ilp,j) = czero
                 end do
                 call stdlib_zlacpy( 'L', p-1, m-q-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp),ldu1 )
                 call stdlib_zungqr( p, p, m-q, u1, ldu1, work(itaup1),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 do j = 2, m-p
                    u2(1_ilp,j) = czero
                 end do
                 call stdlib_zlacpy( 'L', m-p-1, m-q-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_zungqr( m-p, m-p, m-q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_zlacpy( 'U', m-q, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_zlacpy( 'U', p-(m-q), q-(m-q), x11(m-q+1,m-q+1), ldx11,v1t(m-q+1,m-q+&
                           1_ilp), ldv1t )
                 call stdlib_zlacpy( 'U', -p+q, q-p, x21(m-q+1,p+1), ldx21,v1t(p+1,p+1), ldv1t )
                           
                 call stdlib_zunglq( q, q, q, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_zbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, rwork(iphi), &
              u2, ldu2, u1, ldu1, cdum, 1_ilp,v1t, ldv1t, rwork(ib11d), rwork(ib11e),rwork(ib12d), &
              rwork(ib12e), rwork(ib21d),rwork(ib21e), rwork(ib22d), rwork(ib22e),rwork(ibbcsd), &
                        lbbcsd, childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( p > r ) then
                 do i = 1, r
                    iwork(i) = p - r + i
                 end do
                 do i = r + 1, p
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_zlapmt( .false., p, p, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_zlapmr( .false., p, q, v1t, ldv1t, iwork )
                 end if
              end if
           end if
           return
     end subroutine stdlib_zuncsd2by1




     module subroutine stdlib_cunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
     !! CUNBDB simultaneously bidiagonalizes the blocks of an M-by-M
     !! partitioned unitary matrix X:
     !! [ B11 | B12 0  0 ]
     !! [ X11 | X12 ]   [ P1 |    ] [  0  |  0 -I  0 ] [ Q1 |    ]**H
     !! X = [-----------] = [---------] [----------------] [---------]   .
     !! [ X21 | X22 ]   [    | P2 ] [ B21 | B22 0  0 ] [    | Q2 ]
     !! [  0  |  0  0  I ]
     !! X11 is P-by-Q. Q must be no larger than P, M-P, or M-Q. (If this is
     !! not the case, then X must be transposed and/or permuted. This can be
     !! done in constant time using the TRANS and SIGNS options. See CUNCSD
     !! for details.)
     !! The unitary matrices P1, P2, Q1, and Q2 are P-by-P, (M-P)-by-
     !! (M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. They are
     !! represented implicitly by Householder vectors.
     !! B11, B12, B21, and B22 are Q-by-Q bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ====================================================================
           ! Parameters 
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery
           integer(ilp) :: i, lworkmin, lworkopt
           real(sp) :: z1, z2, z3, z4
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( .not. stdlib_lsame( signs, 'O' ) ) then
              z1 = one
              z2 = one
              z3 = one
              z4 = one
           else
              z1 = one
              z2 = -one
              z3 = one
              z4 = -one
           end if
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -3_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -4_ilp
           else if( q < 0_ilp .or. q > p .or. q > m-p .or.q > m-q ) then
              info = -5_ilp
           else if( colmajor .and. ldx11 < max( 1_ilp, p ) ) then
              info = -7_ilp
           else if( .not.colmajor .and. ldx11 < max( 1_ilp, q ) ) then
              info = -7_ilp
           else if( colmajor .and. ldx12 < max( 1_ilp, p ) ) then
              info = -9_ilp
           else if( .not.colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
              info = -9_ilp
           else if( colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
              info = -11_ilp
           else if( .not.colmajor .and. ldx21 < max( 1_ilp, q ) ) then
              info = -11_ilp
           else if( colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
              info = -13_ilp
           else if( .not.colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
              info = -13_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              lworkopt = m - q
              lworkmin = m - q
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'XORBDB', -info )
              return
           else if( lquery ) then
              return
           end if
           ! handle column-major and row-major separately
           if( colmajor ) then
              ! reduce columns 1, ..., q of x11, x12, x21, and x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_cscal( p-i+1, cmplx( z1, 0.0_sp,KIND=sp), x11(i,i), 1_ilp )
                 else
                    call stdlib_cscal( p-i+1, cmplx( z1*cos(phi(i-1)), 0.0_sp,KIND=sp),x11(i,i), &
                              1_ilp )
                    call stdlib_caxpy( p-i+1, cmplx( -z1*z3*z4*sin(phi(i-1)),0.0_sp,KIND=sp), x12(&
                              i,i-1), 1_ilp, x11(i,i), 1_ilp )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_cscal( m-p-i+1, cmplx( z2, 0.0_sp,KIND=sp), x21(i,i), 1_ilp )
                 else
                    call stdlib_cscal( m-p-i+1, cmplx( z2*cos(phi(i-1)), 0.0_sp,KIND=sp),x21(i,i),&
                               1_ilp )
                    call stdlib_caxpy( m-p-i+1, cmplx( -z2*z3*z4*sin(phi(i-1)),0.0_sp,KIND=sp), &
                              x22(i,i-1), 1_ilp, x21(i,i), 1_ilp )
                 end if
                 theta(i) = atan2( stdlib_scnrm2( m-p-i+1, x21(i,i), 1_ilp ),stdlib_scnrm2( p-i+1, &
                           x11(i,i), 1_ilp ) )
                 if( p > i ) then
                    call stdlib_clarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
                 else if ( p == i ) then
                    call stdlib_clarfgp( p-i+1, x11(i,i), x11(i,i), 1_ilp, taup1(i) )
                 end if
                 x11(i,i) = cone
                 if ( m-p > i ) then
                    call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp,taup2(i) )
                 else if ( m-p == i ) then
                    call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i,i), 1_ilp,taup2(i) )
                 end if
                 x21(i,i) = cone
                 if ( q > i ) then
                    call stdlib_clarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp,conjg(taup1(i)), x11(i,i+1), &
                              ldx11, work )
                    call stdlib_clarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp,conjg(taup2(i)), x21(i,i+1),&
                               ldx21, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_clarf( 'L', p-i+1, m-q-i+1, x11(i,i), 1_ilp,conjg(taup1(i)), x12(i,i),&
                               ldx12, work )
                    call stdlib_clarf( 'L', m-p-i+1, m-q-i+1, x21(i,i), 1_ilp,conjg(taup2(i)), x22(i,&
                              i), ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_cscal( q-i, cmplx( -z1*z3*sin(theta(i)), 0.0_sp,KIND=sp),x11(i,i+&
                              1_ilp), ldx11 )
                    call stdlib_caxpy( q-i, cmplx( z2*z3*cos(theta(i)), 0.0_sp,KIND=sp),x21(i,i+1)&
                              , ldx21, x11(i,i+1), ldx11 )
                 end if
                 call stdlib_cscal( m-q-i+1, cmplx( -z1*z4*sin(theta(i)), 0.0_sp,KIND=sp),x12(i,i)&
                           , ldx12 )
                 call stdlib_caxpy( m-q-i+1, cmplx( z2*z4*cos(theta(i)), 0.0_sp,KIND=sp),x22(i,i),&
                            ldx22, x12(i,i), ldx12 )
                 if( i < q )phi(i) = atan2( stdlib_scnrm2( q-i, x11(i,i+1), ldx11 ),stdlib_scnrm2(&
                            m-q-i+1, x12(i,i), ldx12 ) )
                 if( i < q ) then
                    call stdlib_clacgv( q-i, x11(i,i+1), ldx11 )
                    if ( i == q-1 ) then
                       call stdlib_clarfgp( q-i, x11(i,i+1), x11(i,i+1), ldx11,tauq1(i) )
                    else
                       call stdlib_clarfgp( q-i, x11(i,i+1), x11(i,i+2), ldx11,tauq1(i) )
                    end if
                    x11(i,i+1) = cone
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_clacgv( m-q-i+1, x12(i,i), ldx12 )
                    if ( m-q == i ) then
                       call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                    else
                       call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                                 
                    end if
                 end if
                 x12(i,i) = cone
                 if( i < q ) then
                    call stdlib_clarf( 'R', p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x11(i+1,i+1), &
                              ldx11, work )
                    call stdlib_clarf( 'R', m-p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 if ( p > i ) then
                    call stdlib_clarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if ( m-p > i ) then
                    call stdlib_clarf( 'R', m-p-i, m-q-i+1, x12(i,i), ldx12,tauq2(i), x22(i+1,i), &
                              ldx22, work )
                 end if
                 if( i < q )call stdlib_clacgv( q-i, x11(i,i+1), ldx11 )
                 call stdlib_clacgv( m-q-i+1, x12(i,i), ldx12 )
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_cscal( m-q-i+1, cmplx( -z1*z4, 0.0_sp,KIND=sp), x12(i,i),ldx12 )
                           
                 call stdlib_clacgv( m-q-i+1, x12(i,i), ldx12 )
                 if ( i >= m-q ) then
                    call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                 else
                    call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                 end if
                 x12(i,i) = cone
                 if ( p > i ) then
                    call stdlib_clarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_clarf( 'R', m-p-q, m-q-i+1, x12(i,i), ldx12,tauq2(i),&
                            x22(q+1,i), ldx22, work )
                 call stdlib_clacgv( m-q-i+1, x12(i,i), ldx12 )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_cscal( m-p-q-i+1, cmplx( z2*z4, 0.0_sp,KIND=sp),x22(q+i,p+i), ldx22 )
                           
                 call stdlib_clacgv( m-p-q-i+1, x22(q+i,p+i), ldx22 )
                 call stdlib_clarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i+1),ldx22, tauq2(p+i) )
                           
                 x22(q+i,p+i) = cone
                 call stdlib_clarf( 'R', m-p-q-i, m-p-q-i+1, x22(q+i,p+i), ldx22,tauq2(p+i), x22(&
                           q+i+1,p+i), ldx22, work )
                 call stdlib_clacgv( m-p-q-i+1, x22(q+i,p+i), ldx22 )
              end do
           else
              ! reduce columns 1, ..., q of x11, x12, x21, x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_cscal( p-i+1, cmplx( z1, 0.0_sp,KIND=sp), x11(i,i),ldx11 )
                 else
                    call stdlib_cscal( p-i+1, cmplx( z1*cos(phi(i-1)), 0.0_sp,KIND=sp),x11(i,i), &
                              ldx11 )
                    call stdlib_caxpy( p-i+1, cmplx( -z1*z3*z4*sin(phi(i-1)),0.0_sp,KIND=sp), x12(&
                              i-1,i), ldx12, x11(i,i), ldx11 )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_cscal( m-p-i+1, cmplx( z2, 0.0_sp,KIND=sp), x21(i,i),ldx21 )
                              
                 else
                    call stdlib_cscal( m-p-i+1, cmplx( z2*cos(phi(i-1)), 0.0_sp,KIND=sp),x21(i,i),&
                               ldx21 )
                    call stdlib_caxpy( m-p-i+1, cmplx( -z2*z3*z4*sin(phi(i-1)),0.0_sp,KIND=sp), &
                              x22(i-1,i), ldx22, x21(i,i), ldx21 )
                 end if
                 theta(i) = atan2( stdlib_scnrm2( m-p-i+1, x21(i,i), ldx21 ),stdlib_scnrm2( p-i+1,&
                            x11(i,i), ldx11 ) )
                 call stdlib_clacgv( p-i+1, x11(i,i), ldx11 )
                 call stdlib_clacgv( m-p-i+1, x21(i,i), ldx21 )
                 call stdlib_clarfgp( p-i+1, x11(i,i), x11(i,i+1), ldx11, taup1(i) )
                 x11(i,i) = cone
                 if ( i == m-p ) then
                    call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i,i), ldx21,taup2(i) )
                 else
                    call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i,i+1), ldx21,taup2(i) )
                 end if
                 x21(i,i) = cone
                 call stdlib_clarf( 'R', q-i, p-i+1, x11(i,i), ldx11, taup1(i),x11(i+1,i), ldx11, &
                           work )
                 call stdlib_clarf( 'R', m-q-i+1, p-i+1, x11(i,i), ldx11, taup1(i),x12(i,i), &
                           ldx12, work )
                 call stdlib_clarf( 'R', q-i, m-p-i+1, x21(i,i), ldx21, taup2(i),x21(i+1,i), &
                           ldx21, work )
                 call stdlib_clarf( 'R', m-q-i+1, m-p-i+1, x21(i,i), ldx21,taup2(i), x22(i,i), &
                           ldx22, work )
                 call stdlib_clacgv( p-i+1, x11(i,i), ldx11 )
                 call stdlib_clacgv( m-p-i+1, x21(i,i), ldx21 )
                 if( i < q ) then
                    call stdlib_cscal( q-i, cmplx( -z1*z3*sin(theta(i)), 0.0_sp,KIND=sp),x11(i+1,&
                              i), 1_ilp )
                    call stdlib_caxpy( q-i, cmplx( z2*z3*cos(theta(i)), 0.0_sp,KIND=sp),x21(i+1,i)&
                              , 1_ilp, x11(i+1,i), 1_ilp )
                 end if
                 call stdlib_cscal( m-q-i+1, cmplx( -z1*z4*sin(theta(i)), 0.0_sp,KIND=sp),x12(i,i)&
                           , 1_ilp )
                 call stdlib_caxpy( m-q-i+1, cmplx( z2*z4*cos(theta(i)), 0.0_sp,KIND=sp),x22(i,i),&
                            1_ilp, x12(i,i), 1_ilp )
                 if( i < q )phi(i) = atan2( stdlib_scnrm2( q-i, x11(i+1,i), 1_ilp ),stdlib_scnrm2( m-&
                           q-i+1, x12(i,i), 1_ilp ) )
                 if( i < q ) then
                    call stdlib_clarfgp( q-i, x11(i+1,i), x11(i+2,i), 1_ilp, tauq1(i) )
                    x11(i+1,i) = cone
                 end if
                 call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = cone
                 if( i < q ) then
                    call stdlib_clarf( 'L', q-i, p-i, x11(i+1,i), 1_ilp,conjg(tauq1(i)), x11(i+1,i+1),&
                               ldx11, work )
                    call stdlib_clarf( 'L', q-i, m-p-i, x11(i+1,i), 1_ilp,conjg(tauq1(i)), x21(i+1,i+&
                              1_ilp), ldx21, work )
                 end if
                 call stdlib_clarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp, conjg(tauq2(i)),x12(i,i+1), &
                           ldx12, work )
                 if ( m-p > i ) then
                    call stdlib_clarf( 'L', m-q-i+1, m-p-i, x12(i,i), 1_ilp,conjg(tauq2(i)), x22(i,i+&
                              1_ilp), ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_cscal( m-q-i+1, cmplx( -z1*z4, 0.0_sp,KIND=sp), x12(i,i), 1_ilp )
                 call stdlib_clarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = cone
                 if ( p > i ) then
                    call stdlib_clarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp,conjg(tauq2(i)), x12(i,i+1),&
                               ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_clarf( 'L', m-q-i+1, m-p-q, x12(i,i), 1_ilp,conjg(tauq2(&
                           i)), x22(i,q+1), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_cscal( m-p-q-i+1, cmplx( z2*z4, 0.0_sp,KIND=sp),x22(p+i,q+i), 1_ilp )
                           
                 call stdlib_clarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i+1,q+i), 1_ilp,tauq2(p+i) )
                           
                 x22(p+i,q+i) = cone
                 if ( m-p-q /= i ) then
                    call stdlib_clarf( 'L', m-p-q-i+1, m-p-q-i, x22(p+i,q+i), 1_ilp,conjg(tauq2(p+i)),&
                               x22(p+i,q+i+1), ldx22,work )
                 end if
              end do
           end if
           return
     end subroutine stdlib_cunbdb

     module subroutine stdlib_zunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
     !! ZUNBDB simultaneously bidiagonalizes the blocks of an M-by-M
     !! partitioned unitary matrix X:
     !! [ B11 | B12 0  0 ]
     !! [ X11 | X12 ]   [ P1 |    ] [  0  |  0 -I  0 ] [ Q1 |    ]**H
     !! X = [-----------] = [---------] [----------------] [---------]   .
     !! [ X21 | X22 ]   [    | P2 ] [ B21 | B22 0  0 ] [    | Q2 ]
     !! [  0  |  0  0  I ]
     !! X11 is P-by-Q. Q must be no larger than P, M-P, or M-Q. (If this is
     !! not the case, then X must be transposed and/or permuted. This can be
     !! done in constant time using the TRANS and SIGNS options. See ZUNCSD
     !! for details.)
     !! The unitary matrices P1, P2, Q1, and Q2 are P-by-P, (M-P)-by-
     !! (M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. They are
     !! represented implicitly by Householder vectors.
     !! B11, B12, B21, and B22 are Q-by-Q bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ====================================================================
           ! Parameters 
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery
           integer(ilp) :: i, lworkmin, lworkopt
           real(dp) :: z1, z2, z3, z4
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( .not. stdlib_lsame( signs, 'O' ) ) then
              z1 = one
              z2 = one
              z3 = one
              z4 = one
           else
              z1 = one
              z2 = -one
              z3 = one
              z4 = -one
           end if
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -3_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -4_ilp
           else if( q < 0_ilp .or. q > p .or. q > m-p .or.q > m-q ) then
              info = -5_ilp
           else if( colmajor .and. ldx11 < max( 1_ilp, p ) ) then
              info = -7_ilp
           else if( .not.colmajor .and. ldx11 < max( 1_ilp, q ) ) then
              info = -7_ilp
           else if( colmajor .and. ldx12 < max( 1_ilp, p ) ) then
              info = -9_ilp
           else if( .not.colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
              info = -9_ilp
           else if( colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
              info = -11_ilp
           else if( .not.colmajor .and. ldx21 < max( 1_ilp, q ) ) then
              info = -11_ilp
           else if( colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
              info = -13_ilp
           else if( .not.colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
              info = -13_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              lworkopt = m - q
              lworkmin = m - q
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'XORBDB', -info )
              return
           else if( lquery ) then
              return
           end if
           ! handle column-major and row-major separately
           if( colmajor ) then
              ! reduce columns 1, ..., q of x11, x12, x21, and x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_zscal( p-i+1, cmplx( z1, 0.0_dp,KIND=dp), x11(i,i), 1_ilp )
                 else
                    call stdlib_zscal( p-i+1, cmplx( z1*cos(phi(i-1)), 0.0_dp,KIND=dp),x11(i,i), &
                              1_ilp )
                    call stdlib_zaxpy( p-i+1, cmplx( -z1*z3*z4*sin(phi(i-1)),0.0_dp,KIND=dp), x12(&
                              i,i-1), 1_ilp, x11(i,i), 1_ilp )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_zscal( m-p-i+1, cmplx( z2, 0.0_dp,KIND=dp), x21(i,i), 1_ilp )
                 else
                    call stdlib_zscal( m-p-i+1, cmplx( z2*cos(phi(i-1)), 0.0_dp,KIND=dp),x21(i,i),&
                               1_ilp )
                    call stdlib_zaxpy( m-p-i+1, cmplx( -z2*z3*z4*sin(phi(i-1)),0.0_dp,KIND=dp), &
                              x22(i,i-1), 1_ilp, x21(i,i), 1_ilp )
                 end if
                 theta(i) = atan2( stdlib_dznrm2( m-p-i+1, x21(i,i), 1_ilp ),stdlib_dznrm2( p-i+1, &
                           x11(i,i), 1_ilp ) )
                 if( p > i ) then
                    call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
                 else if ( p == i ) then
                    call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i,i), 1_ilp, taup1(i) )
                 end if
                 x11(i,i) = cone
                 if ( m-p > i ) then
                    call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp,taup2(i) )
                 else if ( m-p == i ) then
                    call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i,i), 1_ilp,taup2(i) )
                 end if
                 x21(i,i) = cone
                 if ( q > i ) then
                    call stdlib_zlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp,conjg(taup1(i)), x11(i,i+1), &
                              ldx11, work )
                    call stdlib_zlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp,conjg(taup2(i)), x21(i,i+1),&
                               ldx21, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_zlarf( 'L', p-i+1, m-q-i+1, x11(i,i), 1_ilp,conjg(taup1(i)), x12(i,i),&
                               ldx12, work )
                    call stdlib_zlarf( 'L', m-p-i+1, m-q-i+1, x21(i,i), 1_ilp,conjg(taup2(i)), x22(i,&
                              i), ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_zscal( q-i, cmplx( -z1*z3*sin(theta(i)), 0.0_dp,KIND=dp),x11(i,i+&
                              1_ilp), ldx11 )
                    call stdlib_zaxpy( q-i, cmplx( z2*z3*cos(theta(i)), 0.0_dp,KIND=dp),x21(i,i+1)&
                              , ldx21, x11(i,i+1), ldx11 )
                 end if
                 call stdlib_zscal( m-q-i+1, cmplx( -z1*z4*sin(theta(i)), 0.0_dp,KIND=dp),x12(i,i)&
                           , ldx12 )
                 call stdlib_zaxpy( m-q-i+1, cmplx( z2*z4*cos(theta(i)), 0.0_dp,KIND=dp),x22(i,i),&
                            ldx22, x12(i,i), ldx12 )
                 if( i < q )phi(i) = atan2( stdlib_dznrm2( q-i, x11(i,i+1), ldx11 ),stdlib_dznrm2(&
                            m-q-i+1, x12(i,i), ldx12 ) )
                 if( i < q ) then
                    call stdlib_zlacgv( q-i, x11(i,i+1), ldx11 )
                    if ( i == q-1 ) then
                       call stdlib_zlarfgp( q-i, x11(i,i+1), x11(i,i+1), ldx11,tauq1(i) )
                    else
                       call stdlib_zlarfgp( q-i, x11(i,i+1), x11(i,i+2), ldx11,tauq1(i) )
                    end if
                    x11(i,i+1) = cone
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_zlacgv( m-q-i+1, x12(i,i), ldx12 )
                    if ( m-q == i ) then
                       call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                    else
                       call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                                 
                    end if
                 end if
                 x12(i,i) = cone
                 if( i < q ) then
                    call stdlib_zlarf( 'R', p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x11(i+1,i+1), &
                              ldx11, work )
                    call stdlib_zlarf( 'R', m-p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 if ( p > i ) then
                    call stdlib_zlarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if ( m-p > i ) then
                    call stdlib_zlarf( 'R', m-p-i, m-q-i+1, x12(i,i), ldx12,tauq2(i), x22(i+1,i), &
                              ldx22, work )
                 end if
                 if( i < q )call stdlib_zlacgv( q-i, x11(i,i+1), ldx11 )
                 call stdlib_zlacgv( m-q-i+1, x12(i,i), ldx12 )
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_zscal( m-q-i+1, cmplx( -z1*z4, 0.0_dp,KIND=dp), x12(i,i),ldx12 )
                           
                 call stdlib_zlacgv( m-q-i+1, x12(i,i), ldx12 )
                 if ( i >= m-q ) then
                    call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                 else
                    call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                 end if
                 x12(i,i) = cone
                 if ( p > i ) then
                    call stdlib_zlarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_zlarf( 'R', m-p-q, m-q-i+1, x12(i,i), ldx12,tauq2(i),&
                            x22(q+1,i), ldx22, work )
                 call stdlib_zlacgv( m-q-i+1, x12(i,i), ldx12 )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_zscal( m-p-q-i+1, cmplx( z2*z4, 0.0_dp,KIND=dp),x22(q+i,p+i), ldx22 )
                           
                 call stdlib_zlacgv( m-p-q-i+1, x22(q+i,p+i), ldx22 )
                 call stdlib_zlarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i+1),ldx22, tauq2(p+i) )
                           
                 x22(q+i,p+i) = cone
                 call stdlib_zlarf( 'R', m-p-q-i, m-p-q-i+1, x22(q+i,p+i), ldx22,tauq2(p+i), x22(&
                           q+i+1,p+i), ldx22, work )
                 call stdlib_zlacgv( m-p-q-i+1, x22(q+i,p+i), ldx22 )
              end do
           else
              ! reduce columns 1, ..., q of x11, x12, x21, x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_zscal( p-i+1, cmplx( z1, 0.0_dp,KIND=dp), x11(i,i),ldx11 )
                 else
                    call stdlib_zscal( p-i+1, cmplx( z1*cos(phi(i-1)), 0.0_dp,KIND=dp),x11(i,i), &
                              ldx11 )
                    call stdlib_zaxpy( p-i+1, cmplx( -z1*z3*z4*sin(phi(i-1)),0.0_dp,KIND=dp), x12(&
                              i-1,i), ldx12, x11(i,i), ldx11 )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_zscal( m-p-i+1, cmplx( z2, 0.0_dp,KIND=dp), x21(i,i),ldx21 )
                              
                 else
                    call stdlib_zscal( m-p-i+1, cmplx( z2*cos(phi(i-1)), 0.0_dp,KIND=dp),x21(i,i),&
                               ldx21 )
                    call stdlib_zaxpy( m-p-i+1, cmplx( -z2*z3*z4*sin(phi(i-1)),0.0_dp,KIND=dp), &
                              x22(i-1,i), ldx22, x21(i,i), ldx21 )
                 end if
                 theta(i) = atan2( stdlib_dznrm2( m-p-i+1, x21(i,i), ldx21 ),stdlib_dznrm2( p-i+1,&
                            x11(i,i), ldx11 ) )
                 call stdlib_zlacgv( p-i+1, x11(i,i), ldx11 )
                 call stdlib_zlacgv( m-p-i+1, x21(i,i), ldx21 )
                 call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i,i+1), ldx11, taup1(i) )
                 x11(i,i) = cone
                 if ( i == m-p ) then
                    call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i,i), ldx21,taup2(i) )
                 else
                    call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i,i+1), ldx21,taup2(i) )
                 end if
                 x21(i,i) = cone
                 call stdlib_zlarf( 'R', q-i, p-i+1, x11(i,i), ldx11, taup1(i),x11(i+1,i), ldx11, &
                           work )
                 call stdlib_zlarf( 'R', m-q-i+1, p-i+1, x11(i,i), ldx11, taup1(i),x12(i,i), &
                           ldx12, work )
                 call stdlib_zlarf( 'R', q-i, m-p-i+1, x21(i,i), ldx21, taup2(i),x21(i+1,i), &
                           ldx21, work )
                 call stdlib_zlarf( 'R', m-q-i+1, m-p-i+1, x21(i,i), ldx21,taup2(i), x22(i,i), &
                           ldx22, work )
                 call stdlib_zlacgv( p-i+1, x11(i,i), ldx11 )
                 call stdlib_zlacgv( m-p-i+1, x21(i,i), ldx21 )
                 if( i < q ) then
                    call stdlib_zscal( q-i, cmplx( -z1*z3*sin(theta(i)), 0.0_dp,KIND=dp),x11(i+1,&
                              i), 1_ilp )
                    call stdlib_zaxpy( q-i, cmplx( z2*z3*cos(theta(i)), 0.0_dp,KIND=dp),x21(i+1,i)&
                              , 1_ilp, x11(i+1,i), 1_ilp )
                 end if
                 call stdlib_zscal( m-q-i+1, cmplx( -z1*z4*sin(theta(i)), 0.0_dp,KIND=dp),x12(i,i)&
                           , 1_ilp )
                 call stdlib_zaxpy( m-q-i+1, cmplx( z2*z4*cos(theta(i)), 0.0_dp,KIND=dp),x22(i,i),&
                            1_ilp, x12(i,i), 1_ilp )
                 if( i < q )phi(i) = atan2( stdlib_dznrm2( q-i, x11(i+1,i), 1_ilp ),stdlib_dznrm2( m-&
                           q-i+1, x12(i,i), 1_ilp ) )
                 if( i < q ) then
                    call stdlib_zlarfgp( q-i, x11(i+1,i), x11(i+2,i), 1_ilp, tauq1(i) )
                    x11(i+1,i) = cone
                 end if
                 call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = cone
                 if( i < q ) then
                    call stdlib_zlarf( 'L', q-i, p-i, x11(i+1,i), 1_ilp,conjg(tauq1(i)), x11(i+1,i+1),&
                               ldx11, work )
                    call stdlib_zlarf( 'L', q-i, m-p-i, x11(i+1,i), 1_ilp,conjg(tauq1(i)), x21(i+1,i+&
                              1_ilp), ldx21, work )
                 end if
                 call stdlib_zlarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp,conjg(tauq2(i)), x12(i,i+1), &
                           ldx12, work )
                 if ( m-p > i ) then
                    call stdlib_zlarf( 'L', m-q-i+1, m-p-i, x12(i,i), 1_ilp,conjg(tauq2(i)), x22(i,i+&
                              1_ilp), ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_zscal( m-q-i+1, cmplx( -z1*z4, 0.0_dp,KIND=dp), x12(i,i), 1_ilp )
                 call stdlib_zlarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = cone
                 if ( p > i ) then
                    call stdlib_zlarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp,conjg(tauq2(i)), x12(i,i+1),&
                               ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_zlarf( 'L', m-q-i+1, m-p-q, x12(i,i), 1_ilp,conjg(tauq2(&
                           i)), x22(i,q+1), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_zscal( m-p-q-i+1, cmplx( z2*z4, 0.0_dp,KIND=dp),x22(p+i,q+i), 1_ilp )
                           
                 call stdlib_zlarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i+1,q+i), 1_ilp,tauq2(p+i) )
                           
                 x22(p+i,q+i) = cone
                 if ( m-p-q /= i ) then
                    call stdlib_zlarf( 'L', m-p-q-i+1, m-p-q-i, x22(p+i,q+i), 1_ilp,conjg(tauq2(p+i)),&
                               x22(p+i,q+i+1), ldx22,work )
                 end if
              end do
           end if
           return
     end subroutine stdlib_zunbdb




     module subroutine stdlib_cunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! CUNBDB1 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. Q must be no larger than P,
     !! M-P, or M-Q. Routines CUNBDB2, CUNBDB3, and CUNBDB4 handle cases in
     !! which Q is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are Q-by-Q bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < q .or. m-p < q ) then
              info = -2_ilp
           else if( q < 0_ilp .or. m-q < q ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-2
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB1', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., q of x11 and x21
           do i = 1, q
              call stdlib_clarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              theta(i) = atan2( real( x21(i,i),KIND=sp), real( x11(i,i),KIND=sp) )
              c = cos( theta(i) )
              s = sin( theta(i) )
              x11(i,i) = cone
              x21(i,i) = cone
              call stdlib_clarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
              call stdlib_clarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
              if( i < q ) then
                 call stdlib_csrot( q-i, x11(i,i+1), ldx11, x21(i,i+1), ldx21, c,s )
                 call stdlib_clacgv( q-i, x21(i,i+1), ldx21 )
                 call stdlib_clarfgp( q-i, x21(i,i+1), x21(i,i+2), ldx21, tauq1(i) )
                 s = real( x21(i,i+1),KIND=sp)
                 x21(i,i+1) = cone
                 call stdlib_clarf( 'R', p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
                 call stdlib_clarf( 'R', m-p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x21(i+1,i+1), &
                           ldx21, work(ilarf) )
                 call stdlib_clacgv( q-i, x21(i,i+1), ldx21 )
                 c = sqrt( stdlib_scnrm2( p-i, x11(i+1,i+1), 1_ilp )**2_ilp+ stdlib_scnrm2( m-p-i, x21(i+&
                           1_ilp,i+1), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
                 call stdlib_cunbdb5( p-i, m-p-i, q-i-1, x11(i+1,i+1), 1_ilp,x21(i+1,i+1), 1_ilp, x11(i+1,&
                           i+2), ldx11,x21(i+1,i+2), ldx21, work(iorbdb5), lorbdb5,childinfo )
              end if
           end do
           return
     end subroutine stdlib_cunbdb1

     module subroutine stdlib_zunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! ZUNBDB1 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. Q must be no larger than P,
     !! M-P, or M-Q. Routines ZUNBDB2, ZUNBDB3, and ZUNBDB4 handle cases in
     !! which Q is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are Q-by-Q bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < q .or. m-p < q ) then
              info = -2_ilp
           else if( q < 0_ilp .or. m-q < q ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-2
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB1', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., q of x11 and x21
           do i = 1, q
              call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              theta(i) = atan2( real( x21(i,i),KIND=dp), real( x11(i,i),KIND=dp) )
              c = cos( theta(i) )
              s = sin( theta(i) )
              x11(i,i) = cone
              x21(i,i) = cone
              call stdlib_zlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
              call stdlib_zlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
              if( i < q ) then
                 call stdlib_zdrot( q-i, x11(i,i+1), ldx11, x21(i,i+1), ldx21, c,s )
                 call stdlib_zlacgv( q-i, x21(i,i+1), ldx21 )
                 call stdlib_zlarfgp( q-i, x21(i,i+1), x21(i,i+2), ldx21, tauq1(i) )
                 s = real( x21(i,i+1),KIND=dp)
                 x21(i,i+1) = cone
                 call stdlib_zlarf( 'R', p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
                 call stdlib_zlarf( 'R', m-p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x21(i+1,i+1), &
                           ldx21, work(ilarf) )
                 call stdlib_zlacgv( q-i, x21(i,i+1), ldx21 )
                 c = sqrt( stdlib_dznrm2( p-i, x11(i+1,i+1), 1_ilp )**2_ilp+ stdlib_dznrm2( m-p-i, x21(i+&
                           1_ilp,i+1), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
                 call stdlib_zunbdb5( p-i, m-p-i, q-i-1, x11(i+1,i+1), 1_ilp,x21(i+1,i+1), 1_ilp, x11(i+1,&
                           i+2), ldx11,x21(i+1,i+2), ldx21, work(iorbdb5), lorbdb5,childinfo )
              end if
           end do
           return
     end subroutine stdlib_zunbdb1




     module subroutine stdlib_cunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! CUNBDB2 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. P must be no larger than M-P,
     !! Q, or M-Q. Routines CUNBDB1, CUNBDB3, and CUNBDB4 handle cases in
     !! which P is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are P-by-P bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < 0_ilp .or. p > m-p ) then
              info = -2_ilp
           else if( q < 0_ilp .or. q < p .or. m-q < p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB2', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., p of x11 and x21
           do i = 1, p
              if( i > 1_ilp ) then
                 call stdlib_csrot( q-i+1, x11(i,i), ldx11, x21(i-1,i), ldx21, c,s )
              end if
              call stdlib_clacgv( q-i+1, x11(i,i), ldx11 )
              call stdlib_clarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              c = real( x11(i,i),KIND=sp)
              x11(i,i) = cone
              call stdlib_clarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_clarf( 'R', m-p-i+1, q-i+1, x11(i,i), ldx11, tauq1(i),x21(i,i), ldx21, &
                        work(ilarf) )
              call stdlib_clacgv( q-i+1, x11(i,i), ldx11 )
              s = sqrt( stdlib_scnrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_scnrm2( m-p-i+1, x21(i,i), &
                        1_ilp )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_cunbdb5( p-i, m-p-i+1, q-i, x11(i+1,i), 1_ilp, x21(i,i), 1_ilp,x11(i+1,i+1), &
                        ldx11, x21(i,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_cscal( p-i, cnegone, x11(i+1,i), 1_ilp )
              call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              if( i < p ) then
                 call stdlib_clarfgp( p-i, x11(i+1,i), x11(i+2,i), 1_ilp, taup1(i) )
                 phi(i) = atan2( real( x11(i+1,i),KIND=sp), real( x21(i,i),KIND=sp) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x11(i+1,i) = cone
                 call stdlib_clarf( 'L', p-i, q-i, x11(i+1,i), 1_ilp, conjg(taup1(i)),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
              end if
              x21(i,i) = cone
              call stdlib_clarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to the identity matrix
           do i = p + 1, q
              call stdlib_clarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              x21(i,i) = cone
              call stdlib_clarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
           end do
           return
     end subroutine stdlib_cunbdb2

     module subroutine stdlib_zunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! ZUNBDB2 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. P must be no larger than M-P,
     !! Q, or M-Q. Routines ZUNBDB1, ZUNBDB3, and ZUNBDB4 handle cases in
     !! which P is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are P-by-P bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < 0_ilp .or. p > m-p ) then
              info = -2_ilp
           else if( q < 0_ilp .or. q < p .or. m-q < p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB2', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., p of x11 and x21
           do i = 1, p
              if( i > 1_ilp ) then
                 call stdlib_zdrot( q-i+1, x11(i,i), ldx11, x21(i-1,i), ldx21, c,s )
              end if
              call stdlib_zlacgv( q-i+1, x11(i,i), ldx11 )
              call stdlib_zlarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              c = real( x11(i,i),KIND=dp)
              x11(i,i) = cone
              call stdlib_zlarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_zlarf( 'R', m-p-i+1, q-i+1, x11(i,i), ldx11, tauq1(i),x21(i,i), ldx21, &
                        work(ilarf) )
              call stdlib_zlacgv( q-i+1, x11(i,i), ldx11 )
              s = sqrt( stdlib_dznrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_dznrm2( m-p-i+1, x21(i,i), &
                        1_ilp )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_zunbdb5( p-i, m-p-i+1, q-i, x11(i+1,i), 1_ilp, x21(i,i), 1_ilp,x11(i+1,i+1), &
                        ldx11, x21(i,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_zscal( p-i, cnegone, x11(i+1,i), 1_ilp )
              call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              if( i < p ) then
                 call stdlib_zlarfgp( p-i, x11(i+1,i), x11(i+2,i), 1_ilp, taup1(i) )
                 phi(i) = atan2( real( x11(i+1,i),KIND=dp), real( x21(i,i),KIND=dp) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x11(i+1,i) = cone
                 call stdlib_zlarf( 'L', p-i, q-i, x11(i+1,i), 1_ilp, conjg(taup1(i)),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
              end if
              x21(i,i) = cone
              call stdlib_zlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to the identity matrix
           do i = p + 1, q
              call stdlib_zlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              x21(i,i) = cone
              call stdlib_zlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, conjg(taup2(i)),x21(i,i+1), &
                        ldx21, work(ilarf) )
           end do
           return
     end subroutine stdlib_zunbdb2




     module subroutine stdlib_cunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! CUNBDB3 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-P must be no larger than P,
     !! Q, or M-Q. Routines CUNBDB1, CUNBDB2, and CUNBDB4 handle cases in
     !! which M-P is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-P)-by-(M-P) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( 2_ilp*p < m .or. p > m ) then
              info = -2_ilp
           else if( q < m-p .or. m-q < m-p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., m-p of x11 and x21
           do i = 1, m-p
              if( i > 1_ilp ) then
                 call stdlib_csrot( q-i+1, x11(i-1,i), ldx11, x21(i,i), ldx11, c,s )
              end if
              call stdlib_clacgv( q-i+1, x21(i,i), ldx21 )
              call stdlib_clarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              s = real( x21(i,i),KIND=sp)
              x21(i,i) = cone
              call stdlib_clarf( 'R', p-i+1, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i,i), ldx11, &
                        work(ilarf) )
              call stdlib_clarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_clacgv( q-i+1, x21(i,i), ldx21 )
              c = sqrt( stdlib_scnrm2( p-i+1, x11(i,i), 1_ilp )**2_ilp+ stdlib_scnrm2( m-p-i, x21(i+1,i), &
                        1_ilp )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_cunbdb5( p-i+1, m-p-i, q-i, x11(i,i), 1_ilp, x21(i+1,i), 1_ilp,x11(i,i+1), &
                        ldx11, x21(i+1,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_clarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              if( i < m-p ) then
                 call stdlib_clarfgp( m-p-i, x21(i+1,i), x21(i+2,i), 1_ilp, taup2(i) )
                 phi(i) = atan2( real( x21(i+1,i),KIND=sp), real( x11(i,i),KIND=sp) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x21(i+1,i) = cone
                 call stdlib_clarf( 'L', m-p-i, q-i, x21(i+1,i), 1_ilp, conjg(taup2(i)),x21(i+1,i+1), &
                           ldx21, work(ilarf) )
              end if
              x11(i,i) = cone
              call stdlib_clarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
           end do
           ! reduce the bottom-right portion of x11 to the identity matrix
           do i = m-p + 1, q
              call stdlib_clarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              x11(i,i) = cone
              call stdlib_clarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
           end do
           return
     end subroutine stdlib_cunbdb3

     module subroutine stdlib_zunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! ZUNBDB3 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-P must be no larger than P,
     !! Q, or M-Q. Routines ZUNBDB1, ZUNBDB2, and ZUNBDB4 handle cases in
     !! which M-P is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-P)-by-(M-P) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( 2_ilp*p < m .or. p > m ) then
              info = -2_ilp
           else if( q < m-p .or. m-q < m-p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., m-p of x11 and x21
           do i = 1, m-p
              if( i > 1_ilp ) then
                 call stdlib_zdrot( q-i+1, x11(i-1,i), ldx11, x21(i,i), ldx11, c,s )
              end if
              call stdlib_zlacgv( q-i+1, x21(i,i), ldx21 )
              call stdlib_zlarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              s = real( x21(i,i),KIND=dp)
              x21(i,i) = cone
              call stdlib_zlarf( 'R', p-i+1, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i,i), ldx11, &
                        work(ilarf) )
              call stdlib_zlarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_zlacgv( q-i+1, x21(i,i), ldx21 )
              c = sqrt( stdlib_dznrm2( p-i+1, x11(i,i), 1_ilp )**2_ilp+ stdlib_dznrm2( m-p-i, x21(i+1,i), &
                        1_ilp )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_zunbdb5( p-i+1, m-p-i, q-i, x11(i,i), 1_ilp, x21(i+1,i), 1_ilp,x11(i,i+1), &
                        ldx11, x21(i+1,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              if( i < m-p ) then
                 call stdlib_zlarfgp( m-p-i, x21(i+1,i), x21(i+2,i), 1_ilp, taup2(i) )
                 phi(i) = atan2( real( x21(i+1,i),KIND=dp), real( x11(i,i),KIND=dp) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x21(i+1,i) = cone
                 call stdlib_zlarf( 'L', m-p-i, q-i, x21(i+1,i), 1_ilp,conjg(taup2(i)), x21(i+1,i+1), &
                           ldx21,work(ilarf) )
              end if
              x11(i,i) = cone
              call stdlib_zlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
           end do
           ! reduce the bottom-right portion of x11 to the identity matrix
           do i = m-p + 1, q
              call stdlib_zlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              x11(i,i) = cone
              call stdlib_zlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, conjg(taup1(i)),x11(i,i+1), ldx11, &
                        work(ilarf) )
           end do
           return
     end subroutine stdlib_zunbdb3




     module subroutine stdlib_cunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! CUNBDB4 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-Q must be no larger than P,
     !! M-P, or Q. Routines CUNBDB1, CUNBDB2, and CUNBDB3 handle cases in
     !! which M-Q is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-Q)-by-(M-Q) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               phantom, work, lwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, j, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < m-q .or. m-p < m-q ) then
              info = -2_ilp
           else if( q < m-q .or. q > m ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( q-1, p-1, m-p-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q
              lworkopt = ilarf + llarf - 1_ilp
              lworkopt = max( lworkopt, iorbdb5 + lorbdb5 - 1_ilp )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB4', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., m-q of x11 and x21
           do i = 1, m-q
              if( i == 1_ilp ) then
                 do j = 1, m
                    phantom(j) = czero
                 end do
                 call stdlib_cunbdb5( p, m-p, q, phantom(1_ilp), 1_ilp, phantom(p+1), 1_ilp,x11, ldx11, x21, &
                           ldx21, work(iorbdb5),lorbdb5, childinfo )
                 call stdlib_cscal( p, cnegone, phantom(1_ilp), 1_ilp )
                 call stdlib_clarfgp( p, phantom(1_ilp), phantom(2_ilp), 1_ilp, taup1(1_ilp) )
                 call stdlib_clarfgp( m-p, phantom(p+1), phantom(p+2), 1_ilp, taup2(1_ilp) )
                 theta(i) = atan2( real( phantom(1_ilp),KIND=sp), real( phantom(p+1),KIND=sp) )
                           
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 phantom(1_ilp) = cone
                 phantom(p+1) = cone
                 call stdlib_clarf( 'L', p, q, phantom(1_ilp), 1_ilp, conjg(taup1(1_ilp)), x11,ldx11, work(&
                           ilarf) )
                 call stdlib_clarf( 'L', m-p, q, phantom(p+1), 1_ilp, conjg(taup2(1_ilp)),x21, ldx21, &
                           work(ilarf) )
              else
                 call stdlib_cunbdb5( p-i+1, m-p-i+1, q-i+1, x11(i,i-1), 1_ilp,x21(i,i-1), 1_ilp, x11(i,i)&
                           , ldx11, x21(i,i),ldx21, work(iorbdb5), lorbdb5, childinfo )
                 call stdlib_cscal( p-i+1, cnegone, x11(i,i-1), 1_ilp )
                 call stdlib_clarfgp( p-i+1, x11(i,i-1), x11(i+1,i-1), 1_ilp, taup1(i) )
                 call stdlib_clarfgp( m-p-i+1, x21(i,i-1), x21(i+1,i-1), 1_ilp,taup2(i) )
                 theta(i) = atan2( real( x11(i,i-1),KIND=sp), real( x21(i,i-1),KIND=sp) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 x11(i,i-1) = cone
                 x21(i,i-1) = cone
                 call stdlib_clarf( 'L', p-i+1, q-i+1, x11(i,i-1), 1_ilp,conjg(taup1(i)), x11(i,i), &
                           ldx11, work(ilarf) )
                 call stdlib_clarf( 'L', m-p-i+1, q-i+1, x21(i,i-1), 1_ilp,conjg(taup2(i)), x21(i,i), &
                           ldx21, work(ilarf) )
              end if
              call stdlib_csrot( q-i+1, x11(i,i), ldx11, x21(i,i), ldx21, s, -c )
              call stdlib_clacgv( q-i+1, x21(i,i), ldx21 )
              call stdlib_clarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              c = real( x21(i,i),KIND=sp)
              x21(i,i) = cone
              call stdlib_clarf( 'R', p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_clarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_clacgv( q-i+1, x21(i,i), ldx21 )
              if( i < m-q ) then
                 s = sqrt( stdlib_scnrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_scnrm2( m-p-i, x21(i+1,&
                           i), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
              end if
           end do
           ! reduce the bottom-right portion of x11 to [ i 0 ]
           do i = m - q + 1, p
              call stdlib_clacgv( q-i+1, x11(i,i), ldx11 )
              call stdlib_clarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              x11(i,i) = cone
              call stdlib_clarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_clarf( 'R', q-p, q-i+1, x11(i,i), ldx11, tauq1(i),x21(m-q+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_clacgv( q-i+1, x11(i,i), ldx11 )
           end do
           ! reduce the bottom-right portion of x21 to [ 0 i ]
           do i = p + 1, q
              call stdlib_clacgv( q-i+1, x21(m-q+i-p,i), ldx21 )
              call stdlib_clarfgp( q-i+1, x21(m-q+i-p,i), x21(m-q+i-p,i+1), ldx21,tauq1(i) )
                        
              x21(m-q+i-p,i) = cone
              call stdlib_clarf( 'R', q-i, q-i+1, x21(m-q+i-p,i), ldx21, tauq1(i),x21(m-q+i-p+1,i)&
                        , ldx21, work(ilarf) )
              call stdlib_clacgv( q-i+1, x21(m-q+i-p,i), ldx21 )
           end do
           return
     end subroutine stdlib_cunbdb4

     module subroutine stdlib_zunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! ZUNBDB4 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-Q must be no larger than P,
     !! M-P, or Q. Routines ZUNBDB1, ZUNBDB2, and ZUNBDB3 handle cases in
     !! which M-Q is not the minimum dimension.
     !! The unitary matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-Q)-by-(M-Q) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               phantom, work, lwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, j, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < m-q .or. m-p < m-q ) then
              info = -2_ilp
           else if( q < m-q .or. q > m ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( q-1, p-1, m-p-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q
              lworkopt = ilarf + llarf - 1_ilp
              lworkopt = max( lworkopt, iorbdb5 + lorbdb5 - 1_ilp )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB4', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., m-q of x11 and x21
           do i = 1, m-q
              if( i == 1_ilp ) then
                 do j = 1, m
                    phantom(j) = czero
                 end do
                 call stdlib_zunbdb5( p, m-p, q, phantom(1_ilp), 1_ilp, phantom(p+1), 1_ilp,x11, ldx11, x21, &
                           ldx21, work(iorbdb5),lorbdb5, childinfo )
                 call stdlib_zscal( p, cnegone, phantom(1_ilp), 1_ilp )
                 call stdlib_zlarfgp( p, phantom(1_ilp), phantom(2_ilp), 1_ilp, taup1(1_ilp) )
                 call stdlib_zlarfgp( m-p, phantom(p+1), phantom(p+2), 1_ilp, taup2(1_ilp) )
                 theta(i) = atan2( real( phantom(1_ilp),KIND=dp), real( phantom(p+1),KIND=dp) )
                           
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 phantom(1_ilp) = cone
                 phantom(p+1) = cone
                 call stdlib_zlarf( 'L', p, q, phantom(1_ilp), 1_ilp, conjg(taup1(1_ilp)), x11,ldx11, work(&
                           ilarf) )
                 call stdlib_zlarf( 'L', m-p, q, phantom(p+1), 1_ilp, conjg(taup2(1_ilp)),x21, ldx21, &
                           work(ilarf) )
              else
                 call stdlib_zunbdb5( p-i+1, m-p-i+1, q-i+1, x11(i,i-1), 1_ilp,x21(i,i-1), 1_ilp, x11(i,i)&
                           , ldx11, x21(i,i),ldx21, work(iorbdb5), lorbdb5, childinfo )
                 call stdlib_zscal( p-i+1, cnegone, x11(i,i-1), 1_ilp )
                 call stdlib_zlarfgp( p-i+1, x11(i,i-1), x11(i+1,i-1), 1_ilp, taup1(i) )
                 call stdlib_zlarfgp( m-p-i+1, x21(i,i-1), x21(i+1,i-1), 1_ilp,taup2(i) )
                 theta(i) = atan2( real( x11(i,i-1),KIND=dp), real( x21(i,i-1),KIND=dp) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 x11(i,i-1) = cone
                 x21(i,i-1) = cone
                 call stdlib_zlarf( 'L', p-i+1, q-i+1, x11(i,i-1), 1_ilp,conjg(taup1(i)), x11(i,i), &
                           ldx11, work(ilarf) )
                 call stdlib_zlarf( 'L', m-p-i+1, q-i+1, x21(i,i-1), 1_ilp,conjg(taup2(i)), x21(i,i), &
                           ldx21, work(ilarf) )
              end if
              call stdlib_zdrot( q-i+1, x11(i,i), ldx11, x21(i,i), ldx21, s, -c )
              call stdlib_zlacgv( q-i+1, x21(i,i), ldx21 )
              call stdlib_zlarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              c = real( x21(i,i),KIND=dp)
              x21(i,i) = cone
              call stdlib_zlarf( 'R', p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_zlarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_zlacgv( q-i+1, x21(i,i), ldx21 )
              if( i < m-q ) then
                 s = sqrt( stdlib_dznrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_dznrm2( m-p-i, x21(i+1,&
                           i), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
              end if
           end do
           ! reduce the bottom-right portion of x11 to [ i 0 ]
           do i = m - q + 1, p
              call stdlib_zlacgv( q-i+1, x11(i,i), ldx11 )
              call stdlib_zlarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              x11(i,i) = cone
              call stdlib_zlarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_zlarf( 'R', q-p, q-i+1, x11(i,i), ldx11, tauq1(i),x21(m-q+1,i), ldx21, &
                        work(ilarf) )
              call stdlib_zlacgv( q-i+1, x11(i,i), ldx11 )
           end do
           ! reduce the bottom-right portion of x21 to [ 0 i ]
           do i = p + 1, q
              call stdlib_zlacgv( q-i+1, x21(m-q+i-p,i), ldx21 )
              call stdlib_zlarfgp( q-i+1, x21(m-q+i-p,i), x21(m-q+i-p,i+1), ldx21,tauq1(i) )
                        
              x21(m-q+i-p,i) = cone
              call stdlib_zlarf( 'R', q-i, q-i+1, x21(m-q+i-p,i), ldx21, tauq1(i),x21(m-q+i-p+1,i)&
                        , ldx21, work(ilarf) )
              call stdlib_zlacgv( q-i+1, x21(m-q+i-p,i), ldx21 )
           end do
           return
     end subroutine stdlib_zunbdb4




     pure module subroutine stdlib_cunbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! CUNBDB5 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then some other vector from the orthogonal complement
     !! is returned. This vector is chosen in an arbitrary but deterministic
     !! way.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, j
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB5', -info )
              return
           end if
           ! project x onto the orthogonal complement of q
           call stdlib_cunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2, ldq2,work, lwork, &
                     childinfo )
           ! if the projection is nonzero, then return
           if( stdlib_scnrm2(m1,x1,incx1) /= czero.or. stdlib_scnrm2(m2,x2,incx2) /= czero ) &
                     then
              return
           end if
           ! project each standard basis vector e_1,...,e_m1 in turn, stopping
           ! when a nonzero projection is found
           do i = 1, m1
              do j = 1, m1
                 x1(j) = czero
              end do
              x1(i) = cone
              do j = 1, m2
                 x2(j) = czero
              end do
              call stdlib_cunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_scnrm2(m1,x1,incx1) /= czero.or. stdlib_scnrm2(m2,x2,incx2) /= czero ) &
                        then
                 return
              end if
           end do
           ! project each standard basis vector e_(m1+1),...,e_(m1+m2) in turn,
           ! stopping when a nonzero projection is found
           do i = 1, m2
              do j = 1, m1
                 x1(j) = czero
              end do
              do j = 1, m2
                 x2(j) = czero
              end do
              x2(i) = cone
              call stdlib_cunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_scnrm2(m1,x1,incx1) /= czero.or. stdlib_scnrm2(m2,x2,incx2) /= czero ) &
                        then
                 return
              end if
           end do
           return
     end subroutine stdlib_cunbdb5

     pure module subroutine stdlib_zunbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! ZUNBDB5 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then some other vector from the orthogonal complement
     !! is returned. This vector is chosen in an arbitrary but deterministic
     !! way.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, j
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB5', -info )
              return
           end if
           ! project x onto the orthogonal complement of q
           call stdlib_zunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2, ldq2,work, lwork, &
                     childinfo )
           ! if the projection is nonzero, then return
           if( stdlib_dznrm2(m1,x1,incx1) /= czero.or. stdlib_dznrm2(m2,x2,incx2) /= czero ) &
                     then
              return
           end if
           ! project each standard basis vector e_1,...,e_m1 in turn, stopping
           ! when a nonzero projection is found
           do i = 1, m1
              do j = 1, m1
                 x1(j) = czero
              end do
              x1(i) = cone
              do j = 1, m2
                 x2(j) = czero
              end do
              call stdlib_zunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_dznrm2(m1,x1,incx1) /= czero.or. stdlib_dznrm2(m2,x2,incx2) /= czero ) &
                        then
                 return
              end if
           end do
           ! project each standard basis vector e_(m1+1),...,e_(m1+m2) in turn,
           ! stopping when a nonzero projection is found
           do i = 1, m2
              do j = 1, m1
                 x1(j) = czero
              end do
              do j = 1, m2
                 x2(j) = czero
              end do
              x2(i) = cone
              call stdlib_zunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_dznrm2(m1,x1,incx1) /= czero.or. stdlib_dznrm2(m2,x2,incx2) /= czero ) &
                        then
                 return
              end if
           end do
           return
     end subroutine stdlib_zunbdb5




     pure module subroutine stdlib_cunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, lwork, info )
     !! CUNBDB6 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then the zero vector is returned.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: alphasq = 0.01_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: normsq1, normsq2, scl1, scl2, ssq1, ssq2
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'CUNBDB6', -info )
              return
           end if
           ! first, project x onto the orthogonal complement of q's column
           ! space
           scl1 = zero
           ssq1 = one
           call stdlib_classq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_classq( m2, x2, incx2, scl2, ssq2 )
           normsq1 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = czero
              end do
           else
              call stdlib_cgemv( 'C', m1, n, cone, q1, ldq1, x1, incx1, czero, work,1_ilp )
           end if
           call stdlib_cgemv( 'C', m2, n, cone, q2, ldq2, x2, incx2, cone, work, 1_ilp )
           call stdlib_cgemv( 'N', m1, n, cnegone, q1, ldq1, work, 1_ilp, cone, x1,incx1 )
           call stdlib_cgemv( 'N', m2, n, cnegone, q2, ldq2, work, 1_ilp, cone, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_classq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_classq( m2, x2, incx2, scl2, ssq2 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if projection is sufficiently large in norm, then stop.
           ! if projection is czero, then stop.
           ! otherwise, project again.
           if( normsq2 >= alphasq*normsq1 ) then
              return
           end if
           if( normsq2 == czero ) then
              return
           end if
           normsq1 = normsq2
           do i = 1, n
              work(i) = czero
           end do
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = czero
              end do
           else
              call stdlib_cgemv( 'C', m1, n, cone, q1, ldq1, x1, incx1, czero, work,1_ilp )
           end if
           call stdlib_cgemv( 'C', m2, n, cone, q2, ldq2, x2, incx2, cone, work, 1_ilp )
           call stdlib_cgemv( 'N', m1, n, cnegone, q1, ldq1, work, 1_ilp, cone, x1,incx1 )
           call stdlib_cgemv( 'N', m2, n, cnegone, q2, ldq2, work, 1_ilp, cone, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_classq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_classq( m1, x1, incx1, scl1, ssq1 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if second projection is sufficiently large in norm, then do
           ! nothing more. alternatively, if it shrunk significantly, then
           ! truncate it to czero.
           if( normsq2 < alphasq*normsq1 ) then
              do i = 1, m1
                 x1(i) = czero
              end do
              do i = 1, m2
                 x2(i) = czero
              end do
           end if
           return
     end subroutine stdlib_cunbdb6

     pure module subroutine stdlib_zunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! ZUNBDB6 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then the zero vector is returned.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: alphasq = 0.01_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: normsq1, normsq2, scl1, scl2, ssq1, ssq2
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZUNBDB6', -info )
              return
           end if
           ! first, project x onto the orthogonal complement of q's column
           ! space
           scl1 = zero
           ssq1 = one
           call stdlib_zlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_zlassq( m2, x2, incx2, scl2, ssq2 )
           normsq1 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = czero
              end do
           else
              call stdlib_zgemv( 'C', m1, n, cone, q1, ldq1, x1, incx1, czero, work,1_ilp )
           end if
           call stdlib_zgemv( 'C', m2, n, cone, q2, ldq2, x2, incx2, cone, work, 1_ilp )
           call stdlib_zgemv( 'N', m1, n, cnegone, q1, ldq1, work, 1_ilp, cone, x1,incx1 )
           call stdlib_zgemv( 'N', m2, n, cnegone, q2, ldq2, work, 1_ilp, cone, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_zlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_zlassq( m2, x2, incx2, scl2, ssq2 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if projection is sufficiently large in norm, then stop.
           ! if projection is czero, then stop.
           ! otherwise, project again.
           if( normsq2 >= alphasq*normsq1 ) then
              return
           end if
           if( normsq2 == czero ) then
              return
           end if
           normsq1 = normsq2
           do i = 1, n
              work(i) = czero
           end do
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = czero
              end do
           else
              call stdlib_zgemv( 'C', m1, n, cone, q1, ldq1, x1, incx1, czero, work,1_ilp )
           end if
           call stdlib_zgemv( 'C', m2, n, cone, q2, ldq2, x2, incx2, cone, work, 1_ilp )
           call stdlib_zgemv( 'N', m1, n, cnegone, q1, ldq1, work, 1_ilp, cone, x1,incx1 )
           call stdlib_zgemv( 'N', m2, n, cnegone, q2, ldq2, work, 1_ilp, cone, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_zlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_zlassq( m1, x1, incx1, scl1, ssq1 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if second projection is sufficiently large in norm, then do
           ! nothing more. alternatively, if it shrunk significantly, then
           ! truncate it to czero.
           if( normsq2 < alphasq*normsq1 ) then
              do i = 1, m1
                 x1(i) = czero
              end do
              do i = 1, m2
                 x2(i) = czero
              end do
           end if
           return
     end subroutine stdlib_zunbdb6



end submodule stdlib_lapack_cosine_sine
