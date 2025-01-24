submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_svd_drivers3
  implicit none


  contains

     pure module subroutine stdlib_sbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
     !! SBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**T
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**T*VT instead of
     !! P**T, for given real input matrices U and VT.  When U and VT are the
     !! orthogonal matrices that reduce a general matrix A to bidiagonal
     !! form:  A = U*B*VT, as computed by SGEBRD, then
     !! A = (U*Q) * S * (P**T*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
     !! for a given real input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: hndrth = 0.01_sp
           real(sp), parameter :: hndrd = 100.0_sp
           real(sp), parameter :: meigth = -0.125_sp
           integer(ilp), parameter :: maxitr = 6_ilp
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp) :: i, idir, isub, iter, iterdivn, j, ll, lll, m, maxitdivn, nm1, nm12, &
                     nm13, oldll, oldm
           real(sp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ncvt<0_ilp ) then
              info = -3_ilp
           else if( nru<0_ilp ) then
              info = -4_ilp
           else if( ncc<0_ilp ) then
              info = -5_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -11_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_slasq1( n, d, e, work, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp
           end if
           nm1 = n - 1_ilp
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp
           ! get machine constants
           eps = stdlib_slamch( 'EPSILON' )
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 work( i ) = cs
                 work( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp )call stdlib_slasr( 'R', 'V', 'F', nru, n, work( 1_ilp ), work( n ), u,ldu )
                        
              if( ncc>0_ilp )call stdlib_slasr( 'L', 'V', 'F', n, ncc, work( 1_ilp ), work( n ), c,ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=sp) )
              thresh = max( tol*sminoa, maxitr*(n*(n*unfl)) )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*(n*(n*unfl)) )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxitdivn = maxitr*n
           iterdivn = 0_ilp
           iter = -1_ilp
           oldll = -1_ilp
           oldm = -1_ilp
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>=n ) then
              iter = iter - n
              iterdivn = iterdivn + 1_ilp
              if( iterdivn>=maxitdivn )go to 200
           end if
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_slasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp )call stdlib_srot( ncvt, vt( m-1, 1_ilp ), ldvt, vt( m, 1_ilp ), ldvt, cosr,sinr &
                        )
              if( nru>0_ilp )call stdlib_srot( nru, u( 1_ilp, m-1 ), 1_ilp, u( 1_ilp, m ), 1_ilp, cosl, sinl )
              if( ncc>0_ilp )call stdlib_srot( ncc, c( m-1, 1_ilp ), ldc, c( m, 1_ilp ), ldc, cosl,sinl )
                        
              m = m - 2_ilp
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp ) then
                 sll = abs( d( ll ) )
                 call stdlib_slas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_slas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_slartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_slartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll+1 ) = cs
                    work( i-ll+1+nm1 ) = sn
                    work( i-ll+1+nm12 ) = oldcs
                    work( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_slasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp ),work( n ), &
                           vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_slasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_slasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_slartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_slartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll ) = cs
                    work( i-ll+nm1 ) = -sn
                    work( i-ll+nm12 ) = oldcs
                    work( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_slasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_slasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp ),work( n ), u(&
                            1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_slasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp ),work( n ), c(&
                            ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_slartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    work( i-ll+1 ) = cosr
                    work( i-ll+1+nm1 ) = sinr
                    work( i-ll+1+nm12 ) = cosl
                    work( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_slasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp ),work( n ), &
                           vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_slasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_slasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_slartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    work( i-ll ) = cosr
                    work( i-ll+nm1 ) = -sinr
                    work( i-ll+nm12 ) = cosl
                    work( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp )call stdlib_slasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_slasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp ),work( n ), u(&
                            1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_slasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp ),work( n ), c(&
                            ll, 1_ilp ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp )call stdlib_sscal( ncvt, negone, vt( i, 1_ilp ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp
              smin = d( 1_ilp )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp )call stdlib_sswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( n+1-i, 1_ilp ),ldvt )
                           
                 if( nru>0_ilp )call stdlib_sswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, n+1-i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_sswap( ncc, c( isub, 1_ilp ), ldc, c( n+1-i, 1_ilp ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           220 continue
           return
     end subroutine stdlib_sbdsqr

     pure module subroutine stdlib_dbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
     !! DBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**T
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**T*VT instead of
     !! P**T, for given real input matrices U and VT.  When U and VT are the
     !! orthogonal matrices that reduce a general matrix A to bidiagonal
     !! form:  A = U*B*VT, as computed by DGEBRD, then
     !! A = (U*Q) * S * (P**T*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
     !! for a given real input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: hndrth = 0.01_dp
           real(dp), parameter :: hndrd = 100.0_dp
           real(dp), parameter :: meigth = -0.125_dp
           integer(ilp), parameter :: maxitr = 6_ilp
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp) :: i, idir, isub, iter, iterdivn, j, ll, lll, m, maxitdivn, nm1, nm12, &
                     nm13, oldll, oldm
           real(dp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ncvt<0_ilp ) then
              info = -3_ilp
           else if( nru<0_ilp ) then
              info = -4_ilp
           else if( ncc<0_ilp ) then
              info = -5_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -11_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_dlasq1( n, d, e, work, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp
           end if
           nm1 = n - 1_ilp
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp
           ! get machine constants
           eps = stdlib_dlamch( 'EPSILON' )
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 work( i ) = cs
                 work( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp )call stdlib_dlasr( 'R', 'V', 'F', nru, n, work( 1_ilp ), work( n ), u,ldu )
                        
              if( ncc>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', n, ncc, work( 1_ilp ), work( n ), c,ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=dp) )
              thresh = max( tol*sminoa, maxitr*(n*(n*unfl)) )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*(n*(n*unfl)) )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxitdivn = maxitr*n
           iterdivn = 0_ilp
           iter = -1_ilp
           oldll = -1_ilp
           oldm = -1_ilp
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>=n ) then
              iter = iter - n
              iterdivn = iterdivn + 1_ilp
              if( iterdivn>=maxitdivn )go to 200
           end if
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_dlasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp )call stdlib_drot( ncvt, vt( m-1, 1_ilp ), ldvt, vt( m, 1_ilp ), ldvt, cosr,sinr &
                        )
              if( nru>0_ilp )call stdlib_drot( nru, u( 1_ilp, m-1 ), 1_ilp, u( 1_ilp, m ), 1_ilp, cosl, sinl )
              if( ncc>0_ilp )call stdlib_drot( ncc, c( m-1, 1_ilp ), ldc, c( m, 1_ilp ), ldc, cosl,sinl )
                        
              m = m - 2_ilp
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp ) then
                 sll = abs( d( ll ) )
                 call stdlib_dlas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_dlas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_dlartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_dlartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll+1 ) = cs
                    work( i-ll+1+nm1 ) = sn
                    work( i-ll+1+nm12 ) = oldcs
                    work( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp ),work( n ), &
                           vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_dlasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_dlartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_dlartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll ) = cs
                    work( i-ll+nm1 ) = -sn
                    work( i-ll+nm12 ) = oldcs
                    work( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_dlasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_dlasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp ),work( n ), u(&
                            1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_dlasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp ),work( n ), c(&
                            ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_dlartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    work( i-ll+1 ) = cosr
                    work( i-ll+1+nm1 ) = sinr
                    work( i-ll+1+nm12 ) = cosl
                    work( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp ),work( n ), &
                           vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_dlasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_dlartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    work( i-ll ) = cosr
                    work( i-ll+nm1 ) = -sinr
                    work( i-ll+nm12 ) = cosl
                    work( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp )call stdlib_dlasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_dlasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp ),work( n ), u(&
                            1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_dlasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp ),work( n ), c(&
                            ll, 1_ilp ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp )call stdlib_dscal( ncvt, negone, vt( i, 1_ilp ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp
              smin = d( 1_ilp )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp )call stdlib_dswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( n+1-i, 1_ilp ),ldvt )
                           
                 if( nru>0_ilp )call stdlib_dswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, n+1-i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_dswap( ncc, c( isub, 1_ilp ), ldc, c( n+1-i, 1_ilp ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           220 continue
           return
     end subroutine stdlib_dbdsqr


     pure module subroutine stdlib_cbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
     !! CBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**H
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**H*VT instead of
     !! P**H, for given complex input matrices U and VT.  When U and VT are
     !! the unitary matrices that reduce a general matrix A to bidiagonal
     !! form: A = U*B*VT, as computed by CGEBRD, then
     !! A = (U*Q) * S * (P**H*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**H*C
     !! for a given complex input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: hndrth = 0.01_sp
           real(sp), parameter :: hndrd = 100.0_sp
           real(sp), parameter :: meigth = -0.125_sp
           integer(ilp), parameter :: maxitr = 6_ilp
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp) :: i, idir, isub, iter, j, ll, lll, m, maxit, nm1, nm12, nm13, oldll, &
                     oldm
           real(sp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ncvt<0_ilp ) then
              info = -3_ilp
           else if( nru<0_ilp ) then
              info = -4_ilp
           else if( ncc<0_ilp ) then
              info = -5_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -11_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_slasq1( n, d, e, rwork, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp
           end if
           nm1 = n - 1_ilp
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp
           ! get machine constants
           eps = stdlib_slamch( 'EPSILON' )
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 rwork( i ) = cs
                 rwork( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp )call stdlib_clasr( 'R', 'V', 'F', nru, n, rwork( 1_ilp ), rwork( n ),u, ldu )
                        
              if( ncc>0_ilp )call stdlib_clasr( 'L', 'V', 'F', n, ncc, rwork( 1_ilp ), rwork( n ),c, ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=sp) )
              thresh = max( tol*sminoa, maxitr*n*n*unfl )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*n*n*unfl )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxit = maxitr*n*n
           iter = 0_ilp
           oldll = -1_ilp
           oldm = -1_ilp
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>maxit )go to 200
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_slasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp )call stdlib_csrot( ncvt, vt( m-1, 1_ilp ), ldvt, vt( m, 1_ilp ), ldvt,cosr, &
                        sinr )
              if( nru>0_ilp )call stdlib_csrot( nru, u( 1_ilp, m-1 ), 1_ilp, u( 1_ilp, m ), 1_ilp, cosl, sinl )
                        
              if( ncc>0_ilp )call stdlib_csrot( ncc, c( m-1, 1_ilp ), ldc, c( m, 1_ilp ), ldc, cosl,sinl )
                        
              m = m - 2_ilp
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp ) then
                 sll = abs( d( ll ) )
                 call stdlib_slas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_slas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_slartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_slartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll+1 ) = cs
                    rwork( i-ll+1+nm1 ) = sn
                    rwork( i-ll+1+nm12 ) = oldcs
                    rwork( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_clasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp ),rwork( n )&
                           , vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_clasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_clasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_slartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_slartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll ) = cs
                    rwork( i-ll+nm1 ) = -sn
                    rwork( i-ll+nm12 ) = oldcs
                    rwork( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_clasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_clasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp ),rwork( n ), &
                           u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_clasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp ),rwork( n ), &
                           c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_slartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    rwork( i-ll+1 ) = cosr
                    rwork( i-ll+1+nm1 ) = sinr
                    rwork( i-ll+1+nm12 ) = cosl
                    rwork( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_clasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp ),rwork( n )&
                           , vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_clasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_clasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_slartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    rwork( i-ll ) = cosr
                    rwork( i-ll+nm1 ) = -sinr
                    rwork( i-ll+nm12 ) = cosl
                    rwork( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp )call stdlib_clasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_clasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp ),rwork( n ), &
                           u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_clasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp ),rwork( n ), &
                           c( ll, 1_ilp ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp )call stdlib_csscal( ncvt, negone, vt( i, 1_ilp ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp
              smin = d( 1_ilp )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp )call stdlib_cswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( n+1-i, 1_ilp ),ldvt )
                           
                 if( nru>0_ilp )call stdlib_cswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, n+1-i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_cswap( ncc, c( isub, 1_ilp ), ldc, c( n+1-i, 1_ilp ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           220 continue
           return
     end subroutine stdlib_cbdsqr

     pure module subroutine stdlib_zbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
     !! ZBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**H
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**H*VT instead of
     !! P**H, for given complex input matrices U and VT.  When U and VT are
     !! the unitary matrices that reduce a general matrix A to bidiagonal
     !! form: A = U*B*VT, as computed by ZGEBRD, then
     !! A = (U*Q) * S * (P**H*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**H*C
     !! for a given complex input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: hndrth = 0.01_dp
           real(dp), parameter :: hndrd = 100.0_dp
           real(dp), parameter :: meigth = -0.125_dp
           integer(ilp), parameter :: maxitr = 6_ilp
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp) :: i, idir, isub, iter, j, ll, lll, m, maxit, nm1, nm12, nm13, oldll, &
                     oldm
           real(dp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ncvt<0_ilp ) then
              info = -3_ilp
           else if( nru<0_ilp ) then
              info = -4_ilp
           else if( ncc<0_ilp ) then
              info = -5_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -11_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_dlasq1( n, d, e, rwork, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp
           end if
           nm1 = n - 1_ilp
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp
           ! get machine constants
           eps = stdlib_dlamch( 'EPSILON' )
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 rwork( i ) = cs
                 rwork( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp )call stdlib_zlasr( 'R', 'V', 'F', nru, n, rwork( 1_ilp ), rwork( n ),u, ldu )
                        
              if( ncc>0_ilp )call stdlib_zlasr( 'L', 'V', 'F', n, ncc, rwork( 1_ilp ), rwork( n ),c, ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=dp) )
              thresh = max( tol*sminoa, maxitr*n*n*unfl )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*n*n*unfl )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxit = maxitr*n*n
           iter = 0_ilp
           oldll = -1_ilp
           oldm = -1_ilp
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>maxit )go to 200
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_dlasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp )call stdlib_zdrot( ncvt, vt( m-1, 1_ilp ), ldvt, vt( m, 1_ilp ), ldvt,cosr, &
                        sinr )
              if( nru>0_ilp )call stdlib_zdrot( nru, u( 1_ilp, m-1 ), 1_ilp, u( 1_ilp, m ), 1_ilp, cosl, sinl )
                        
              if( ncc>0_ilp )call stdlib_zdrot( ncc, c( m-1, 1_ilp ), ldc, c( m, 1_ilp ), ldc, cosl,sinl )
                        
              m = m - 2_ilp
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp ) then
                 sll = abs( d( ll ) )
                 call stdlib_dlas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_dlas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_dlartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_dlartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll+1 ) = cs
                    rwork( i-ll+1+nm1 ) = sn
                    rwork( i-ll+1+nm12 ) = oldcs
                    rwork( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_zlasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp ),rwork( n )&
                           , vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_zlasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_zlasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_dlartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_dlartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll ) = cs
                    rwork( i-ll+nm1 ) = -sn
                    rwork( i-ll+nm12 ) = oldcs
                    rwork( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_zlasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_zlasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp ),rwork( n ), &
                           u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_zlasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp ),rwork( n ), &
                           c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_dlartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    rwork( i-ll+1 ) = cosr
                    rwork( i-ll+1+nm1 ) = sinr
                    rwork( i-ll+1+nm12 ) = cosl
                    rwork( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp )call stdlib_zlasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp ),rwork( n )&
                           , vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_zlasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_zlasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_dlartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    rwork( i-ll ) = cosr
                    rwork( i-ll+nm1 ) = -sinr
                    rwork( i-ll+nm12 ) = cosl
                    rwork( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp )call stdlib_zlasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp ), ldvt )
                 if( nru>0_ilp )call stdlib_zlasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp ),rwork( n ), &
                           u( 1_ilp, ll ), ldu )
                 if( ncc>0_ilp )call stdlib_zlasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp ),rwork( n ), &
                           c( ll, 1_ilp ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp )call stdlib_zdscal( ncvt, negone, vt( i, 1_ilp ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp
              smin = d( 1_ilp )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp )call stdlib_zswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( n+1-i, 1_ilp ),ldvt )
                           
                 if( nru>0_ilp )call stdlib_zswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, n+1-i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_zswap( ncc, c( isub, 1_ilp ), ldc, c( n+1-i, 1_ilp ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           220 continue
           return
     end subroutine stdlib_zbdsqr




     pure module subroutine stdlib_sbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
     !! SBDSDC computes the singular value decomposition (SVD) of a real
     !! N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
     !! using a divide and conquer method, where S is a diagonal matrix
     !! with non-negative diagonal elements (the singular values of B), and
     !! U and VT are orthogonal matrices of left and right singular vectors,
     !! respectively. SBDSDC can be used to compute all singular values,
     !! and optionally, singular vectors or singular vectors in compact form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See SLASD3 for details.
     !! The code currently calls SLASDQ if singular values only are desired.
     !! However, it can be slightly modified to compute singular values
     !! using the divide and conquer method.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iq(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
        ! changed dimension statement in comment describing e from (n) to
        ! (n-1).  sven, 17 feb 05.
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: difl, difr, givcol, givnum, givptr, i, ic, icompq, ierr, ii, is, iu, &
           iuplo, ivt, j, k, kk, mlvl, nm1, nsize, perm, poles, qstart, smlsiz, smlszp, sqre, &
                     start, wstart, z
           real(sp) :: cs, eps, orgnrm, p, r, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           iuplo = 0_ilp
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp
           if( stdlib_lsame( compq, 'N' ) ) then
              icompq = 0_ilp
           else if( stdlib_lsame( compq, 'P' ) ) then
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              icompq = 2_ilp
           else
              icompq = -1_ilp
           end if
           if( iuplo==0_ilp ) then
              info = -1_ilp
           else if( icompq<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ( ldu<1_ilp ) .or. ( ( icompq==2_ilp ) .and. ( ldu<n ) ) ) then
              info = -7_ilp
           else if( ( ldvt<1_ilp ) .or. ( ( icompq==2_ilp ) .and. ( ldvt<n ) ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SBDSDC', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'SBDSDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           if( n==1_ilp ) then
              if( icompq==1_ilp ) then
                 q( 1_ilp ) = sign( one, d( 1_ilp ) )
                 q( 1_ilp+smlsiz*n ) = one
              else if( icompq==2_ilp ) then
                 u( 1_ilp, 1_ilp ) = sign( one, d( 1_ilp ) )
                 vt( 1_ilp, 1_ilp ) = one
              end if
              d( 1_ilp ) = abs( d( 1_ilp ) )
              return
           end if
           nm1 = n - 1_ilp
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           wstart = 1_ilp
           qstart = 3_ilp
           if( icompq==1_ilp ) then
              call stdlib_scopy( n, d, 1_ilp, q( 1_ilp ), 1_ilp )
              call stdlib_scopy( n-1, e, 1_ilp, q( n+1 ), 1_ilp )
           end if
           if( iuplo==2_ilp ) then
              qstart = 5_ilp
              if( icompq == 2_ilp ) wstart = 2_ilp*n - 1_ilp
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( icompq==1_ilp ) then
                    q( i+2*n ) = cs
                    q( i+3*n ) = sn
                 else if( icompq==2_ilp ) then
                    work( i ) = cs
                    work( nm1+i ) = -sn
                 end if
              end do
           end if
           ! if icompq = 0, use stdlib_slasdq to compute the singular values.
           if( icompq==0_ilp ) then
              ! ignore wstart, instead using work( 1 ), since the two vectors
              ! for cs and -sn above are added only if icompq == 2,
              ! and adding them exceeds documented work size of 4*n.
              call stdlib_slasdq( 'U', 0_ilp, n, 0_ilp, 0_ilp, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work( 1_ilp ), &
                        info )
              go to 40
           end if
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              if( icompq==2_ilp ) then
                 call stdlib_slaset( 'A', n, n, zero, one, u, ldu )
                 call stdlib_slaset( 'A', n, n, zero, one, vt, ldvt )
                 call stdlib_slasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work( &
                           wstart ), info )
              else if( icompq==1_ilp ) then
                 iu = 1_ilp
                 ivt = iu + n
                 call stdlib_slaset( 'A', n, n, zero, one, q( iu+( qstart-1 )*n ),n )
                 call stdlib_slaset( 'A', n, n, zero, one, q( ivt+( qstart-1 )*n ),n )
                 call stdlib_slasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e,q( ivt+( qstart-1 )*n ), n,q( iu+( &
                           qstart-1 )*n ), n,q( iu+( qstart-1 )*n ), n, work( wstart ),info )
              end if
              go to 40
           end if
           if( icompq==2_ilp ) then
              call stdlib_slaset( 'A', n, n, zero, one, u, ldu )
              call stdlib_slaset( 'A', n, n, zero, one, vt, ldvt )
           end if
           ! scale.
           orgnrm = stdlib_slanst( 'M', n, d, e )
           if( orgnrm==zero )return
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, ierr )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, ierr )
           eps = stdlib_slamch( 'EPSILON' )
           mlvl = int( log( real( n,KIND=sp) / real( smlsiz+1,KIND=sp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           if( icompq==1_ilp ) then
              iu = 1_ilp
              ivt = 1_ilp + smlsiz
              difl = ivt + smlszp
              difr = difl + mlvl
              z = difr + mlvl*2_ilp
              ic = z + mlvl
              is = ic + 1_ilp
              poles = is + 1_ilp
              givnum = poles + 2_ilp*mlvl
              k = 1_ilp
              givptr = 2_ilp
              perm = 3_ilp
              givcol = perm + mlvl
           end if
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           start = 1_ilp
           sqre = 0_ilp
           loop_30: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
              ! subproblem found. first determine its size and then
              ! apply divide and conquer on it.
                 if( i<nm1 ) then
              ! a subproblem with e(i) small for i < nm1.
                    nsize = i - start + 1_ilp
                 else if( abs( e( i ) )>=eps ) then
              ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - start + 1_ilp
                 else
              ! a subproblem with e(nm1) small. this implies an
              ! 1-by-1 subproblem at d(n). solve this 1-by-1 problem
              ! first.
                    nsize = i - start + 1_ilp
                    if( icompq==2_ilp ) then
                       u( n, n ) = sign( one, d( n ) )
                       vt( n, n ) = one
                    else if( icompq==1_ilp ) then
                       q( n+( qstart-1 )*n ) = sign( one, d( n ) )
                       q( n+( smlsiz+qstart-1 )*n ) = one
                    end if
                    d( n ) = abs( d( n ) )
                 end if
                 if( icompq==2_ilp ) then
                    call stdlib_slasd0( nsize, sqre, d( start ), e( start ),u( start, start ), &
                              ldu, vt( start, start ),ldvt, smlsiz, iwork, work( wstart ), info )
                 else
                    call stdlib_slasda( icompq, smlsiz, nsize, sqre, d( start ),e( start ), q( &
                    start+( iu+qstart-2 )*n ), n,q( start+( ivt+qstart-2 )*n ),iq( start+k*n ), q(&
                     start+( difl+qstart-2 )*n ), q( start+( difr+qstart-2 )*n ),q( start+( z+&
                     qstart-2 )*n ),q( start+( poles+qstart-2 )*n ),iq( start+givptr*n ), iq( &
                     start+givcol*n ),n, iq( start+perm*n ),q( start+( givnum+qstart-2 )*n ),q( &
                     start+( ic+qstart-2 )*n ),q( start+( is+qstart-2 )*n ),work( wstart ), iwork,&
                                info )
                 end if
                 if( info/=0_ilp ) then
                    return
                 end if
                 start = i + 1_ilp
              end if
           end do loop_30
           ! unscale
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, ierr )
           40 continue
           ! use selection sort to minimize swaps of singular vectors
           do ii = 2, n
              i = ii - 1_ilp
              kk = i
              p = d( i )
              do j = ii, n
                 if( d( j )>p ) then
                    kk = j
                    p = d( j )
                 end if
              end do
              if( kk/=i ) then
                 d( kk ) = d( i )
                 d( i ) = p
                 if( icompq==1_ilp ) then
                    iq( i ) = kk
                 else if( icompq==2_ilp ) then
                    call stdlib_sswap( n, u( 1_ilp, i ), 1_ilp, u( 1_ilp, kk ), 1_ilp )
                    call stdlib_sswap( n, vt( i, 1_ilp ), ldvt, vt( kk, 1_ilp ), ldvt )
                 end if
              else if( icompq==1_ilp ) then
                 iq( i ) = i
              end if
           end do
           ! if icompq = 1, use iq(n,1) as the indicator for uplo
           if( icompq==1_ilp ) then
              if( iuplo==1_ilp ) then
                 iq( n ) = 1_ilp
              else
                 iq( n ) = 0_ilp
              end if
           end if
           ! if b is lower bidiagonal, update u by those givens rotations
           ! which rotated b to be upper bidiagonal
           if( ( iuplo==2_ilp ) .and. ( icompq==2_ilp ) )call stdlib_slasr( 'L', 'V', 'B', n, n, work( 1_ilp )&
                     , work( n ), u, ldu )
           return
     end subroutine stdlib_sbdsdc

     pure module subroutine stdlib_dbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
     !! DBDSDC computes the singular value decomposition (SVD) of a real
     !! N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
     !! using a divide and conquer method, where S is a diagonal matrix
     !! with non-negative diagonal elements (the singular values of B), and
     !! U and VT are orthogonal matrices of left and right singular vectors,
     !! respectively. DBDSDC can be used to compute all singular values,
     !! and optionally, singular vectors or singular vectors in compact form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See DLASD3 for details.
     !! The code currently calls DLASDQ if singular values only are desired.
     !! However, it can be slightly modified to compute singular values
     !! using the divide and conquer method.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iq(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
        ! changed dimension statement in comment describing e from (n) to
        ! (n-1).  sven, 17 feb 05.
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: difl, difr, givcol, givnum, givptr, i, ic, icompq, ierr, ii, is, iu, &
           iuplo, ivt, j, k, kk, mlvl, nm1, nsize, perm, poles, qstart, smlsiz, smlszp, sqre, &
                     start, wstart, z
           real(dp) :: cs, eps, orgnrm, p, r, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           iuplo = 0_ilp
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp
           if( stdlib_lsame( compq, 'N' ) ) then
              icompq = 0_ilp
           else if( stdlib_lsame( compq, 'P' ) ) then
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              icompq = 2_ilp
           else
              icompq = -1_ilp
           end if
           if( iuplo==0_ilp ) then
              info = -1_ilp
           else if( icompq<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ( ldu<1_ilp ) .or. ( ( icompq==2_ilp ) .and. ( ldu<n ) ) ) then
              info = -7_ilp
           else if( ( ldvt<1_ilp ) .or. ( ( icompq==2_ilp ) .and. ( ldvt<n ) ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DBDSDC', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'DBDSDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           if( n==1_ilp ) then
              if( icompq==1_ilp ) then
                 q( 1_ilp ) = sign( one, d( 1_ilp ) )
                 q( 1_ilp+smlsiz*n ) = one
              else if( icompq==2_ilp ) then
                 u( 1_ilp, 1_ilp ) = sign( one, d( 1_ilp ) )
                 vt( 1_ilp, 1_ilp ) = one
              end if
              d( 1_ilp ) = abs( d( 1_ilp ) )
              return
           end if
           nm1 = n - 1_ilp
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           wstart = 1_ilp
           qstart = 3_ilp
           if( icompq==1_ilp ) then
              call stdlib_dcopy( n, d, 1_ilp, q( 1_ilp ), 1_ilp )
              call stdlib_dcopy( n-1, e, 1_ilp, q( n+1 ), 1_ilp )
           end if
           if( iuplo==2_ilp ) then
              qstart = 5_ilp
              if( icompq == 2_ilp ) wstart = 2_ilp*n - 1_ilp
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( icompq==1_ilp ) then
                    q( i+2*n ) = cs
                    q( i+3*n ) = sn
                 else if( icompq==2_ilp ) then
                    work( i ) = cs
                    work( nm1+i ) = -sn
                 end if
              end do
           end if
           ! if icompq = 0, use stdlib_dlasdq to compute the singular values.
           if( icompq==0_ilp ) then
              ! ignore wstart, instead using work( 1 ), since the two vectors
              ! for cs and -sn above are added only if icompq == 2,
              ! and adding them exceeds documented work size of 4*n.
              call stdlib_dlasdq( 'U', 0_ilp, n, 0_ilp, 0_ilp, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work( 1_ilp ), &
                        info )
              go to 40
           end if
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              if( icompq==2_ilp ) then
                 call stdlib_dlaset( 'A', n, n, zero, one, u, ldu )
                 call stdlib_dlaset( 'A', n, n, zero, one, vt, ldvt )
                 call stdlib_dlasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work( &
                           wstart ), info )
              else if( icompq==1_ilp ) then
                 iu = 1_ilp
                 ivt = iu + n
                 call stdlib_dlaset( 'A', n, n, zero, one, q( iu+( qstart-1 )*n ),n )
                 call stdlib_dlaset( 'A', n, n, zero, one, q( ivt+( qstart-1 )*n ),n )
                 call stdlib_dlasdq( 'U', 0_ilp, n, n, n, 0_ilp, d, e,q( ivt+( qstart-1 )*n ), n,q( iu+( &
                           qstart-1 )*n ), n,q( iu+( qstart-1 )*n ), n, work( wstart ),info )
              end if
              go to 40
           end if
           if( icompq==2_ilp ) then
              call stdlib_dlaset( 'A', n, n, zero, one, u, ldu )
              call stdlib_dlaset( 'A', n, n, zero, one, vt, ldvt )
           end if
           ! scale.
           orgnrm = stdlib_dlanst( 'M', n, d, e )
           if( orgnrm==zero )return
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, ierr )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, nm1, 1_ilp, e, nm1, ierr )
           eps = (0.9e+0_dp)*stdlib_dlamch( 'EPSILON' )
           mlvl = int( log( real( n,KIND=dp) / real( smlsiz+1,KIND=dp) ) / log( two ),KIND=ilp) + &
                     1_ilp
           smlszp = smlsiz + 1_ilp
           if( icompq==1_ilp ) then
              iu = 1_ilp
              ivt = 1_ilp + smlsiz
              difl = ivt + smlszp
              difr = difl + mlvl
              z = difr + mlvl*2_ilp
              ic = z + mlvl
              is = ic + 1_ilp
              poles = is + 1_ilp
              givnum = poles + 2_ilp*mlvl
              k = 1_ilp
              givptr = 2_ilp
              perm = 3_ilp
              givcol = perm + mlvl
           end if
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           start = 1_ilp
           sqre = 0_ilp
           loop_30: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - start + 1_ilp
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - start + 1_ilp
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n). solve this 1-by-1 problem
                    ! first.
                    nsize = i - start + 1_ilp
                    if( icompq==2_ilp ) then
                       u( n, n ) = sign( one, d( n ) )
                       vt( n, n ) = one
                    else if( icompq==1_ilp ) then
                       q( n+( qstart-1 )*n ) = sign( one, d( n ) )
                       q( n+( smlsiz+qstart-1 )*n ) = one
                    end if
                    d( n ) = abs( d( n ) )
                 end if
                 if( icompq==2_ilp ) then
                    call stdlib_dlasd0( nsize, sqre, d( start ), e( start ),u( start, start ), &
                              ldu, vt( start, start ),ldvt, smlsiz, iwork, work( wstart ), info )
                 else
                    call stdlib_dlasda( icompq, smlsiz, nsize, sqre, d( start ),e( start ), q( &
                    start+( iu+qstart-2 )*n ), n,q( start+( ivt+qstart-2 )*n ),iq( start+k*n ), q(&
                     start+( difl+qstart-2 )*n ), q( start+( difr+qstart-2 )*n ),q( start+( z+&
                     qstart-2 )*n ),q( start+( poles+qstart-2 )*n ),iq( start+givptr*n ), iq( &
                     start+givcol*n ),n, iq( start+perm*n ),q( start+( givnum+qstart-2 )*n ),q( &
                     start+( ic+qstart-2 )*n ),q( start+( is+qstart-2 )*n ),work( wstart ), iwork,&
                                info )
                 end if
                 if( info/=0_ilp ) then
                    return
                 end if
                 start = i + 1_ilp
              end if
           end do loop_30
           ! unscale
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, ierr )
           40 continue
           ! use selection sort to minimize swaps of singular vectors
           do ii = 2, n
              i = ii - 1_ilp
              kk = i
              p = d( i )
              do j = ii, n
                 if( d( j )>p ) then
                    kk = j
                    p = d( j )
                 end if
              end do
              if( kk/=i ) then
                 d( kk ) = d( i )
                 d( i ) = p
                 if( icompq==1_ilp ) then
                    iq( i ) = kk
                 else if( icompq==2_ilp ) then
                    call stdlib_dswap( n, u( 1_ilp, i ), 1_ilp, u( 1_ilp, kk ), 1_ilp )
                    call stdlib_dswap( n, vt( i, 1_ilp ), ldvt, vt( kk, 1_ilp ), ldvt )
                 end if
              else if( icompq==1_ilp ) then
                 iq( i ) = i
              end if
           end do
           ! if icompq = 1, use iq(n,1) as the indicator for uplo
           if( icompq==1_ilp ) then
              if( iuplo==1_ilp ) then
                 iq( n ) = 1_ilp
              else
                 iq( n ) = 0_ilp
              end if
           end if
           ! if b is lower bidiagonal, update u by those givens rotations
           ! which rotated b to be upper bidiagonal
           if( ( iuplo==2_ilp ) .and. ( icompq==2_ilp ) )call stdlib_dlasr( 'L', 'V', 'B', n, n, work( 1_ilp )&
                     , work( n ), u, ldu )
           return
     end subroutine stdlib_dbdsdc




     pure module subroutine stdlib_I64_sbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
     !! SBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**T
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**T*VT instead of
     !! P**T, for given real input matrices U and VT.  When U and VT are the
     !! orthogonal matrices that reduce a general matrix A to bidiagonal
     !! form:  A = U*B*VT, as computed by SGEBRD, then
     !! A = (U*Q) * S * (P**T*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
     !! for a given real input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: hndrth = 0.01_sp
           real(sp), parameter :: hndrd = 100.0_sp
           real(sp), parameter :: meigth = -0.125_sp
           integer(ilp64), parameter :: maxitr = 6_ilp64
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp64) :: i, idir, isub, iter, iterdivn, j, ll, lll, m, maxitdivn, nm1, nm12, &
                     nm13, oldll, oldm
           real(sp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( ncvt<0_ilp64 ) then
              info = -3_ilp64
           else if( nru<0_ilp64 ) then
              info = -4_ilp64
           else if( ncc<0_ilp64 ) then
              info = -5_ilp64
           else if( ( ncvt==0_ilp64 .and. ldvt<1_ilp64 ) .or.( ncvt>0_ilp64 .and. ldvt<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldu<max( 1_ilp64, nru ) ) then
              info = -11_ilp64
           else if( ( ncc==0_ilp64 .and. ldc<1_ilp64 ) .or.( ncc>0_ilp64 .and. ldc<max( 1_ilp64, n ) ) ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp64 ) .or. ( nru>0_ilp64 ) .or. ( ncc>0_ilp64 )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_I64_slasq1( n, d, e, work, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp64
           end if
           nm1 = n - 1_ilp64
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp64
           ! get machine constants
           eps = stdlib_I64_slamch( 'EPSILON' )
           unfl = stdlib_I64_slamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_I64_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 work( i ) = cs
                 work( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp64 )call stdlib_I64_slasr( 'R', 'V', 'F', nru, n, work( 1_ilp64 ), work( n ), u,ldu )
                        
              if( ncc>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'F', n, ncc, work( 1_ilp64 ), work( n ), c,ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp64 ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=sp) )
              thresh = max( tol*sminoa, maxitr*(n*(n*unfl)) )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*(n*(n*unfl)) )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxitdivn = maxitr*n
           iterdivn = 0_ilp64
           iter = -1_ilp64
           oldll = -1_ilp64
           oldm = -1_ilp64
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>=n ) then
              iter = iter - n
              iterdivn = iterdivn + 1_ilp64
              if( iterdivn>=maxitdivn )go to 200
           end if
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp64
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp64
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp64
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_I64_slasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp64 )call stdlib_I64_srot( ncvt, vt( m-1, 1_ilp64 ), ldvt, vt( m, 1_ilp64 ), ldvt, cosr,sinr &
                        )
              if( nru>0_ilp64 )call stdlib_I64_srot( nru, u( 1_ilp64, m-1 ), 1_ilp64, u( 1_ilp64, m ), 1_ilp64, cosl, sinl )
              if( ncc>0_ilp64 )call stdlib_I64_srot( ncc, c( m-1, 1_ilp64 ), ldc, c( m, 1_ilp64 ), ldc, cosl,sinl )
                        
              m = m - 2_ilp64
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp64
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp64
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp64 ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp64 ) then
                 sll = abs( d( ll ) )
                 call stdlib_I64_slas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_I64_slas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp64<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_I64_slartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_I64_slartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll+1 ) = cs
                    work( i-ll+1+nm1 ) = sn
                    work( i-ll+1+nm12 ) = oldcs
                    work( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp64 ),work( n ), &
                           vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_slasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_I64_slartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_I64_slartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll ) = cs
                    work( i-ll+nm1 ) = -sn
                    work( i-ll+nm12 ) = oldcs
                    work( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_slasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp64 ),work( n ), u(&
                            1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp64 ),work( n ), c(&
                            ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_I64_slartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_I64_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    work( i-ll+1 ) = cosr
                    work( i-ll+1+nm1 ) = sinr
                    work( i-ll+1+nm12 ) = cosl
                    work( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp64 ),work( n ), &
                           vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_slasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_I64_slartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_I64_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    work( i-ll ) = cosr
                    work( i-ll+nm1 ) = -sinr
                    work( i-ll+nm12 ) = cosl
                    work( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_slasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp64 ),work( n ), u(&
                            1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_slasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp64 ),work( n ), c(&
                            ll, 1_ilp64 ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_sscal( ncvt, negone, vt( i, 1_ilp64 ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp64
              smin = d( 1_ilp64 )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp64 )call stdlib_I64_sswap( ncvt, vt( isub, 1_ilp64 ), ldvt, vt( n+1-i, 1_ilp64 ),ldvt )
                           
                 if( nru>0_ilp64 )call stdlib_I64_sswap( nru, u( 1_ilp64, isub ), 1_ilp64, u( 1_ilp64, n+1-i ), 1_ilp64 )
                 if( ncc>0_ilp64 )call stdlib_I64_sswap( ncc, c( isub, 1_ilp64 ), ldc, c( n+1-i, 1_ilp64 ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp64
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp64
           end do
           220 continue
           return
     end subroutine stdlib_I64_sbdsqr

     pure module subroutine stdlib_I64_dbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
     !! DBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**T
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**T*VT instead of
     !! P**T, for given real input matrices U and VT.  When U and VT are the
     !! orthogonal matrices that reduce a general matrix A to bidiagonal
     !! form:  A = U*B*VT, as computed by DGEBRD, then
     !! A = (U*Q) * S * (P**T*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
     !! for a given real input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: hndrth = 0.01_dp
           real(dp), parameter :: hndrd = 100.0_dp
           real(dp), parameter :: meigth = -0.125_dp
           integer(ilp64), parameter :: maxitr = 6_ilp64
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp64) :: i, idir, isub, iter, iterdivn, j, ll, lll, m, maxitdivn, nm1, nm12, &
                     nm13, oldll, oldm
           real(dp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( ncvt<0_ilp64 ) then
              info = -3_ilp64
           else if( nru<0_ilp64 ) then
              info = -4_ilp64
           else if( ncc<0_ilp64 ) then
              info = -5_ilp64
           else if( ( ncvt==0_ilp64 .and. ldvt<1_ilp64 ) .or.( ncvt>0_ilp64 .and. ldvt<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldu<max( 1_ilp64, nru ) ) then
              info = -11_ilp64
           else if( ( ncc==0_ilp64 .and. ldc<1_ilp64 ) .or.( ncc>0_ilp64 .and. ldc<max( 1_ilp64, n ) ) ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp64 ) .or. ( nru>0_ilp64 ) .or. ( ncc>0_ilp64 )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_I64_dlasq1( n, d, e, work, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp64
           end if
           nm1 = n - 1_ilp64
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp64
           ! get machine constants
           eps = stdlib_I64_dlamch( 'EPSILON' )
           unfl = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_I64_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 work( i ) = cs
                 work( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp64 )call stdlib_I64_dlasr( 'R', 'V', 'F', nru, n, work( 1_ilp64 ), work( n ), u,ldu )
                        
              if( ncc>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'F', n, ncc, work( 1_ilp64 ), work( n ), c,ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp64 ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=dp) )
              thresh = max( tol*sminoa, maxitr*(n*(n*unfl)) )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*(n*(n*unfl)) )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxitdivn = maxitr*n
           iterdivn = 0_ilp64
           iter = -1_ilp64
           oldll = -1_ilp64
           oldm = -1_ilp64
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>=n ) then
              iter = iter - n
              iterdivn = iterdivn + 1_ilp64
              if( iterdivn>=maxitdivn )go to 200
           end if
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp64
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp64
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp64
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_I64_dlasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp64 )call stdlib_I64_drot( ncvt, vt( m-1, 1_ilp64 ), ldvt, vt( m, 1_ilp64 ), ldvt, cosr,sinr &
                        )
              if( nru>0_ilp64 )call stdlib_I64_drot( nru, u( 1_ilp64, m-1 ), 1_ilp64, u( 1_ilp64, m ), 1_ilp64, cosl, sinl )
              if( ncc>0_ilp64 )call stdlib_I64_drot( ncc, c( m-1, 1_ilp64 ), ldc, c( m, 1_ilp64 ), ldc, cosl,sinl )
                        
              m = m - 2_ilp64
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp64
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp64
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp64 ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp64 ) then
                 sll = abs( d( ll ) )
                 call stdlib_I64_dlas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_I64_dlas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp64<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_I64_dlartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_I64_dlartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll+1 ) = cs
                    work( i-ll+1+nm1 ) = sn
                    work( i-ll+1+nm12 ) = oldcs
                    work( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp64 ),work( n ), &
                           vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_dlasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_I64_dlartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_I64_dlartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    work( i-ll ) = cs
                    work( i-ll+nm1 ) = -sn
                    work( i-ll+nm12 ) = oldcs
                    work( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_dlasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp64 ),work( n ), u(&
                            1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp64 ),work( n ), c(&
                            ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_I64_dlartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_I64_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    work( i-ll+1 ) = cosr
                    work( i-ll+1+nm1 ) = sinr
                    work( i-ll+1+nm12 ) = cosl
                    work( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'F', m-ll+1, ncvt, work( 1_ilp64 ),work( n ), &
                           vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_dlasr( 'R', 'V', 'F', nru, m-ll+1, work( nm12+1 ),work( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'F', m-ll+1, ncc, work( nm12+1 ),work( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_I64_dlartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_I64_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    work( i-ll ) = cosr
                    work( i-ll+nm1 ) = -sinr
                    work( i-ll+nm12 ) = cosl
                    work( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'B', m-ll+1, ncvt, work( nm12+1 ),work( &
                           nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_dlasr( 'R', 'V', 'B', nru, m-ll+1, work( 1_ilp64 ),work( n ), u(&
                            1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_dlasr( 'L', 'V', 'B', m-ll+1, ncc, work( 1_ilp64 ),work( n ), c(&
                            ll, 1_ilp64 ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_dscal( ncvt, negone, vt( i, 1_ilp64 ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp64
              smin = d( 1_ilp64 )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp64 )call stdlib_I64_dswap( ncvt, vt( isub, 1_ilp64 ), ldvt, vt( n+1-i, 1_ilp64 ),ldvt )
                           
                 if( nru>0_ilp64 )call stdlib_I64_dswap( nru, u( 1_ilp64, isub ), 1_ilp64, u( 1_ilp64, n+1-i ), 1_ilp64 )
                 if( ncc>0_ilp64 )call stdlib_I64_dswap( ncc, c( isub, 1_ilp64 ), ldc, c( n+1-i, 1_ilp64 ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp64
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp64
           end do
           220 continue
           return
     end subroutine stdlib_I64_dbdsqr


     pure module subroutine stdlib_I64_cbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
     !! CBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**H
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**H*VT instead of
     !! P**H, for given complex input matrices U and VT.  When U and VT are
     !! the unitary matrices that reduce a general matrix A to bidiagonal
     !! form: A = U*B*VT, as computed by CGEBRD, then
     !! A = (U*Q) * S * (P**H*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**H*C
     !! for a given complex input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: hndrth = 0.01_sp
           real(sp), parameter :: hndrd = 100.0_sp
           real(sp), parameter :: meigth = -0.125_sp
           integer(ilp64), parameter :: maxitr = 6_ilp64
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp64) :: i, idir, isub, iter, j, ll, lll, m, maxit, nm1, nm12, nm13, oldll, &
                     oldm
           real(sp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( ncvt<0_ilp64 ) then
              info = -3_ilp64
           else if( nru<0_ilp64 ) then
              info = -4_ilp64
           else if( ncc<0_ilp64 ) then
              info = -5_ilp64
           else if( ( ncvt==0_ilp64 .and. ldvt<1_ilp64 ) .or.( ncvt>0_ilp64 .and. ldvt<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldu<max( 1_ilp64, nru ) ) then
              info = -11_ilp64
           else if( ( ncc==0_ilp64 .and. ldc<1_ilp64 ) .or.( ncc>0_ilp64 .and. ldc<max( 1_ilp64, n ) ) ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp64 ) .or. ( nru>0_ilp64 ) .or. ( ncc>0_ilp64 )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_I64_slasq1( n, d, e, rwork, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp64
           end if
           nm1 = n - 1_ilp64
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp64
           ! get machine constants
           eps = stdlib_I64_slamch( 'EPSILON' )
           unfl = stdlib_I64_slamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_I64_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 rwork( i ) = cs
                 rwork( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp64 )call stdlib_I64_clasr( 'R', 'V', 'F', nru, n, rwork( 1_ilp64 ), rwork( n ),u, ldu )
                        
              if( ncc>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'F', n, ncc, rwork( 1_ilp64 ), rwork( n ),c, ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp64 ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=sp) )
              thresh = max( tol*sminoa, maxitr*n*n*unfl )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*n*n*unfl )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxit = maxitr*n*n
           iter = 0_ilp64
           oldll = -1_ilp64
           oldm = -1_ilp64
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>maxit )go to 200
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp64
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp64
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp64
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_I64_slasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp64 )call stdlib_I64_csrot( ncvt, vt( m-1, 1_ilp64 ), ldvt, vt( m, 1_ilp64 ), ldvt,cosr, &
                        sinr )
              if( nru>0_ilp64 )call stdlib_I64_csrot( nru, u( 1_ilp64, m-1 ), 1_ilp64, u( 1_ilp64, m ), 1_ilp64, cosl, sinl )
                        
              if( ncc>0_ilp64 )call stdlib_I64_csrot( ncc, c( m-1, 1_ilp64 ), ldc, c( m, 1_ilp64 ), ldc, cosl,sinl )
                        
              m = m - 2_ilp64
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp64
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp64
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp64 ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp64 ) then
                 sll = abs( d( ll ) )
                 call stdlib_I64_slas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_I64_slas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp64<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_I64_slartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_I64_slartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll+1 ) = cs
                    rwork( i-ll+1+nm1 ) = sn
                    rwork( i-ll+1+nm12 ) = oldcs
                    rwork( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp64 ),rwork( n )&
                           , vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_clasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_I64_slartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_I64_slartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll ) = cs
                    rwork( i-ll+nm1 ) = -sn
                    rwork( i-ll+nm12 ) = oldcs
                    rwork( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_clasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp64 ),rwork( n ), &
                           u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp64 ),rwork( n ), &
                           c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_I64_slartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_I64_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    rwork( i-ll+1 ) = cosr
                    rwork( i-ll+1+nm1 ) = sinr
                    rwork( i-ll+1+nm12 ) = cosl
                    rwork( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp64 ),rwork( n )&
                           , vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_clasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_I64_slartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_I64_slartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    rwork( i-ll ) = cosr
                    rwork( i-ll+nm1 ) = -sinr
                    rwork( i-ll+nm12 ) = cosl
                    rwork( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_clasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp64 ),rwork( n ), &
                           u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_clasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp64 ),rwork( n ), &
                           c( ll, 1_ilp64 ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_csscal( ncvt, negone, vt( i, 1_ilp64 ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp64
              smin = d( 1_ilp64 )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp64 )call stdlib_I64_cswap( ncvt, vt( isub, 1_ilp64 ), ldvt, vt( n+1-i, 1_ilp64 ),ldvt )
                           
                 if( nru>0_ilp64 )call stdlib_I64_cswap( nru, u( 1_ilp64, isub ), 1_ilp64, u( 1_ilp64, n+1-i ), 1_ilp64 )
                 if( ncc>0_ilp64 )call stdlib_I64_cswap( ncc, c( isub, 1_ilp64 ), ldc, c( n+1-i, 1_ilp64 ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp64
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp64
           end do
           220 continue
           return
     end subroutine stdlib_I64_cbdsqr

     pure module subroutine stdlib_I64_zbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
     !! ZBDSQR computes the singular values and, optionally, the right and/or
     !! left singular vectors from the singular value decomposition (SVD) of
     !! a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
     !! zero-shift QR algorithm.  The SVD of B has the form
     !! B = Q * S * P**H
     !! where S is the diagonal matrix of singular values, Q is an orthogonal
     !! matrix of left singular vectors, and P is an orthogonal matrix of
     !! right singular vectors.  If left singular vectors are requested, this
     !! subroutine actually returns U*Q instead of Q, and, if right singular
     !! vectors are requested, this subroutine returns P**H*VT instead of
     !! P**H, for given complex input matrices U and VT.  When U and VT are
     !! the unitary matrices that reduce a general matrix A to bidiagonal
     !! form: A = U*B*VT, as computed by ZGEBRD, then
     !! A = (U*Q) * S * (P**H*VT)
     !! is the SVD of A.  Optionally, the subroutine may also compute Q**H*C
     !! for a given complex input matrix C.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
     !! no. 5, pp. 873-912, Sept 1990) and
     !! "Accurate singular values and differential qd algorithms," by
     !! B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
     !! Department, University of California at Berkeley, July 1992
     !! for a detailed description of the algorithm.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: hndrth = 0.01_dp
           real(dp), parameter :: hndrd = 100.0_dp
           real(dp), parameter :: meigth = -0.125_dp
           integer(ilp64), parameter :: maxitr = 6_ilp64
           
           
           
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: lower, rotate
           integer(ilp64) :: i, idir, isub, iter, j, ll, lll, m, maxit, nm1, nm12, nm13, oldll, &
                     oldm
           real(dp) :: abse, abss, cosl, cosr, cs, eps, f, g, h, mu, oldcs, oldsn, r, shift, &
           sigmn, sigmx, sinl, sinr, sll, smax, smin, sminl, sminoa, sn, thresh, tol, tolmul, &
                     unfl
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.lower ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( ncvt<0_ilp64 ) then
              info = -3_ilp64
           else if( nru<0_ilp64 ) then
              info = -4_ilp64
           else if( ncc<0_ilp64 ) then
              info = -5_ilp64
           else if( ( ncvt==0_ilp64 .and. ldvt<1_ilp64 ) .or.( ncvt>0_ilp64 .and. ldvt<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldu<max( 1_ilp64, nru ) ) then
              info = -11_ilp64
           else if( ( ncc==0_ilp64 .and. ldc<1_ilp64 ) .or.( ncc>0_ilp64 .and. ldc<max( 1_ilp64, n ) ) ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZBDSQR', -info )
              return
           end if
           if( n==0 )return
           if( n==1 )go to 160
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp64 ) .or. ( nru>0_ilp64 ) .or. ( ncc>0_ilp64 )
           ! if no singular vectors desired, use qd algorithm
           if( .not.rotate ) then
              call stdlib_I64_dlasq1( n, d, e, rwork, info )
           ! if info equals 2, dqds didn't finish, try to finish
              if( info /= 2 ) return
              info = 0_ilp64
           end if
           nm1 = n - 1_ilp64
           nm12 = nm1 + nm1
           nm13 = nm12 + nm1
           idir = 0_ilp64
           ! get machine constants
           eps = stdlib_I64_dlamch( 'EPSILON' )
           unfl = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           if( lower ) then
              do i = 1, n - 1
                 call stdlib_I64_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 rwork( i ) = cs
                 rwork( nm1+i ) = sn
              end do
              ! update singular vectors if desired
              if( nru>0_ilp64 )call stdlib_I64_zlasr( 'R', 'V', 'F', nru, n, rwork( 1_ilp64 ), rwork( n ),u, ldu )
                        
              if( ncc>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'F', n, ncc, rwork( 1_ilp64 ), rwork( n ),c, ldc )
                        
           end if
           ! compute singular values to relative accuracy tol
           ! (by setting tol to be negative, algorithm will compute
           ! singular values to absolute accuracy abs(tol)*norm(input matrix))
           tolmul = max( ten, min( hndrd, eps**meigth ) )
           tol = tolmul*eps
           ! compute approximate maximum, minimum singular values
           smax = zero
           do i = 1, n
              smax = max( smax, abs( d( i ) ) )
           end do
           do i = 1, n - 1
              smax = max( smax, abs( e( i ) ) )
           end do
           sminl = zero
           if( tol>=zero ) then
              ! relative accuracy desired
              sminoa = abs( d( 1_ilp64 ) )
              if( sminoa==zero )go to 50
              mu = sminoa
              do i = 2, n
                 mu = abs( d( i ) )*( mu / ( mu+abs( e( i-1 ) ) ) )
                 sminoa = min( sminoa, mu )
                 if( sminoa==zero )go to 50
              end do
              50 continue
              sminoa = sminoa / sqrt( real( n,KIND=dp) )
              thresh = max( tol*sminoa, maxitr*n*n*unfl )
           else
              ! absolute accuracy desired
              thresh = max( abs( tol )*smax, maxitr*n*n*unfl )
           end if
           ! prepare for main iteration loop for the singular values
           ! (maxit is the maximum number of passes through the inner
           ! loop permitted before nonconvergence signalled.)
           maxit = maxitr*n*n
           iter = 0_ilp64
           oldll = -1_ilp64
           oldm = -1_ilp64
           ! m points to last element of unconverged part of matrix
           m = n
           ! begin main iteration loop
           60 continue
           ! check for convergence or exceeding iteration count
           if( m<=1 )go to 160
           if( iter>maxit )go to 200
           ! find diagonal block of matrix to work on
           if( tol<zero .and. abs( d( m ) )<=thresh )d( m ) = zero
           smax = abs( d( m ) )
           smin = smax
           do lll = 1, m - 1
              ll = m - lll
              abss = abs( d( ll ) )
              abse = abs( e( ll ) )
              if( tol<zero .and. abss<=thresh )d( ll ) = zero
              if( abse<=thresh )go to 80
              smin = min( smin, abss )
              smax = max( smax, abss, abse )
           end do
           ll = 0_ilp64
           go to 90
           80 continue
           e( ll ) = zero
           ! matrix splits since e(ll) = 0
           if( ll==m-1 ) then
              ! convergence of bottom singular value, return to top of loop
              m = m - 1_ilp64
              go to 60
           end if
           90 continue
           ll = ll + 1_ilp64
           ! e(ll) through e(m-1) are nonzero, e(ll-1) is zero
           if( ll==m-1 ) then
              ! 2 by 2 block, handle separately
              call stdlib_I64_dlasv2( d( m-1 ), e( m-1 ), d( m ), sigmn, sigmx, sinr,cosr, sinl, cosl &
                        )
              d( m-1 ) = sigmx
              e( m-1 ) = zero
              d( m ) = sigmn
              ! compute singular vectors, if desired
              if( ncvt>0_ilp64 )call stdlib_I64_zdrot( ncvt, vt( m-1, 1_ilp64 ), ldvt, vt( m, 1_ilp64 ), ldvt,cosr, &
                        sinr )
              if( nru>0_ilp64 )call stdlib_I64_zdrot( nru, u( 1_ilp64, m-1 ), 1_ilp64, u( 1_ilp64, m ), 1_ilp64, cosl, sinl )
                        
              if( ncc>0_ilp64 )call stdlib_I64_zdrot( ncc, c( m-1, 1_ilp64 ), ldc, c( m, 1_ilp64 ), ldc, cosl,sinl )
                        
              m = m - 2_ilp64
              go to 60
           end if
           ! if working on new submatrix, choose shift direction
           ! (from larger end diagonal element towards smaller)
           if( ll>oldm .or. m<oldll ) then
              if( abs( d( ll ) )>=abs( d( m ) ) ) then
                 ! chase bulge from top (big end) to bottom (small end)
                 idir = 1_ilp64
              else
                 ! chase bulge from bottom (big end) to top (small end)
                 idir = 2_ilp64
              end if
           end if
           ! apply convergence tests
           if( idir==1_ilp64 ) then
              ! run convergence test in forward direction
              ! first apply standard test to bottom of matrix
              if( abs( e( m-1 ) )<=abs( tol )*abs( d( m ) ) .or.( tol<zero .and. abs( e( m-1 ) )&
                        <=thresh ) ) then
                 e( m-1 ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion forward
                 mu = abs( d( ll ) )
                 sminl = mu
                 do lll = ll, m - 1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll+1 ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           else
              ! run convergence test in backward direction
              ! first apply standard test to top of matrix
              if( abs( e( ll ) )<=abs( tol )*abs( d( ll ) ) .or.( tol<zero .and. abs( e( ll ) )&
                        <=thresh ) ) then
                 e( ll ) = zero
                 go to 60
              end if
              if( tol>=zero ) then
                 ! if relative accuracy desired,
                 ! apply convergence criterion backward
                 mu = abs( d( m ) )
                 sminl = mu
                 do lll = m - 1, ll, -1
                    if( abs( e( lll ) )<=tol*mu ) then
                       e( lll ) = zero
                       go to 60
                    end if
                    mu = abs( d( lll ) )*( mu / ( mu+abs( e( lll ) ) ) )
                    sminl = min( sminl, mu )
                 end do
              end if
           end if
           oldll = ll
           oldm = m
           ! compute shift.  first, test if shifting would ruin relative
           ! accuracy, and if so set the shift to zero.
           if( tol>=zero .and. n*tol*( sminl / smax )<=max( eps, hndrth*tol ) ) then
              ! use a zero shift to avoid loss of relative accuracy
              shift = zero
           else
              ! compute the shift from 2-by-2 block at end of matrix
              if( idir==1_ilp64 ) then
                 sll = abs( d( ll ) )
                 call stdlib_I64_dlas2( d( m-1 ), e( m-1 ), d( m ), shift, r )
              else
                 sll = abs( d( m ) )
                 call stdlib_I64_dlas2( d( ll ), e( ll ), d( ll+1 ), shift, r )
              end if
              ! test if shift negligible, and if so set to zero
              if( sll>zero ) then
                 if( ( shift / sll )**2_ilp64<eps )shift = zero
              end if
           end if
           ! increment iteration count
           iter = iter + m - ll
           ! if shift = 0, do simplified qr iteration
           if( shift==zero ) then
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = ll, m - 1
                    call stdlib_I64_dlartg( d( i )*cs, e( i ), cs, sn, r )
                    if( i>ll )e( i-1 ) = oldsn*r
                    call stdlib_I64_dlartg( oldcs*r, d( i+1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll+1 ) = cs
                    rwork( i-ll+1+nm1 ) = sn
                    rwork( i-ll+1+nm12 ) = oldcs
                    rwork( i-ll+1+nm13 ) = oldsn
                 end do
                 h = d( m )*cs
                 d( m ) = h*oldcs
                 e( m-1 ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp64 ),rwork( n )&
                           , vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_zlasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 cs = one
                 oldcs = one
                 do i = m, ll + 1, -1
                    call stdlib_I64_dlartg( d( i )*cs, e( i-1 ), cs, sn, r )
                    if( i<m )e( i ) = oldsn*r
                    call stdlib_I64_dlartg( oldcs*r, d( i-1 )*sn, oldcs, oldsn, d( i ) )
                    rwork( i-ll ) = cs
                    rwork( i-ll+nm1 ) = -sn
                    rwork( i-ll+nm12 ) = oldcs
                    rwork( i-ll+nm13 ) = -oldsn
                 end do
                 h = d( ll )*cs
                 d( ll ) = h*oldcs
                 e( ll ) = h*oldsn
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_zlasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp64 ),rwork( n ), &
                           u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp64 ),rwork( n ), &
                           c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
              end if
           else
              ! use nonzero shift
              if( idir==1_ilp64 ) then
                 ! chase bulge from top to bottom
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( ll ) )-shift )*( sign( one, d( ll ) )+shift / d( ll ) )
                 g = e( ll )
                 do i = ll, m - 1
                    call stdlib_I64_dlartg( f, g, cosr, sinr, r )
                    if( i>ll )e( i-1 ) = r
                    f = cosr*d( i ) + sinr*e( i )
                    e( i ) = cosr*e( i ) - sinr*d( i )
                    g = sinr*d( i+1 )
                    d( i+1 ) = cosr*d( i+1 )
                    call stdlib_I64_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i ) + sinl*d( i+1 )
                    d( i+1 ) = cosl*d( i+1 ) - sinl*e( i )
                    if( i<m-1 ) then
                       g = sinl*e( i+1 )
                       e( i+1 ) = cosl*e( i+1 )
                    end if
                    rwork( i-ll+1 ) = cosr
                    rwork( i-ll+1+nm1 ) = sinr
                    rwork( i-ll+1+nm12 ) = cosl
                    rwork( i-ll+1+nm13 ) = sinl
                 end do
                 e( m-1 ) = f
                 ! update singular vectors
                 if( ncvt>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'F', m-ll+1, ncvt, rwork( 1_ilp64 ),rwork( n )&
                           , vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_zlasr( 'R', 'V', 'F', nru, m-ll+1, rwork( nm12+1 ),rwork( &
                           nm13+1 ), u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'F', m-ll+1, ncc, rwork( nm12+1 ),rwork( &
                           nm13+1 ), c( ll, 1_ilp64 ), ldc )
                 ! test convergence
                 if( abs( e( m-1 ) )<=thresh )e( m-1 ) = zero
              else
                 ! chase bulge from bottom to top
                 ! save cosines and sines for later singular vector updates
                 f = ( abs( d( m ) )-shift )*( sign( one, d( m ) )+shift /d( m ) )
                 g = e( m-1 )
                 do i = m, ll + 1, -1
                    call stdlib_I64_dlartg( f, g, cosr, sinr, r )
                    if( i<m )e( i ) = r
                    f = cosr*d( i ) + sinr*e( i-1 )
                    e( i-1 ) = cosr*e( i-1 ) - sinr*d( i )
                    g = sinr*d( i-1 )
                    d( i-1 ) = cosr*d( i-1 )
                    call stdlib_I64_dlartg( f, g, cosl, sinl, r )
                    d( i ) = r
                    f = cosl*e( i-1 ) + sinl*d( i-1 )
                    d( i-1 ) = cosl*d( i-1 ) - sinl*e( i-1 )
                    if( i>ll+1 ) then
                       g = sinl*e( i-2 )
                       e( i-2 ) = cosl*e( i-2 )
                    end if
                    rwork( i-ll ) = cosr
                    rwork( i-ll+nm1 ) = -sinr
                    rwork( i-ll+nm12 ) = cosl
                    rwork( i-ll+nm13 ) = -sinl
                 end do
                 e( ll ) = f
                 ! test convergence
                 if( abs( e( ll ) )<=thresh )e( ll ) = zero
                 ! update singular vectors if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'B', m-ll+1, ncvt, rwork( nm12+1 ),&
                           rwork( nm13+1 ), vt( ll, 1_ilp64 ), ldvt )
                 if( nru>0_ilp64 )call stdlib_I64_zlasr( 'R', 'V', 'B', nru, m-ll+1, rwork( 1_ilp64 ),rwork( n ), &
                           u( 1_ilp64, ll ), ldu )
                 if( ncc>0_ilp64 )call stdlib_I64_zlasr( 'L', 'V', 'B', m-ll+1, ncc, rwork( 1_ilp64 ),rwork( n ), &
                           c( ll, 1_ilp64 ), ldc )
              end if
           end if
           ! qr iteration finished, go back and check convergence
           go to 60
           ! all singular values converged, so make them positive
           160 continue
           do i = 1, n
              if( d( i )<zero ) then
                 d( i ) = -d( i )
                 ! change sign of singular vectors, if desired
                 if( ncvt>0_ilp64 )call stdlib_I64_zdscal( ncvt, negone, vt( i, 1_ilp64 ), ldvt )
              end if
           end do
           ! sort the singular values into decreasing order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n - 1
              ! scan for smallest d(i)
              isub = 1_ilp64
              smin = d( 1_ilp64 )
              do j = 2, n + 1 - i
                 if( d( j )<=smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=n+1-i ) then
                 ! swap singular values and vectors
                 d( isub ) = d( n+1-i )
                 d( n+1-i ) = smin
                 if( ncvt>0_ilp64 )call stdlib_I64_zswap( ncvt, vt( isub, 1_ilp64 ), ldvt, vt( n+1-i, 1_ilp64 ),ldvt )
                           
                 if( nru>0_ilp64 )call stdlib_I64_zswap( nru, u( 1_ilp64, isub ), 1_ilp64, u( 1_ilp64, n+1-i ), 1_ilp64 )
                 if( ncc>0_ilp64 )call stdlib_I64_zswap( ncc, c( isub, 1_ilp64 ), ldc, c( n+1-i, 1_ilp64 ), ldc )
                           
              end if
           end do
           go to 220
           ! maximum number of iterations exceeded, failure to converge
           200 continue
           info = 0_ilp64
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp64
           end do
           220 continue
           return
     end subroutine stdlib_I64_zbdsqr




     pure module subroutine stdlib_I64_sbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
     !! SBDSDC computes the singular value decomposition (SVD) of a real
     !! N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
     !! using a divide and conquer method, where S is a diagonal matrix
     !! with non-negative diagonal elements (the singular values of B), and
     !! U and VT are orthogonal matrices of left and right singular vectors,
     !! respectively. SBDSDC can be used to compute all singular values,
     !! and optionally, singular vectors or singular vectors in compact form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See SLASD3 for details.
     !! The code currently calls SLASDQ if singular values only are desired.
     !! However, it can be slightly modified to compute singular values
     !! using the divide and conquer method.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldu, ldvt, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iq(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
        ! changed dimension statement in comment describing e from (n) to
        ! (n-1).  sven, 17 feb 05.
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: difl, difr, givcol, givnum, givptr, i, ic, icompq, ierr, ii, is, iu, &
           iuplo, ivt, j, k, kk, mlvl, nm1, nsize, perm, poles, qstart, smlsiz, smlszp, sqre, &
                     start, wstart, z
           real(sp) :: cs, eps, orgnrm, p, r, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           iuplo = 0_ilp64
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp64
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp64
           if( stdlib_lsame( compq, 'N' ) ) then
              icompq = 0_ilp64
           else if( stdlib_lsame( compq, 'P' ) ) then
              icompq = 1_ilp64
           else if( stdlib_lsame( compq, 'I' ) ) then
              icompq = 2_ilp64
           else
              icompq = -1_ilp64
           end if
           if( iuplo==0_ilp64 ) then
              info = -1_ilp64
           else if( icompq<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ( ldu<1_ilp64 ) .or. ( ( icompq==2_ilp64 ) .and. ( ldu<n ) ) ) then
              info = -7_ilp64
           else if( ( ldvt<1_ilp64 ) .or. ( ( icompq==2_ilp64 ) .and. ( ldvt<n ) ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SBDSDC', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_I64_ilaenv( 9_ilp64, 'SBDSDC', ' ', 0_ilp64, 0_ilp64, 0_ilp64, 0_ilp64 )
           if( n==1_ilp64 ) then
              if( icompq==1_ilp64 ) then
                 q( 1_ilp64 ) = sign( one, d( 1_ilp64 ) )
                 q( 1_ilp64+smlsiz*n ) = one
              else if( icompq==2_ilp64 ) then
                 u( 1_ilp64, 1_ilp64 ) = sign( one, d( 1_ilp64 ) )
                 vt( 1_ilp64, 1_ilp64 ) = one
              end if
              d( 1_ilp64 ) = abs( d( 1_ilp64 ) )
              return
           end if
           nm1 = n - 1_ilp64
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           wstart = 1_ilp64
           qstart = 3_ilp64
           if( icompq==1_ilp64 ) then
              call stdlib_I64_scopy( n, d, 1_ilp64, q( 1_ilp64 ), 1_ilp64 )
              call stdlib_I64_scopy( n-1, e, 1_ilp64, q( n+1 ), 1_ilp64 )
           end if
           if( iuplo==2_ilp64 ) then
              qstart = 5_ilp64
              if( icompq == 2_ilp64 ) wstart = 2_ilp64*n - 1_ilp64
              do i = 1, n - 1
                 call stdlib_I64_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( icompq==1_ilp64 ) then
                    q( i+2*n ) = cs
                    q( i+3*n ) = sn
                 else if( icompq==2_ilp64 ) then
                    work( i ) = cs
                    work( nm1+i ) = -sn
                 end if
              end do
           end if
           ! if icompq = 0, use stdlib_slasdq to compute the singular values.
           if( icompq==0_ilp64 ) then
              ! ignore wstart, instead using work( 1 ), since the two vectors
              ! for cs and -sn above are added only if icompq == 2,
              ! and adding them exceeds documented work size of 4*n.
              call stdlib_I64_slasdq( 'U', 0_ilp64, n, 0_ilp64, 0_ilp64, 0_ilp64, d, e, vt, ldvt, u, ldu, u,ldu, work( 1_ilp64 ), &
                        info )
              go to 40
           end if
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              if( icompq==2_ilp64 ) then
                 call stdlib_I64_slaset( 'A', n, n, zero, one, u, ldu )
                 call stdlib_I64_slaset( 'A', n, n, zero, one, vt, ldvt )
                 call stdlib_I64_slasdq( 'U', 0_ilp64, n, n, n, 0_ilp64, d, e, vt, ldvt, u, ldu, u,ldu, work( &
                           wstart ), info )
              else if( icompq==1_ilp64 ) then
                 iu = 1_ilp64
                 ivt = iu + n
                 call stdlib_I64_slaset( 'A', n, n, zero, one, q( iu+( qstart-1 )*n ),n )
                 call stdlib_I64_slaset( 'A', n, n, zero, one, q( ivt+( qstart-1 )*n ),n )
                 call stdlib_I64_slasdq( 'U', 0_ilp64, n, n, n, 0_ilp64, d, e,q( ivt+( qstart-1 )*n ), n,q( iu+( &
                           qstart-1 )*n ), n,q( iu+( qstart-1 )*n ), n, work( wstart ),info )
              end if
              go to 40
           end if
           if( icompq==2_ilp64 ) then
              call stdlib_I64_slaset( 'A', n, n, zero, one, u, ldu )
              call stdlib_I64_slaset( 'A', n, n, zero, one, vt, ldvt )
           end if
           ! scale.
           orgnrm = stdlib_I64_slanst( 'M', n, d, e )
           if( orgnrm==zero )return
           call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, orgnrm, one, n, 1_ilp64, d, n, ierr )
           call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, orgnrm, one, nm1, 1_ilp64, e, nm1, ierr )
           eps = stdlib_I64_slamch( 'EPSILON' )
           mlvl = int( log( real( n,KIND=sp) / real( smlsiz+1,KIND=sp) ) / log( two ),KIND=ilp64) + &
                     1_ilp64
           smlszp = smlsiz + 1_ilp64
           if( icompq==1_ilp64 ) then
              iu = 1_ilp64
              ivt = 1_ilp64 + smlsiz
              difl = ivt + smlszp
              difr = difl + mlvl
              z = difr + mlvl*2_ilp64
              ic = z + mlvl
              is = ic + 1_ilp64
              poles = is + 1_ilp64
              givnum = poles + 2_ilp64*mlvl
              k = 1_ilp64
              givptr = 2_ilp64
              perm = 3_ilp64
              givcol = perm + mlvl
           end if
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           start = 1_ilp64
           sqre = 0_ilp64
           loop_30: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
              ! subproblem found. first determine its size and then
              ! apply divide and conquer on it.
                 if( i<nm1 ) then
              ! a subproblem with e(i) small for i < nm1.
                    nsize = i - start + 1_ilp64
                 else if( abs( e( i ) )>=eps ) then
              ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - start + 1_ilp64
                 else
              ! a subproblem with e(nm1) small. this implies an
              ! 1-by-1 subproblem at d(n). solve this 1-by-1 problem
              ! first.
                    nsize = i - start + 1_ilp64
                    if( icompq==2_ilp64 ) then
                       u( n, n ) = sign( one, d( n ) )
                       vt( n, n ) = one
                    else if( icompq==1_ilp64 ) then
                       q( n+( qstart-1 )*n ) = sign( one, d( n ) )
                       q( n+( smlsiz+qstart-1 )*n ) = one
                    end if
                    d( n ) = abs( d( n ) )
                 end if
                 if( icompq==2_ilp64 ) then
                    call stdlib_I64_slasd0( nsize, sqre, d( start ), e( start ),u( start, start ), &
                              ldu, vt( start, start ),ldvt, smlsiz, iwork, work( wstart ), info )
                 else
                    call stdlib_I64_slasda( icompq, smlsiz, nsize, sqre, d( start ),e( start ), q( &
                    start+( iu+qstart-2 )*n ), n,q( start+( ivt+qstart-2 )*n ),iq( start+k*n ), q(&
                     start+( difl+qstart-2 )*n ), q( start+( difr+qstart-2 )*n ),q( start+( z+&
                     qstart-2 )*n ),q( start+( poles+qstart-2 )*n ),iq( start+givptr*n ), iq( &
                     start+givcol*n ),n, iq( start+perm*n ),q( start+( givnum+qstart-2 )*n ),q( &
                     start+( ic+qstart-2 )*n ),q( start+( is+qstart-2 )*n ),work( wstart ), iwork,&
                                info )
                 end if
                 if( info/=0_ilp64 ) then
                    return
                 end if
                 start = i + 1_ilp64
              end if
           end do loop_30
           ! unscale
           call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, one, orgnrm, n, 1_ilp64, d, n, ierr )
           40 continue
           ! use selection sort to minimize swaps of singular vectors
           do ii = 2, n
              i = ii - 1_ilp64
              kk = i
              p = d( i )
              do j = ii, n
                 if( d( j )>p ) then
                    kk = j
                    p = d( j )
                 end if
              end do
              if( kk/=i ) then
                 d( kk ) = d( i )
                 d( i ) = p
                 if( icompq==1_ilp64 ) then
                    iq( i ) = kk
                 else if( icompq==2_ilp64 ) then
                    call stdlib_I64_sswap( n, u( 1_ilp64, i ), 1_ilp64, u( 1_ilp64, kk ), 1_ilp64 )
                    call stdlib_I64_sswap( n, vt( i, 1_ilp64 ), ldvt, vt( kk, 1_ilp64 ), ldvt )
                 end if
              else if( icompq==1_ilp64 ) then
                 iq( i ) = i
              end if
           end do
           ! if icompq = 1, use iq(n,1) as the indicator for uplo
           if( icompq==1_ilp64 ) then
              if( iuplo==1_ilp64 ) then
                 iq( n ) = 1_ilp64
              else
                 iq( n ) = 0_ilp64
              end if
           end if
           ! if b is lower bidiagonal, update u by those givens rotations
           ! which rotated b to be upper bidiagonal
           if( ( iuplo==2_ilp64 ) .and. ( icompq==2_ilp64 ) )call stdlib_I64_slasr( 'L', 'V', 'B', n, n, work( 1_ilp64 )&
                     , work( n ), u, ldu )
           return
     end subroutine stdlib_I64_sbdsdc

     pure module subroutine stdlib_I64_dbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
     !! DBDSDC computes the singular value decomposition (SVD) of a real
     !! N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
     !! using a divide and conquer method, where S is a diagonal matrix
     !! with non-negative diagonal elements (the singular values of B), and
     !! U and VT are orthogonal matrices of left and right singular vectors,
     !! respectively. DBDSDC can be used to compute all singular values,
     !! and optionally, singular vectors or singular vectors in compact form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See DLASD3 for details.
     !! The code currently calls DLASDQ if singular values only are desired.
     !! However, it can be slightly modified to compute singular values
     !! using the divide and conquer method.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldu, ldvt, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iq(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
        ! changed dimension statement in comment describing e from (n) to
        ! (n-1).  sven, 17 feb 05.
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: difl, difr, givcol, givnum, givptr, i, ic, icompq, ierr, ii, is, iu, &
           iuplo, ivt, j, k, kk, mlvl, nm1, nsize, perm, poles, qstart, smlsiz, smlszp, sqre, &
                     start, wstart, z
           real(dp) :: cs, eps, orgnrm, p, r, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           iuplo = 0_ilp64
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp64
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp64
           if( stdlib_lsame( compq, 'N' ) ) then
              icompq = 0_ilp64
           else if( stdlib_lsame( compq, 'P' ) ) then
              icompq = 1_ilp64
           else if( stdlib_lsame( compq, 'I' ) ) then
              icompq = 2_ilp64
           else
              icompq = -1_ilp64
           end if
           if( iuplo==0_ilp64 ) then
              info = -1_ilp64
           else if( icompq<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ( ldu<1_ilp64 ) .or. ( ( icompq==2_ilp64 ) .and. ( ldu<n ) ) ) then
              info = -7_ilp64
           else if( ( ldvt<1_ilp64 ) .or. ( ( icompq==2_ilp64 ) .and. ( ldvt<n ) ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DBDSDC', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_I64_ilaenv( 9_ilp64, 'DBDSDC', ' ', 0_ilp64, 0_ilp64, 0_ilp64, 0_ilp64 )
           if( n==1_ilp64 ) then
              if( icompq==1_ilp64 ) then
                 q( 1_ilp64 ) = sign( one, d( 1_ilp64 ) )
                 q( 1_ilp64+smlsiz*n ) = one
              else if( icompq==2_ilp64 ) then
                 u( 1_ilp64, 1_ilp64 ) = sign( one, d( 1_ilp64 ) )
                 vt( 1_ilp64, 1_ilp64 ) = one
              end if
              d( 1_ilp64 ) = abs( d( 1_ilp64 ) )
              return
           end if
           nm1 = n - 1_ilp64
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left
           wstart = 1_ilp64
           qstart = 3_ilp64
           if( icompq==1_ilp64 ) then
              call stdlib_I64_dcopy( n, d, 1_ilp64, q( 1_ilp64 ), 1_ilp64 )
              call stdlib_I64_dcopy( n-1, e, 1_ilp64, q( n+1 ), 1_ilp64 )
           end if
           if( iuplo==2_ilp64 ) then
              qstart = 5_ilp64
              if( icompq == 2_ilp64 ) wstart = 2_ilp64*n - 1_ilp64
              do i = 1, n - 1
                 call stdlib_I64_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( icompq==1_ilp64 ) then
                    q( i+2*n ) = cs
                    q( i+3*n ) = sn
                 else if( icompq==2_ilp64 ) then
                    work( i ) = cs
                    work( nm1+i ) = -sn
                 end if
              end do
           end if
           ! if icompq = 0, use stdlib_dlasdq to compute the singular values.
           if( icompq==0_ilp64 ) then
              ! ignore wstart, instead using work( 1 ), since the two vectors
              ! for cs and -sn above are added only if icompq == 2,
              ! and adding them exceeds documented work size of 4*n.
              call stdlib_I64_dlasdq( 'U', 0_ilp64, n, 0_ilp64, 0_ilp64, 0_ilp64, d, e, vt, ldvt, u, ldu, u,ldu, work( 1_ilp64 ), &
                        info )
              go to 40
           end if
           ! if n is smaller than the minimum divide size smlsiz, then solve
           ! the problem with another solver.
           if( n<=smlsiz ) then
              if( icompq==2_ilp64 ) then
                 call stdlib_I64_dlaset( 'A', n, n, zero, one, u, ldu )
                 call stdlib_I64_dlaset( 'A', n, n, zero, one, vt, ldvt )
                 call stdlib_I64_dlasdq( 'U', 0_ilp64, n, n, n, 0_ilp64, d, e, vt, ldvt, u, ldu, u,ldu, work( &
                           wstart ), info )
              else if( icompq==1_ilp64 ) then
                 iu = 1_ilp64
                 ivt = iu + n
                 call stdlib_I64_dlaset( 'A', n, n, zero, one, q( iu+( qstart-1 )*n ),n )
                 call stdlib_I64_dlaset( 'A', n, n, zero, one, q( ivt+( qstart-1 )*n ),n )
                 call stdlib_I64_dlasdq( 'U', 0_ilp64, n, n, n, 0_ilp64, d, e,q( ivt+( qstart-1 )*n ), n,q( iu+( &
                           qstart-1 )*n ), n,q( iu+( qstart-1 )*n ), n, work( wstart ),info )
              end if
              go to 40
           end if
           if( icompq==2_ilp64 ) then
              call stdlib_I64_dlaset( 'A', n, n, zero, one, u, ldu )
              call stdlib_I64_dlaset( 'A', n, n, zero, one, vt, ldvt )
           end if
           ! scale.
           orgnrm = stdlib_I64_dlanst( 'M', n, d, e )
           if( orgnrm==zero )return
           call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, orgnrm, one, n, 1_ilp64, d, n, ierr )
           call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, orgnrm, one, nm1, 1_ilp64, e, nm1, ierr )
           eps = (0.9e+0_dp)*stdlib_I64_dlamch( 'EPSILON' )
           mlvl = int( log( real( n,KIND=dp) / real( smlsiz+1,KIND=dp) ) / log( two ),KIND=ilp64) + &
                     1_ilp64
           smlszp = smlsiz + 1_ilp64
           if( icompq==1_ilp64 ) then
              iu = 1_ilp64
              ivt = 1_ilp64 + smlsiz
              difl = ivt + smlszp
              difr = difl + mlvl
              z = difr + mlvl*2_ilp64
              ic = z + mlvl
              is = ic + 1_ilp64
              poles = is + 1_ilp64
              givnum = poles + 2_ilp64*mlvl
              k = 1_ilp64
              givptr = 2_ilp64
              perm = 3_ilp64
              givcol = perm + mlvl
           end if
           do i = 1, n
              if( abs( d( i ) )<eps ) then
                 d( i ) = sign( eps, d( i ) )
              end if
           end do
           start = 1_ilp64
           sqre = 0_ilp64
           loop_30: do i = 1, nm1
              if( ( abs( e( i ) )<eps ) .or. ( i==nm1 ) ) then
                 ! subproblem found. first determine its size and then
                 ! apply divide and conquer on it.
                 if( i<nm1 ) then
                    ! a subproblem with e(i) small for i < nm1.
                    nsize = i - start + 1_ilp64
                 else if( abs( e( i ) )>=eps ) then
                    ! a subproblem with e(nm1) not too small but i = nm1.
                    nsize = n - start + 1_ilp64
                 else
                    ! a subproblem with e(nm1) small. this implies an
                    ! 1-by-1 subproblem at d(n). solve this 1-by-1 problem
                    ! first.
                    nsize = i - start + 1_ilp64
                    if( icompq==2_ilp64 ) then
                       u( n, n ) = sign( one, d( n ) )
                       vt( n, n ) = one
                    else if( icompq==1_ilp64 ) then
                       q( n+( qstart-1 )*n ) = sign( one, d( n ) )
                       q( n+( smlsiz+qstart-1 )*n ) = one
                    end if
                    d( n ) = abs( d( n ) )
                 end if
                 if( icompq==2_ilp64 ) then
                    call stdlib_I64_dlasd0( nsize, sqre, d( start ), e( start ),u( start, start ), &
                              ldu, vt( start, start ),ldvt, smlsiz, iwork, work( wstart ), info )
                 else
                    call stdlib_I64_dlasda( icompq, smlsiz, nsize, sqre, d( start ),e( start ), q( &
                    start+( iu+qstart-2 )*n ), n,q( start+( ivt+qstart-2 )*n ),iq( start+k*n ), q(&
                     start+( difl+qstart-2 )*n ), q( start+( difr+qstart-2 )*n ),q( start+( z+&
                     qstart-2 )*n ),q( start+( poles+qstart-2 )*n ),iq( start+givptr*n ), iq( &
                     start+givcol*n ),n, iq( start+perm*n ),q( start+( givnum+qstart-2 )*n ),q( &
                     start+( ic+qstart-2 )*n ),q( start+( is+qstart-2 )*n ),work( wstart ), iwork,&
                                info )
                 end if
                 if( info/=0_ilp64 ) then
                    return
                 end if
                 start = i + 1_ilp64
              end if
           end do loop_30
           ! unscale
           call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, one, orgnrm, n, 1_ilp64, d, n, ierr )
           40 continue
           ! use selection sort to minimize swaps of singular vectors
           do ii = 2, n
              i = ii - 1_ilp64
              kk = i
              p = d( i )
              do j = ii, n
                 if( d( j )>p ) then
                    kk = j
                    p = d( j )
                 end if
              end do
              if( kk/=i ) then
                 d( kk ) = d( i )
                 d( i ) = p
                 if( icompq==1_ilp64 ) then
                    iq( i ) = kk
                 else if( icompq==2_ilp64 ) then
                    call stdlib_I64_dswap( n, u( 1_ilp64, i ), 1_ilp64, u( 1_ilp64, kk ), 1_ilp64 )
                    call stdlib_I64_dswap( n, vt( i, 1_ilp64 ), ldvt, vt( kk, 1_ilp64 ), ldvt )
                 end if
              else if( icompq==1_ilp64 ) then
                 iq( i ) = i
              end if
           end do
           ! if icompq = 1, use iq(n,1) as the indicator for uplo
           if( icompq==1_ilp64 ) then
              if( iuplo==1_ilp64 ) then
                 iq( n ) = 1_ilp64
              else
                 iq( n ) = 0_ilp64
              end if
           end if
           ! if b is lower bidiagonal, update u by those givens rotations
           ! which rotated b to be upper bidiagonal
           if( ( iuplo==2_ilp64 ) .and. ( icompq==2_ilp64 ) )call stdlib_I64_dlasr( 'L', 'V', 'B', n, n, work( 1_ilp64 )&
                     , work( n ), u, ldu )
           return
     end subroutine stdlib_I64_dbdsdc



end submodule stdlib_lapack_eigv_svd_drivers3
