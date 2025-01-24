submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_svd_bidiag_qr
  implicit none


  contains

     pure module subroutine stdlib_slasq1( n, d, e, work, info )
     !! SLASQ1 computes the singular values of a real N-by-N bidiagonal
     !! matrix with diagonal D and off-diagonal E. The singular values
     !! are computed to high relative accuracy, in the absence of
     !! denormalization, underflow and overflow. The algorithm was first
     !! presented in
     !! "Accurate singular values and differential qd algorithms" by K. V.
     !! Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
     !! 1994,
     !! and the present implementation is described in "An implementation of
     !! the dqds Algorithm (Positive Case)", LAPACK Working Note.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo
           real(sp) :: eps, scale, safmin, sigmn, sigmx
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SLASQ1', -info )
              return
           else if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              d( 1_ilp ) = abs( d( 1_ilp ) )
              return
           else if( n==2_ilp ) then
              call stdlib_slas2( d( 1_ilp ), e( 1_ilp ), d( 2_ilp ), sigmn, sigmx )
              d( 1_ilp ) = sigmx
              d( 2_ilp ) = sigmn
              return
           end if
           ! estimate the largest singular value.
           sigmx = zero
           do i = 1, n - 1
              d( i ) = abs( d( i ) )
              sigmx = max( sigmx, abs( e( i ) ) )
           end do
           d( n ) = abs( d( n ) )
           ! early return if sigmx is zero (matrix is already diagonal).
           if( sigmx==zero ) then
              call stdlib_slasrt( 'D', n, d, iinfo )
              return
           end if
           do i = 1, n
              sigmx = max( sigmx, d( i ) )
           end do
           ! copy d and e into work (in the z format) and scale (squaring the
           ! input data makes scaling by a power of the radix pointless).
           eps = stdlib_slamch( 'PRECISION' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           scale = sqrt( eps / safmin )
           call stdlib_scopy( n, d, 1_ilp, work( 1_ilp ), 2_ilp )
           call stdlib_scopy( n-1, e, 1_ilp, work( 2_ilp ), 2_ilp )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, sigmx, scale, 2_ilp*n-1, 1_ilp, work, 2_ilp*n-1,iinfo )
           ! compute the q's and e's.
           do i = 1, 2*n - 1
              work( i ) = work( i )**2_ilp
           end do
           work( 2_ilp*n ) = zero
           call stdlib_slasq2( n, work, info )
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = sqrt( work( i ) )
              end do
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, d, n, iinfo )
           else if( info==2_ilp ) then
           ! maximum number of iterations exceeded.  move data from work
           ! into d and e so the calling subroutine can try to finish
              do i = 1, n
                 d( i ) = sqrt( work( 2_ilp*i-1 ) )
                 e( i ) = sqrt( work( 2_ilp*i ) )
              end do
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, d, n, iinfo )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, e, n, iinfo )
           end if
           return
     end subroutine stdlib_slasq1

     pure module subroutine stdlib_dlasq1( n, d, e, work, info )
     !! DLASQ1 computes the singular values of a real N-by-N bidiagonal
     !! matrix with diagonal D and off-diagonal E. The singular values
     !! are computed to high relative accuracy, in the absence of
     !! denormalization, underflow and overflow. The algorithm was first
     !! presented in
     !! "Accurate singular values and differential qd algorithms" by K. V.
     !! Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
     !! 1994,
     !! and the present implementation is described in "An implementation of
     !! the dqds Algorithm (Positive Case)", LAPACK Working Note.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo
           real(dp) :: eps, scale, safmin, sigmn, sigmx
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DLASQ1', -info )
              return
           else if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              d( 1_ilp ) = abs( d( 1_ilp ) )
              return
           else if( n==2_ilp ) then
              call stdlib_dlas2( d( 1_ilp ), e( 1_ilp ), d( 2_ilp ), sigmn, sigmx )
              d( 1_ilp ) = sigmx
              d( 2_ilp ) = sigmn
              return
           end if
           ! estimate the largest singular value.
           sigmx = zero
           do i = 1, n - 1
              d( i ) = abs( d( i ) )
              sigmx = max( sigmx, abs( e( i ) ) )
           end do
           d( n ) = abs( d( n ) )
           ! early return if sigmx is zero (matrix is already diagonal).
           if( sigmx==zero ) then
              call stdlib_dlasrt( 'D', n, d, iinfo )
              return
           end if
           do i = 1, n
              sigmx = max( sigmx, d( i ) )
           end do
           ! copy d and e into work (in the z format) and scale (squaring the
           ! input data makes scaling by a power of the radix pointless).
           eps = stdlib_dlamch( 'PRECISION' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           scale = sqrt( eps / safmin )
           call stdlib_dcopy( n, d, 1_ilp, work( 1_ilp ), 2_ilp )
           call stdlib_dcopy( n-1, e, 1_ilp, work( 2_ilp ), 2_ilp )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, sigmx, scale, 2_ilp*n-1, 1_ilp, work, 2_ilp*n-1,iinfo )
           ! compute the q's and e's.
           do i = 1, 2*n - 1
              work( i ) = work( i )**2_ilp
           end do
           work( 2_ilp*n ) = zero
           call stdlib_dlasq2( n, work, info )
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = sqrt( work( i ) )
              end do
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, d, n, iinfo )
           else if( info==2_ilp ) then
           ! maximum number of iterations exceeded.  move data from work
           ! into d and e so the calling subroutine can try to finish
              do i = 1, n
                 d( i ) = sqrt( work( 2_ilp*i-1 ) )
                 e( i ) = sqrt( work( 2_ilp*i ) )
              end do
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, d, n, iinfo )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, scale, sigmx, n, 1_ilp, e, n, iinfo )
           end if
           return
     end subroutine stdlib_dlasq1




     pure module subroutine stdlib_slasq2( n, z, info )
     !! SLASQ2 computes all the eigenvalues of the symmetric positive
     !! definite tridiagonal matrix associated with the qd array Z to high
     !! relative accuracy are computed to high relative accuracy, in the
     !! absence of denormalization, underflow and overflow.
     !! To see the relation of Z to the tridiagonal matrix, let L be a
     !! unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
     !! let U be an upper bidiagonal matrix with 1's above and diagonal
     !! Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
     !! symmetric tridiagonal to which it is similar.
     !! Note : SLASQ2 defines a logical variable, IEEE, which is true
     !! on machines which follow ieee-754 floating-point standard in their
     !! handling of infinities and NaNs, and false otherwise. This variable
     !! is passed to SLASQ3.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cbias = 1.50_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           logical(lk) :: ieee
           integer(ilp) :: i0, i4, iinfo, ipn4, iter, iwhila, iwhilb, k, kmin, n0, nbig, ndiv, &
                     nfail, pp, splt, ttype, i1, n1
           real(sp) :: d, dee, deemin, desig, dmin, dmin1, dmin2, dn, dn1, dn2, e, emax, emin, &
           eps, g, oldemn, qmax, qmin, s, safmin, sigma, t, tau, temp, tol, tol2, trace, zmax, &
                     tempe, tempq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           ! (in case stdlib_slasq2 is not called by stdlib_slasq1)
           info = 0_ilp
           eps = stdlib_slamch( 'PRECISION' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           tol = eps*hundrd
           tol2 = tol**2_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SLASQ2', 1_ilp )
              return
           else if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              ! 1-by-1 case.
              if( z( 1_ilp )<zero ) then
                 info = -201_ilp
                 call stdlib_xerbla( 'SLASQ2', 2_ilp )
              end if
              return
           else if( n==2_ilp ) then
              ! 2-by-2 case.
              if( z( 1_ilp )<zero ) then
                 info = -201_ilp
                 call stdlib_xerbla( 'SLASQ2', 2_ilp )
                 return
              else if( z( 2_ilp )<zero ) then
                 info = -202_ilp
                 call stdlib_xerbla( 'SLASQ2', 2_ilp )
                 return
              else if( z( 3_ilp )<zero ) then
                info = -203_ilp
                call stdlib_xerbla( 'SLASQ2', 2_ilp )
                return
              else if( z( 3_ilp )>z( 1_ilp ) ) then
                 d = z( 3_ilp )
                 z( 3_ilp ) = z( 1_ilp )
                 z( 1_ilp ) = d
              end if
              z( 5_ilp ) = z( 1_ilp ) + z( 2_ilp ) + z( 3_ilp )
              if( z( 2_ilp )>z( 3_ilp )*tol2 ) then
                 t = half*( ( z( 1_ilp )-z( 3_ilp ) )+z( 2_ilp ) )
                 s = z( 3_ilp )*( z( 2_ilp ) / t )
                 if( s<=t ) then
                    s = z( 3_ilp )*( z( 2_ilp ) / ( t*( one+sqrt( one+s / t ) ) ) )
                 else
                    s = z( 3_ilp )*( z( 2_ilp ) / ( t+sqrt( t )*sqrt( t+s ) ) )
                 end if
                 t = z( 1_ilp ) + ( s+z( 2_ilp ) )
                 z( 3_ilp ) = z( 3_ilp )*( z( 1_ilp ) / t )
                 z( 1_ilp ) = t
              end if
              z( 2_ilp ) = z( 3_ilp )
              z( 6_ilp ) = z( 2_ilp ) + z( 1_ilp )
              return
           end if
           ! check for negative data and compute sums of q's and e's.
           z( 2_ilp*n ) = zero
           emin = z( 2_ilp )
           qmax = zero
           zmax = zero
           d = zero
           e = zero
           do k = 1, 2*( n-1 ), 2
              if( z( k )<zero ) then
                 info = -( 200_ilp+k )
                 call stdlib_xerbla( 'SLASQ2', 2_ilp )
                 return
              else if( z( k+1 )<zero ) then
                 info = -( 200_ilp+k+1 )
                 call stdlib_xerbla( 'SLASQ2', 2_ilp )
                 return
              end if
              d = d + z( k )
              e = e + z( k+1 )
              qmax = max( qmax, z( k ) )
              emin = min( emin, z( k+1 ) )
              zmax = max( qmax, zmax, z( k+1 ) )
           end do
           if( z( 2_ilp*n-1 )<zero ) then
              info = -( 200_ilp+2*n-1 )
              call stdlib_xerbla( 'SLASQ2', 2_ilp )
              return
           end if
           d = d + z( 2_ilp*n-1 )
           qmax = max( qmax, z( 2_ilp*n-1 ) )
           zmax = max( qmax, zmax )
           ! check for diagonality.
           if( e==zero ) then
              do k = 2, n
                 z( k ) = z( 2_ilp*k-1 )
              end do
              call stdlib_slasrt( 'D', n, z, iinfo )
              z( 2_ilp*n-1 ) = d
              return
           end if
           trace = d + e
           ! check for zero data.
           if( trace==zero ) then
              z( 2_ilp*n-1 ) = zero
              return
           end if
           ! check whether the machine is ieee conformable.
           ! ieee = ( stdlib_ilaenv( 10, 'slasq2', 'n', 1, 2, 3, 4 )==1 )
           ! [11/15/2008] the case ieee=.true. has a problem in single precision with
           ! some the test matrices of type 16. the double precision code is fine.
           ieee = .false.
           ! rearrange data for locality: z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
           do k = 2*n, 2, -2
              z( 2_ilp*k ) = zero
              z( 2_ilp*k-1 ) = z( k )
              z( 2_ilp*k-2 ) = zero
              z( 2_ilp*k-3 ) = z( k-1 )
           end do
           i0 = 1_ilp
           n0 = n
           ! reverse the qd-array, if warranted.
           if( cbias*z( 4_ilp*i0-3 )<z( 4_ilp*n0-3 ) ) then
              ipn4 = 4_ilp*( i0+n0 )
              do i4 = 4*i0, 2*( i0+n0-1 ), 4
                 temp = z( i4-3 )
                 z( i4-3 ) = z( ipn4-i4-3 )
                 z( ipn4-i4-3 ) = temp
                 temp = z( i4-1 )
                 z( i4-1 ) = z( ipn4-i4-5 )
                 z( ipn4-i4-5 ) = temp
              end do
           end if
           ! initial split checking via dqd and li's test.
           pp = 0_ilp
           loop_80: do k = 1, 2
              d = z( 4_ilp*n0+pp-3 )
              do i4 = 4*( n0-1 ) + pp, 4*i0 + pp, -4
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    d = z( i4-3 )
                 else
                    d = z( i4-3 )*( d / ( d+z( i4-1 ) ) )
                 end if
              end do
              ! dqd maps z to zz plus li's test.
              emin = z( 4_ilp*i0+pp+1 )
              d = z( 4_ilp*i0+pp-3 )
              do i4 = 4*i0 + pp, 4*( n0-1 ) + pp, 4
                 z( i4-2*pp-2 ) = d + z( i4-1 )
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    z( i4-2*pp-2 ) = d
                    z( i4-2*pp ) = zero
                    d = z( i4+1 )
                 else if( safmin*z( i4+1 )<z( i4-2*pp-2 ) .and.safmin*z( i4-2*pp-2 )<z( i4+1 ) ) &
                           then
                    temp = z( i4+1 ) / z( i4-2*pp-2 )
                    z( i4-2*pp ) = z( i4-1 )*temp
                    d = d*temp
                 else
                    z( i4-2*pp ) = z( i4+1 )*( z( i4-1 ) / z( i4-2*pp-2 ) )
                    d = z( i4+1 )*( d / z( i4-2*pp-2 ) )
                 end if
                 emin = min( emin, z( i4-2*pp ) )
              end do
              z( 4_ilp*n0-pp-2 ) = d
              ! now find qmax.
              qmax = z( 4_ilp*i0-pp-2 )
              do i4 = 4*i0 - pp + 2, 4*n0 - pp - 2, 4
                 qmax = max( qmax, z( i4 ) )
              end do
              ! prepare for the next iteration on k.
              pp = 1_ilp - pp
           end do loop_80
           ! initialise variables to pass to stdlib_slasq3.
           ttype = 0_ilp
           dmin1 = zero
           dmin2 = zero
           dn    = zero
           dn1   = zero
           dn2   = zero
           g     = zero
           tau   = zero
           iter = 2_ilp
           nfail = 0_ilp
           ndiv = 2_ilp*( n0-i0 )
           loop_160: do iwhila = 1, n + 1
              if( n0<1 )go to 170
              ! while array unfinished do
              ! e(n0) holds the value of sigma when submatrix in i0:n0
              ! splits from the rest of the array, but is negated.
              desig = zero
              if( n0==n ) then
                 sigma = zero
              else
                 sigma = -z( 4_ilp*n0-1 )
              end if
              if( sigma<zero ) then
                 info = 1_ilp
                 return
              end if
              ! find last unreduced submatrix's top index i0, find qmax and
              ! emin. find gershgorin-type bound if q's much greater than e's.
              emax = zero
              if( n0>i0 ) then
                 emin = abs( z( 4_ilp*n0-5 ) )
              else
                 emin = zero
              end if
              qmin = z( 4_ilp*n0-3 )
              qmax = qmin
              do i4 = 4*n0, 8, -4
                 if( z( i4-5 )<=zero )go to 100
                 if( qmin>=four*emax ) then
                    qmin = min( qmin, z( i4-3 ) )
                    emax = max( emax, z( i4-5 ) )
                 end if
                 qmax = max( qmax, z( i4-7 )+z( i4-5 ) )
                 emin = min( emin, z( i4-5 ) )
              end do
              i4 = 4_ilp
              100 continue
              i0 = i4 / 4_ilp
              pp = 0_ilp
              if( n0-i0>1_ilp ) then
                 dee = z( 4_ilp*i0-3 )
                 deemin = dee
                 kmin = i0
                 do i4 = 4*i0+1, 4*n0-3, 4
                    dee = z( i4 )*( dee /( dee+z( i4-2 ) ) )
                    if( dee<=deemin ) then
                       deemin = dee
                       kmin = ( i4+3 )/4_ilp
                    end if
                 end do
                 if( (kmin-i0)*2_ilp<n0-kmin .and.deemin<=half*z(4_ilp*n0-3) ) then
                    ipn4 = 4_ilp*( i0+n0 )
                    pp = 2_ilp
                    do i4 = 4*i0, 2*( i0+n0-1 ), 4
                       temp = z( i4-3 )
                       z( i4-3 ) = z( ipn4-i4-3 )
                       z( ipn4-i4-3 ) = temp
                       temp = z( i4-2 )
                       z( i4-2 ) = z( ipn4-i4-2 )
                       z( ipn4-i4-2 ) = temp
                       temp = z( i4-1 )
                       z( i4-1 ) = z( ipn4-i4-5 )
                       z( ipn4-i4-5 ) = temp
                       temp = z( i4 )
                       z( i4 ) = z( ipn4-i4-4 )
                       z( ipn4-i4-4 ) = temp
                    end do
                 end if
              end if
              ! put -(initial shift) into dmin.
              dmin = -max( zero, qmin-two*sqrt( qmin )*sqrt( emax ) )
              ! now i0:n0 is unreduced.
              ! pp = 0 for ping, pp = 1 for pong.
              ! pp = 2 indicates that flipping was applied to the z array and
                     ! and that the tests for deflation upon entry in stdlib_slasq3
                     ! should not be performed.
              nbig = 100_ilp*( n0-i0+1 )
              loop_140: do iwhilb = 1, nbig
                 if( i0>n0 )go to 150
                 ! while submatrix unfinished take a good dqds step.
                 call stdlib_slasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
                           ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
                 pp = 1_ilp - pp
                 ! when emin is very small check for splits.
                 if( pp==0_ilp .and. n0-i0>=3_ilp ) then
                    if( z( 4_ilp*n0 )<=tol2*qmax .or.z( 4_ilp*n0-1 )<=tol2*sigma ) then
                       splt = i0 - 1_ilp
                       qmax = z( 4_ilp*i0-3 )
                       emin = z( 4_ilp*i0-1 )
                       oldemn = z( 4_ilp*i0 )
                       do i4 = 4*i0, 4*( n0-3 ), 4
                          if( z( i4 )<=tol2*z( i4-3 ) .or.z( i4-1 )<=tol2*sigma ) then
                             z( i4-1 ) = -sigma
                             splt = i4 / 4_ilp
                             qmax = zero
                             emin = z( i4+3 )
                             oldemn = z( i4+4 )
                          else
                             qmax = max( qmax, z( i4+1 ) )
                             emin = min( emin, z( i4-1 ) )
                             oldemn = min( oldemn, z( i4 ) )
                          end if
                       end do
                       z( 4_ilp*n0-1 ) = emin
                       z( 4_ilp*n0 ) = oldemn
                       i0 = splt + 1_ilp
                    end if
                 end if
              end do loop_140
              info = 2_ilp
              ! maximum number of iterations exceeded, restore the shift
              ! sigma and place the new d's and e's in a qd array.
              ! this might need to be done for several blocks
              i1 = i0
              n1 = n0
              145 continue
              tempq = z( 4_ilp*i0-3 )
              z( 4_ilp*i0-3 ) = z( 4_ilp*i0-3 ) + sigma
              do k = i0+1, n0
                 tempe = z( 4_ilp*k-5 )
                 z( 4_ilp*k-5 ) = z( 4_ilp*k-5 ) * (tempq / z( 4_ilp*k-7 ))
                 tempq = z( 4_ilp*k-3 )
                 z( 4_ilp*k-3 ) = z( 4_ilp*k-3 ) + sigma + tempe - z( 4_ilp*k-5 )
              end do
              ! prepare to do this on the previous block if there is one
              if( i1>1_ilp ) then
                 n1 = i1-1
                 do while( ( i1>=2 ) .and. ( z(4*i1-5)>=zero ) )
                    i1 = i1 - 1_ilp
                 end do
                 if( i1>=1_ilp ) then
                    sigma = -z(4_ilp*n1-1)
                    go to 145
                 end if
              end if
              do k = 1, n
                 z( 2_ilp*k-1 ) = z( 4_ilp*k-3 )
              ! only the block 1..n0 is unfinished.  the rest of the e's
              ! must be essentially zero, although sometimes other data
              ! has been stored in them.
                 if( k<n0 ) then
                    z( 2_ilp*k ) = z( 4_ilp*k-1 )
                 else
                    z( 2_ilp*k ) = 0_ilp
                 end if
              end do
              return
              ! end iwhilb
              150 continue
           end do loop_160
           info = 3_ilp
           return
           ! end iwhila
           170 continue
           ! move q's to the front.
           do k = 2, n
              z( k ) = z( 4_ilp*k-3 )
           end do
           ! sort and compute sum of eigenvalues.
           call stdlib_slasrt( 'D', n, z, iinfo )
           e = zero
           do k = n, 1, -1
              e = e + z( k )
           end do
           ! store trace, sum(eigenvalues) and information on performance.
           z( 2_ilp*n+1 ) = trace
           z( 2_ilp*n+2 ) = e
           z( 2_ilp*n+3 ) = real( iter,KIND=sp)
           z( 2_ilp*n+4 ) = real( ndiv,KIND=sp) / real( n**2_ilp,KIND=sp)
           z( 2_ilp*n+5 ) = hundrd*nfail / real( iter,KIND=sp)
           return
     end subroutine stdlib_slasq2

     pure module subroutine stdlib_dlasq2( n, z, info )
     !! DLASQ2 computes all the eigenvalues of the symmetric positive
     !! definite tridiagonal matrix associated with the qd array Z to high
     !! relative accuracy are computed to high relative accuracy, in the
     !! absence of denormalization, underflow and overflow.
     !! To see the relation of Z to the tridiagonal matrix, let L be a
     !! unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
     !! let U be an upper bidiagonal matrix with 1's above and diagonal
     !! Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
     !! symmetric tridiagonal to which it is similar.
     !! Note : DLASQ2 defines a logical variable, IEEE, which is true
     !! on machines which follow ieee-754 floating-point standard in their
     !! handling of infinities and NaNs, and false otherwise. This variable
     !! is passed to DLASQ3.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cbias = 1.50_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           logical(lk) :: ieee
           integer(ilp) :: i0, i1, i4, iinfo, ipn4, iter, iwhila, iwhilb, k, kmin, n0, n1, nbig, &
                     ndiv, nfail, pp, splt, ttype
           real(dp) :: d, dee, deemin, desig, dmin, dmin1, dmin2, dn, dn1, dn2, e, emax, emin, &
           eps, g, oldemn, qmax, qmin, s, safmin, sigma, t, tau, temp, tol, tol2, trace, zmax, &
                     tempe, tempq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           ! (in case stdlib_dlasq2 is not called by stdlib_dlasq1)
           info = 0_ilp
           eps = stdlib_dlamch( 'PRECISION' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           tol = eps*hundrd
           tol2 = tol**2_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DLASQ2', 1_ilp )
              return
           else if( n==0_ilp ) then
              return
           else if( n==1_ilp ) then
              ! 1-by-1 case.
              if( z( 1_ilp )<zero ) then
                 info = -201_ilp
                 call stdlib_xerbla( 'DLASQ2', 2_ilp )
              end if
              return
           else if( n==2_ilp ) then
              ! 2-by-2 case.
              if( z( 1_ilp )<zero ) then
                 info = -201_ilp
                 call stdlib_xerbla( 'DLASQ2', 2_ilp )
                 return
              else if( z( 2_ilp )<zero ) then
                 info = -202_ilp
                 call stdlib_xerbla( 'DLASQ2', 2_ilp )
                 return
              else if( z( 3_ilp )<zero ) then
                info = -203_ilp
                call stdlib_xerbla( 'DLASQ2', 2_ilp )
                return
              else if( z( 3_ilp )>z( 1_ilp ) ) then
                 d = z( 3_ilp )
                 z( 3_ilp ) = z( 1_ilp )
                 z( 1_ilp ) = d
              end if
              z( 5_ilp ) = z( 1_ilp ) + z( 2_ilp ) + z( 3_ilp )
              if( z( 2_ilp )>z( 3_ilp )*tol2 ) then
                 t = half*( ( z( 1_ilp )-z( 3_ilp ) )+z( 2_ilp ) )
                 s = z( 3_ilp )*( z( 2_ilp ) / t )
                 if( s<=t ) then
                    s = z( 3_ilp )*( z( 2_ilp ) / ( t*( one+sqrt( one+s / t ) ) ) )
                 else
                    s = z( 3_ilp )*( z( 2_ilp ) / ( t+sqrt( t )*sqrt( t+s ) ) )
                 end if
                 t = z( 1_ilp ) + ( s+z( 2_ilp ) )
                 z( 3_ilp ) = z( 3_ilp )*( z( 1_ilp ) / t )
                 z( 1_ilp ) = t
              end if
              z( 2_ilp ) = z( 3_ilp )
              z( 6_ilp ) = z( 2_ilp ) + z( 1_ilp )
              return
           end if
           ! check for negative data and compute sums of q's and e's.
           z( 2_ilp*n ) = zero
           emin = z( 2_ilp )
           qmax = zero
           zmax = zero
           d = zero
           e = zero
           do k = 1, 2*( n-1 ), 2
              if( z( k )<zero ) then
                 info = -( 200_ilp+k )
                 call stdlib_xerbla( 'DLASQ2', 2_ilp )
                 return
              else if( z( k+1 )<zero ) then
                 info = -( 200_ilp+k+1 )
                 call stdlib_xerbla( 'DLASQ2', 2_ilp )
                 return
              end if
              d = d + z( k )
              e = e + z( k+1 )
              qmax = max( qmax, z( k ) )
              emin = min( emin, z( k+1 ) )
              zmax = max( qmax, zmax, z( k+1 ) )
           end do
           if( z( 2_ilp*n-1 )<zero ) then
              info = -( 200_ilp+2*n-1 )
              call stdlib_xerbla( 'DLASQ2', 2_ilp )
              return
           end if
           d = d + z( 2_ilp*n-1 )
           qmax = max( qmax, z( 2_ilp*n-1 ) )
           zmax = max( qmax, zmax )
           ! check for diagonality.
           if( e==zero ) then
              do k = 2, n
                 z( k ) = z( 2_ilp*k-1 )
              end do
              call stdlib_dlasrt( 'D', n, z, iinfo )
              z( 2_ilp*n-1 ) = d
              return
           end if
           trace = d + e
           ! check for zero data.
           if( trace==zero ) then
              z( 2_ilp*n-1 ) = zero
              return
           end if
           ! check whether the machine is ieee conformable.
           ieee = ( stdlib_ilaenv( 10_ilp, 'DLASQ2', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )==1_ilp )
           ! rearrange data for locality: z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
           do k = 2*n, 2, -2
              z( 2_ilp*k ) = zero
              z( 2_ilp*k-1 ) = z( k )
              z( 2_ilp*k-2 ) = zero
              z( 2_ilp*k-3 ) = z( k-1 )
           end do
           i0 = 1_ilp
           n0 = n
           ! reverse the qd-array, if warranted.
           if( cbias*z( 4_ilp*i0-3 )<z( 4_ilp*n0-3 ) ) then
              ipn4 = 4_ilp*( i0+n0 )
              do i4 = 4*i0, 2*( i0+n0-1 ), 4
                 temp = z( i4-3 )
                 z( i4-3 ) = z( ipn4-i4-3 )
                 z( ipn4-i4-3 ) = temp
                 temp = z( i4-1 )
                 z( i4-1 ) = z( ipn4-i4-5 )
                 z( ipn4-i4-5 ) = temp
              end do
           end if
           ! initial split checking via dqd and li's test.
           pp = 0_ilp
           loop_80: do k = 1, 2
              d = z( 4_ilp*n0+pp-3 )
              do i4 = 4*( n0-1 ) + pp, 4*i0 + pp, -4
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    d = z( i4-3 )
                 else
                    d = z( i4-3 )*( d / ( d+z( i4-1 ) ) )
                 end if
              end do
              ! dqd maps z to zz plus li's test.
              emin = z( 4_ilp*i0+pp+1 )
              d = z( 4_ilp*i0+pp-3 )
              do i4 = 4*i0 + pp, 4*( n0-1 ) + pp, 4
                 z( i4-2*pp-2 ) = d + z( i4-1 )
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    z( i4-2*pp-2 ) = d
                    z( i4-2*pp ) = zero
                    d = z( i4+1 )
                 else if( safmin*z( i4+1 )<z( i4-2*pp-2 ) .and.safmin*z( i4-2*pp-2 )<z( i4+1 ) ) &
                           then
                    temp = z( i4+1 ) / z( i4-2*pp-2 )
                    z( i4-2*pp ) = z( i4-1 )*temp
                    d = d*temp
                 else
                    z( i4-2*pp ) = z( i4+1 )*( z( i4-1 ) / z( i4-2*pp-2 ) )
                    d = z( i4+1 )*( d / z( i4-2*pp-2 ) )
                 end if
                 emin = min( emin, z( i4-2*pp ) )
              end do
              z( 4_ilp*n0-pp-2 ) = d
              ! now find qmax.
              qmax = z( 4_ilp*i0-pp-2 )
              do i4 = 4*i0 - pp + 2, 4*n0 - pp - 2, 4
                 qmax = max( qmax, z( i4 ) )
              end do
              ! prepare for the next iteration on k.
              pp = 1_ilp - pp
           end do loop_80
           ! initialise variables to pass to stdlib_dlasq3.
           ttype = 0_ilp
           dmin1 = zero
           dmin2 = zero
           dn    = zero
           dn1   = zero
           dn2   = zero
           g     = zero
           tau   = zero
           iter = 2_ilp
           nfail = 0_ilp
           ndiv = 2_ilp*( n0-i0 )
           loop_160: do iwhila = 1, n + 1
              if( n0<1 )go to 170
              ! while array unfinished do
              ! e(n0) holds the value of sigma when submatrix in i0:n0
              ! splits from the rest of the array, but is negated.
              desig = zero
              if( n0==n ) then
                 sigma = zero
              else
                 sigma = -z( 4_ilp*n0-1 )
              end if
              if( sigma<zero ) then
                 info = 1_ilp
                 return
              end if
              ! find last unreduced submatrix's top index i0, find qmax and
              ! emin. find gershgorin-type bound if q's much greater than e's.
              emax = zero
              if( n0>i0 ) then
                 emin = abs( z( 4_ilp*n0-5 ) )
              else
                 emin = zero
              end if
              qmin = z( 4_ilp*n0-3 )
              qmax = qmin
              do i4 = 4*n0, 8, -4
                 if( z( i4-5 )<=zero )go to 100
                 if( qmin>=four*emax ) then
                    qmin = min( qmin, z( i4-3 ) )
                    emax = max( emax, z( i4-5 ) )
                 end if
                 qmax = max( qmax, z( i4-7 )+z( i4-5 ) )
                 emin = min( emin, z( i4-5 ) )
              end do
              i4 = 4_ilp
              100 continue
              i0 = i4 / 4_ilp
              pp = 0_ilp
              if( n0-i0>1_ilp ) then
                 dee = z( 4_ilp*i0-3 )
                 deemin = dee
                 kmin = i0
                 do i4 = 4*i0+1, 4*n0-3, 4
                    dee = z( i4 )*( dee /( dee+z( i4-2 ) ) )
                    if( dee<=deemin ) then
                       deemin = dee
                       kmin = ( i4+3 )/4_ilp
                    end if
                 end do
                 if( (kmin-i0)*2_ilp<n0-kmin .and.deemin<=half*z(4_ilp*n0-3) ) then
                    ipn4 = 4_ilp*( i0+n0 )
                    pp = 2_ilp
                    do i4 = 4*i0, 2*( i0+n0-1 ), 4
                       temp = z( i4-3 )
                       z( i4-3 ) = z( ipn4-i4-3 )
                       z( ipn4-i4-3 ) = temp
                       temp = z( i4-2 )
                       z( i4-2 ) = z( ipn4-i4-2 )
                       z( ipn4-i4-2 ) = temp
                       temp = z( i4-1 )
                       z( i4-1 ) = z( ipn4-i4-5 )
                       z( ipn4-i4-5 ) = temp
                       temp = z( i4 )
                       z( i4 ) = z( ipn4-i4-4 )
                       z( ipn4-i4-4 ) = temp
                    end do
                 end if
              end if
              ! put -(initial shift) into dmin.
              dmin = -max( zero, qmin-two*sqrt( qmin )*sqrt( emax ) )
              ! now i0:n0 is unreduced.
              ! pp = 0 for ping, pp = 1 for pong.
              ! pp = 2 indicates that flipping was applied to the z array and
                     ! and that the tests for deflation upon entry in stdlib_dlasq3
                     ! should not be performed.
              nbig = 100_ilp*( n0-i0+1 )
              loop_140: do iwhilb = 1, nbig
                 if( i0>n0 )go to 150
                 ! while submatrix unfinished take a good dqds step.
                 call stdlib_dlasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
                           ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
                 pp = 1_ilp - pp
                 ! when emin is very small check for splits.
                 if( pp==0_ilp .and. n0-i0>=3_ilp ) then
                    if( z( 4_ilp*n0 )<=tol2*qmax .or.z( 4_ilp*n0-1 )<=tol2*sigma ) then
                       splt = i0 - 1_ilp
                       qmax = z( 4_ilp*i0-3 )
                       emin = z( 4_ilp*i0-1 )
                       oldemn = z( 4_ilp*i0 )
                       do i4 = 4*i0, 4*( n0-3 ), 4
                          if( z( i4 )<=tol2*z( i4-3 ) .or.z( i4-1 )<=tol2*sigma ) then
                             z( i4-1 ) = -sigma
                             splt = i4 / 4_ilp
                             qmax = zero
                             emin = z( i4+3 )
                             oldemn = z( i4+4 )
                          else
                             qmax = max( qmax, z( i4+1 ) )
                             emin = min( emin, z( i4-1 ) )
                             oldemn = min( oldemn, z( i4 ) )
                          end if
                       end do
                       z( 4_ilp*n0-1 ) = emin
                       z( 4_ilp*n0 ) = oldemn
                       i0 = splt + 1_ilp
                    end if
                 end if
              end do loop_140
              info = 2_ilp
              ! maximum number of iterations exceeded, restore the shift
              ! sigma and place the new d's and e's in a qd array.
              ! this might need to be done for several blocks
              i1 = i0
              n1 = n0
              145 continue
              tempq = z( 4_ilp*i0-3 )
              z( 4_ilp*i0-3 ) = z( 4_ilp*i0-3 ) + sigma
              do k = i0+1, n0
                 tempe = z( 4_ilp*k-5 )
                 z( 4_ilp*k-5 ) = z( 4_ilp*k-5 ) * (tempq / z( 4_ilp*k-7 ))
                 tempq = z( 4_ilp*k-3 )
                 z( 4_ilp*k-3 ) = z( 4_ilp*k-3 ) + sigma + tempe - z( 4_ilp*k-5 )
              end do
              ! prepare to do this on the previous block if there is one
              if( i1>1_ilp ) then
                 n1 = i1-1
                 do while( ( i1>=2 ) .and. ( z(4*i1-5)>=zero ) )
                    i1 = i1 - 1_ilp
                 end do
                 sigma = -z(4_ilp*n1-1)
                 go to 145
              end if
              do k = 1, n
                 z( 2_ilp*k-1 ) = z( 4_ilp*k-3 )
              ! only the block 1..n0 is unfinished.  the rest of the e's
              ! must be essentially zero, although sometimes other data
              ! has been stored in them.
                 if( k<n0 ) then
                    z( 2_ilp*k ) = z( 4_ilp*k-1 )
                 else
                    z( 2_ilp*k ) = 0_ilp
                 end if
              end do
              return
              ! end iwhilb
              150 continue
           end do loop_160
           info = 3_ilp
           return
           ! end iwhila
           170 continue
           ! move q's to the front.
           do k = 2, n
              z( k ) = z( 4_ilp*k-3 )
           end do
           ! sort and compute sum of eigenvalues.
           call stdlib_dlasrt( 'D', n, z, iinfo )
           e = zero
           do k = n, 1, -1
              e = e + z( k )
           end do
           ! store trace, sum(eigenvalues) and information on performance.
           z( 2_ilp*n+1 ) = trace
           z( 2_ilp*n+2 ) = e
           z( 2_ilp*n+3 ) = real( iter,KIND=dp)
           z( 2_ilp*n+4 ) = real( ndiv,KIND=dp) / real( n**2_ilp,KIND=dp)
           z( 2_ilp*n+5 ) = hundrd*nfail / real( iter,KIND=dp)
           return
     end subroutine stdlib_dlasq2




     pure module subroutine stdlib_slasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
     !! SLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
     !! In case of failure it changes shifts, and tries again until output
     !! is positive.
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0
           integer(ilp), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(sp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(sp), intent(out) :: dmin, sigma
           integer(ilp), intent(inout) :: ttype
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cbias = 1.50_sp
           real(sp), parameter :: qurtr = 0.250_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           integer(ilp) :: ipn4, j4, n0in, nn
           real(sp) :: eps, s, t, temp, tol, tol2
           ! Intrinsic Functions 
           ! Executable Statements 
           n0in = n0
           eps = stdlib_slamch( 'PRECISION' )
           tol = eps*hundrd
           tol2 = tol**2_ilp
           ! check for deflation.
           10 continue
           if( n0<i0 )return
           if( n0==i0 )go to 20
           nn = 4_ilp*n0 + pp
           if( n0==( i0+1 ) )go to 40
           ! check whether e(n0-1) is negligible, 1 eigenvalue.
           if( z( nn-5 )>tol2*( sigma+z( nn-3 ) ) .and.z( nn-2*pp-4 )>tol2*z( nn-7 ) )go to 30

           20 continue
           z( 4_ilp*n0-3 ) = z( 4_ilp*n0+pp-3 ) + sigma
           n0 = n0 - 1_ilp
           go to 10
           ! check  whether e(n0-2) is negligible, 2 eigenvalues.
           30 continue
           if( z( nn-9 )>tol2*sigma .and.z( nn-2*pp-8 )>tol2*z( nn-11 ) )go to 50
           40 continue
           if( z( nn-3 )>z( nn-7 ) ) then
              s = z( nn-3 )
              z( nn-3 ) = z( nn-7 )
              z( nn-7 ) = s
           end if
           t = half*( ( z( nn-7 )-z( nn-3 ) )+z( nn-5 ) )
           if( z( nn-5 )>z( nn-3 )*tol2.and.t/=zero ) then
              s = z( nn-3 )*( z( nn-5 ) / t )
              if( s<=t ) then
                 s = z( nn-3 )*( z( nn-5 ) /( t*( one+sqrt( one+s / t ) ) ) )
              else
                 s = z( nn-3 )*( z( nn-5 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
              end if
              t = z( nn-7 ) + ( s+z( nn-5 ) )
              z( nn-3 ) = z( nn-3 )*( z( nn-7 ) / t )
              z( nn-7 ) = t
           end if
           z( 4_ilp*n0-7 ) = z( nn-7 ) + sigma
           z( 4_ilp*n0-3 ) = z( nn-3 ) + sigma
           n0 = n0 - 2_ilp
           go to 10
           50 continue
           if( pp==2_ilp )pp = 0_ilp
           ! reverse the qd-array, if warranted.
           if( dmin<=zero .or. n0<n0in ) then
              if( cbias*z( 4_ilp*i0+pp-3 )<z( 4_ilp*n0+pp-3 ) ) then
                 ipn4 = 4_ilp*( i0+n0 )
                 do j4 = 4*i0, 2*( i0+n0-1 ), 4
                    temp = z( j4-3 )
                    z( j4-3 ) = z( ipn4-j4-3 )
                    z( ipn4-j4-3 ) = temp
                    temp = z( j4-2 )
                    z( j4-2 ) = z( ipn4-j4-2 )
                    z( ipn4-j4-2 ) = temp
                    temp = z( j4-1 )
                    z( j4-1 ) = z( ipn4-j4-5 )
                    z( ipn4-j4-5 ) = temp
                    temp = z( j4 )
                    z( j4 ) = z( ipn4-j4-4 )
                    z( ipn4-j4-4 ) = temp
                 end do
                 if( n0-i0<=4_ilp ) then
                    z( 4_ilp*n0+pp-1 ) = z( 4_ilp*i0+pp-1 )
                    z( 4_ilp*n0-pp ) = z( 4_ilp*i0-pp )
                 end if
                 dmin2 = min( dmin2, z( 4_ilp*n0+pp-1 ) )
                 z( 4_ilp*n0+pp-1 ) = min( z( 4_ilp*n0+pp-1 ), z( 4_ilp*i0+pp-1 ),z( 4_ilp*i0+pp+3 ) )
                 z( 4_ilp*n0-pp ) = min( z( 4_ilp*n0-pp ), z( 4_ilp*i0-pp ),z( 4_ilp*i0-pp+4 ) )
                 qmax = max( qmax, z( 4_ilp*i0+pp-3 ), z( 4_ilp*i0+pp+1 ) )
                 dmin = -zero
              end if
           end if
           ! choose a shift.
           call stdlib_slasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1,dn2, tau, ttype, &
                     g )
           ! call dqds until dmin > 0.
           70 continue
           call stdlib_slasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2, dn,dn1, dn2, ieee, &
                     eps )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp
           ! check status.
           if( dmin>=zero .and. dmin1>=zero ) then
              ! success.
              go to 90
           else if( dmin<zero .and. dmin1>zero .and.z( 4_ilp*( n0-1 )-pp )<tol*( sigma+dn1 ) .and.abs(&
                      dn )<tol*sigma ) then
              ! convergence hidden by negative dn.
              z( 4_ilp*( n0-1 )-pp+2 ) = zero
              dmin = zero
              go to 90
           else if( dmin<zero ) then
              ! tau too big. select new tau and try again.
              nfail = nfail + 1_ilp
              if( ttype<-22_ilp ) then
                 ! failed twice. play it safe.
                 tau = zero
              else if( dmin1>zero ) then
                 ! late failure. gives excellent shift.
                 tau = ( tau+dmin )*( one-two*eps )
                 ttype = ttype - 11_ilp
              else
                 ! early failure. divide by 4.
                 tau = qurtr*tau
                 ttype = ttype - 12_ilp
              end if
              go to 70
           else if( stdlib_sisnan( dmin ) ) then
              ! nan.
              if( tau==zero ) then
                 go to 80
              else
                 tau = zero
                 go to 70
              end if
           else
              ! possible underflow. play it safe.
              go to 80
           end if
           ! risk of underflow.
           80 continue
           call stdlib_slasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn, dn1, dn2 )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp
           tau = zero
           90 continue
           if( tau<sigma ) then
              desig = desig + tau
              t = sigma + desig
              desig = desig - ( t-sigma )
           else
              t = sigma + tau
              desig = sigma - ( t-tau ) + desig
           end if
           sigma = t
           return
     end subroutine stdlib_slasq3

     pure module subroutine stdlib_dlasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
     !! DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
     !! In case of failure it changes shifts, and tries again until output
     !! is positive.
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0
           integer(ilp), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(dp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(dp), intent(out) :: dmin, sigma
           integer(ilp), intent(inout) :: ttype
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cbias = 1.50_dp
           real(dp), parameter :: qurtr = 0.250_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           integer(ilp) :: ipn4, j4, n0in, nn
           real(dp) :: eps, s, t, temp, tol, tol2
           ! Intrinsic Functions 
           ! Executable Statements 
           n0in = n0
           eps = stdlib_dlamch( 'PRECISION' )
           tol = eps*hundrd
           tol2 = tol**2_ilp
           ! check for deflation.
           10 continue
           if( n0<i0 )return
           if( n0==i0 )go to 20
           nn = 4_ilp*n0 + pp
           if( n0==( i0+1 ) )go to 40
           ! check whether e(n0-1) is negligible, 1 eigenvalue.
           if( z( nn-5 )>tol2*( sigma+z( nn-3 ) ) .and.z( nn-2*pp-4 )>tol2*z( nn-7 ) ) go to 30
           20 continue
           z( 4_ilp*n0-3 ) = z( 4_ilp*n0+pp-3 ) + sigma
           n0 = n0 - 1_ilp
           go to 10
           ! check  whether e(n0-2) is negligible, 2 eigenvalues.
           30 continue
           if( z( nn-9 )>tol2*sigma .and.z( nn-2*pp-8 )>tol2*z( nn-11 ) )go to 50
           40 continue
           if( z( nn-3 )>z( nn-7 ) ) then
              s = z( nn-3 )
              z( nn-3 ) = z( nn-7 )
              z( nn-7 ) = s
           end if
           t = half*( ( z( nn-7 )-z( nn-3 ) )+z( nn-5 ) )
           if( z( nn-5 )>z( nn-3 )*tol2.and.t/=zero ) then
              s = z( nn-3 )*( z( nn-5 ) / t )
              if( s<=t ) then
                 s = z( nn-3 )*( z( nn-5 ) /( t*( one+sqrt( one+s / t ) ) ) )
              else
                 s = z( nn-3 )*( z( nn-5 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
              end if
              t = z( nn-7 ) + ( s+z( nn-5 ) )
              z( nn-3 ) = z( nn-3 )*( z( nn-7 ) / t )
              z( nn-7 ) = t
           end if
           z( 4_ilp*n0-7 ) = z( nn-7 ) + sigma
           z( 4_ilp*n0-3 ) = z( nn-3 ) + sigma
           n0 = n0 - 2_ilp
           go to 10
           50 continue
           if( pp==2_ilp )pp = 0_ilp
           ! reverse the qd-array, if warranted.
           if( dmin<=zero .or. n0<n0in ) then
              if( cbias*z( 4_ilp*i0+pp-3 )<z( 4_ilp*n0+pp-3 ) ) then
                 ipn4 = 4_ilp*( i0+n0 )
                 do j4 = 4*i0, 2*( i0+n0-1 ), 4
                    temp = z( j4-3 )
                    z( j4-3 ) = z( ipn4-j4-3 )
                    z( ipn4-j4-3 ) = temp
                    temp = z( j4-2 )
                    z( j4-2 ) = z( ipn4-j4-2 )
                    z( ipn4-j4-2 ) = temp
                    temp = z( j4-1 )
                    z( j4-1 ) = z( ipn4-j4-5 )
                    z( ipn4-j4-5 ) = temp
                    temp = z( j4 )
                    z( j4 ) = z( ipn4-j4-4 )
                    z( ipn4-j4-4 ) = temp
                 end do
                 if( n0-i0<=4_ilp ) then
                    z( 4_ilp*n0+pp-1 ) = z( 4_ilp*i0+pp-1 )
                    z( 4_ilp*n0-pp ) = z( 4_ilp*i0-pp )
                 end if
                 dmin2 = min( dmin2, z( 4_ilp*n0+pp-1 ) )
                 z( 4_ilp*n0+pp-1 ) = min( z( 4_ilp*n0+pp-1 ), z( 4_ilp*i0+pp-1 ),z( 4_ilp*i0+pp+3 ) )
                 z( 4_ilp*n0-pp ) = min( z( 4_ilp*n0-pp ), z( 4_ilp*i0-pp ),z( 4_ilp*i0-pp+4 ) )
                 qmax = max( qmax, z( 4_ilp*i0+pp-3 ), z( 4_ilp*i0+pp+1 ) )
                 dmin = -zero
              end if
           end if
           ! choose a shift.
           call stdlib_dlasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1,dn2, tau, ttype, &
                     g )
           ! call dqds until dmin > 0.
           70 continue
           call stdlib_dlasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2, dn,dn1, dn2, ieee, &
                     eps )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp
           ! check status.
           if( dmin>=zero .and. dmin1>=zero ) then
              ! success.
              go to 90
           else if( dmin<zero .and. dmin1>zero .and.z( 4_ilp*( n0-1 )-pp )<tol*( sigma+dn1 ) .and.abs(&
                      dn )<tol*sigma ) then
              ! convergence hidden by negative dn.
              z( 4_ilp*( n0-1 )-pp+2 ) = zero
              dmin = zero
              go to 90
           else if( dmin<zero ) then
              ! tau too big. select new tau and try again.
              nfail = nfail + 1_ilp
              if( ttype<-22_ilp ) then
                 ! failed twice. play it safe.
                 tau = zero
              else if( dmin1>zero ) then
                 ! late failure. gives excellent shift.
                 tau = ( tau+dmin )*( one-two*eps )
                 ttype = ttype - 11_ilp
              else
                 ! early failure. divide by 4.
                 tau = qurtr*tau
                 ttype = ttype - 12_ilp
              end if
              go to 70
           else if( stdlib_disnan( dmin ) ) then
              ! nan.
              if( tau==zero ) then
                 go to 80
              else
                 tau = zero
                 go to 70
              end if
           else
              ! possible underflow. play it safe.
              go to 80
           end if
           ! risk of underflow.
           80 continue
           call stdlib_dlasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn, dn1, dn2 )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp
           tau = zero
           90 continue
           if( tau<sigma ) then
              desig = desig + tau
              t = sigma + desig
              desig = desig - ( t-sigma )
           else
              t = sigma + tau
              desig = sigma - ( t-tau ) + desig
           end if
           sigma = t
           return
     end subroutine stdlib_dlasq3




     pure module subroutine stdlib_slasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
     !! SLASQ4 computes an approximation TAU to the smallest eigenvalue
     !! using values of d from the previous transform.
               ttype, g )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i0, n0, n0in, pp
           integer(ilp), intent(out) :: ttype
           real(sp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(sp), intent(inout) :: g
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(in) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cnst1 = 0.5630_sp
           real(sp), parameter :: cnst2 = 1.010_sp
           real(sp), parameter :: cnst3 = 1.050_sp
           real(sp), parameter :: qurtr = 0.250_sp
           real(sp), parameter :: third = 0.3330_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           integer(ilp) :: i4, nn, np
           real(sp) :: a2, b1, b2, gam, gap1, gap2, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! a negative dmin forces the shift to take that absolute value
           ! ttype records the type of shift.
           if( dmin<=zero ) then
              tau = -dmin
              ttype = -1_ilp
              return
           end if
           nn = 4_ilp*n0 + pp
           if( n0in==n0 ) then
              ! no eigenvalues deflated.
              if( dmin==dn .or. dmin==dn1 ) then
                 b1 = sqrt( z( nn-3 ) )*sqrt( z( nn-5 ) )
                 b2 = sqrt( z( nn-7 ) )*sqrt( z( nn-9 ) )
                 a2 = z( nn-7 ) + z( nn-5 )
                 ! cases 2 and 3.
                 if( dmin==dn .and. dmin1==dn1 ) then
                    gap2 = dmin2 - a2 - dmin2*qurtr
                    if( gap2>zero .and. gap2>b2 ) then
                       gap1 = a2 - dn - ( b2 / gap2 )*b2
                    else
                       gap1 = a2 - dn - ( b1+b2 )
                    end if
                    if( gap1>zero .and. gap1>b1 ) then
                       s = max( dn-( b1 / gap1 )*b1, half*dmin )
                       ttype = -2_ilp
                    else
                       s = zero
                       if( dn>b1 )s = dn - b1
                       if( a2>( b1+b2 ) )s = min( s, a2-( b1+b2 ) )
                       s = max( s, third*dmin )
                       ttype = -3_ilp
                    end if
                 else
                    ! case 4.
                    ttype = -4_ilp
                    s = qurtr*dmin
                    if( dmin==dn ) then
                       gam = dn
                       a2 = zero
                       if( z( nn-5 ) > z( nn-7 ) )return
                       b2 = z( nn-5 ) / z( nn-7 )
                       np = nn - 9_ilp
                    else
                       np = nn - 2_ilp*pp
                       gam = dn1
                       if( z( np-4 ) > z( np-2 ) )return
                       a2 = z( np-4 ) / z( np-2 )
                       if( z( nn-9 ) > z( nn-11 ) )return
                       b2 = z( nn-9 ) / z( nn-11 )
                       np = nn - 13_ilp
                    end if
                    ! approximate contribution to norm squared from i < nn-1.
                    a2 = a2 + b2
                    do i4 = np, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 20
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 20
                    end do
                    20 continue
                    a2 = cnst3*a2
                    ! rayleigh quotient residual bound.
                    if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
                 end if
              else if( dmin==dn2 ) then
                 ! case 5.
                 ttype = -5_ilp
                 s = qurtr*dmin
                 ! compute contribution to norm squared from i > nn-2.
                 np = nn - 2_ilp*pp
                 b1 = z( np-2 )
                 b2 = z( np-6 )
                 gam = dn2
                 if( z( np-8 )>b2 .or. z( np-4 )>b1 )return
                 a2 = ( z( np-8 ) / b2 )*( one+z( np-4 ) / b1 )
                 ! approximate contribution to norm squared from i < nn-2.
                 if( n0-i0>2_ilp ) then
                    b2 = z( nn-13 ) / z( nn-15 )
                    a2 = a2 + b2
                    do i4 = nn - 17, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 40
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 40
                    end do
                    40 continue
                    a2 = cnst3*a2
                 end if
                 if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
              else
                 ! case 6, no information to guide us.
                 if( ttype==-6_ilp ) then
                    g = g + third*( one-g )
                 else if( ttype==-18_ilp ) then
                    g = qurtr*third
                 else
                    g = qurtr
                 end if
                 s = g*dmin
                 ttype = -6_ilp
              end if
           else if( n0in==( n0+1 ) ) then
              ! one eigenvalue just deflated. use dmin1, dn1 for dmin and dn.
              if( dmin1==dn1 .and. dmin2==dn2 ) then
                 ! cases 7 and 8.
                 ttype = -7_ilp
                 s = third*dmin1
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 60
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    a2 = b1
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*max( b1, a2 )<b2 )go to 60
                 end do
                 60 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin1 / ( one+b2**2_ilp )
                 gap2 = half*dmin2 - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                    ttype = -8_ilp
                 end if
              else
                 ! case 9.
                 s = qurtr*dmin1
                 if( dmin1==dn1 )s = half*dmin1
                 ttype = -9_ilp
              end if
           else if( n0in==( n0+2 ) ) then
              ! two eigenvalues deflated. use dmin2, dn2 for dmin and dn.
              ! cases 10 and 11.
              if( dmin2==dn2 .and. two*z( nn-5 )<z( nn-7 ) ) then
                 ttype = -10_ilp
                 s = third*dmin2
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 80
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*b1<b2 )go to 80
                 end do
                 80 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin2 / ( one+b2**2_ilp )
                 gap2 = z( nn-7 ) + z( nn-9 ) -sqrt( z( nn-11 ) )*sqrt( z( nn-9 ) ) - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                 end if
              else
                 s = qurtr*dmin2
                 ttype = -11_ilp
              end if
           else if( n0in>( n0+2 ) ) then
              ! case 12, more than two eigenvalues deflated. no information.
              s = zero
              ttype = -12_ilp
           end if
           tau = s
           return
     end subroutine stdlib_slasq4

     pure module subroutine stdlib_dlasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
     !! DLASQ4 computes an approximation TAU to the smallest eigenvalue
     !! using values of d from the previous transform.
               ttype, g )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i0, n0, n0in, pp
           integer(ilp), intent(out) :: ttype
           real(dp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(dp), intent(inout) :: g
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(in) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cnst1 = 0.5630_dp
           real(dp), parameter :: cnst2 = 1.010_dp
           real(dp), parameter :: cnst3 = 1.050_dp
           real(dp), parameter :: qurtr = 0.250_dp
           real(dp), parameter :: third = 0.3330_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           integer(ilp) :: i4, nn, np
           real(dp) :: a2, b1, b2, gam, gap1, gap2, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! a negative dmin forces the shift to take that absolute value
           ! ttype records the type of shift.
           if( dmin<=zero ) then
              tau = -dmin
              ttype = -1_ilp
              return
           end if
           nn = 4_ilp*n0 + pp
           if( n0in==n0 ) then
              ! no eigenvalues deflated.
              if( dmin==dn .or. dmin==dn1 ) then
                 b1 = sqrt( z( nn-3 ) )*sqrt( z( nn-5 ) )
                 b2 = sqrt( z( nn-7 ) )*sqrt( z( nn-9 ) )
                 a2 = z( nn-7 ) + z( nn-5 )
                 ! cases 2 and 3.
                 if( dmin==dn .and. dmin1==dn1 ) then
                    gap2 = dmin2 - a2 - dmin2*qurtr
                    if( gap2>zero .and. gap2>b2 ) then
                       gap1 = a2 - dn - ( b2 / gap2 )*b2
                    else
                       gap1 = a2 - dn - ( b1+b2 )
                    end if
                    if( gap1>zero .and. gap1>b1 ) then
                       s = max( dn-( b1 / gap1 )*b1, half*dmin )
                       ttype = -2_ilp
                    else
                       s = zero
                       if( dn>b1 )s = dn - b1
                       if( a2>( b1+b2 ) )s = min( s, a2-( b1+b2 ) )
                       s = max( s, third*dmin )
                       ttype = -3_ilp
                    end if
                 else
                    ! case 4.
                    ttype = -4_ilp
                    s = qurtr*dmin
                    if( dmin==dn ) then
                       gam = dn
                       a2 = zero
                       if( z( nn-5 ) > z( nn-7 ) )return
                       b2 = z( nn-5 ) / z( nn-7 )
                       np = nn - 9_ilp
                    else
                       np = nn - 2_ilp*pp
                       gam = dn1
                       if( z( np-4 ) > z( np-2 ) )return
                       a2 = z( np-4 ) / z( np-2 )
                       if( z( nn-9 ) > z( nn-11 ) )return
                       b2 = z( nn-9 ) / z( nn-11 )
                       np = nn - 13_ilp
                    end if
                    ! approximate contribution to norm squared from i < nn-1.
                    a2 = a2 + b2
                    do i4 = np, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 20
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 20
                    end do
                    20 continue
                    a2 = cnst3*a2
                    ! rayleigh quotient residual bound.
                    if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
                 end if
              else if( dmin==dn2 ) then
                 ! case 5.
                 ttype = -5_ilp
                 s = qurtr*dmin
                 ! compute contribution to norm squared from i > nn-2.
                 np = nn - 2_ilp*pp
                 b1 = z( np-2 )
                 b2 = z( np-6 )
                 gam = dn2
                 if( z( np-8 )>b2 .or. z( np-4 )>b1 )return
                 a2 = ( z( np-8 ) / b2 )*( one+z( np-4 ) / b1 )
                 ! approximate contribution to norm squared from i < nn-2.
                 if( n0-i0>2_ilp ) then
                    b2 = z( nn-13 ) / z( nn-15 )
                    a2 = a2 + b2
                    do i4 = nn - 17, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 40
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 40
                    end do
                    40 continue
                    a2 = cnst3*a2
                 end if
                 if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
              else
                 ! case 6, no information to guide us.
                 if( ttype==-6_ilp ) then
                    g = g + third*( one-g )
                 else if( ttype==-18_ilp ) then
                    g = qurtr*third
                 else
                    g = qurtr
                 end if
                 s = g*dmin
                 ttype = -6_ilp
              end if
           else if( n0in==( n0+1 ) ) then
              ! one eigenvalue just deflated. use dmin1, dn1 for dmin and dn.
              if( dmin1==dn1 .and. dmin2==dn2 ) then
                 ! cases 7 and 8.
                 ttype = -7_ilp
                 s = third*dmin1
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 60
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    a2 = b1
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*max( b1, a2 )<b2 )go to 60
                 end do
                 60 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin1 / ( one+b2**2_ilp )
                 gap2 = half*dmin2 - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                    ttype = -8_ilp
                 end if
              else
                 ! case 9.
                 s = qurtr*dmin1
                 if( dmin1==dn1 )s = half*dmin1
                 ttype = -9_ilp
              end if
           else if( n0in==( n0+2 ) ) then
              ! two eigenvalues deflated. use dmin2, dn2 for dmin and dn.
              ! cases 10 and 11.
              if( dmin2==dn2 .and. two*z( nn-5 )<z( nn-7 ) ) then
                 ttype = -10_ilp
                 s = third*dmin2
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 80
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*b1<b2 )go to 80
                 end do
                 80 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin2 / ( one+b2**2_ilp )
                 gap2 = z( nn-7 ) + z( nn-9 ) -sqrt( z( nn-11 ) )*sqrt( z( nn-9 ) ) - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                 end if
              else
                 s = qurtr*dmin2
                 ttype = -11_ilp
              end if
           else if( n0in>( n0+2 ) ) then
              ! case 12, more than two eigenvalues deflated. no information.
              s = zero
              ttype = -12_ilp
           end if
           tau = s
           return
     end subroutine stdlib_dlasq4




     pure module subroutine stdlib_slasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
     !! SLASQ5 computes one dqds transform in ping-pong form, one
     !! version for IEEE machines another for non IEEE machines.
               ieee, eps )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(sp), intent(inout) :: tau
           real(sp), intent(in) :: sigma, eps
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j4, j4p2
           real(sp) :: d, emin, temp, dthresh
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           dthresh = eps*(sigma+tau)
           if( tau<dthresh*half ) tau = zero
           if( tau/=zero ) then
              j4 = 4_ilp*i0 + pp - 3_ilp
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           else
           ! this is the version that sets d's to zero if they are small enough
              j4 = 4_ilp*i0 + pp - 3_ilp
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           end if
           z( j4+2 ) = dn
           z( 4_ilp*n0-pp ) = emin
           return
     end subroutine stdlib_slasq5

     pure module subroutine stdlib_dlasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
     !! DLASQ5 computes one dqds transform in ping-pong form, one
     !! version for IEEE machines another for non IEEE machines.
               ieee, eps )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(dp), intent(inout) :: tau
           real(dp), intent(in) :: sigma, eps
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j4, j4p2
           real(dp) :: d, emin, temp, dthresh
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           dthresh = eps*(sigma+tau)
           if( tau<dthresh*half ) tau = zero
           if( tau/=zero ) then
           j4 = 4_ilp*i0 + pp - 3_ilp
           emin = z( j4+4 )
           d = z( j4 ) - tau
           dmin = d
           dmin1 = -z( j4 )
           if( ieee ) then
              ! code for ieee arithmetic.
              if( pp==0_ilp ) then
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-2 ) = d + z( j4-1 )
                    temp = z( j4+1 ) / z( j4-2 )
                    d = d*temp - tau
                    dmin = min( dmin, d )
                    z( j4 ) = z( j4-1 )*temp
                    emin = min( z( j4 ), emin )
                 end do
              else
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-3 ) = d + z( j4 )
                    temp = z( j4+2 ) / z( j4-3 )
                    d = d*temp - tau
                    dmin = min( dmin, d )
                    z( j4-1 ) = z( j4 )*temp
                    emin = min( z( j4-1 ), emin )
                 end do
              end if
              ! unroll last two steps.
              dnm2 = d
              dmin2 = dmin
              j4 = 4_ilp*( n0-2 ) - pp
              j4p2 = j4 + 2_ilp*pp - 1_ilp
              z( j4-2 ) = dnm2 + z( j4p2 )
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
              dmin = min( dmin, dnm1 )
              dmin1 = dmin
              j4 = j4 + 4_ilp
              j4p2 = j4 + 2_ilp*pp - 1_ilp
              z( j4-2 ) = dnm1 + z( j4p2 )
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
              dmin = min( dmin, dn )
           else
              ! code for non ieee arithmetic.
              if( pp==0_ilp ) then
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-2 ) = d + z( j4-1 )
                    if( d<zero ) then
                       return
                    else
                       z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                       d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                    end if
                    dmin = min( dmin, d )
                    emin = min( emin, z( j4 ) )
                 end do
              else
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-3 ) = d + z( j4 )
                    if( d<zero ) then
                       return
                    else
                       z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                       d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                    end if
                    dmin = min( dmin, d )
                    emin = min( emin, z( j4-1 ) )
                 end do
              end if
              ! unroll last two steps.
              dnm2 = d
              dmin2 = dmin
              j4 = 4_ilp*( n0-2 ) - pp
              j4p2 = j4 + 2_ilp*pp - 1_ilp
              z( j4-2 ) = dnm2 + z( j4p2 )
              if( dnm2<zero ) then
                 return
              else
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
              end if
              dmin = min( dmin, dnm1 )
              dmin1 = dmin
              j4 = j4 + 4_ilp
              j4p2 = j4 + 2_ilp*pp - 1_ilp
              z( j4-2 ) = dnm1 + z( j4p2 )
              if( dnm1<zero ) then
                 return
              else
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
              end if
              dmin = min( dmin, dn )
           end if
           else
           ! this is the version that sets d's to zero if they are small enough
              j4 = 4_ilp*i0 + pp - 3_ilp
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       if( d<dthresh) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       if( d<dthresh) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp
                 j4p2 = j4 + 2_ilp*pp - 1_ilp
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           end if
           z( j4+2 ) = dn
           z( 4_ilp*n0-pp ) = emin
           return
     end subroutine stdlib_dlasq5




     pure module subroutine stdlib_slasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
     !! SLASQ6 computes one dqd (shift equal to zero) transform in
     !! ping-pong form, with protection against underflow and overflow.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j4, j4p2
           real(sp) :: d, emin, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           j4 = 4_ilp*i0 + pp - 3_ilp
           emin = z( j4+4 )
           d = z( j4 )
           dmin = d
           if( pp==0_ilp ) then
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-2 ) = d + z( j4-1 )
                 if( z( j4-2 )==zero ) then
                    z( j4 ) = zero
                    d = z( j4+1 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+1 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4+1 ) ) &
                           then
                    temp = z( j4+1 ) / z( j4-2 )
                    z( j4 ) = z( j4-1 )*temp
                    d = d*temp
                 else
                    z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                    d = z( j4+1 )*( d / z( j4-2 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4 ) )
              end do
           else
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-3 ) = d + z( j4 )
                 if( z( j4-3 )==zero ) then
                    z( j4-1 ) = zero
                    d = z( j4+2 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+2 )<z( j4-3 ) .and.safmin*z( j4-3 )<z( j4+2 ) ) &
                           then
                    temp = z( j4+2 ) / z( j4-3 )
                    z( j4-1 ) = z( j4 )*temp
                    d = d*temp
                 else
                    z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                    d = z( j4+2 )*( d / z( j4-3 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4-1 ) )
              end do
           end if
           ! unroll last two steps.
           dnm2 = d
           dmin2 = dmin
           j4 = 4_ilp*( n0-2 ) - pp
           j4p2 = j4 + 2_ilp*pp - 1_ilp
           z( j4-2 ) = dnm2 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dnm1 = z( j4p2+2 )
              dmin = dnm1
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dnm1 = dnm2*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) )
           end if
           dmin = min( dmin, dnm1 )
           dmin1 = dmin
           j4 = j4 + 4_ilp
           j4p2 = j4 + 2_ilp*pp - 1_ilp
           z( j4-2 ) = dnm1 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dn = z( j4p2+2 )
              dmin = dn
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dn = dnm1*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) )
           end if
           dmin = min( dmin, dn )
           z( j4+2 ) = dn
           z( 4_ilp*n0-pp ) = emin
           return
     end subroutine stdlib_slasq6

     pure module subroutine stdlib_dlasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
     !! DLASQ6 computes one dqd (shift equal to zero) transform in
     !! ping-pong form, with protection against underflow and overflow.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j4, j4p2
           real(dp) :: d, emin, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           j4 = 4_ilp*i0 + pp - 3_ilp
           emin = z( j4+4 )
           d = z( j4 )
           dmin = d
           if( pp==0_ilp ) then
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-2 ) = d + z( j4-1 )
                 if( z( j4-2 )==zero ) then
                    z( j4 ) = zero
                    d = z( j4+1 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+1 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4+1 ) ) &
                           then
                    temp = z( j4+1 ) / z( j4-2 )
                    z( j4 ) = z( j4-1 )*temp
                    d = d*temp
                 else
                    z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                    d = z( j4+1 )*( d / z( j4-2 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4 ) )
              end do
           else
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-3 ) = d + z( j4 )
                 if( z( j4-3 )==zero ) then
                    z( j4-1 ) = zero
                    d = z( j4+2 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+2 )<z( j4-3 ) .and.safmin*z( j4-3 )<z( j4+2 ) ) &
                           then
                    temp = z( j4+2 ) / z( j4-3 )
                    z( j4-1 ) = z( j4 )*temp
                    d = d*temp
                 else
                    z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                    d = z( j4+2 )*( d / z( j4-3 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4-1 ) )
              end do
           end if
           ! unroll last two steps.
           dnm2 = d
           dmin2 = dmin
           j4 = 4_ilp*( n0-2 ) - pp
           j4p2 = j4 + 2_ilp*pp - 1_ilp
           z( j4-2 ) = dnm2 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dnm1 = z( j4p2+2 )
              dmin = dnm1
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dnm1 = dnm2*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) )
           end if
           dmin = min( dmin, dnm1 )
           dmin1 = dmin
           j4 = j4 + 4_ilp
           j4p2 = j4 + 2_ilp*pp - 1_ilp
           z( j4-2 ) = dnm1 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dn = z( j4p2+2 )
              dmin = dn
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dn = dnm1*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) )
           end if
           dmin = min( dmin, dn )
           z( j4+2 ) = dn
           z( 4_ilp*n0-pp ) = emin
           return
     end subroutine stdlib_dlasq6




     pure module subroutine stdlib_I64_slasq1( n, d, e, work, info )
     !! SLASQ1 computes the singular values of a real N-by-N bidiagonal
     !! matrix with diagonal D and off-diagonal E. The singular values
     !! are computed to high relative accuracy, in the absence of
     !! denormalization, underflow and overflow. The algorithm was first
     !! presented in
     !! "Accurate singular values and differential qd algorithms" by K. V.
     !! Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
     !! 1994,
     !! and the present implementation is described in "An implementation of
     !! the dqds Algorithm (Positive Case)", LAPACK Working Note.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, iinfo
           real(sp) :: eps, scale, safmin, sigmn, sigmx
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
              call stdlib_I64_xerbla( 'SLASQ1', -info )
              return
           else if( n==0_ilp64 ) then
              return
           else if( n==1_ilp64 ) then
              d( 1_ilp64 ) = abs( d( 1_ilp64 ) )
              return
           else if( n==2_ilp64 ) then
              call stdlib_I64_slas2( d( 1_ilp64 ), e( 1_ilp64 ), d( 2_ilp64 ), sigmn, sigmx )
              d( 1_ilp64 ) = sigmx
              d( 2_ilp64 ) = sigmn
              return
           end if
           ! estimate the largest singular value.
           sigmx = zero
           do i = 1, n - 1
              d( i ) = abs( d( i ) )
              sigmx = max( sigmx, abs( e( i ) ) )
           end do
           d( n ) = abs( d( n ) )
           ! early return if sigmx is zero (matrix is already diagonal).
           if( sigmx==zero ) then
              call stdlib_I64_slasrt( 'D', n, d, iinfo )
              return
           end if
           do i = 1, n
              sigmx = max( sigmx, d( i ) )
           end do
           ! copy d and e into work (in the z format) and scale (squaring the
           ! input data makes scaling by a power of the radix pointless).
           eps = stdlib_I64_slamch( 'PRECISION' )
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           scale = sqrt( eps / safmin )
           call stdlib_I64_scopy( n, d, 1_ilp64, work( 1_ilp64 ), 2_ilp64 )
           call stdlib_I64_scopy( n-1, e, 1_ilp64, work( 2_ilp64 ), 2_ilp64 )
           call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, sigmx, scale, 2_ilp64*n-1, 1_ilp64, work, 2_ilp64*n-1,iinfo )
           ! compute the q's and e's.
           do i = 1, 2*n - 1
              work( i ) = work( i )**2_ilp64
           end do
           work( 2_ilp64*n ) = zero
           call stdlib_I64_slasq2( n, work, info )
           if( info==0_ilp64 ) then
              do i = 1, n
                 d( i ) = sqrt( work( i ) )
              end do
              call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, d, n, iinfo )
           else if( info==2_ilp64 ) then
           ! maximum number of iterations exceeded.  move data from work
           ! into d and e so the calling subroutine can try to finish
              do i = 1, n
                 d( i ) = sqrt( work( 2_ilp64*i-1 ) )
                 e( i ) = sqrt( work( 2_ilp64*i ) )
              end do
              call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, d, n, iinfo )
              call stdlib_I64_slascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, e, n, iinfo )
           end if
           return
     end subroutine stdlib_I64_slasq1

     pure module subroutine stdlib_I64_dlasq1( n, d, e, work, info )
     !! DLASQ1 computes the singular values of a real N-by-N bidiagonal
     !! matrix with diagonal D and off-diagonal E. The singular values
     !! are computed to high relative accuracy, in the absence of
     !! denormalization, underflow and overflow. The algorithm was first
     !! presented in
     !! "Accurate singular values and differential qd algorithms" by K. V.
     !! Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
     !! 1994,
     !! and the present implementation is described in "An implementation of
     !! the dqds Algorithm (Positive Case)", LAPACK Working Note.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, iinfo
           real(dp) :: eps, scale, safmin, sigmn, sigmx
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
              call stdlib_I64_xerbla( 'DLASQ1', -info )
              return
           else if( n==0_ilp64 ) then
              return
           else if( n==1_ilp64 ) then
              d( 1_ilp64 ) = abs( d( 1_ilp64 ) )
              return
           else if( n==2_ilp64 ) then
              call stdlib_I64_dlas2( d( 1_ilp64 ), e( 1_ilp64 ), d( 2_ilp64 ), sigmn, sigmx )
              d( 1_ilp64 ) = sigmx
              d( 2_ilp64 ) = sigmn
              return
           end if
           ! estimate the largest singular value.
           sigmx = zero
           do i = 1, n - 1
              d( i ) = abs( d( i ) )
              sigmx = max( sigmx, abs( e( i ) ) )
           end do
           d( n ) = abs( d( n ) )
           ! early return if sigmx is zero (matrix is already diagonal).
           if( sigmx==zero ) then
              call stdlib_I64_dlasrt( 'D', n, d, iinfo )
              return
           end if
           do i = 1, n
              sigmx = max( sigmx, d( i ) )
           end do
           ! copy d and e into work (in the z format) and scale (squaring the
           ! input data makes scaling by a power of the radix pointless).
           eps = stdlib_I64_dlamch( 'PRECISION' )
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           scale = sqrt( eps / safmin )
           call stdlib_I64_dcopy( n, d, 1_ilp64, work( 1_ilp64 ), 2_ilp64 )
           call stdlib_I64_dcopy( n-1, e, 1_ilp64, work( 2_ilp64 ), 2_ilp64 )
           call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, sigmx, scale, 2_ilp64*n-1, 1_ilp64, work, 2_ilp64*n-1,iinfo )
           ! compute the q's and e's.
           do i = 1, 2*n - 1
              work( i ) = work( i )**2_ilp64
           end do
           work( 2_ilp64*n ) = zero
           call stdlib_I64_dlasq2( n, work, info )
           if( info==0_ilp64 ) then
              do i = 1, n
                 d( i ) = sqrt( work( i ) )
              end do
              call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, d, n, iinfo )
           else if( info==2_ilp64 ) then
           ! maximum number of iterations exceeded.  move data from work
           ! into d and e so the calling subroutine can try to finish
              do i = 1, n
                 d( i ) = sqrt( work( 2_ilp64*i-1 ) )
                 e( i ) = sqrt( work( 2_ilp64*i ) )
              end do
              call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, d, n, iinfo )
              call stdlib_I64_dlascl( 'G', 0_ilp64, 0_ilp64, scale, sigmx, n, 1_ilp64, e, n, iinfo )
           end if
           return
     end subroutine stdlib_I64_dlasq1




     pure module subroutine stdlib_I64_slasq2( n, z, info )
     !! SLASQ2 computes all the eigenvalues of the symmetric positive
     !! definite tridiagonal matrix associated with the qd array Z to high
     !! relative accuracy are computed to high relative accuracy, in the
     !! absence of denormalization, underflow and overflow.
     !! To see the relation of Z to the tridiagonal matrix, let L be a
     !! unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
     !! let U be an upper bidiagonal matrix with 1's above and diagonal
     !! Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
     !! symmetric tridiagonal to which it is similar.
     !! Note : SLASQ2 defines a logical variable, IEEE, which is true
     !! on machines which follow ieee-754 floating-point standard in their
     !! handling of infinities and NaNs, and false otherwise. This variable
     !! is passed to SLASQ3.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cbias = 1.50_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           logical(lk) :: ieee
           integer(ilp64) :: i0, i4, iinfo, ipn4, iter, iwhila, iwhilb, k, kmin, n0, nbig, ndiv, &
                     nfail, pp, splt, ttype, i1, n1
           real(sp) :: d, dee, deemin, desig, dmin, dmin1, dmin2, dn, dn1, dn2, e, emax, emin, &
           eps, g, oldemn, qmax, qmin, s, safmin, sigma, t, tau, temp, tol, tol2, trace, zmax, &
                     tempe, tempq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           ! (in case stdlib_I64_slasq2 is not called by stdlib_I64_slasq1)
           info = 0_ilp64
           eps = stdlib_I64_slamch( 'PRECISION' )
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           tol = eps*hundrd
           tol2 = tol**2_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
              call stdlib_I64_xerbla( 'SLASQ2', 1_ilp64 )
              return
           else if( n==0_ilp64 ) then
              return
           else if( n==1_ilp64 ) then
              ! 1-by-1 case.
              if( z( 1_ilp64 )<zero ) then
                 info = -201_ilp64
                 call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
              end if
              return
           else if( n==2_ilp64 ) then
              ! 2-by-2 case.
              if( z( 1_ilp64 )<zero ) then
                 info = -201_ilp64
                 call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
                 return
              else if( z( 2_ilp64 )<zero ) then
                 info = -202_ilp64
                 call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
                 return
              else if( z( 3_ilp64 )<zero ) then
                info = -203_ilp64
                call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
                return
              else if( z( 3_ilp64 )>z( 1_ilp64 ) ) then
                 d = z( 3_ilp64 )
                 z( 3_ilp64 ) = z( 1_ilp64 )
                 z( 1_ilp64 ) = d
              end if
              z( 5_ilp64 ) = z( 1_ilp64 ) + z( 2_ilp64 ) + z( 3_ilp64 )
              if( z( 2_ilp64 )>z( 3_ilp64 )*tol2 ) then
                 t = half*( ( z( 1_ilp64 )-z( 3_ilp64 ) )+z( 2_ilp64 ) )
                 s = z( 3_ilp64 )*( z( 2_ilp64 ) / t )
                 if( s<=t ) then
                    s = z( 3_ilp64 )*( z( 2_ilp64 ) / ( t*( one+sqrt( one+s / t ) ) ) )
                 else
                    s = z( 3_ilp64 )*( z( 2_ilp64 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
                 end if
                 t = z( 1_ilp64 ) + ( s+z( 2_ilp64 ) )
                 z( 3_ilp64 ) = z( 3_ilp64 )*( z( 1_ilp64 ) / t )
                 z( 1_ilp64 ) = t
              end if
              z( 2_ilp64 ) = z( 3_ilp64 )
              z( 6_ilp64 ) = z( 2_ilp64 ) + z( 1_ilp64 )
              return
           end if
           ! check for negative data and compute sums of q's and e's.
           z( 2_ilp64*n ) = zero
           emin = z( 2_ilp64 )
           qmax = zero
           zmax = zero
           d = zero
           e = zero
           do k = 1, 2*( n-1 ), 2
              if( z( k )<zero ) then
                 info = -( 200_ilp64+k )
                 call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
                 return
              else if( z( k+1 )<zero ) then
                 info = -( 200_ilp64+k+1 )
                 call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
                 return
              end if
              d = d + z( k )
              e = e + z( k+1 )
              qmax = max( qmax, z( k ) )
              emin = min( emin, z( k+1 ) )
              zmax = max( qmax, zmax, z( k+1 ) )
           end do
           if( z( 2_ilp64*n-1 )<zero ) then
              info = -( 200_ilp64+2*n-1 )
              call stdlib_I64_xerbla( 'SLASQ2', 2_ilp64 )
              return
           end if
           d = d + z( 2_ilp64*n-1 )
           qmax = max( qmax, z( 2_ilp64*n-1 ) )
           zmax = max( qmax, zmax )
           ! check for diagonality.
           if( e==zero ) then
              do k = 2, n
                 z( k ) = z( 2_ilp64*k-1 )
              end do
              call stdlib_I64_slasrt( 'D', n, z, iinfo )
              z( 2_ilp64*n-1 ) = d
              return
           end if
           trace = d + e
           ! check for zero data.
           if( trace==zero ) then
              z( 2_ilp64*n-1 ) = zero
              return
           end if
           ! check whether the machine is ieee conformable.
           ! ieee = ( stdlib_I64_ilaenv( 10, 'slasq2', 'n', 1, 2, 3, 4 )==1 )
           ! [11/15/2008] the case ieee=.true. has a problem in single precision with
           ! some the test matrices of type 16. the double precision code is fine.
           ieee = .false.
           ! rearrange data for locality: z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
           do k = 2*n, 2, -2
              z( 2_ilp64*k ) = zero
              z( 2_ilp64*k-1 ) = z( k )
              z( 2_ilp64*k-2 ) = zero
              z( 2_ilp64*k-3 ) = z( k-1 )
           end do
           i0 = 1_ilp64
           n0 = n
           ! reverse the qd-array, if warranted.
           if( cbias*z( 4_ilp64*i0-3 )<z( 4_ilp64*n0-3 ) ) then
              ipn4 = 4_ilp64*( i0+n0 )
              do i4 = 4*i0, 2*( i0+n0-1 ), 4
                 temp = z( i4-3 )
                 z( i4-3 ) = z( ipn4-i4-3 )
                 z( ipn4-i4-3 ) = temp
                 temp = z( i4-1 )
                 z( i4-1 ) = z( ipn4-i4-5 )
                 z( ipn4-i4-5 ) = temp
              end do
           end if
           ! initial split checking via dqd and li's test.
           pp = 0_ilp64
           loop_80: do k = 1, 2
              d = z( 4_ilp64*n0+pp-3 )
              do i4 = 4*( n0-1 ) + pp, 4*i0 + pp, -4
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    d = z( i4-3 )
                 else
                    d = z( i4-3 )*( d / ( d+z( i4-1 ) ) )
                 end if
              end do
              ! dqd maps z to zz plus li's test.
              emin = z( 4_ilp64*i0+pp+1 )
              d = z( 4_ilp64*i0+pp-3 )
              do i4 = 4*i0 + pp, 4*( n0-1 ) + pp, 4
                 z( i4-2*pp-2 ) = d + z( i4-1 )
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    z( i4-2*pp-2 ) = d
                    z( i4-2*pp ) = zero
                    d = z( i4+1 )
                 else if( safmin*z( i4+1 )<z( i4-2*pp-2 ) .and.safmin*z( i4-2*pp-2 )<z( i4+1 ) ) &
                           then
                    temp = z( i4+1 ) / z( i4-2*pp-2 )
                    z( i4-2*pp ) = z( i4-1 )*temp
                    d = d*temp
                 else
                    z( i4-2*pp ) = z( i4+1 )*( z( i4-1 ) / z( i4-2*pp-2 ) )
                    d = z( i4+1 )*( d / z( i4-2*pp-2 ) )
                 end if
                 emin = min( emin, z( i4-2*pp ) )
              end do
              z( 4_ilp64*n0-pp-2 ) = d
              ! now find qmax.
              qmax = z( 4_ilp64*i0-pp-2 )
              do i4 = 4*i0 - pp + 2, 4*n0 - pp - 2, 4
                 qmax = max( qmax, z( i4 ) )
              end do
              ! prepare for the next iteration on k.
              pp = 1_ilp64 - pp
           end do loop_80
           ! initialise variables to pass to stdlib_I64_slasq3.
           ttype = 0_ilp64
           dmin1 = zero
           dmin2 = zero
           dn    = zero
           dn1   = zero
           dn2   = zero
           g     = zero
           tau   = zero
           iter = 2_ilp64
           nfail = 0_ilp64
           ndiv = 2_ilp64*( n0-i0 )
           loop_160: do iwhila = 1, n + 1
              if( n0<1 )go to 170
              ! while array unfinished do
              ! e(n0) holds the value of sigma when submatrix in i0:n0
              ! splits from the rest of the array, but is negated.
              desig = zero
              if( n0==n ) then
                 sigma = zero
              else
                 sigma = -z( 4_ilp64*n0-1 )
              end if
              if( sigma<zero ) then
                 info = 1_ilp64
                 return
              end if
              ! find last unreduced submatrix's top index i0, find qmax and
              ! emin. find gershgorin-type bound if q's much greater than e's.
              emax = zero
              if( n0>i0 ) then
                 emin = abs( z( 4_ilp64*n0-5 ) )
              else
                 emin = zero
              end if
              qmin = z( 4_ilp64*n0-3 )
              qmax = qmin
              do i4 = 4*n0, 8, -4
                 if( z( i4-5 )<=zero )go to 100
                 if( qmin>=four*emax ) then
                    qmin = min( qmin, z( i4-3 ) )
                    emax = max( emax, z( i4-5 ) )
                 end if
                 qmax = max( qmax, z( i4-7 )+z( i4-5 ) )
                 emin = min( emin, z( i4-5 ) )
              end do
              i4 = 4_ilp64
              100 continue
              i0 = i4 / 4_ilp64
              pp = 0_ilp64
              if( n0-i0>1_ilp64 ) then
                 dee = z( 4_ilp64*i0-3 )
                 deemin = dee
                 kmin = i0
                 do i4 = 4*i0+1, 4*n0-3, 4
                    dee = z( i4 )*( dee /( dee+z( i4-2 ) ) )
                    if( dee<=deemin ) then
                       deemin = dee
                       kmin = ( i4+3 )/4_ilp64
                    end if
                 end do
                 if( (kmin-i0)*2_ilp64<n0-kmin .and.deemin<=half*z(4_ilp64*n0-3) ) then
                    ipn4 = 4_ilp64*( i0+n0 )
                    pp = 2_ilp64
                    do i4 = 4*i0, 2*( i0+n0-1 ), 4
                       temp = z( i4-3 )
                       z( i4-3 ) = z( ipn4-i4-3 )
                       z( ipn4-i4-3 ) = temp
                       temp = z( i4-2 )
                       z( i4-2 ) = z( ipn4-i4-2 )
                       z( ipn4-i4-2 ) = temp
                       temp = z( i4-1 )
                       z( i4-1 ) = z( ipn4-i4-5 )
                       z( ipn4-i4-5 ) = temp
                       temp = z( i4 )
                       z( i4 ) = z( ipn4-i4-4 )
                       z( ipn4-i4-4 ) = temp
                    end do
                 end if
              end if
              ! put -(initial shift) into dmin.
              dmin = -max( zero, qmin-two*sqrt( qmin )*sqrt( emax ) )
              ! now i0:n0 is unreduced.
              ! pp = 0 for ping, pp = 1 for pong.
              ! pp = 2 indicates that flipping was applied to the z array and
                     ! and that the tests for deflation upon entry in stdlib_I64_slasq3
                     ! should not be performed.
              nbig = 100_ilp64*( n0-i0+1 )
              loop_140: do iwhilb = 1, nbig
                 if( i0>n0 )go to 150
                 ! while submatrix unfinished take a good dqds step.
                 call stdlib_I64_slasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
                           ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
                 pp = 1_ilp64 - pp
                 ! when emin is very small check for splits.
                 if( pp==0_ilp64 .and. n0-i0>=3_ilp64 ) then
                    if( z( 4_ilp64*n0 )<=tol2*qmax .or.z( 4_ilp64*n0-1 )<=tol2*sigma ) then
                       splt = i0 - 1_ilp64
                       qmax = z( 4_ilp64*i0-3 )
                       emin = z( 4_ilp64*i0-1 )
                       oldemn = z( 4_ilp64*i0 )
                       do i4 = 4*i0, 4*( n0-3 ), 4
                          if( z( i4 )<=tol2*z( i4-3 ) .or.z( i4-1 )<=tol2*sigma ) then
                             z( i4-1 ) = -sigma
                             splt = i4 / 4_ilp64
                             qmax = zero
                             emin = z( i4+3 )
                             oldemn = z( i4+4 )
                          else
                             qmax = max( qmax, z( i4+1 ) )
                             emin = min( emin, z( i4-1 ) )
                             oldemn = min( oldemn, z( i4 ) )
                          end if
                       end do
                       z( 4_ilp64*n0-1 ) = emin
                       z( 4_ilp64*n0 ) = oldemn
                       i0 = splt + 1_ilp64
                    end if
                 end if
              end do loop_140
              info = 2_ilp64
              ! maximum number of iterations exceeded, restore the shift
              ! sigma and place the new d's and e's in a qd array.
              ! this might need to be done for several blocks
              i1 = i0
              n1 = n0
              145 continue
              tempq = z( 4_ilp64*i0-3 )
              z( 4_ilp64*i0-3 ) = z( 4_ilp64*i0-3 ) + sigma
              do k = i0+1, n0
                 tempe = z( 4_ilp64*k-5 )
                 z( 4_ilp64*k-5 ) = z( 4_ilp64*k-5 ) * (tempq / z( 4_ilp64*k-7 ))
                 tempq = z( 4_ilp64*k-3 )
                 z( 4_ilp64*k-3 ) = z( 4_ilp64*k-3 ) + sigma + tempe - z( 4_ilp64*k-5 )
              end do
              ! prepare to do this on the previous block if there is one
              if( i1>1_ilp64 ) then
                 n1 = i1-1
                 do while( ( i1>=2 ) .and. ( z(4*i1-5)>=zero ) )
                    i1 = i1 - 1_ilp64
                 end do
                 if( i1>=1_ilp64 ) then
                    sigma = -z(4_ilp64*n1-1)
                    go to 145
                 end if
              end if
              do k = 1, n
                 z( 2_ilp64*k-1 ) = z( 4_ilp64*k-3 )
              ! only the block 1..n0 is unfinished.  the rest of the e's
              ! must be essentially zero, although sometimes other data
              ! has been stored in them.
                 if( k<n0 ) then
                    z( 2_ilp64*k ) = z( 4_ilp64*k-1 )
                 else
                    z( 2_ilp64*k ) = 0_ilp64
                 end if
              end do
              return
              ! end iwhilb
              150 continue
           end do loop_160
           info = 3_ilp64
           return
           ! end iwhila
           170 continue
           ! move q's to the front.
           do k = 2, n
              z( k ) = z( 4_ilp64*k-3 )
           end do
           ! sort and compute sum of eigenvalues.
           call stdlib_I64_slasrt( 'D', n, z, iinfo )
           e = zero
           do k = n, 1, -1
              e = e + z( k )
           end do
           ! store trace, sum(eigenvalues) and information on performance.
           z( 2_ilp64*n+1 ) = trace
           z( 2_ilp64*n+2 ) = e
           z( 2_ilp64*n+3 ) = real( iter,KIND=sp)
           z( 2_ilp64*n+4 ) = real( ndiv,KIND=sp) / real( n**2_ilp64,KIND=sp)
           z( 2_ilp64*n+5 ) = hundrd*nfail / real( iter,KIND=sp)
           return
     end subroutine stdlib_I64_slasq2

     pure module subroutine stdlib_I64_dlasq2( n, z, info )
     !! DLASQ2 computes all the eigenvalues of the symmetric positive
     !! definite tridiagonal matrix associated with the qd array Z to high
     !! relative accuracy are computed to high relative accuracy, in the
     !! absence of denormalization, underflow and overflow.
     !! To see the relation of Z to the tridiagonal matrix, let L be a
     !! unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
     !! let U be an upper bidiagonal matrix with 1's above and diagonal
     !! Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
     !! symmetric tridiagonal to which it is similar.
     !! Note : DLASQ2 defines a logical variable, IEEE, which is true
     !! on machines which follow ieee-754 floating-point standard in their
     !! handling of infinities and NaNs, and false otherwise. This variable
     !! is passed to DLASQ3.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cbias = 1.50_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           logical(lk) :: ieee
           integer(ilp64) :: i0, i1, i4, iinfo, ipn4, iter, iwhila, iwhilb, k, kmin, n0, n1, nbig, &
                     ndiv, nfail, pp, splt, ttype
           real(dp) :: d, dee, deemin, desig, dmin, dmin1, dmin2, dn, dn1, dn2, e, emax, emin, &
           eps, g, oldemn, qmax, qmin, s, safmin, sigma, t, tau, temp, tol, tol2, trace, zmax, &
                     tempe, tempq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           ! (in case stdlib_I64_dlasq2 is not called by stdlib_I64_dlasq1)
           info = 0_ilp64
           eps = stdlib_I64_dlamch( 'PRECISION' )
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           tol = eps*hundrd
           tol2 = tol**2_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
              call stdlib_I64_xerbla( 'DLASQ2', 1_ilp64 )
              return
           else if( n==0_ilp64 ) then
              return
           else if( n==1_ilp64 ) then
              ! 1-by-1 case.
              if( z( 1_ilp64 )<zero ) then
                 info = -201_ilp64
                 call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
              end if
              return
           else if( n==2_ilp64 ) then
              ! 2-by-2 case.
              if( z( 1_ilp64 )<zero ) then
                 info = -201_ilp64
                 call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
                 return
              else if( z( 2_ilp64 )<zero ) then
                 info = -202_ilp64
                 call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
                 return
              else if( z( 3_ilp64 )<zero ) then
                info = -203_ilp64
                call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
                return
              else if( z( 3_ilp64 )>z( 1_ilp64 ) ) then
                 d = z( 3_ilp64 )
                 z( 3_ilp64 ) = z( 1_ilp64 )
                 z( 1_ilp64 ) = d
              end if
              z( 5_ilp64 ) = z( 1_ilp64 ) + z( 2_ilp64 ) + z( 3_ilp64 )
              if( z( 2_ilp64 )>z( 3_ilp64 )*tol2 ) then
                 t = half*( ( z( 1_ilp64 )-z( 3_ilp64 ) )+z( 2_ilp64 ) )
                 s = z( 3_ilp64 )*( z( 2_ilp64 ) / t )
                 if( s<=t ) then
                    s = z( 3_ilp64 )*( z( 2_ilp64 ) / ( t*( one+sqrt( one+s / t ) ) ) )
                 else
                    s = z( 3_ilp64 )*( z( 2_ilp64 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
                 end if
                 t = z( 1_ilp64 ) + ( s+z( 2_ilp64 ) )
                 z( 3_ilp64 ) = z( 3_ilp64 )*( z( 1_ilp64 ) / t )
                 z( 1_ilp64 ) = t
              end if
              z( 2_ilp64 ) = z( 3_ilp64 )
              z( 6_ilp64 ) = z( 2_ilp64 ) + z( 1_ilp64 )
              return
           end if
           ! check for negative data and compute sums of q's and e's.
           z( 2_ilp64*n ) = zero
           emin = z( 2_ilp64 )
           qmax = zero
           zmax = zero
           d = zero
           e = zero
           do k = 1, 2*( n-1 ), 2
              if( z( k )<zero ) then
                 info = -( 200_ilp64+k )
                 call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
                 return
              else if( z( k+1 )<zero ) then
                 info = -( 200_ilp64+k+1 )
                 call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
                 return
              end if
              d = d + z( k )
              e = e + z( k+1 )
              qmax = max( qmax, z( k ) )
              emin = min( emin, z( k+1 ) )
              zmax = max( qmax, zmax, z( k+1 ) )
           end do
           if( z( 2_ilp64*n-1 )<zero ) then
              info = -( 200_ilp64+2*n-1 )
              call stdlib_I64_xerbla( 'DLASQ2', 2_ilp64 )
              return
           end if
           d = d + z( 2_ilp64*n-1 )
           qmax = max( qmax, z( 2_ilp64*n-1 ) )
           zmax = max( qmax, zmax )
           ! check for diagonality.
           if( e==zero ) then
              do k = 2, n
                 z( k ) = z( 2_ilp64*k-1 )
              end do
              call stdlib_I64_dlasrt( 'D', n, z, iinfo )
              z( 2_ilp64*n-1 ) = d
              return
           end if
           trace = d + e
           ! check for zero data.
           if( trace==zero ) then
              z( 2_ilp64*n-1 ) = zero
              return
           end if
           ! check whether the machine is ieee conformable.
           ieee = ( stdlib_I64_ilaenv( 10_ilp64, 'DLASQ2', 'N', 1_ilp64, 2_ilp64, 3_ilp64, 4_ilp64 )==1_ilp64 )
           ! rearrange data for locality: z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
           do k = 2*n, 2, -2
              z( 2_ilp64*k ) = zero
              z( 2_ilp64*k-1 ) = z( k )
              z( 2_ilp64*k-2 ) = zero
              z( 2_ilp64*k-3 ) = z( k-1 )
           end do
           i0 = 1_ilp64
           n0 = n
           ! reverse the qd-array, if warranted.
           if( cbias*z( 4_ilp64*i0-3 )<z( 4_ilp64*n0-3 ) ) then
              ipn4 = 4_ilp64*( i0+n0 )
              do i4 = 4*i0, 2*( i0+n0-1 ), 4
                 temp = z( i4-3 )
                 z( i4-3 ) = z( ipn4-i4-3 )
                 z( ipn4-i4-3 ) = temp
                 temp = z( i4-1 )
                 z( i4-1 ) = z( ipn4-i4-5 )
                 z( ipn4-i4-5 ) = temp
              end do
           end if
           ! initial split checking via dqd and li's test.
           pp = 0_ilp64
           loop_80: do k = 1, 2
              d = z( 4_ilp64*n0+pp-3 )
              do i4 = 4*( n0-1 ) + pp, 4*i0 + pp, -4
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    d = z( i4-3 )
                 else
                    d = z( i4-3 )*( d / ( d+z( i4-1 ) ) )
                 end if
              end do
              ! dqd maps z to zz plus li's test.
              emin = z( 4_ilp64*i0+pp+1 )
              d = z( 4_ilp64*i0+pp-3 )
              do i4 = 4*i0 + pp, 4*( n0-1 ) + pp, 4
                 z( i4-2*pp-2 ) = d + z( i4-1 )
                 if( z( i4-1 )<=tol2*d ) then
                    z( i4-1 ) = -zero
                    z( i4-2*pp-2 ) = d
                    z( i4-2*pp ) = zero
                    d = z( i4+1 )
                 else if( safmin*z( i4+1 )<z( i4-2*pp-2 ) .and.safmin*z( i4-2*pp-2 )<z( i4+1 ) ) &
                           then
                    temp = z( i4+1 ) / z( i4-2*pp-2 )
                    z( i4-2*pp ) = z( i4-1 )*temp
                    d = d*temp
                 else
                    z( i4-2*pp ) = z( i4+1 )*( z( i4-1 ) / z( i4-2*pp-2 ) )
                    d = z( i4+1 )*( d / z( i4-2*pp-2 ) )
                 end if
                 emin = min( emin, z( i4-2*pp ) )
              end do
              z( 4_ilp64*n0-pp-2 ) = d
              ! now find qmax.
              qmax = z( 4_ilp64*i0-pp-2 )
              do i4 = 4*i0 - pp + 2, 4*n0 - pp - 2, 4
                 qmax = max( qmax, z( i4 ) )
              end do
              ! prepare for the next iteration on k.
              pp = 1_ilp64 - pp
           end do loop_80
           ! initialise variables to pass to stdlib_I64_dlasq3.
           ttype = 0_ilp64
           dmin1 = zero
           dmin2 = zero
           dn    = zero
           dn1   = zero
           dn2   = zero
           g     = zero
           tau   = zero
           iter = 2_ilp64
           nfail = 0_ilp64
           ndiv = 2_ilp64*( n0-i0 )
           loop_160: do iwhila = 1, n + 1
              if( n0<1 )go to 170
              ! while array unfinished do
              ! e(n0) holds the value of sigma when submatrix in i0:n0
              ! splits from the rest of the array, but is negated.
              desig = zero
              if( n0==n ) then
                 sigma = zero
              else
                 sigma = -z( 4_ilp64*n0-1 )
              end if
              if( sigma<zero ) then
                 info = 1_ilp64
                 return
              end if
              ! find last unreduced submatrix's top index i0, find qmax and
              ! emin. find gershgorin-type bound if q's much greater than e's.
              emax = zero
              if( n0>i0 ) then
                 emin = abs( z( 4_ilp64*n0-5 ) )
              else
                 emin = zero
              end if
              qmin = z( 4_ilp64*n0-3 )
              qmax = qmin
              do i4 = 4*n0, 8, -4
                 if( z( i4-5 )<=zero )go to 100
                 if( qmin>=four*emax ) then
                    qmin = min( qmin, z( i4-3 ) )
                    emax = max( emax, z( i4-5 ) )
                 end if
                 qmax = max( qmax, z( i4-7 )+z( i4-5 ) )
                 emin = min( emin, z( i4-5 ) )
              end do
              i4 = 4_ilp64
              100 continue
              i0 = i4 / 4_ilp64
              pp = 0_ilp64
              if( n0-i0>1_ilp64 ) then
                 dee = z( 4_ilp64*i0-3 )
                 deemin = dee
                 kmin = i0
                 do i4 = 4*i0+1, 4*n0-3, 4
                    dee = z( i4 )*( dee /( dee+z( i4-2 ) ) )
                    if( dee<=deemin ) then
                       deemin = dee
                       kmin = ( i4+3 )/4_ilp64
                    end if
                 end do
                 if( (kmin-i0)*2_ilp64<n0-kmin .and.deemin<=half*z(4_ilp64*n0-3) ) then
                    ipn4 = 4_ilp64*( i0+n0 )
                    pp = 2_ilp64
                    do i4 = 4*i0, 2*( i0+n0-1 ), 4
                       temp = z( i4-3 )
                       z( i4-3 ) = z( ipn4-i4-3 )
                       z( ipn4-i4-3 ) = temp
                       temp = z( i4-2 )
                       z( i4-2 ) = z( ipn4-i4-2 )
                       z( ipn4-i4-2 ) = temp
                       temp = z( i4-1 )
                       z( i4-1 ) = z( ipn4-i4-5 )
                       z( ipn4-i4-5 ) = temp
                       temp = z( i4 )
                       z( i4 ) = z( ipn4-i4-4 )
                       z( ipn4-i4-4 ) = temp
                    end do
                 end if
              end if
              ! put -(initial shift) into dmin.
              dmin = -max( zero, qmin-two*sqrt( qmin )*sqrt( emax ) )
              ! now i0:n0 is unreduced.
              ! pp = 0 for ping, pp = 1 for pong.
              ! pp = 2 indicates that flipping was applied to the z array and
                     ! and that the tests for deflation upon entry in stdlib_I64_dlasq3
                     ! should not be performed.
              nbig = 100_ilp64*( n0-i0+1 )
              loop_140: do iwhilb = 1, nbig
                 if( i0>n0 )go to 150
                 ! while submatrix unfinished take a good dqds step.
                 call stdlib_I64_dlasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
                           ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
                 pp = 1_ilp64 - pp
                 ! when emin is very small check for splits.
                 if( pp==0_ilp64 .and. n0-i0>=3_ilp64 ) then
                    if( z( 4_ilp64*n0 )<=tol2*qmax .or.z( 4_ilp64*n0-1 )<=tol2*sigma ) then
                       splt = i0 - 1_ilp64
                       qmax = z( 4_ilp64*i0-3 )
                       emin = z( 4_ilp64*i0-1 )
                       oldemn = z( 4_ilp64*i0 )
                       do i4 = 4*i0, 4*( n0-3 ), 4
                          if( z( i4 )<=tol2*z( i4-3 ) .or.z( i4-1 )<=tol2*sigma ) then
                             z( i4-1 ) = -sigma
                             splt = i4 / 4_ilp64
                             qmax = zero
                             emin = z( i4+3 )
                             oldemn = z( i4+4 )
                          else
                             qmax = max( qmax, z( i4+1 ) )
                             emin = min( emin, z( i4-1 ) )
                             oldemn = min( oldemn, z( i4 ) )
                          end if
                       end do
                       z( 4_ilp64*n0-1 ) = emin
                       z( 4_ilp64*n0 ) = oldemn
                       i0 = splt + 1_ilp64
                    end if
                 end if
              end do loop_140
              info = 2_ilp64
              ! maximum number of iterations exceeded, restore the shift
              ! sigma and place the new d's and e's in a qd array.
              ! this might need to be done for several blocks
              i1 = i0
              n1 = n0
              145 continue
              tempq = z( 4_ilp64*i0-3 )
              z( 4_ilp64*i0-3 ) = z( 4_ilp64*i0-3 ) + sigma
              do k = i0+1, n0
                 tempe = z( 4_ilp64*k-5 )
                 z( 4_ilp64*k-5 ) = z( 4_ilp64*k-5 ) * (tempq / z( 4_ilp64*k-7 ))
                 tempq = z( 4_ilp64*k-3 )
                 z( 4_ilp64*k-3 ) = z( 4_ilp64*k-3 ) + sigma + tempe - z( 4_ilp64*k-5 )
              end do
              ! prepare to do this on the previous block if there is one
              if( i1>1_ilp64 ) then
                 n1 = i1-1
                 do while( ( i1>=2 ) .and. ( z(4*i1-5)>=zero ) )
                    i1 = i1 - 1_ilp64
                 end do
                 sigma = -z(4_ilp64*n1-1)
                 go to 145
              end if
              do k = 1, n
                 z( 2_ilp64*k-1 ) = z( 4_ilp64*k-3 )
              ! only the block 1..n0 is unfinished.  the rest of the e's
              ! must be essentially zero, although sometimes other data
              ! has been stored in them.
                 if( k<n0 ) then
                    z( 2_ilp64*k ) = z( 4_ilp64*k-1 )
                 else
                    z( 2_ilp64*k ) = 0_ilp64
                 end if
              end do
              return
              ! end iwhilb
              150 continue
           end do loop_160
           info = 3_ilp64
           return
           ! end iwhila
           170 continue
           ! move q's to the front.
           do k = 2, n
              z( k ) = z( 4_ilp64*k-3 )
           end do
           ! sort and compute sum of eigenvalues.
           call stdlib_I64_dlasrt( 'D', n, z, iinfo )
           e = zero
           do k = n, 1, -1
              e = e + z( k )
           end do
           ! store trace, sum(eigenvalues) and information on performance.
           z( 2_ilp64*n+1 ) = trace
           z( 2_ilp64*n+2 ) = e
           z( 2_ilp64*n+3 ) = real( iter,KIND=dp)
           z( 2_ilp64*n+4 ) = real( ndiv,KIND=dp) / real( n**2_ilp64,KIND=dp)
           z( 2_ilp64*n+5 ) = hundrd*nfail / real( iter,KIND=dp)
           return
     end subroutine stdlib_I64_dlasq2




     pure module subroutine stdlib_I64_slasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
     !! SLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
     !! In case of failure it changes shifts, and tries again until output
     !! is positive.
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp64), intent(in) :: i0
           integer(ilp64), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(sp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(sp), intent(out) :: dmin, sigma
           integer(ilp64), intent(inout) :: ttype
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cbias = 1.50_sp
           real(sp), parameter :: qurtr = 0.250_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           integer(ilp64) :: ipn4, j4, n0in, nn
           real(sp) :: eps, s, t, temp, tol, tol2
           ! Intrinsic Functions 
           ! Executable Statements 
           n0in = n0
           eps = stdlib_I64_slamch( 'PRECISION' )
           tol = eps*hundrd
           tol2 = tol**2_ilp64
           ! check for deflation.
           10 continue
           if( n0<i0 )return
           if( n0==i0 )go to 20
           nn = 4_ilp64*n0 + pp
           if( n0==( i0+1 ) )go to 40
           ! check whether e(n0-1) is negligible, 1 eigenvalue.
           if( z( nn-5 )>tol2*( sigma+z( nn-3 ) ) .and.z( nn-2*pp-4 )>tol2*z( nn-7 ) )go to 30

           20 continue
           z( 4_ilp64*n0-3 ) = z( 4_ilp64*n0+pp-3 ) + sigma
           n0 = n0 - 1_ilp64
           go to 10
           ! check  whether e(n0-2) is negligible, 2 eigenvalues.
           30 continue
           if( z( nn-9 )>tol2*sigma .and.z( nn-2*pp-8 )>tol2*z( nn-11 ) )go to 50
           40 continue
           if( z( nn-3 )>z( nn-7 ) ) then
              s = z( nn-3 )
              z( nn-3 ) = z( nn-7 )
              z( nn-7 ) = s
           end if
           t = half*( ( z( nn-7 )-z( nn-3 ) )+z( nn-5 ) )
           if( z( nn-5 )>z( nn-3 )*tol2.and.t/=zero ) then
              s = z( nn-3 )*( z( nn-5 ) / t )
              if( s<=t ) then
                 s = z( nn-3 )*( z( nn-5 ) /( t*( one+sqrt( one+s / t ) ) ) )
              else
                 s = z( nn-3 )*( z( nn-5 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
              end if
              t = z( nn-7 ) + ( s+z( nn-5 ) )
              z( nn-3 ) = z( nn-3 )*( z( nn-7 ) / t )
              z( nn-7 ) = t
           end if
           z( 4_ilp64*n0-7 ) = z( nn-7 ) + sigma
           z( 4_ilp64*n0-3 ) = z( nn-3 ) + sigma
           n0 = n0 - 2_ilp64
           go to 10
           50 continue
           if( pp==2_ilp64 )pp = 0_ilp64
           ! reverse the qd-array, if warranted.
           if( dmin<=zero .or. n0<n0in ) then
              if( cbias*z( 4_ilp64*i0+pp-3 )<z( 4_ilp64*n0+pp-3 ) ) then
                 ipn4 = 4_ilp64*( i0+n0 )
                 do j4 = 4*i0, 2*( i0+n0-1 ), 4
                    temp = z( j4-3 )
                    z( j4-3 ) = z( ipn4-j4-3 )
                    z( ipn4-j4-3 ) = temp
                    temp = z( j4-2 )
                    z( j4-2 ) = z( ipn4-j4-2 )
                    z( ipn4-j4-2 ) = temp
                    temp = z( j4-1 )
                    z( j4-1 ) = z( ipn4-j4-5 )
                    z( ipn4-j4-5 ) = temp
                    temp = z( j4 )
                    z( j4 ) = z( ipn4-j4-4 )
                    z( ipn4-j4-4 ) = temp
                 end do
                 if( n0-i0<=4_ilp64 ) then
                    z( 4_ilp64*n0+pp-1 ) = z( 4_ilp64*i0+pp-1 )
                    z( 4_ilp64*n0-pp ) = z( 4_ilp64*i0-pp )
                 end if
                 dmin2 = min( dmin2, z( 4_ilp64*n0+pp-1 ) )
                 z( 4_ilp64*n0+pp-1 ) = min( z( 4_ilp64*n0+pp-1 ), z( 4_ilp64*i0+pp-1 ),z( 4_ilp64*i0+pp+3 ) )
                 z( 4_ilp64*n0-pp ) = min( z( 4_ilp64*n0-pp ), z( 4_ilp64*i0-pp ),z( 4_ilp64*i0-pp+4 ) )
                 qmax = max( qmax, z( 4_ilp64*i0+pp-3 ), z( 4_ilp64*i0+pp+1 ) )
                 dmin = -zero
              end if
           end if
           ! choose a shift.
           call stdlib_I64_slasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1,dn2, tau, ttype, &
                     g )
           ! call dqds until dmin > 0.
           70 continue
           call stdlib_I64_slasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2, dn,dn1, dn2, ieee, &
                     eps )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp64
           ! check status.
           if( dmin>=zero .and. dmin1>=zero ) then
              ! success.
              go to 90
           else if( dmin<zero .and. dmin1>zero .and.z( 4_ilp64*( n0-1 )-pp )<tol*( sigma+dn1 ) .and.abs(&
                      dn )<tol*sigma ) then
              ! convergence hidden by negative dn.
              z( 4_ilp64*( n0-1 )-pp+2 ) = zero
              dmin = zero
              go to 90
           else if( dmin<zero ) then
              ! tau too big. select new tau and try again.
              nfail = nfail + 1_ilp64
              if( ttype<-22_ilp64 ) then
                 ! failed twice. play it safe.
                 tau = zero
              else if( dmin1>zero ) then
                 ! late failure. gives excellent shift.
                 tau = ( tau+dmin )*( one-two*eps )
                 ttype = ttype - 11_ilp64
              else
                 ! early failure. divide by 4.
                 tau = qurtr*tau
                 ttype = ttype - 12_ilp64
              end if
              go to 70
           else if( stdlib_I64_sisnan( dmin ) ) then
              ! nan.
              if( tau==zero ) then
                 go to 80
              else
                 tau = zero
                 go to 70
              end if
           else
              ! possible underflow. play it safe.
              go to 80
           end if
           ! risk of underflow.
           80 continue
           call stdlib_I64_slasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn, dn1, dn2 )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp64
           tau = zero
           90 continue
           if( tau<sigma ) then
              desig = desig + tau
              t = sigma + desig
              desig = desig - ( t-sigma )
           else
              t = sigma + tau
              desig = sigma - ( t-tau ) + desig
           end if
           sigma = t
           return
     end subroutine stdlib_I64_slasq3

     pure module subroutine stdlib_I64_dlasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
     !! DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
     !! In case of failure it changes shifts, and tries again until output
     !! is positive.
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp64), intent(in) :: i0
           integer(ilp64), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(dp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(dp), intent(out) :: dmin, sigma
           integer(ilp64), intent(inout) :: ttype
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cbias = 1.50_dp
           real(dp), parameter :: qurtr = 0.250_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           integer(ilp64) :: ipn4, j4, n0in, nn
           real(dp) :: eps, s, t, temp, tol, tol2
           ! Intrinsic Functions 
           ! Executable Statements 
           n0in = n0
           eps = stdlib_I64_dlamch( 'PRECISION' )
           tol = eps*hundrd
           tol2 = tol**2_ilp64
           ! check for deflation.
           10 continue
           if( n0<i0 )return
           if( n0==i0 )go to 20
           nn = 4_ilp64*n0 + pp
           if( n0==( i0+1 ) )go to 40
           ! check whether e(n0-1) is negligible, 1 eigenvalue.
           if( z( nn-5 )>tol2*( sigma+z( nn-3 ) ) .and.z( nn-2*pp-4 )>tol2*z( nn-7 ) ) go to 30
           20 continue
           z( 4_ilp64*n0-3 ) = z( 4_ilp64*n0+pp-3 ) + sigma
           n0 = n0 - 1_ilp64
           go to 10
           ! check  whether e(n0-2) is negligible, 2 eigenvalues.
           30 continue
           if( z( nn-9 )>tol2*sigma .and.z( nn-2*pp-8 )>tol2*z( nn-11 ) )go to 50
           40 continue
           if( z( nn-3 )>z( nn-7 ) ) then
              s = z( nn-3 )
              z( nn-3 ) = z( nn-7 )
              z( nn-7 ) = s
           end if
           t = half*( ( z( nn-7 )-z( nn-3 ) )+z( nn-5 ) )
           if( z( nn-5 )>z( nn-3 )*tol2.and.t/=zero ) then
              s = z( nn-3 )*( z( nn-5 ) / t )
              if( s<=t ) then
                 s = z( nn-3 )*( z( nn-5 ) /( t*( one+sqrt( one+s / t ) ) ) )
              else
                 s = z( nn-3 )*( z( nn-5 ) / ( t+sqrt( t )*sqrt( t+s ) ) )
              end if
              t = z( nn-7 ) + ( s+z( nn-5 ) )
              z( nn-3 ) = z( nn-3 )*( z( nn-7 ) / t )
              z( nn-7 ) = t
           end if
           z( 4_ilp64*n0-7 ) = z( nn-7 ) + sigma
           z( 4_ilp64*n0-3 ) = z( nn-3 ) + sigma
           n0 = n0 - 2_ilp64
           go to 10
           50 continue
           if( pp==2_ilp64 )pp = 0_ilp64
           ! reverse the qd-array, if warranted.
           if( dmin<=zero .or. n0<n0in ) then
              if( cbias*z( 4_ilp64*i0+pp-3 )<z( 4_ilp64*n0+pp-3 ) ) then
                 ipn4 = 4_ilp64*( i0+n0 )
                 do j4 = 4*i0, 2*( i0+n0-1 ), 4
                    temp = z( j4-3 )
                    z( j4-3 ) = z( ipn4-j4-3 )
                    z( ipn4-j4-3 ) = temp
                    temp = z( j4-2 )
                    z( j4-2 ) = z( ipn4-j4-2 )
                    z( ipn4-j4-2 ) = temp
                    temp = z( j4-1 )
                    z( j4-1 ) = z( ipn4-j4-5 )
                    z( ipn4-j4-5 ) = temp
                    temp = z( j4 )
                    z( j4 ) = z( ipn4-j4-4 )
                    z( ipn4-j4-4 ) = temp
                 end do
                 if( n0-i0<=4_ilp64 ) then
                    z( 4_ilp64*n0+pp-1 ) = z( 4_ilp64*i0+pp-1 )
                    z( 4_ilp64*n0-pp ) = z( 4_ilp64*i0-pp )
                 end if
                 dmin2 = min( dmin2, z( 4_ilp64*n0+pp-1 ) )
                 z( 4_ilp64*n0+pp-1 ) = min( z( 4_ilp64*n0+pp-1 ), z( 4_ilp64*i0+pp-1 ),z( 4_ilp64*i0+pp+3 ) )
                 z( 4_ilp64*n0-pp ) = min( z( 4_ilp64*n0-pp ), z( 4_ilp64*i0-pp ),z( 4_ilp64*i0-pp+4 ) )
                 qmax = max( qmax, z( 4_ilp64*i0+pp-3 ), z( 4_ilp64*i0+pp+1 ) )
                 dmin = -zero
              end if
           end if
           ! choose a shift.
           call stdlib_I64_dlasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1,dn2, tau, ttype, &
                     g )
           ! call dqds until dmin > 0.
           70 continue
           call stdlib_I64_dlasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2, dn,dn1, dn2, ieee, &
                     eps )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp64
           ! check status.
           if( dmin>=zero .and. dmin1>=zero ) then
              ! success.
              go to 90
           else if( dmin<zero .and. dmin1>zero .and.z( 4_ilp64*( n0-1 )-pp )<tol*( sigma+dn1 ) .and.abs(&
                      dn )<tol*sigma ) then
              ! convergence hidden by negative dn.
              z( 4_ilp64*( n0-1 )-pp+2 ) = zero
              dmin = zero
              go to 90
           else if( dmin<zero ) then
              ! tau too big. select new tau and try again.
              nfail = nfail + 1_ilp64
              if( ttype<-22_ilp64 ) then
                 ! failed twice. play it safe.
                 tau = zero
              else if( dmin1>zero ) then
                 ! late failure. gives excellent shift.
                 tau = ( tau+dmin )*( one-two*eps )
                 ttype = ttype - 11_ilp64
              else
                 ! early failure. divide by 4.
                 tau = qurtr*tau
                 ttype = ttype - 12_ilp64
              end if
              go to 70
           else if( stdlib_I64_disnan( dmin ) ) then
              ! nan.
              if( tau==zero ) then
                 go to 80
              else
                 tau = zero
                 go to 70
              end if
           else
              ! possible underflow. play it safe.
              go to 80
           end if
           ! risk of underflow.
           80 continue
           call stdlib_I64_dlasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn, dn1, dn2 )
           ndiv = ndiv + ( n0-i0+2 )
           iter = iter + 1_ilp64
           tau = zero
           90 continue
           if( tau<sigma ) then
              desig = desig + tau
              t = sigma + desig
              desig = desig - ( t-sigma )
           else
              t = sigma + tau
              desig = sigma - ( t-tau ) + desig
           end if
           sigma = t
           return
     end subroutine stdlib_I64_dlasq3




     pure module subroutine stdlib_I64_slasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
     !! SLASQ4 computes an approximation TAU to the smallest eigenvalue
     !! using values of d from the previous transform.
               ttype, g )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: i0, n0, n0in, pp
           integer(ilp64), intent(out) :: ttype
           real(sp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(sp), intent(inout) :: g
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(in) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: cnst1 = 0.5630_sp
           real(sp), parameter :: cnst2 = 1.010_sp
           real(sp), parameter :: cnst3 = 1.050_sp
           real(sp), parameter :: qurtr = 0.250_sp
           real(sp), parameter :: third = 0.3330_sp
           real(sp), parameter :: hundrd = 100.0_sp
           
           
           ! Local Scalars 
           integer(ilp64) :: i4, nn, np
           real(sp) :: a2, b1, b2, gam, gap1, gap2, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! a negative dmin forces the shift to take that absolute value
           ! ttype records the type of shift.
           if( dmin<=zero ) then
              tau = -dmin
              ttype = -1_ilp64
              return
           end if
           nn = 4_ilp64*n0 + pp
           if( n0in==n0 ) then
              ! no eigenvalues deflated.
              if( dmin==dn .or. dmin==dn1 ) then
                 b1 = sqrt( z( nn-3 ) )*sqrt( z( nn-5 ) )
                 b2 = sqrt( z( nn-7 ) )*sqrt( z( nn-9 ) )
                 a2 = z( nn-7 ) + z( nn-5 )
                 ! cases 2 and 3.
                 if( dmin==dn .and. dmin1==dn1 ) then
                    gap2 = dmin2 - a2 - dmin2*qurtr
                    if( gap2>zero .and. gap2>b2 ) then
                       gap1 = a2 - dn - ( b2 / gap2 )*b2
                    else
                       gap1 = a2 - dn - ( b1+b2 )
                    end if
                    if( gap1>zero .and. gap1>b1 ) then
                       s = max( dn-( b1 / gap1 )*b1, half*dmin )
                       ttype = -2_ilp64
                    else
                       s = zero
                       if( dn>b1 )s = dn - b1
                       if( a2>( b1+b2 ) )s = min( s, a2-( b1+b2 ) )
                       s = max( s, third*dmin )
                       ttype = -3_ilp64
                    end if
                 else
                    ! case 4.
                    ttype = -4_ilp64
                    s = qurtr*dmin
                    if( dmin==dn ) then
                       gam = dn
                       a2 = zero
                       if( z( nn-5 ) > z( nn-7 ) )return
                       b2 = z( nn-5 ) / z( nn-7 )
                       np = nn - 9_ilp64
                    else
                       np = nn - 2_ilp64*pp
                       gam = dn1
                       if( z( np-4 ) > z( np-2 ) )return
                       a2 = z( np-4 ) / z( np-2 )
                       if( z( nn-9 ) > z( nn-11 ) )return
                       b2 = z( nn-9 ) / z( nn-11 )
                       np = nn - 13_ilp64
                    end if
                    ! approximate contribution to norm squared from i < nn-1.
                    a2 = a2 + b2
                    do i4 = np, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 20
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 20
                    end do
                    20 continue
                    a2 = cnst3*a2
                    ! rayleigh quotient residual bound.
                    if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
                 end if
              else if( dmin==dn2 ) then
                 ! case 5.
                 ttype = -5_ilp64
                 s = qurtr*dmin
                 ! compute contribution to norm squared from i > nn-2.
                 np = nn - 2_ilp64*pp
                 b1 = z( np-2 )
                 b2 = z( np-6 )
                 gam = dn2
                 if( z( np-8 )>b2 .or. z( np-4 )>b1 )return
                 a2 = ( z( np-8 ) / b2 )*( one+z( np-4 ) / b1 )
                 ! approximate contribution to norm squared from i < nn-2.
                 if( n0-i0>2_ilp64 ) then
                    b2 = z( nn-13 ) / z( nn-15 )
                    a2 = a2 + b2
                    do i4 = nn - 17, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 40
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 40
                    end do
                    40 continue
                    a2 = cnst3*a2
                 end if
                 if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
              else
                 ! case 6, no information to guide us.
                 if( ttype==-6_ilp64 ) then
                    g = g + third*( one-g )
                 else if( ttype==-18_ilp64 ) then
                    g = qurtr*third
                 else
                    g = qurtr
                 end if
                 s = g*dmin
                 ttype = -6_ilp64
              end if
           else if( n0in==( n0+1 ) ) then
              ! one eigenvalue just deflated. use dmin1, dn1 for dmin and dn.
              if( dmin1==dn1 .and. dmin2==dn2 ) then
                 ! cases 7 and 8.
                 ttype = -7_ilp64
                 s = third*dmin1
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 60
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    a2 = b1
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*max( b1, a2 )<b2 )go to 60
                 end do
                 60 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin1 / ( one+b2**2_ilp64 )
                 gap2 = half*dmin2 - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                    ttype = -8_ilp64
                 end if
              else
                 ! case 9.
                 s = qurtr*dmin1
                 if( dmin1==dn1 )s = half*dmin1
                 ttype = -9_ilp64
              end if
           else if( n0in==( n0+2 ) ) then
              ! two eigenvalues deflated. use dmin2, dn2 for dmin and dn.
              ! cases 10 and 11.
              if( dmin2==dn2 .and. two*z( nn-5 )<z( nn-7 ) ) then
                 ttype = -10_ilp64
                 s = third*dmin2
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 80
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*b1<b2 )go to 80
                 end do
                 80 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin2 / ( one+b2**2_ilp64 )
                 gap2 = z( nn-7 ) + z( nn-9 ) -sqrt( z( nn-11 ) )*sqrt( z( nn-9 ) ) - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                 end if
              else
                 s = qurtr*dmin2
                 ttype = -11_ilp64
              end if
           else if( n0in>( n0+2 ) ) then
              ! case 12, more than two eigenvalues deflated. no information.
              s = zero
              ttype = -12_ilp64
           end if
           tau = s
           return
     end subroutine stdlib_I64_slasq4

     pure module subroutine stdlib_I64_dlasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
     !! DLASQ4 computes an approximation TAU to the smallest eigenvalue
     !! using values of d from the previous transform.
               ttype, g )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: i0, n0, n0in, pp
           integer(ilp64), intent(out) :: ttype
           real(dp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(dp), intent(inout) :: g
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(in) :: z(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: cnst1 = 0.5630_dp
           real(dp), parameter :: cnst2 = 1.010_dp
           real(dp), parameter :: cnst3 = 1.050_dp
           real(dp), parameter :: qurtr = 0.250_dp
           real(dp), parameter :: third = 0.3330_dp
           real(dp), parameter :: hundrd = 100.0_dp
           
           
           ! Local Scalars 
           integer(ilp64) :: i4, nn, np
           real(dp) :: a2, b1, b2, gam, gap1, gap2, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! a negative dmin forces the shift to take that absolute value
           ! ttype records the type of shift.
           if( dmin<=zero ) then
              tau = -dmin
              ttype = -1_ilp64
              return
           end if
           nn = 4_ilp64*n0 + pp
           if( n0in==n0 ) then
              ! no eigenvalues deflated.
              if( dmin==dn .or. dmin==dn1 ) then
                 b1 = sqrt( z( nn-3 ) )*sqrt( z( nn-5 ) )
                 b2 = sqrt( z( nn-7 ) )*sqrt( z( nn-9 ) )
                 a2 = z( nn-7 ) + z( nn-5 )
                 ! cases 2 and 3.
                 if( dmin==dn .and. dmin1==dn1 ) then
                    gap2 = dmin2 - a2 - dmin2*qurtr
                    if( gap2>zero .and. gap2>b2 ) then
                       gap1 = a2 - dn - ( b2 / gap2 )*b2
                    else
                       gap1 = a2 - dn - ( b1+b2 )
                    end if
                    if( gap1>zero .and. gap1>b1 ) then
                       s = max( dn-( b1 / gap1 )*b1, half*dmin )
                       ttype = -2_ilp64
                    else
                       s = zero
                       if( dn>b1 )s = dn - b1
                       if( a2>( b1+b2 ) )s = min( s, a2-( b1+b2 ) )
                       s = max( s, third*dmin )
                       ttype = -3_ilp64
                    end if
                 else
                    ! case 4.
                    ttype = -4_ilp64
                    s = qurtr*dmin
                    if( dmin==dn ) then
                       gam = dn
                       a2 = zero
                       if( z( nn-5 ) > z( nn-7 ) )return
                       b2 = z( nn-5 ) / z( nn-7 )
                       np = nn - 9_ilp64
                    else
                       np = nn - 2_ilp64*pp
                       gam = dn1
                       if( z( np-4 ) > z( np-2 ) )return
                       a2 = z( np-4 ) / z( np-2 )
                       if( z( nn-9 ) > z( nn-11 ) )return
                       b2 = z( nn-9 ) / z( nn-11 )
                       np = nn - 13_ilp64
                    end if
                    ! approximate contribution to norm squared from i < nn-1.
                    a2 = a2 + b2
                    do i4 = np, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 20
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 20
                    end do
                    20 continue
                    a2 = cnst3*a2
                    ! rayleigh quotient residual bound.
                    if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
                 end if
              else if( dmin==dn2 ) then
                 ! case 5.
                 ttype = -5_ilp64
                 s = qurtr*dmin
                 ! compute contribution to norm squared from i > nn-2.
                 np = nn - 2_ilp64*pp
                 b1 = z( np-2 )
                 b2 = z( np-6 )
                 gam = dn2
                 if( z( np-8 )>b2 .or. z( np-4 )>b1 )return
                 a2 = ( z( np-8 ) / b2 )*( one+z( np-4 ) / b1 )
                 ! approximate contribution to norm squared from i < nn-2.
                 if( n0-i0>2_ilp64 ) then
                    b2 = z( nn-13 ) / z( nn-15 )
                    a2 = a2 + b2
                    do i4 = nn - 17, 4*i0 - 1 + pp, -4
                       if( b2==zero )go to 40
                       b1 = b2
                       if( z( i4 ) > z( i4-2 ) )return
                       b2 = b2*( z( i4 ) / z( i4-2 ) )
                       a2 = a2 + b2
                       if( hundrd*max( b2, b1 )<a2 .or. cnst1<a2 )go to 40
                    end do
                    40 continue
                    a2 = cnst3*a2
                 end if
                 if( a2<cnst1 )s = gam*( one-sqrt( a2 ) ) / ( one+a2 )
              else
                 ! case 6, no information to guide us.
                 if( ttype==-6_ilp64 ) then
                    g = g + third*( one-g )
                 else if( ttype==-18_ilp64 ) then
                    g = qurtr*third
                 else
                    g = qurtr
                 end if
                 s = g*dmin
                 ttype = -6_ilp64
              end if
           else if( n0in==( n0+1 ) ) then
              ! one eigenvalue just deflated. use dmin1, dn1 for dmin and dn.
              if( dmin1==dn1 .and. dmin2==dn2 ) then
                 ! cases 7 and 8.
                 ttype = -7_ilp64
                 s = third*dmin1
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 60
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    a2 = b1
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*max( b1, a2 )<b2 )go to 60
                 end do
                 60 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin1 / ( one+b2**2_ilp64 )
                 gap2 = half*dmin2 - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                    ttype = -8_ilp64
                 end if
              else
                 ! case 9.
                 s = qurtr*dmin1
                 if( dmin1==dn1 )s = half*dmin1
                 ttype = -9_ilp64
              end if
           else if( n0in==( n0+2 ) ) then
              ! two eigenvalues deflated. use dmin2, dn2 for dmin and dn.
              ! cases 10 and 11.
              if( dmin2==dn2 .and. two*z( nn-5 )<z( nn-7 ) ) then
                 ttype = -10_ilp64
                 s = third*dmin2
                 if( z( nn-5 )>z( nn-7 ) )return
                 b1 = z( nn-5 ) / z( nn-7 )
                 b2 = b1
                 if( b2==zero )go to 80
                 do i4 = 4*n0 - 9 + pp, 4*i0 - 1 + pp, -4
                    if( z( i4 )>z( i4-2 ) )return
                    b1 = b1*( z( i4 ) / z( i4-2 ) )
                    b2 = b2 + b1
                    if( hundrd*b1<b2 )go to 80
                 end do
                 80 continue
                 b2 = sqrt( cnst3*b2 )
                 a2 = dmin2 / ( one+b2**2_ilp64 )
                 gap2 = z( nn-7 ) + z( nn-9 ) -sqrt( z( nn-11 ) )*sqrt( z( nn-9 ) ) - a2
                 if( gap2>zero .and. gap2>b2*a2 ) then
                    s = max( s, a2*( one-cnst2*a2*( b2 / gap2 )*b2 ) )
                 else
                    s = max( s, a2*( one-cnst2*b2 ) )
                 end if
              else
                 s = qurtr*dmin2
                 ttype = -11_ilp64
              end if
           else if( n0in>( n0+2 ) ) then
              ! case 12, more than two eigenvalues deflated. no information.
              s = zero
              ttype = -12_ilp64
           end if
           tau = s
           return
     end subroutine stdlib_I64_dlasq4




     pure module subroutine stdlib_I64_slasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
     !! SLASQ5 computes one dqds transform in ping-pong form, one
     !! version for IEEE machines another for non IEEE machines.
               ieee, eps )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp64), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(sp), intent(inout) :: tau
           real(sp), intent(in) :: sigma, eps
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j4, j4p2
           real(sp) :: d, emin, temp, dthresh
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           dthresh = eps*(sigma+tau)
           if( tau<dthresh*half ) tau = zero
           if( tau/=zero ) then
              j4 = 4_ilp64*i0 + pp - 3_ilp64
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           else
           ! this is the version that sets d's to zero if they are small enough
              j4 = 4_ilp64*i0 + pp - 3_ilp64
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           end if
           z( j4+2 ) = dn
           z( 4_ilp64*n0-pp ) = emin
           return
     end subroutine stdlib_I64_slasq5

     pure module subroutine stdlib_I64_dlasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
     !! DLASQ5 computes one dqds transform in ping-pong form, one
     !! version for IEEE machines another for non IEEE machines.
               ieee, eps )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ieee
           integer(ilp64), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(dp), intent(inout) :: tau
           real(dp), intent(in) :: sigma, eps
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j4, j4p2
           real(dp) :: d, emin, temp, dthresh
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           dthresh = eps*(sigma+tau)
           if( tau<dthresh*half ) tau = zero
           if( tau/=zero ) then
           j4 = 4_ilp64*i0 + pp - 3_ilp64
           emin = z( j4+4 )
           d = z( j4 ) - tau
           dmin = d
           dmin1 = -z( j4 )
           if( ieee ) then
              ! code for ieee arithmetic.
              if( pp==0_ilp64 ) then
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-2 ) = d + z( j4-1 )
                    temp = z( j4+1 ) / z( j4-2 )
                    d = d*temp - tau
                    dmin = min( dmin, d )
                    z( j4 ) = z( j4-1 )*temp
                    emin = min( z( j4 ), emin )
                 end do
              else
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-3 ) = d + z( j4 )
                    temp = z( j4+2 ) / z( j4-3 )
                    d = d*temp - tau
                    dmin = min( dmin, d )
                    z( j4-1 ) = z( j4 )*temp
                    emin = min( z( j4-1 ), emin )
                 end do
              end if
              ! unroll last two steps.
              dnm2 = d
              dmin2 = dmin
              j4 = 4_ilp64*( n0-2 ) - pp
              j4p2 = j4 + 2_ilp64*pp - 1_ilp64
              z( j4-2 ) = dnm2 + z( j4p2 )
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
              dmin = min( dmin, dnm1 )
              dmin1 = dmin
              j4 = j4 + 4_ilp64
              j4p2 = j4 + 2_ilp64*pp - 1_ilp64
              z( j4-2 ) = dnm1 + z( j4p2 )
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
              dmin = min( dmin, dn )
           else
              ! code for non ieee arithmetic.
              if( pp==0_ilp64 ) then
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-2 ) = d + z( j4-1 )
                    if( d<zero ) then
                       return
                    else
                       z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                       d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                    end if
                    dmin = min( dmin, d )
                    emin = min( emin, z( j4 ) )
                 end do
              else
                 do j4 = 4*i0, 4*( n0-3 ), 4
                    z( j4-3 ) = d + z( j4 )
                    if( d<zero ) then
                       return
                    else
                       z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                       d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                    end if
                    dmin = min( dmin, d )
                    emin = min( emin, z( j4-1 ) )
                 end do
              end if
              ! unroll last two steps.
              dnm2 = d
              dmin2 = dmin
              j4 = 4_ilp64*( n0-2 ) - pp
              j4p2 = j4 + 2_ilp64*pp - 1_ilp64
              z( j4-2 ) = dnm2 + z( j4p2 )
              if( dnm2<zero ) then
                 return
              else
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
              end if
              dmin = min( dmin, dnm1 )
              dmin1 = dmin
              j4 = j4 + 4_ilp64
              j4p2 = j4 + 2_ilp64*pp - 1_ilp64
              z( j4-2 ) = dnm1 + z( j4p2 )
              if( dnm1<zero ) then
                 return
              else
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
              end if
              dmin = min( dmin, dn )
           end if
           else
           ! this is the version that sets d's to zero if they are small enough
              j4 = 4_ilp64*i0 + pp - 3_ilp64
              emin = z( j4+4 )
              d = z( j4 ) - tau
              dmin = d
              dmin1 = -z( j4 )
              if( ieee ) then
           ! code for ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       temp = z( j4+1 ) / z( j4-2 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4 ) = z( j4-1 )*temp
                       emin = min( z( j4 ), emin )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       temp = z( j4+2 ) / z( j4-3 )
                       d = d*temp - tau
                       if( d<dthresh ) d = zero
                       dmin = min( dmin, d )
                       z( j4-1 ) = z( j4 )*temp
                       emin = min( z( j4-1 ), emin )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                 dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 dmin = min( dmin, dn )
              else
           ! code for non ieee arithmetic.
                 if( pp==0_ilp64 ) then
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-2 ) = d + z( j4-1 )
                       if( d<zero ) then
                          return
                       else
                          z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                          d = z( j4+1 )*( d / z( j4-2 ) ) - tau
                       end if
                       if( d<dthresh) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4 ) )
                    end do
                 else
                    do j4 = 4*i0, 4*( n0-3 ), 4
                       z( j4-3 ) = d + z( j4 )
                       if( d<zero ) then
                          return
                       else
                          z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                          d = z( j4+2 )*( d / z( j4-3 ) ) - tau
                       end if
                       if( d<dthresh) d = zero
                       dmin = min( dmin, d )
                       emin = min( emin, z( j4-1 ) )
                    end do
                 end if
           ! unroll last two steps.
                 dnm2 = d
                 dmin2 = dmin
                 j4 = 4_ilp64*( n0-2 ) - pp
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm2 + z( j4p2 )
                 if( dnm2<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dnm1 )
                 dmin1 = dmin
                 j4 = j4 + 4_ilp64
                 j4p2 = j4 + 2_ilp64*pp - 1_ilp64
                 z( j4-2 ) = dnm1 + z( j4p2 )
                 if( dnm1<zero ) then
                    return
                 else
                    z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
                    dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) ) - tau
                 end if
                 dmin = min( dmin, dn )
              end if
           end if
           z( j4+2 ) = dn
           z( 4_ilp64*n0-pp ) = emin
           return
     end subroutine stdlib_I64_dlasq5




     pure module subroutine stdlib_I64_slasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
     !! SLASQ6 computes one dqd (shift equal to zero) transform in
     !! ping-pong form, with protection against underflow and overflow.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           ! Array Arguments 
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j4, j4p2
           real(sp) :: d, emin, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           j4 = 4_ilp64*i0 + pp - 3_ilp64
           emin = z( j4+4 )
           d = z( j4 )
           dmin = d
           if( pp==0_ilp64 ) then
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-2 ) = d + z( j4-1 )
                 if( z( j4-2 )==zero ) then
                    z( j4 ) = zero
                    d = z( j4+1 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+1 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4+1 ) ) &
                           then
                    temp = z( j4+1 ) / z( j4-2 )
                    z( j4 ) = z( j4-1 )*temp
                    d = d*temp
                 else
                    z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                    d = z( j4+1 )*( d / z( j4-2 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4 ) )
              end do
           else
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-3 ) = d + z( j4 )
                 if( z( j4-3 )==zero ) then
                    z( j4-1 ) = zero
                    d = z( j4+2 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+2 )<z( j4-3 ) .and.safmin*z( j4-3 )<z( j4+2 ) ) &
                           then
                    temp = z( j4+2 ) / z( j4-3 )
                    z( j4-1 ) = z( j4 )*temp
                    d = d*temp
                 else
                    z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                    d = z( j4+2 )*( d / z( j4-3 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4-1 ) )
              end do
           end if
           ! unroll last two steps.
           dnm2 = d
           dmin2 = dmin
           j4 = 4_ilp64*( n0-2 ) - pp
           j4p2 = j4 + 2_ilp64*pp - 1_ilp64
           z( j4-2 ) = dnm2 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dnm1 = z( j4p2+2 )
              dmin = dnm1
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dnm1 = dnm2*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) )
           end if
           dmin = min( dmin, dnm1 )
           dmin1 = dmin
           j4 = j4 + 4_ilp64
           j4p2 = j4 + 2_ilp64*pp - 1_ilp64
           z( j4-2 ) = dnm1 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dn = z( j4p2+2 )
              dmin = dn
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dn = dnm1*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) )
           end if
           dmin = min( dmin, dn )
           z( j4+2 ) = dn
           z( 4_ilp64*n0-pp ) = emin
           return
     end subroutine stdlib_I64_slasq6

     pure module subroutine stdlib_I64_dlasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
     !! DLASQ6 computes one dqd (shift equal to zero) transform in
     !! ping-pong form, with protection against underflow and overflow.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           ! Array Arguments 
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j4, j4p2
           real(dp) :: d, emin, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ( n0-i0-1 )<=0 )return
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           j4 = 4_ilp64*i0 + pp - 3_ilp64
           emin = z( j4+4 )
           d = z( j4 )
           dmin = d
           if( pp==0_ilp64 ) then
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-2 ) = d + z( j4-1 )
                 if( z( j4-2 )==zero ) then
                    z( j4 ) = zero
                    d = z( j4+1 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+1 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4+1 ) ) &
                           then
                    temp = z( j4+1 ) / z( j4-2 )
                    z( j4 ) = z( j4-1 )*temp
                    d = d*temp
                 else
                    z( j4 ) = z( j4+1 )*( z( j4-1 ) / z( j4-2 ) )
                    d = z( j4+1 )*( d / z( j4-2 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4 ) )
              end do
           else
              do j4 = 4*i0, 4*( n0-3 ), 4
                 z( j4-3 ) = d + z( j4 )
                 if( z( j4-3 )==zero ) then
                    z( j4-1 ) = zero
                    d = z( j4+2 )
                    dmin = d
                    emin = zero
                 else if( safmin*z( j4+2 )<z( j4-3 ) .and.safmin*z( j4-3 )<z( j4+2 ) ) &
                           then
                    temp = z( j4+2 ) / z( j4-3 )
                    z( j4-1 ) = z( j4 )*temp
                    d = d*temp
                 else
                    z( j4-1 ) = z( j4+2 )*( z( j4 ) / z( j4-3 ) )
                    d = z( j4+2 )*( d / z( j4-3 ) )
                 end if
                 dmin = min( dmin, d )
                 emin = min( emin, z( j4-1 ) )
              end do
           end if
           ! unroll last two steps.
           dnm2 = d
           dmin2 = dmin
           j4 = 4_ilp64*( n0-2 ) - pp
           j4p2 = j4 + 2_ilp64*pp - 1_ilp64
           z( j4-2 ) = dnm2 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dnm1 = z( j4p2+2 )
              dmin = dnm1
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dnm1 = dnm2*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dnm1 = z( j4p2+2 )*( dnm2 / z( j4-2 ) )
           end if
           dmin = min( dmin, dnm1 )
           dmin1 = dmin
           j4 = j4 + 4_ilp64
           j4p2 = j4 + 2_ilp64*pp - 1_ilp64
           z( j4-2 ) = dnm1 + z( j4p2 )
           if( z( j4-2 )==zero ) then
              z( j4 ) = zero
              dn = z( j4p2+2 )
              dmin = dn
              emin = zero
           else if( safmin*z( j4p2+2 )<z( j4-2 ) .and.safmin*z( j4-2 )<z( j4p2+2 ) ) then
              temp = z( j4p2+2 ) / z( j4-2 )
              z( j4 ) = z( j4p2 )*temp
              dn = dnm1*temp
           else
              z( j4 ) = z( j4p2+2 )*( z( j4p2 ) / z( j4-2 ) )
              dn = z( j4p2+2 )*( dnm1 / z( j4-2 ) )
           end if
           dmin = min( dmin, dn )
           z( j4+2 ) = dn
           z( 4_ilp64*n0-pp ) = emin
           return
     end subroutine stdlib_I64_dlasq6



end submodule stdlib_lapack_svd_bidiag_qr
