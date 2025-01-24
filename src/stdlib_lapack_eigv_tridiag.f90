submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_tridiag
  implicit none


  contains

     pure module subroutine stdlib_slaebz( ijob, nitmax, n, mmax, minp, nbmin, abstol,reltol, pivmin, d, &
     !! SLAEBZ contains the iteration loops which compute and use the
     !! function N(w), which is the count of eigenvalues of a symmetric
     !! tridiagonal matrix T less than or equal to its argument  w.  It
     !! performs a choice of two types of loops:
     !! IJOB=1, followed by
     !! IJOB=2: It takes as input a list of intervals and returns a list of
     !! sufficiently small intervals whose union contains the same
     !! eigenvalues as the union of the original intervals.
     !! The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
     !! The output interval (AB(j,1),AB(j,2)] will contain
     !! eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
     !! IJOB=3: It performs a binary search in each input interval
     !! (AB(j,1),AB(j,2)] for a point  w(j)  such that
     !! N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
     !! the search.  If such a w(j) is found, then on output
     !! AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
     !! (AB(j,1),AB(j,2)] will be a small interval containing the
     !! point where N(w) jumps through NVAL(j), unless that point
     !! lies outside the initial interval.
     !! Note that the intervals are in all cases half-open intervals,
     !! i.e., of the form  (a,b] , which includes  b  but not  a .
     !! To avoid underflow, the matrix should be scaled so that its largest
     !! element is no greater than  overflow**(1/2) * underflow**(1/4)
     !! in absolute value.  To assure the most accurate computation
     !! of small eigenvalues, the matrix should be scaled to be
     !! not much smaller than that, either.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966
     !! Note: the arguments are, in general, *not* checked for unreasonable
     !! values.
               e, e2, nval, ab, c, mout,nab, work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, minp, mmax, n, nbmin, nitmax
           integer(ilp), intent(out) :: info, mout
           real(sp), intent(in) :: abstol, pivmin, reltol
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(inout) :: nab(mmax,*), nval(*)
           real(sp), intent(inout) :: ab(mmax,*), c(*)
           real(sp), intent(in) :: d(*), e(*), e2(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: itmp1, itmp2, j, ji, jit, jp, kf, kfnew, kl, klnew
           real(sp) :: tmp1, tmp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! check for errors
           info = 0_ilp
           if( ijob<1_ilp .or. ijob>3_ilp ) then
              info = -1_ilp
              return
           end if
           ! initialize nab
           if( ijob==1_ilp ) then
              ! compute the number of eigenvalues in the initial intervals.
              mout = 0_ilp
              do ji = 1, minp
                 do jp = 1, 2
                    tmp1 = d( 1_ilp ) - ab( ji, jp )
                    if( abs( tmp1 )<pivmin )tmp1 = -pivmin
                    nab( ji, jp ) = 0_ilp
                    if( tmp1<=zero )nab( ji, jp ) = 1_ilp
                    do j = 2, n
                       tmp1 = d( j ) - e2( j-1 ) / tmp1 - ab( ji, jp )
                       if( abs( tmp1 )<pivmin )tmp1 = -pivmin
                       if( tmp1<=zero )nab( ji, jp ) = nab( ji, jp ) + 1_ilp
                    end do
                 end do
                 mout = mout + nab( ji, 2_ilp ) - nab( ji, 1_ilp )
              end do
              return
           end if
           ! initialize for loop
           ! kf and kl have the following meaning:
              ! intervals 1,...,kf-1 have converged.
              ! intervals kf,...,kl  still need to be refined.
           kf = 1_ilp
           kl = minp
           ! if ijob=2, initialize c.
           ! if ijob=3, use the user-supplied starting point.
           if( ijob==2_ilp ) then
              do ji = 1, minp
                 c( ji ) = half*( ab( ji, 1_ilp )+ab( ji, 2_ilp ) )
              end do
           end if
           ! iteration loop
           loop_130: do jit = 1, nitmax
              ! loop over intervals
              if( kl-kf+1>=nbmin .and. nbmin>0_ilp ) then
                 ! begin of parallel version of the loop
                 do ji = kf, kl
                    ! compute n(c), the number of eigenvalues less than c
                    work( ji ) = d( 1_ilp ) - c( ji )
                    iwork( ji ) = 0_ilp
                    if( work( ji )<=pivmin ) then
                       iwork( ji ) = 1_ilp
                       work( ji ) = min( work( ji ), -pivmin )
                    end if
                    do j = 2, n
                       work( ji ) = d( j ) - e2( j-1 ) / work( ji ) - c( ji )
                       if( work( ji )<=pivmin ) then
                          iwork( ji ) = iwork( ji ) + 1_ilp
                          work( ji ) = min( work( ji ), -pivmin )
                       end if
                    end do
                 end do
                 if( ijob<=2_ilp ) then
                    ! ijob=2: choose all intervals containing eigenvalues.
                    klnew = kl
                    loop_70: do ji = kf, kl
                       ! insure that n(w) is monotone
                       iwork( ji ) = min( nab( ji, 2_ilp ),max( nab( ji, 1_ilp ), iwork( ji ) ) )
                       ! update the queue -- add intervals if both halves
                       ! contain eigenvalues.
                       if( iwork( ji )==nab( ji, 2_ilp ) ) then
                          ! no eigenvalue in the upper interval:
                          ! just use the lower interval.
                          ab( ji, 2_ilp ) = c( ji )
                       else if( iwork( ji )==nab( ji, 1_ilp ) ) then
                          ! no eigenvalue in the lower interval:
                          ! just use the upper interval.
                          ab( ji, 1_ilp ) = c( ji )
                       else
                          klnew = klnew + 1_ilp
                          if( klnew<=mmax ) then
                             ! eigenvalue in both intervals -- add upper to
                             ! queue.
                             ab( klnew, 2_ilp ) = ab( ji, 2_ilp )
                             nab( klnew, 2_ilp ) = nab( ji, 2_ilp )
                             ab( klnew, 1_ilp ) = c( ji )
                             nab( klnew, 1_ilp ) = iwork( ji )
                             ab( ji, 2_ilp ) = c( ji )
                             nab( ji, 2_ilp ) = iwork( ji )
                          else
                             info = mmax + 1_ilp
                          end if
                       end if
                    end do loop_70
                    if( info/=0 )return
                    kl = klnew
                 else
                    ! ijob=3: binary search.  keep only the interval containing
                            ! w   s.t. n(w) = nval
                    do ji = kf, kl
                       if( iwork( ji )<=nval( ji ) ) then
                          ab( ji, 1_ilp ) = c( ji )
                          nab( ji, 1_ilp ) = iwork( ji )
                       end if
                       if( iwork( ji )>=nval( ji ) ) then
                          ab( ji, 2_ilp ) = c( ji )
                          nab( ji, 2_ilp ) = iwork( ji )
                       end if
                    end do
                 end if
              else
                 ! end of parallel version of the loop
                 ! begin of serial version of the loop
                 klnew = kl
                 loop_100: do ji = kf, kl
                    ! compute n(w), the number of eigenvalues less than w
                    tmp1 = c( ji )
                    tmp2 = d( 1_ilp ) - tmp1
                    itmp1 = 0_ilp
                    if( tmp2<=pivmin ) then
                       itmp1 = 1_ilp
                       tmp2 = min( tmp2, -pivmin )
                    end if
                    do j = 2, n
                       tmp2 = d( j ) - e2( j-1 ) / tmp2 - tmp1
                       if( tmp2<=pivmin ) then
                          itmp1 = itmp1 + 1_ilp
                          tmp2 = min( tmp2, -pivmin )
                       end if
                    end do
                    if( ijob<=2_ilp ) then
                       ! ijob=2: choose all intervals containing eigenvalues.
                       ! insure that n(w) is monotone
                       itmp1 = min( nab( ji, 2_ilp ),max( nab( ji, 1_ilp ), itmp1 ) )
                       ! update the queue -- add intervals if both halves
                       ! contain eigenvalues.
                       if( itmp1==nab( ji, 2_ilp ) ) then
                          ! no eigenvalue in the upper interval:
                          ! just use the lower interval.
                          ab( ji, 2_ilp ) = tmp1
                       else if( itmp1==nab( ji, 1_ilp ) ) then
                          ! no eigenvalue in the lower interval:
                          ! just use the upper interval.
                          ab( ji, 1_ilp ) = tmp1
                       else if( klnew<mmax ) then
                          ! eigenvalue in both intervals -- add upper to queue.
                          klnew = klnew + 1_ilp
                          ab( klnew, 2_ilp ) = ab( ji, 2_ilp )
                          nab( klnew, 2_ilp ) = nab( ji, 2_ilp )
                          ab( klnew, 1_ilp ) = tmp1
                          nab( klnew, 1_ilp ) = itmp1
                          ab( ji, 2_ilp ) = tmp1
                          nab( ji, 2_ilp ) = itmp1
                       else
                          info = mmax + 1_ilp
                          return
                       end if
                    else
                       ! ijob=3: binary search.  keep only the interval
                               ! containing  w  s.t. n(w) = nval
                       if( itmp1<=nval( ji ) ) then
                          ab( ji, 1_ilp ) = tmp1
                          nab( ji, 1_ilp ) = itmp1
                       end if
                       if( itmp1>=nval( ji ) ) then
                          ab( ji, 2_ilp ) = tmp1
                          nab( ji, 2_ilp ) = itmp1
                       end if
                    end if
                 end do loop_100
                 kl = klnew
              end if
              ! check for convergence
              kfnew = kf
              loop_110: do ji = kf, kl
                 tmp1 = abs( ab( ji, 2_ilp )-ab( ji, 1_ilp ) )
                 tmp2 = max( abs( ab( ji, 2_ilp ) ), abs( ab( ji, 1_ilp ) ) )
                 if( tmp1<max( abstol, pivmin, reltol*tmp2 ) .or.nab( ji, 1_ilp )>=nab( ji, 2_ilp ) ) &
                           then
                    ! converged -- swap with position kfnew,
                                 ! then increment kfnew
                    if( ji>kfnew ) then
                       tmp1 = ab( ji, 1_ilp )
                       tmp2 = ab( ji, 2_ilp )
                       itmp1 = nab( ji, 1_ilp )
                       itmp2 = nab( ji, 2_ilp )
                       ab( ji, 1_ilp ) = ab( kfnew, 1_ilp )
                       ab( ji, 2_ilp ) = ab( kfnew, 2_ilp )
                       nab( ji, 1_ilp ) = nab( kfnew, 1_ilp )
                       nab( ji, 2_ilp ) = nab( kfnew, 2_ilp )
                       ab( kfnew, 1_ilp ) = tmp1
                       ab( kfnew, 2_ilp ) = tmp2
                       nab( kfnew, 1_ilp ) = itmp1
                       nab( kfnew, 2_ilp ) = itmp2
                       if( ijob==3_ilp ) then
                          itmp1 = nval( ji )
                          nval( ji ) = nval( kfnew )
                          nval( kfnew ) = itmp1
                       end if
                    end if
                    kfnew = kfnew + 1_ilp
                 end if
              end do loop_110
              kf = kfnew
              ! choose midpoints
              do ji = kf, kl
                 c( ji ) = half*( ab( ji, 1_ilp )+ab( ji, 2_ilp ) )
              end do
              ! if no more intervals to refine, quit.
              if( kf>kl )go to 140
           end do loop_130
           ! converged
           140 continue
           info = max( kl+1-kf, 0_ilp )
           mout = kl
           return
     end subroutine stdlib_slaebz

     pure module subroutine stdlib_dlaebz( ijob, nitmax, n, mmax, minp, nbmin, abstol,reltol, pivmin, d, &
     !! DLAEBZ contains the iteration loops which compute and use the
     !! function N(w), which is the count of eigenvalues of a symmetric
     !! tridiagonal matrix T less than or equal to its argument  w.  It
     !! performs a choice of two types of loops:
     !! IJOB=1, followed by
     !! IJOB=2: It takes as input a list of intervals and returns a list of
     !! sufficiently small intervals whose union contains the same
     !! eigenvalues as the union of the original intervals.
     !! The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
     !! The output interval (AB(j,1),AB(j,2)] will contain
     !! eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
     !! IJOB=3: It performs a binary search in each input interval
     !! (AB(j,1),AB(j,2)] for a point  w(j)  such that
     !! N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
     !! the search.  If such a w(j) is found, then on output
     !! AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
     !! (AB(j,1),AB(j,2)] will be a small interval containing the
     !! point where N(w) jumps through NVAL(j), unless that point
     !! lies outside the initial interval.
     !! Note that the intervals are in all cases half-open intervals,
     !! i.e., of the form  (a,b] , which includes  b  but not  a .
     !! To avoid underflow, the matrix should be scaled so that its largest
     !! element is no greater than  overflow**(1/2) * underflow**(1/4)
     !! in absolute value.  To assure the most accurate computation
     !! of small eigenvalues, the matrix should be scaled to be
     !! not much smaller than that, either.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966
     !! Note: the arguments are, in general, *not* checked for unreasonable
     !! values.
               e, e2, nval, ab, c, mout,nab, work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, minp, mmax, n, nbmin, nitmax
           integer(ilp), intent(out) :: info, mout
           real(dp), intent(in) :: abstol, pivmin, reltol
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(inout) :: nab(mmax,*), nval(*)
           real(dp), intent(inout) :: ab(mmax,*), c(*)
           real(dp), intent(in) :: d(*), e(*), e2(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: itmp1, itmp2, j, ji, jit, jp, kf, kfnew, kl, klnew
           real(dp) :: tmp1, tmp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! check for errors
           info = 0_ilp
           if( ijob<1_ilp .or. ijob>3_ilp ) then
              info = -1_ilp
              return
           end if
           ! initialize nab
           if( ijob==1_ilp ) then
              ! compute the number of eigenvalues in the initial intervals.
              mout = 0_ilp
              do ji = 1, minp
                 do jp = 1, 2
                    tmp1 = d( 1_ilp ) - ab( ji, jp )
                    if( abs( tmp1 )<pivmin )tmp1 = -pivmin
                    nab( ji, jp ) = 0_ilp
                    if( tmp1<=zero )nab( ji, jp ) = 1_ilp
                    do j = 2, n
                       tmp1 = d( j ) - e2( j-1 ) / tmp1 - ab( ji, jp )
                       if( abs( tmp1 )<pivmin )tmp1 = -pivmin
                       if( tmp1<=zero )nab( ji, jp ) = nab( ji, jp ) + 1_ilp
                    end do
                 end do
                 mout = mout + nab( ji, 2_ilp ) - nab( ji, 1_ilp )
              end do
              return
           end if
           ! initialize for loop
           ! kf and kl have the following meaning:
              ! intervals 1,...,kf-1 have converged.
              ! intervals kf,...,kl  still need to be refined.
           kf = 1_ilp
           kl = minp
           ! if ijob=2, initialize c.
           ! if ijob=3, use the user-supplied starting point.
           if( ijob==2_ilp ) then
              do ji = 1, minp
                 c( ji ) = half*( ab( ji, 1_ilp )+ab( ji, 2_ilp ) )
              end do
           end if
           ! iteration loop
           loop_130: do jit = 1, nitmax
              ! loop over intervals
              if( kl-kf+1>=nbmin .and. nbmin>0_ilp ) then
                 ! begin of parallel version of the loop
                 do ji = kf, kl
                    ! compute n(c), the number of eigenvalues less than c
                    work( ji ) = d( 1_ilp ) - c( ji )
                    iwork( ji ) = 0_ilp
                    if( work( ji )<=pivmin ) then
                       iwork( ji ) = 1_ilp
                       work( ji ) = min( work( ji ), -pivmin )
                    end if
                    do j = 2, n
                       work( ji ) = d( j ) - e2( j-1 ) / work( ji ) - c( ji )
                       if( work( ji )<=pivmin ) then
                          iwork( ji ) = iwork( ji ) + 1_ilp
                          work( ji ) = min( work( ji ), -pivmin )
                       end if
                    end do
                 end do
                 if( ijob<=2_ilp ) then
                    ! ijob=2: choose all intervals containing eigenvalues.
                    klnew = kl
                    loop_70: do ji = kf, kl
                       ! insure that n(w) is monotone
                       iwork( ji ) = min( nab( ji, 2_ilp ),max( nab( ji, 1_ilp ), iwork( ji ) ) )
                       ! update the queue -- add intervals if both halves
                       ! contain eigenvalues.
                       if( iwork( ji )==nab( ji, 2_ilp ) ) then
                          ! no eigenvalue in the upper interval:
                          ! just use the lower interval.
                          ab( ji, 2_ilp ) = c( ji )
                       else if( iwork( ji )==nab( ji, 1_ilp ) ) then
                          ! no eigenvalue in the lower interval:
                          ! just use the upper interval.
                          ab( ji, 1_ilp ) = c( ji )
                       else
                          klnew = klnew + 1_ilp
                          if( klnew<=mmax ) then
                             ! eigenvalue in both intervals -- add upper to
                             ! queue.
                             ab( klnew, 2_ilp ) = ab( ji, 2_ilp )
                             nab( klnew, 2_ilp ) = nab( ji, 2_ilp )
                             ab( klnew, 1_ilp ) = c( ji )
                             nab( klnew, 1_ilp ) = iwork( ji )
                             ab( ji, 2_ilp ) = c( ji )
                             nab( ji, 2_ilp ) = iwork( ji )
                          else
                             info = mmax + 1_ilp
                          end if
                       end if
                    end do loop_70
                    if( info/=0 )return
                    kl = klnew
                 else
                    ! ijob=3: binary search.  keep only the interval containing
                            ! w   s.t. n(w) = nval
                    do ji = kf, kl
                       if( iwork( ji )<=nval( ji ) ) then
                          ab( ji, 1_ilp ) = c( ji )
                          nab( ji, 1_ilp ) = iwork( ji )
                       end if
                       if( iwork( ji )>=nval( ji ) ) then
                          ab( ji, 2_ilp ) = c( ji )
                          nab( ji, 2_ilp ) = iwork( ji )
                       end if
                    end do
                 end if
              else
                 ! end of parallel version of the loop
                 ! begin of serial version of the loop
                 klnew = kl
                 loop_100: do ji = kf, kl
                    ! compute n(w), the number of eigenvalues less than w
                    tmp1 = c( ji )
                    tmp2 = d( 1_ilp ) - tmp1
                    itmp1 = 0_ilp
                    if( tmp2<=pivmin ) then
                       itmp1 = 1_ilp
                       tmp2 = min( tmp2, -pivmin )
                    end if
                    do j = 2, n
                       tmp2 = d( j ) - e2( j-1 ) / tmp2 - tmp1
                       if( tmp2<=pivmin ) then
                          itmp1 = itmp1 + 1_ilp
                          tmp2 = min( tmp2, -pivmin )
                       end if
                    end do
                    if( ijob<=2_ilp ) then
                       ! ijob=2: choose all intervals containing eigenvalues.
                       ! insure that n(w) is monotone
                       itmp1 = min( nab( ji, 2_ilp ),max( nab( ji, 1_ilp ), itmp1 ) )
                       ! update the queue -- add intervals if both halves
                       ! contain eigenvalues.
                       if( itmp1==nab( ji, 2_ilp ) ) then
                          ! no eigenvalue in the upper interval:
                          ! just use the lower interval.
                          ab( ji, 2_ilp ) = tmp1
                       else if( itmp1==nab( ji, 1_ilp ) ) then
                          ! no eigenvalue in the lower interval:
                          ! just use the upper interval.
                          ab( ji, 1_ilp ) = tmp1
                       else if( klnew<mmax ) then
                          ! eigenvalue in both intervals -- add upper to queue.
                          klnew = klnew + 1_ilp
                          ab( klnew, 2_ilp ) = ab( ji, 2_ilp )
                          nab( klnew, 2_ilp ) = nab( ji, 2_ilp )
                          ab( klnew, 1_ilp ) = tmp1
                          nab( klnew, 1_ilp ) = itmp1
                          ab( ji, 2_ilp ) = tmp1
                          nab( ji, 2_ilp ) = itmp1
                       else
                          info = mmax + 1_ilp
                          return
                       end if
                    else
                       ! ijob=3: binary search.  keep only the interval
                               ! containing  w  s.t. n(w) = nval
                       if( itmp1<=nval( ji ) ) then
                          ab( ji, 1_ilp ) = tmp1
                          nab( ji, 1_ilp ) = itmp1
                       end if
                       if( itmp1>=nval( ji ) ) then
                          ab( ji, 2_ilp ) = tmp1
                          nab( ji, 2_ilp ) = itmp1
                       end if
                    end if
                 end do loop_100
                 kl = klnew
              end if
              ! check for convergence
              kfnew = kf
              loop_110: do ji = kf, kl
                 tmp1 = abs( ab( ji, 2_ilp )-ab( ji, 1_ilp ) )
                 tmp2 = max( abs( ab( ji, 2_ilp ) ), abs( ab( ji, 1_ilp ) ) )
                 if( tmp1<max( abstol, pivmin, reltol*tmp2 ) .or.nab( ji, 1_ilp )>=nab( ji, 2_ilp ) ) &
                           then
                    ! converged -- swap with position kfnew,
                                 ! then increment kfnew
                    if( ji>kfnew ) then
                       tmp1 = ab( ji, 1_ilp )
                       tmp2 = ab( ji, 2_ilp )
                       itmp1 = nab( ji, 1_ilp )
                       itmp2 = nab( ji, 2_ilp )
                       ab( ji, 1_ilp ) = ab( kfnew, 1_ilp )
                       ab( ji, 2_ilp ) = ab( kfnew, 2_ilp )
                       nab( ji, 1_ilp ) = nab( kfnew, 1_ilp )
                       nab( ji, 2_ilp ) = nab( kfnew, 2_ilp )
                       ab( kfnew, 1_ilp ) = tmp1
                       ab( kfnew, 2_ilp ) = tmp2
                       nab( kfnew, 1_ilp ) = itmp1
                       nab( kfnew, 2_ilp ) = itmp2
                       if( ijob==3_ilp ) then
                          itmp1 = nval( ji )
                          nval( ji ) = nval( kfnew )
                          nval( kfnew ) = itmp1
                       end if
                    end if
                    kfnew = kfnew + 1_ilp
                 end if
              end do loop_110
              kf = kfnew
              ! choose midpoints
              do ji = kf, kl
                 c( ji ) = half*( ab( ji, 1_ilp )+ab( ji, 2_ilp ) )
              end do
              ! if no more intervals to refine, quit.
              if( kf>kl )go to 140
           end do loop_130
           ! converged
           140 continue
           info = max( kl+1-kf, 0_ilp )
           mout = kl
           return
     end subroutine stdlib_dlaebz




     pure integer(ilp) module function stdlib_slaneg( n, d, lld, sigma, pivmin, r )
     !! SLANEG computes the Sturm count, the number of negative pivots
     !! encountered while factoring tridiagonal T - sigma I = L D L^T.
     !! This implementation works directly on the factors without forming
     !! the tridiagonal matrix T.  The Sturm count is also the number of
     !! eigenvalues of T less than sigma.
     !! This routine is called from SLARRB.
     !! The current routine does not use the PIVMIN parameter but rather
     !! requires IEEE-754 propagation of Infinities and NaNs.  This
     !! routine also has no input range restrictions but does require
     !! default exception handling such that x/0 produces Inf when x is
     !! non-zero, and Inf/Inf produces NaN.  For more information, see:
     !! Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
     !! Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
     !! Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
     !! (Tech report version in LAWN 172 with the same title.)
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, r
           real(sp), intent(in) :: pivmin, sigma
           ! Array Arguments 
           real(sp), intent(in) :: d(*), lld(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blklen = 128_ilp
           
           ! some architectures propagate infinities and nans very slowly, so
           ! the code computes counts in blklen chunks.  then a nan can
           ! propagate at most blklen columns before being detected.  this is
           ! not a general tuning parameter; it needs only to be just large
           ! enough that the overhead is tiny in common cases.
           
           ! Local Scalars 
           integer(ilp) :: bj, j, neg1, neg2, negcnt
           real(sp) :: bsav, dminus, dplus, gamma, p, t, tmp
           logical(lk) :: sawnan
           ! Intrinsic Functions 
           ! Executable Statements 
           negcnt = 0_ilp
           ! i) upper part: l d l^t - sigma i = l+ d+ l+^t
           t = -sigma
           loop_210: do bj = 1, r-1, blklen
              neg1 = 0_ilp
              bsav = t
              do j = bj, min(bj+blklen-1, r-1)
                 dplus = d( j ) + t
                 if( dplus<zero ) neg1 = neg1 + 1_ilp
                 tmp = t / dplus
                 t = tmp * lld( j ) - sigma
              end do
              sawnan = stdlib_sisnan( t )
           ! run a slower version of the above loop if a nan is detected.
           ! a nan should occur only with a zero pivot after an infinite
           ! pivot.  in that case, substituting 1 for t/dplus is the
           ! correct limit.
              if( sawnan ) then
                 neg1 = 0_ilp
                 t = bsav
                 do j = bj, min(bj+blklen-1, r-1)
                    dplus = d( j ) + t
                    if( dplus<zero ) neg1 = neg1 + 1_ilp
                    tmp = t / dplus
                    if (stdlib_sisnan(tmp)) tmp = one
                    t = tmp * lld(j) - sigma
                 end do
              end if
              negcnt = negcnt + neg1
           end do loop_210
           ! ii) lower part: l d l^t - sigma i = u- d- u-^t
           p = d( n ) - sigma
           do bj = n-1, r, -blklen
              neg2 = 0_ilp
              bsav = p
              do j = bj, max(bj-blklen+1, r), -1
                 dminus = lld( j ) + p
                 if( dminus<zero ) neg2 = neg2 + 1_ilp
                 tmp = p / dminus
                 p = tmp * d( j ) - sigma
              end do
              sawnan = stdlib_sisnan( p )
           ! as above, run a slower version that substitutes 1 for inf/inf.
              if( sawnan ) then
                 neg2 = 0_ilp
                 p = bsav
                 do j = bj, max(bj-blklen+1, r), -1
                    dminus = lld( j ) + p
                    if( dminus<zero ) neg2 = neg2 + 1_ilp
                    tmp = p / dminus
                    if (stdlib_sisnan(tmp)) tmp = one
                    p = tmp * d(j) - sigma
                 end do
              end if
              negcnt = negcnt + neg2
           end do
           ! iii) twist index
             ! t was shifted by sigma initially.
           gamma = (t + sigma) + p
           if( gamma<zero ) negcnt = negcnt+1
           stdlib_slaneg = negcnt
     end function stdlib_slaneg

     pure integer(ilp) module function stdlib_dlaneg( n, d, lld, sigma, pivmin, r )
     !! DLANEG computes the Sturm count, the number of negative pivots
     !! encountered while factoring tridiagonal T - sigma I = L D L^T.
     !! This implementation works directly on the factors without forming
     !! the tridiagonal matrix T.  The Sturm count is also the number of
     !! eigenvalues of T less than sigma.
     !! This routine is called from DLARRB.
     !! The current routine does not use the PIVMIN parameter but rather
     !! requires IEEE-754 propagation of Infinities and NaNs.  This
     !! routine also has no input range restrictions but does require
     !! default exception handling such that x/0 produces Inf when x is
     !! non-zero, and Inf/Inf produces NaN.  For more information, see:
     !! Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
     !! Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
     !! Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
     !! (Tech report version in LAWN 172 with the same title.)
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, r
           real(dp), intent(in) :: pivmin, sigma
           ! Array Arguments 
           real(dp), intent(in) :: d(*), lld(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blklen = 128_ilp
           
           ! some architectures propagate infinities and nans very slowly, so
           ! the code computes counts in blklen chunks.  then a nan can
           ! propagate at most blklen columns before being detected.  this is
           ! not a general tuning parameter; it needs only to be just large
           ! enough that the overhead is tiny in common cases.
           
           ! Local Scalars 
           integer(ilp) :: bj, j, neg1, neg2, negcnt
           real(dp) :: bsav, dminus, dplus, gamma, p, t, tmp
           logical(lk) :: sawnan
           ! Intrinsic Functions 
           ! Executable Statements 
           negcnt = 0_ilp
           ! i) upper part: l d l^t - sigma i = l+ d+ l+^t
           t = -sigma
           loop_210: do bj = 1, r-1, blklen
              neg1 = 0_ilp
              bsav = t
              do j = bj, min(bj+blklen-1, r-1)
                 dplus = d( j ) + t
                 if( dplus<zero ) neg1 = neg1 + 1_ilp
                 tmp = t / dplus
                 t = tmp * lld( j ) - sigma
              end do
              sawnan = stdlib_disnan( t )
           ! run a slower version of the above loop if a nan is detected.
           ! a nan should occur only with a zero pivot after an infinite
           ! pivot.  in that case, substituting 1 for t/dplus is the
           ! correct limit.
              if( sawnan ) then
                 neg1 = 0_ilp
                 t = bsav
                 do j = bj, min(bj+blklen-1, r-1)
                    dplus = d( j ) + t
                    if( dplus<zero ) neg1 = neg1 + 1_ilp
                    tmp = t / dplus
                    if (stdlib_disnan(tmp)) tmp = one
                    t = tmp * lld(j) - sigma
                 end do
              end if
              negcnt = negcnt + neg1
           end do loop_210
           ! ii) lower part: l d l^t - sigma i = u- d- u-^t
           p = d( n ) - sigma
           do bj = n-1, r, -blklen
              neg2 = 0_ilp
              bsav = p
              do j = bj, max(bj-blklen+1, r), -1
                 dminus = lld( j ) + p
                 if( dminus<zero ) neg2 = neg2 + 1_ilp
                 tmp = p / dminus
                 p = tmp * d( j ) - sigma
              end do
              sawnan = stdlib_disnan( p )
           ! as above, run a slower version that substitutes 1 for inf/inf.
              if( sawnan ) then
                 neg2 = 0_ilp
                 p = bsav
                 do j = bj, max(bj-blklen+1, r), -1
                    dminus = lld( j ) + p
                    if( dminus<zero ) neg2 = neg2 + 1_ilp
                    tmp = p / dminus
                    if (stdlib_disnan(tmp)) tmp = one
                    p = tmp * d(j) - sigma
                 end do
              end if
              negcnt = negcnt + neg2
           end do
           ! iii) twist index
             ! t was shifted by sigma initially.
           gamma = (t + sigma) + p
           if( gamma<zero ) negcnt = negcnt+1
           stdlib_dlaneg = negcnt
     end function stdlib_dlaneg




     pure module subroutine stdlib_slaed0( icompq, qsiz, n, d, e, q, ldq, qstore, ldqs,work, iwork, info &
     !! SLAED0 computes all eigenvalues and corresponding eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
               )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*), q(ldq,*)
           real(sp), intent(out) :: qstore(ldqs,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: curlvl, curprb, curr, i, igivcl, igivnm, igivpt, indxq, iperm, iprmpt, &
           iq, iqptr, iwrem, j, k, lgn, matsiz, msd2, smlsiz, smm1, spm1, spm2, submat, subpbs, &
                     tlvls
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>2_ilp ) then
              info = -1_ilp
           else if( ( icompq==1_ilp ) .and. ( qsiz<max( 0_ilp, n ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldqs<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED0', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'SLAED0', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           ! determine the size and placement of the submatrices, and save in
           ! the leading elements of iwork.
           iwork( 1_ilp ) = n
           subpbs = 1_ilp
           tlvls = 0_ilp
           10 continue
           if( iwork( subpbs )>smlsiz ) then
              do j = subpbs, 1, -1
                 iwork( 2_ilp*j ) = ( iwork( j )+1_ilp ) / 2_ilp
                 iwork( 2_ilp*j-1 ) = iwork( j ) / 2_ilp
              end do
              tlvls = tlvls + 1_ilp
              subpbs = 2_ilp*subpbs
              go to 10
           end if
           do j = 2, subpbs
              iwork( j ) = iwork( j ) + iwork( j-1 )
           end do
           ! divide the matrix into subpbs submatrices of size at most smlsiz+1
           ! using rank-1 modifications (cuts).
           spm1 = subpbs - 1_ilp
           do i = 1, spm1
              submat = iwork( i ) + 1_ilp
              smm1 = submat - 1_ilp
              d( smm1 ) = d( smm1 ) - abs( e( smm1 ) )
              d( submat ) = d( submat ) - abs( e( smm1 ) )
           end do
           indxq = 4_ilp*n + 3_ilp
           if( icompq/=2_ilp ) then
              ! set up workspaces for eigenvalues only/accumulate new vectors
              ! routine
              temp = log( real( n,KIND=sp) ) / log( two )
              lgn = int( temp,KIND=ilp)
              if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
              if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
              iprmpt = indxq + n + 1_ilp
              iperm = iprmpt + n*lgn
              iqptr = iperm + n*lgn
              igivpt = iqptr + n + 2_ilp
              igivcl = igivpt + n*lgn
              igivnm = 1_ilp
              iq = igivnm + 2_ilp*n*lgn
              iwrem = iq + n**2_ilp + 1_ilp
              ! initialize pointers
              do i = 0, subpbs
                 iwork( iprmpt+i ) = 1_ilp
                 iwork( igivpt+i ) = 1_ilp
              end do
              iwork( iqptr ) = 1_ilp
           end if
           ! solve each submatrix eigenproblem at the bottom of the divide and
           ! conquer tree.
           curr = 0_ilp
           loop_70: do i = 0, spm1
              if( i==0_ilp ) then
                 submat = 1_ilp
                 matsiz = iwork( 1_ilp )
              else
                 submat = iwork( i ) + 1_ilp
                 matsiz = iwork( i+1 ) - iwork( i )
              end if
              if( icompq==2_ilp ) then
                 call stdlib_ssteqr( 'I', matsiz, d( submat ), e( submat ),q( submat, submat ), &
                           ldq, work, info )
                 if( info/=0 )go to 130
              else
                 call stdlib_ssteqr( 'I', matsiz, d( submat ), e( submat ),work( iq-1+iwork( &
                           iqptr+curr ) ), matsiz, work,info )
                 if( info/=0 )go to 130
                 if( icompq==1_ilp ) then
                    call stdlib_sgemm( 'N', 'N', qsiz, matsiz, matsiz, one,q( 1_ilp, submat ), ldq, &
                    work( iq-1+iwork( iqptr+curr ) ), matsiz, zero, qstore( 1_ilp, submat ),ldqs )
                              
                 end if
                 iwork( iqptr+curr+1 ) = iwork( iqptr+curr ) + matsiz**2_ilp
                 curr = curr + 1_ilp
              end if
              k = 1_ilp
              do j = submat, iwork( i+1 )
                 iwork( indxq+j ) = k
                 k = k + 1_ilp
              end do
           end do loop_70
           ! successively merge eigensystems of adjacent submatrices
           ! into eigensystem for the corresponding larger matrix.
           ! while ( subpbs > 1 )
           curlvl = 1_ilp
           80 continue
           if( subpbs>1_ilp ) then
              spm2 = subpbs - 2_ilp
              loop_90: do i = 0, spm2, 2
                 if( i==0_ilp ) then
                    submat = 1_ilp
                    matsiz = iwork( 2_ilp )
                    msd2 = iwork( 1_ilp )
                    curprb = 0_ilp
                 else
                    submat = iwork( i ) + 1_ilp
                    matsiz = iwork( i+2 ) - iwork( i )
                    msd2 = matsiz / 2_ilp
                    curprb = curprb + 1_ilp
                 end if
           ! merge lower order eigensystems (of size msd2 and matsiz - msd2)
           ! into an eigensystem of size matsiz.
           ! stdlib_slaed1 is used only for the full eigensystem of a tridiagonal
           ! matrix.
           ! stdlib_slaed7 handles the cases in which eigenvalues only or eigenvalues
           ! and eigenvectors of a full symmetric matrix (which was reduced to
           ! tridiagonal form) are desired.
                 if( icompq==2_ilp ) then
                    call stdlib_slaed1( matsiz, d( submat ), q( submat, submat ),ldq, iwork( &
                    indxq+submat ),e( submat+msd2-1 ), msd2, work,iwork( subpbs+1 ), info )
                              
                 else
                    call stdlib_slaed7( icompq, matsiz, qsiz, tlvls, curlvl, curprb,d( submat ), &
                    qstore( 1_ilp, submat ), ldqs,iwork( indxq+submat ), e( submat+msd2-1 ),msd2, &
                    work( iq ), iwork( iqptr ),iwork( iprmpt ), iwork( iperm ),iwork( igivpt ), &
                    iwork( igivcl ),work( igivnm ), work( iwrem ),iwork( subpbs+1 ), info )
                              
                 end if
                 if( info/=0 )go to 130
                 iwork( i / 2_ilp+1 ) = iwork( i+2 )
              end do loop_90
              subpbs = subpbs / 2_ilp
              curlvl = curlvl + 1_ilp
              go to 80
           end if
           ! end while
           ! re-merge the eigenvalues/vectors which were deflated at the final
           ! merge step.
           if( icompq==1_ilp ) then
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
                 call stdlib_scopy( qsiz, qstore( 1_ilp, j ), 1_ilp, q( 1_ilp, i ), 1_ilp )
              end do
              call stdlib_scopy( n, work, 1_ilp, d, 1_ilp )
           else if( icompq==2_ilp ) then
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
                 call stdlib_scopy( n, q( 1_ilp, j ), 1_ilp, work( n*i+1 ), 1_ilp )
              end do
              call stdlib_scopy( n, work, 1_ilp, d, 1_ilp )
              call stdlib_slacpy( 'A', n, n, work( n+1 ), n, q, ldq )
           else
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
              end do
              call stdlib_scopy( n, work, 1_ilp, d, 1_ilp )
           end if
           go to 140
           130 continue
           info = submat*( n+1 ) + submat + matsiz - 1_ilp
           140 continue
           return
     end subroutine stdlib_slaed0

     pure module subroutine stdlib_dlaed0( icompq, qsiz, n, d, e, q, ldq, qstore, ldqs,work, iwork, info &
     !! DLAED0 computes all eigenvalues and corresponding eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
               )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*), q(ldq,*)
           real(dp), intent(out) :: qstore(ldqs,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: curlvl, curprb, curr, i, igivcl, igivnm, igivpt, indxq, iperm, iprmpt, &
           iq, iqptr, iwrem, j, k, lgn, matsiz, msd2, smlsiz, smm1, spm1, spm2, submat, subpbs, &
                     tlvls
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>2_ilp ) then
              info = -1_ilp
           else if( ( icompq==1_ilp ) .and. ( qsiz<max( 0_ilp, n ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldqs<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED0', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'DLAED0', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           ! determine the size and placement of the submatrices, and save in
           ! the leading elements of iwork.
           iwork( 1_ilp ) = n
           subpbs = 1_ilp
           tlvls = 0_ilp
           10 continue
           if( iwork( subpbs )>smlsiz ) then
              do j = subpbs, 1, -1
                 iwork( 2_ilp*j ) = ( iwork( j )+1_ilp ) / 2_ilp
                 iwork( 2_ilp*j-1 ) = iwork( j ) / 2_ilp
              end do
              tlvls = tlvls + 1_ilp
              subpbs = 2_ilp*subpbs
              go to 10
           end if
           do j = 2, subpbs
              iwork( j ) = iwork( j ) + iwork( j-1 )
           end do
           ! divide the matrix into subpbs submatrices of size at most smlsiz+1
           ! using rank-1 modifications (cuts).
           spm1 = subpbs - 1_ilp
           do i = 1, spm1
              submat = iwork( i ) + 1_ilp
              smm1 = submat - 1_ilp
              d( smm1 ) = d( smm1 ) - abs( e( smm1 ) )
              d( submat ) = d( submat ) - abs( e( smm1 ) )
           end do
           indxq = 4_ilp*n + 3_ilp
           if( icompq/=2_ilp ) then
              ! set up workspaces for eigenvalues only/accumulate new vectors
              ! routine
              temp = log( real( n,KIND=dp) ) / log( two )
              lgn = int( temp,KIND=ilp)
              if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
              if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
              iprmpt = indxq + n + 1_ilp
              iperm = iprmpt + n*lgn
              iqptr = iperm + n*lgn
              igivpt = iqptr + n + 2_ilp
              igivcl = igivpt + n*lgn
              igivnm = 1_ilp
              iq = igivnm + 2_ilp*n*lgn
              iwrem = iq + n**2_ilp + 1_ilp
              ! initialize pointers
              do i = 0, subpbs
                 iwork( iprmpt+i ) = 1_ilp
                 iwork( igivpt+i ) = 1_ilp
              end do
              iwork( iqptr ) = 1_ilp
           end if
           ! solve each submatrix eigenproblem at the bottom of the divide and
           ! conquer tree.
           curr = 0_ilp
           loop_70: do i = 0, spm1
              if( i==0_ilp ) then
                 submat = 1_ilp
                 matsiz = iwork( 1_ilp )
              else
                 submat = iwork( i ) + 1_ilp
                 matsiz = iwork( i+1 ) - iwork( i )
              end if
              if( icompq==2_ilp ) then
                 call stdlib_dsteqr( 'I', matsiz, d( submat ), e( submat ),q( submat, submat ), &
                           ldq, work, info )
                 if( info/=0 )go to 130
              else
                 call stdlib_dsteqr( 'I', matsiz, d( submat ), e( submat ),work( iq-1+iwork( &
                           iqptr+curr ) ), matsiz, work,info )
                 if( info/=0 )go to 130
                 if( icompq==1_ilp ) then
                    call stdlib_dgemm( 'N', 'N', qsiz, matsiz, matsiz, one,q( 1_ilp, submat ), ldq, &
                    work( iq-1+iwork( iqptr+curr ) ), matsiz, zero, qstore( 1_ilp, submat ),ldqs )
                              
                 end if
                 iwork( iqptr+curr+1 ) = iwork( iqptr+curr ) + matsiz**2_ilp
                 curr = curr + 1_ilp
              end if
              k = 1_ilp
              do j = submat, iwork( i+1 )
                 iwork( indxq+j ) = k
                 k = k + 1_ilp
              end do
           end do loop_70
           ! successively merge eigensystems of adjacent submatrices
           ! into eigensystem for the corresponding larger matrix.
           ! while ( subpbs > 1 )
           curlvl = 1_ilp
           80 continue
           if( subpbs>1_ilp ) then
              spm2 = subpbs - 2_ilp
              loop_90: do i = 0, spm2, 2
                 if( i==0_ilp ) then
                    submat = 1_ilp
                    matsiz = iwork( 2_ilp )
                    msd2 = iwork( 1_ilp )
                    curprb = 0_ilp
                 else
                    submat = iwork( i ) + 1_ilp
                    matsiz = iwork( i+2 ) - iwork( i )
                    msd2 = matsiz / 2_ilp
                    curprb = curprb + 1_ilp
                 end if
           ! merge lower order eigensystems (of size msd2 and matsiz - msd2)
           ! into an eigensystem of size matsiz.
           ! stdlib_dlaed1 is used only for the full eigensystem of a tridiagonal
           ! matrix.
           ! stdlib_dlaed7 handles the cases in which eigenvalues only or eigenvalues
           ! and eigenvectors of a full symmetric matrix (which was reduced to
           ! tridiagonal form) are desired.
                 if( icompq==2_ilp ) then
                    call stdlib_dlaed1( matsiz, d( submat ), q( submat, submat ),ldq, iwork( &
                    indxq+submat ),e( submat+msd2-1 ), msd2, work,iwork( subpbs+1 ), info )
                              
                 else
                    call stdlib_dlaed7( icompq, matsiz, qsiz, tlvls, curlvl, curprb,d( submat ), &
                    qstore( 1_ilp, submat ), ldqs,iwork( indxq+submat ), e( submat+msd2-1 ),msd2, &
                    work( iq ), iwork( iqptr ),iwork( iprmpt ), iwork( iperm ),iwork( igivpt ), &
                    iwork( igivcl ),work( igivnm ), work( iwrem ),iwork( subpbs+1 ), info )
                              
                 end if
                 if( info/=0 )go to 130
                 iwork( i / 2_ilp+1 ) = iwork( i+2 )
              end do loop_90
              subpbs = subpbs / 2_ilp
              curlvl = curlvl + 1_ilp
              go to 80
           end if
           ! end while
           ! re-merge the eigenvalues/vectors which were deflated at the final
           ! merge step.
           if( icompq==1_ilp ) then
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
                 call stdlib_dcopy( qsiz, qstore( 1_ilp, j ), 1_ilp, q( 1_ilp, i ), 1_ilp )
              end do
              call stdlib_dcopy( n, work, 1_ilp, d, 1_ilp )
           else if( icompq==2_ilp ) then
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
                 call stdlib_dcopy( n, q( 1_ilp, j ), 1_ilp, work( n*i+1 ), 1_ilp )
              end do
              call stdlib_dcopy( n, work, 1_ilp, d, 1_ilp )
              call stdlib_dlacpy( 'A', n, n, work( n+1 ), n, q, ldq )
           else
              do i = 1, n
                 j = iwork( indxq+i )
                 work( i ) = d( j )
              end do
              call stdlib_dcopy( n, work, 1_ilp, d, 1_ilp )
           end if
           go to 140
           130 continue
           info = submat*( n+1 ) + submat + matsiz - 1_ilp
           140 continue
           return
     end subroutine stdlib_dlaed0


     pure module subroutine stdlib_claed0( qsiz, n, d, e, q, ldq, qstore, ldqs, rwork,iwork, info )
     !! Using the divide and conquer method, CLAED0: computes all eigenvalues
     !! of a symmetric tridiagonal matrix which is one diagonal block of
     !! those from reducing a dense or band Hermitian matrix and
     !! corresponding eigenvectors of the dense or band matrix.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldqs, n, qsiz
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: qstore(ldqs,*)
        ! =====================================================================
        ! warning:      n could be as big as qsiz!
           
           ! Local Scalars 
           integer(ilp) :: curlvl, curprb, curr, i, igivcl, igivnm, igivpt, indxq, iperm, iprmpt, &
           iq, iqptr, iwrem, j, k, lgn, ll, matsiz, msd2, smlsiz, smm1, spm1, spm2, submat, &
                     subpbs, tlvls
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! if( icompq < 0 .or. icompq > 2 ) then
              ! info = -1
           ! else if( ( icompq == 1 ) .and. ( qsiz < max( 0, n ) ) )
          ! $        then
           if( qsiz<max( 0_ilp, n ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldqs<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAED0', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'CLAED0', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           ! determine the size and placement of the submatrices, and save in
           ! the leading elements of iwork.
           iwork( 1_ilp ) = n
           subpbs = 1_ilp
           tlvls = 0_ilp
           10 continue
           if( iwork( subpbs )>smlsiz ) then
              do j = subpbs, 1, -1
                 iwork( 2_ilp*j ) = ( iwork( j )+1_ilp ) / 2_ilp
                 iwork( 2_ilp*j-1 ) = iwork( j ) / 2_ilp
              end do
              tlvls = tlvls + 1_ilp
              subpbs = 2_ilp*subpbs
              go to 10
           end if
           do j = 2, subpbs
              iwork( j ) = iwork( j ) + iwork( j-1 )
           end do
           ! divide the matrix into subpbs submatrices of size at most smlsiz+1
           ! using rank-1 modifications (cuts).
           spm1 = subpbs - 1_ilp
           do i = 1, spm1
              submat = iwork( i ) + 1_ilp
              smm1 = submat - 1_ilp
              d( smm1 ) = d( smm1 ) - abs( e( smm1 ) )
              d( submat ) = d( submat ) - abs( e( smm1 ) )
           end do
           indxq = 4_ilp*n + 3_ilp
           ! set up workspaces for eigenvalues only/accumulate new vectors
           ! routine
           temp = log( real( n,KIND=sp) ) / log( two )
           lgn = int( temp,KIND=ilp)
           if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
           if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
           iprmpt = indxq + n + 1_ilp
           iperm = iprmpt + n*lgn
           iqptr = iperm + n*lgn
           igivpt = iqptr + n + 2_ilp
           igivcl = igivpt + n*lgn
           igivnm = 1_ilp
           iq = igivnm + 2_ilp*n*lgn
           iwrem = iq + n**2_ilp + 1_ilp
           ! initialize pointers
           do i = 0, subpbs
              iwork( iprmpt+i ) = 1_ilp
              iwork( igivpt+i ) = 1_ilp
           end do
           iwork( iqptr ) = 1_ilp
           ! solve each submatrix eigenproblem at the bottom of the divide and
           ! conquer tree.
           curr = 0_ilp
           do i = 0, spm1
              if( i==0_ilp ) then
                 submat = 1_ilp
                 matsiz = iwork( 1_ilp )
              else
                 submat = iwork( i ) + 1_ilp
                 matsiz = iwork( i+1 ) - iwork( i )
              end if
              ll = iq - 1_ilp + iwork( iqptr+curr )
              call stdlib_ssteqr( 'I', matsiz, d( submat ), e( submat ),rwork( ll ), matsiz, &
                        rwork, info )
              call stdlib_clacrm( qsiz, matsiz, q( 1_ilp, submat ), ldq, rwork( ll ),matsiz, qstore( &
                        1_ilp, submat ), ldqs,rwork( iwrem ) )
              iwork( iqptr+curr+1 ) = iwork( iqptr+curr ) + matsiz**2_ilp
              curr = curr + 1_ilp
              if( info>0_ilp ) then
                 info = submat*( n+1 ) + submat + matsiz - 1_ilp
                 return
              end if
              k = 1_ilp
              do j = submat, iwork( i+1 )
                 iwork( indxq+j ) = k
                 k = k + 1_ilp
              end do
           end do
           ! successively merge eigensystems of adjacent submatrices
           ! into eigensystem for the corresponding larger matrix.
           ! while ( subpbs > 1 )
           curlvl = 1_ilp
           80 continue
           if( subpbs>1_ilp ) then
              spm2 = subpbs - 2_ilp
              do i = 0, spm2, 2
                 if( i==0_ilp ) then
                    submat = 1_ilp
                    matsiz = iwork( 2_ilp )
                    msd2 = iwork( 1_ilp )
                    curprb = 0_ilp
                 else
                    submat = iwork( i ) + 1_ilp
                    matsiz = iwork( i+2 ) - iwork( i )
                    msd2 = matsiz / 2_ilp
                    curprb = curprb + 1_ilp
                 end if
           ! merge lower order eigensystems (of size msd2 and matsiz - msd2)
           ! into an eigensystem of size matsiz.  stdlib_claed7 handles the case
           ! when the eigenvectors of a full or band hermitian matrix (which
           ! was reduced to tridiagonal form) are desired.
           ! i am free to use q as a valuable working space until loop 150.
                 call stdlib_claed7( matsiz, msd2, qsiz, tlvls, curlvl, curprb,d( submat ), &
                 qstore( 1_ilp, submat ), ldqs,e( submat+msd2-1 ), iwork( indxq+submat ),rwork( iq ), &
                 iwork( iqptr ), iwork( iprmpt ),iwork( iperm ), iwork( igivpt ),iwork( igivcl ), &
                           rwork( igivnm ),q( 1_ilp, submat ), rwork( iwrem ),iwork( subpbs+1 ), info )
                 if( info>0_ilp ) then
                    info = submat*( n+1 ) + submat + matsiz - 1_ilp
                    return
                 end if
                 iwork( i / 2_ilp+1 ) = iwork( i+2 )
              end do
              subpbs = subpbs / 2_ilp
              curlvl = curlvl + 1_ilp
              go to 80
           end if
           ! end while
           ! re-merge the eigenvalues/vectors which were deflated at the final
           ! merge step.
           do i = 1, n
              j = iwork( indxq+i )
              rwork( i ) = d( j )
              call stdlib_ccopy( qsiz, qstore( 1_ilp, j ), 1_ilp, q( 1_ilp, i ), 1_ilp )
           end do
           call stdlib_scopy( n, rwork, 1_ilp, d, 1_ilp )
           return
     end subroutine stdlib_claed0

     pure module subroutine stdlib_zlaed0( qsiz, n, d, e, q, ldq, qstore, ldqs, rwork,iwork, info )
     !! Using the divide and conquer method, ZLAED0: computes all eigenvalues
     !! of a symmetric tridiagonal matrix which is one diagonal block of
     !! those from reducing a dense or band Hermitian matrix and
     !! corresponding eigenvectors of the dense or band matrix.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldqs, n, qsiz
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: qstore(ldqs,*)
        ! =====================================================================
        ! warning:      n could be as big as qsiz!
           
           ! Local Scalars 
           integer(ilp) :: curlvl, curprb, curr, i, igivcl, igivnm, igivpt, indxq, iperm, iprmpt, &
           iq, iqptr, iwrem, j, k, lgn, ll, matsiz, msd2, smlsiz, smm1, spm1, spm2, submat, &
                     subpbs, tlvls
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! if( icompq < 0 .or. icompq > 2 ) then
              ! info = -1
           ! else if( ( icompq == 1 ) .and. ( qsiz < max( 0, n ) ) )
          ! $        then
           if( qsiz<max( 0_ilp, n ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldqs<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAED0', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           smlsiz = stdlib_ilaenv( 9_ilp, 'ZLAED0', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           ! determine the size and placement of the submatrices, and save in
           ! the leading elements of iwork.
           iwork( 1_ilp ) = n
           subpbs = 1_ilp
           tlvls = 0_ilp
           10 continue
           if( iwork( subpbs )>smlsiz ) then
              do j = subpbs, 1, -1
                 iwork( 2_ilp*j ) = ( iwork( j )+1_ilp ) / 2_ilp
                 iwork( 2_ilp*j-1 ) = iwork( j ) / 2_ilp
              end do
              tlvls = tlvls + 1_ilp
              subpbs = 2_ilp*subpbs
              go to 10
           end if
           do j = 2, subpbs
              iwork( j ) = iwork( j ) + iwork( j-1 )
           end do
           ! divide the matrix into subpbs submatrices of size at most smlsiz+1
           ! using rank-1 modifications (cuts).
           spm1 = subpbs - 1_ilp
           do i = 1, spm1
              submat = iwork( i ) + 1_ilp
              smm1 = submat - 1_ilp
              d( smm1 ) = d( smm1 ) - abs( e( smm1 ) )
              d( submat ) = d( submat ) - abs( e( smm1 ) )
           end do
           indxq = 4_ilp*n + 3_ilp
           ! set up workspaces for eigenvalues only/accumulate new vectors
           ! routine
           temp = log( real( n,KIND=dp) ) / log( two )
           lgn = int( temp,KIND=ilp)
           if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
           if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
           iprmpt = indxq + n + 1_ilp
           iperm = iprmpt + n*lgn
           iqptr = iperm + n*lgn
           igivpt = iqptr + n + 2_ilp
           igivcl = igivpt + n*lgn
           igivnm = 1_ilp
           iq = igivnm + 2_ilp*n*lgn
           iwrem = iq + n**2_ilp + 1_ilp
           ! initialize pointers
           do i = 0, subpbs
              iwork( iprmpt+i ) = 1_ilp
              iwork( igivpt+i ) = 1_ilp
           end do
           iwork( iqptr ) = 1_ilp
           ! solve each submatrix eigenproblem at the bottom of the divide and
           ! conquer tree.
           curr = 0_ilp
           do i = 0, spm1
              if( i==0_ilp ) then
                 submat = 1_ilp
                 matsiz = iwork( 1_ilp )
              else
                 submat = iwork( i ) + 1_ilp
                 matsiz = iwork( i+1 ) - iwork( i )
              end if
              ll = iq - 1_ilp + iwork( iqptr+curr )
              call stdlib_dsteqr( 'I', matsiz, d( submat ), e( submat ),rwork( ll ), matsiz, &
                        rwork, info )
              call stdlib_zlacrm( qsiz, matsiz, q( 1_ilp, submat ), ldq, rwork( ll ),matsiz, qstore( &
                        1_ilp, submat ), ldqs,rwork( iwrem ) )
              iwork( iqptr+curr+1 ) = iwork( iqptr+curr ) + matsiz**2_ilp
              curr = curr + 1_ilp
              if( info>0_ilp ) then
                 info = submat*( n+1 ) + submat + matsiz - 1_ilp
                 return
              end if
              k = 1_ilp
              do j = submat, iwork( i+1 )
                 iwork( indxq+j ) = k
                 k = k + 1_ilp
              end do
           end do
           ! successively merge eigensystems of adjacent submatrices
           ! into eigensystem for the corresponding larger matrix.
           ! while ( subpbs > 1 )
           curlvl = 1_ilp
           80 continue
           if( subpbs>1_ilp ) then
              spm2 = subpbs - 2_ilp
              do i = 0, spm2, 2
                 if( i==0_ilp ) then
                    submat = 1_ilp
                    matsiz = iwork( 2_ilp )
                    msd2 = iwork( 1_ilp )
                    curprb = 0_ilp
                 else
                    submat = iwork( i ) + 1_ilp
                    matsiz = iwork( i+2 ) - iwork( i )
                    msd2 = matsiz / 2_ilp
                    curprb = curprb + 1_ilp
                 end if
           ! merge lower order eigensystems (of size msd2 and matsiz - msd2)
           ! into an eigensystem of size matsiz.  stdlib_zlaed7 handles the case
           ! when the eigenvectors of a full or band hermitian matrix (which
           ! was reduced to tridiagonal form) are desired.
           ! i am free to use q as a valuable working space until loop 150.
                 call stdlib_zlaed7( matsiz, msd2, qsiz, tlvls, curlvl, curprb,d( submat ), &
                 qstore( 1_ilp, submat ), ldqs,e( submat+msd2-1 ), iwork( indxq+submat ),rwork( iq ), &
                 iwork( iqptr ), iwork( iprmpt ),iwork( iperm ), iwork( igivpt ),iwork( igivcl ), &
                           rwork( igivnm ),q( 1_ilp, submat ), rwork( iwrem ),iwork( subpbs+1 ), info )
                 if( info>0_ilp ) then
                    info = submat*( n+1 ) + submat + matsiz - 1_ilp
                    return
                 end if
                 iwork( i / 2_ilp+1 ) = iwork( i+2 )
              end do
              subpbs = subpbs / 2_ilp
              curlvl = curlvl + 1_ilp
              go to 80
           end if
           ! end while
           ! re-merge the eigenvalues/vectors which were deflated at the final
           ! merge step.
           do i = 1, n
              j = iwork( indxq+i )
              rwork( i ) = d( j )
              call stdlib_zcopy( qsiz, qstore( 1_ilp, j ), 1_ilp, q( 1_ilp, i ), 1_ilp )
           end do
           call stdlib_dcopy( n, rwork, 1_ilp, d, 1_ilp )
           return
     end subroutine stdlib_zlaed0




     pure module subroutine stdlib_slaed1( n, d, q, ldq, indxq, rho, cutpnt, work, iwork,info )
     !! SLAED1 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix.  This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and eigenvectors of a tridiagonal matrix.  SLAED7 handles
     !! the case in which eigenvalues only or eigenvalues and eigenvectors
     !! of a full symmetric matrix (which was reduced to tridiagonal form)
     !! are desired.
     !! T = Q(in) ( D(in) + RHO * Z*Z**T ) Q**T(in) = Q(out) * D(out) * Q**T(out)
     !! where Z = Q**T*u, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine SLAED2.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine SLAED4 (as called by SLAED3).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, ldq, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: indxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), q(ldq,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: coltyp, cpp1, i, idlmda, indx, indxc, indxp, iq2, is, iw, iz, k, n1, &
                     n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( min( 1_ilp, n / 2_ilp )>cutpnt .or. ( n / 2_ilp )<cutpnt ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED1', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are integer pointers which indicate
           ! the portion of the workspace
           ! used by a particular array in stdlib_slaed2 and stdlib_slaed3.
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq2 = iw + n
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           call stdlib_scopy( cutpnt, q( cutpnt, 1_ilp ), ldq, work( iz ), 1_ilp )
           cpp1 = cutpnt + 1_ilp
           call stdlib_scopy( n-cutpnt, q( cpp1, cpp1 ), ldq, work( iz+cutpnt ), 1_ilp )
           ! deflate eigenvalues.
           call stdlib_slaed2( k, n, cutpnt, d, q, ldq, indxq, rho, work( iz ),work( idlmda ), &
           work( iw ), work( iq2 ),iwork( indx ), iwork( indxc ), iwork( indxp ),iwork( coltyp ), &
                     info )
           if( info/=0 )go to 20
           ! solve secular equation.
           if( k/=0_ilp ) then
              is = ( iwork( coltyp )+iwork( coltyp+1 ) )*cutpnt +( iwork( coltyp+1 )+iwork( &
                        coltyp+2 ) )*( n-cutpnt ) + iq2
              call stdlib_slaed3( k, n, cutpnt, d, q, ldq, rho, work( idlmda ),work( iq2 ), iwork(&
                         indxc ), iwork( coltyp ),work( iw ), work( is ), info )
              if( info/=0 )go to 20
           ! prepare the indxq sorting permutation.
              n1 = k
              n2 = n - k
              call stdlib_slamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           20 continue
           return
     end subroutine stdlib_slaed1

     pure module subroutine stdlib_dlaed1( n, d, q, ldq, indxq, rho, cutpnt, work, iwork,info )
     !! DLAED1 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix.  This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
     !! the case in which eigenvalues only or eigenvalues and eigenvectors
     !! of a full symmetric matrix (which was reduced to tridiagonal form)
     !! are desired.
     !! T = Q(in) ( D(in) + RHO * Z*Z**T ) Q**T(in) = Q(out) * D(out) * Q**T(out)
     !! where Z = Q**T*u, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine DLAED2.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine DLAED4 (as called by DLAED3).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, ldq, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: indxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), q(ldq,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: coltyp, i, idlmda, indx, indxc, indxp, iq2, is, iw, iz, k, n1, n2, &
                     zpp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( min( 1_ilp, n / 2_ilp )>cutpnt .or. ( n / 2_ilp )<cutpnt ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED1', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are integer pointers which indicate
           ! the portion of the workspace
           ! used by a particular array in stdlib_dlaed2 and stdlib_dlaed3.
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq2 = iw + n
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           call stdlib_dcopy( cutpnt, q( cutpnt, 1_ilp ), ldq, work( iz ), 1_ilp )
           zpp1 = cutpnt + 1_ilp
           call stdlib_dcopy( n-cutpnt, q( zpp1, zpp1 ), ldq, work( iz+cutpnt ), 1_ilp )
           ! deflate eigenvalues.
           call stdlib_dlaed2( k, n, cutpnt, d, q, ldq, indxq, rho, work( iz ),work( idlmda ), &
           work( iw ), work( iq2 ),iwork( indx ), iwork( indxc ), iwork( indxp ),iwork( coltyp ), &
                     info )
           if( info/=0 )go to 20
           ! solve secular equation.
           if( k/=0_ilp ) then
              is = ( iwork( coltyp )+iwork( coltyp+1 ) )*cutpnt +( iwork( coltyp+1 )+iwork( &
                        coltyp+2 ) )*( n-cutpnt ) + iq2
              call stdlib_dlaed3( k, n, cutpnt, d, q, ldq, rho, work( idlmda ),work( iq2 ), iwork(&
                         indxc ), iwork( coltyp ),work( iw ), work( is ), info )
              if( info/=0 )go to 20
           ! prepare the indxq sorting permutation.
              n1 = k
              n2 = n - k
              call stdlib_dlamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           20 continue
           return
     end subroutine stdlib_dlaed1




     pure module subroutine stdlib_slaed2( k, n, n1, d, q, ldq, indxq, rho, z, dlamda, w,q2, indx, indxc,&
     !! SLAED2 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny entry in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
                indxp, coltyp, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldq, n, n1
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: coltyp(*), indx(*), indxc(*), indxp(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(sp), intent(out) :: dlamda(*), q2(*), w(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: mone = -1.0_sp
           
           ! Local Arrays 
           integer(ilp) :: ctot(4_ilp), psm(4_ilp)
           ! Local Scalars 
           integer(ilp) :: ct, i, imax, iq1, iq2, j, jmax, js, k2, n1p1, n2, nj, pj
           real(sp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( min( 1_ilp, ( n / 2_ilp ) )>n1 .or. ( n / 2_ilp )<n1 ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_sscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1.  since z is the concatenation of
           ! two normalized vectors, norm2(z) = sqrt(2).
           t = one / sqrt( two )
           call stdlib_sscal( n, t, z, 1_ilp )
           ! rho = abs( norm(z)**2 * rho )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = n1p1, n
              indxq( i ) = indxq( i ) + n1
           end do
           ! re-integrate the deflated parts from the last pass
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
           end do
           call stdlib_slamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indxc )
           do i = 1, n
              indx( i ) = indxq( indxc( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_isamax( n, z, 1_ilp )
           jmax = stdlib_isamax( n, d, 1_ilp )
           eps = stdlib_slamch( 'EPSILON' )
           tol = eight*eps*max( abs( d( jmax ) ), abs( z( imax ) ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              iq2 = 1_ilp
              do j = 1, n
                 i = indx( j )
                 call stdlib_scopy( n, q( 1_ilp, i ), 1_ilp, q2( iq2 ), 1_ilp )
                 dlamda( j ) = d( i )
                 iq2 = iq2 + n
              end do
              call stdlib_slacpy( 'A', n, n, q2, n, q, ldq )
              call stdlib_scopy( n, dlamda, 1_ilp, d, 1_ilp )
              go to 190
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           do i = 1, n1
              coltyp( i ) = 1_ilp
           end do
           do i = n1p1, n
              coltyp( i ) = 3_ilp
           end do
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              nj = indx( j )
              if( rho*abs( z( nj ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 coltyp( nj ) = 4_ilp
                 indxp( k2 ) = nj
                 if( j==n )go to 100
              else
                 pj = nj
                 go to 80
              end if
           end do
           80 continue
           j = j + 1_ilp
           nj = indx( j )
           if( j>n )go to 100
           if( rho*abs( z( nj ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              coltyp( nj ) = 4_ilp
              indxp( k2 ) = nj
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( pj )
              c = z( nj )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_slapy2( c, s )
              t = d( nj ) - d( pj )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( nj ) = tau
                 z( pj ) = zero
                 if( coltyp( nj )/=coltyp( pj ) )coltyp( nj ) = 2_ilp
                 coltyp( pj ) = 4_ilp
                 call stdlib_srot( n, q( 1_ilp, pj ), 1_ilp, q( 1_ilp, nj ), 1_ilp, c, s )
                 t = d( pj )*c**2_ilp + d( nj )*s**2_ilp
                 d( nj ) = d( pj )*s**2_ilp + d( nj )*c**2_ilp
                 d( pj ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 90 continue
                 if( k2+i<=n ) then
                    if( d( pj )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = pj
                       i = i + 1_ilp
                       go to 90
                    else
                       indxp( k2+i-1 ) = pj
                    end if
                 else
                    indxp( k2+i-1 ) = pj
                 end if
                 pj = nj
              else
                 k = k + 1_ilp
                 dlamda( k ) = d( pj )
                 w( k ) = z( pj )
                 indxp( k ) = pj
                 pj = nj
              end if
           end if
           go to 80
           100 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           dlamda( k ) = d( pj )
           w( k ) = z( pj )
           indxp( k ) = pj
           ! count up the total number of the various types of columns, then
           ! form a permutation which positions the four column types into
           ! four uniform groups (although one or more of these groups may be
           ! empty).
           do j = 1, 4
              ctot( j ) = 0_ilp
           end do
           do j = 1, n
              ct = coltyp( j )
              ctot( ct ) = ctot( ct ) + 1_ilp
           end do
           ! psm(*) = position in submatrix (of types 1 through 4)
           psm( 1_ilp ) = 1_ilp
           psm( 2_ilp ) = 1_ilp + ctot( 1_ilp )
           psm( 3_ilp ) = psm( 2_ilp ) + ctot( 2_ilp )
           psm( 4_ilp ) = psm( 3_ilp ) + ctot( 3_ilp )
           k = n - ctot( 4_ilp )
           ! fill out the indxc array so that the permutation which it induces
           ! will place all type-1 columns first, all type-2 columns next,
           ! then all type-3's, and finally all type-4's.
           do j = 1, n
              js = indxp( j )
              ct = coltyp( js )
              indx( psm( ct ) ) = js
              indxc( psm( ct ) ) = j
              psm( ct ) = psm( ct ) + 1_ilp
           end do
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           i = 1_ilp
           iq1 = 1_ilp
           iq2 = 1_ilp + ( ctot( 1_ilp )+ctot( 2_ilp ) )*n1
           do j = 1, ctot( 1 )
              js = indx( i )
              call stdlib_scopy( n1, q( 1_ilp, js ), 1_ilp, q2( iq1 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq1 = iq1 + n1
           end do
           do j = 1, ctot( 2 )
              js = indx( i )
              call stdlib_scopy( n1, q( 1_ilp, js ), 1_ilp, q2( iq1 ), 1_ilp )
              call stdlib_scopy( n2, q( n1+1, js ), 1_ilp, q2( iq2 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq1 = iq1 + n1
              iq2 = iq2 + n2
           end do
           do j = 1, ctot( 3 )
              js = indx( i )
              call stdlib_scopy( n2, q( n1+1, js ), 1_ilp, q2( iq2 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq2 = iq2 + n2
           end do
           iq1 = iq2
           do j = 1, ctot( 4 )
              js = indx( i )
              call stdlib_scopy( n, q( 1_ilp, js ), 1_ilp, q2( iq2 ), 1_ilp )
              iq2 = iq2 + n
              z( i ) = d( js )
              i = i + 1_ilp
           end do
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              call stdlib_slacpy( 'A', n, ctot( 4_ilp ), q2( iq1 ), n,q( 1_ilp, k+1 ), ldq )
              call stdlib_scopy( n-k, z( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
           end if
           ! copy ctot into coltyp for referencing in stdlib_slaed3.
           do j = 1, 4
              coltyp( j ) = ctot( j )
           end do
           190 continue
           return
     end subroutine stdlib_slaed2

     pure module subroutine stdlib_dlaed2( k, n, n1, d, q, ldq, indxq, rho, z, dlamda, w,q2, indx, indxc,&
     !! DLAED2 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny entry in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
                indxp, coltyp, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldq, n, n1
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: coltyp(*), indx(*), indxc(*), indxp(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(dp), intent(out) :: dlamda(*), q2(*), w(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: mone = -1.0_dp
           
           ! Local Arrays 
           integer(ilp) :: ctot(4_ilp), psm(4_ilp)
           ! Local Scalars 
           integer(ilp) :: ct, i, imax, iq1, iq2, j, jmax, js, k2, n1p1, n2, nj, pj
           real(dp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( min( 1_ilp, ( n / 2_ilp ) )>n1 .or. ( n / 2_ilp )<n1 ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_dscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1.  since z is the concatenation of
           ! two normalized vectors, norm2(z) = sqrt(2).
           t = one / sqrt( two )
           call stdlib_dscal( n, t, z, 1_ilp )
           ! rho = abs( norm(z)**2 * rho )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = n1p1, n
              indxq( i ) = indxq( i ) + n1
           end do
           ! re-integrate the deflated parts from the last pass
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
           end do
           call stdlib_dlamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indxc )
           do i = 1, n
              indx( i ) = indxq( indxc( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_idamax( n, z, 1_ilp )
           jmax = stdlib_idamax( n, d, 1_ilp )
           eps = stdlib_dlamch( 'EPSILON' )
           tol = eight*eps*max( abs( d( jmax ) ), abs( z( imax ) ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              iq2 = 1_ilp
              do j = 1, n
                 i = indx( j )
                 call stdlib_dcopy( n, q( 1_ilp, i ), 1_ilp, q2( iq2 ), 1_ilp )
                 dlamda( j ) = d( i )
                 iq2 = iq2 + n
              end do
              call stdlib_dlacpy( 'A', n, n, q2, n, q, ldq )
              call stdlib_dcopy( n, dlamda, 1_ilp, d, 1_ilp )
              go to 190
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           do i = 1, n1
              coltyp( i ) = 1_ilp
           end do
           do i = n1p1, n
              coltyp( i ) = 3_ilp
           end do
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              nj = indx( j )
              if( rho*abs( z( nj ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 coltyp( nj ) = 4_ilp
                 indxp( k2 ) = nj
                 if( j==n )go to 100
              else
                 pj = nj
                 go to 80
              end if
           end do
           80 continue
           j = j + 1_ilp
           nj = indx( j )
           if( j>n )go to 100
           if( rho*abs( z( nj ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              coltyp( nj ) = 4_ilp
              indxp( k2 ) = nj
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( pj )
              c = z( nj )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_dlapy2( c, s )
              t = d( nj ) - d( pj )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( nj ) = tau
                 z( pj ) = zero
                 if( coltyp( nj )/=coltyp( pj ) )coltyp( nj ) = 2_ilp
                 coltyp( pj ) = 4_ilp
                 call stdlib_drot( n, q( 1_ilp, pj ), 1_ilp, q( 1_ilp, nj ), 1_ilp, c, s )
                 t = d( pj )*c**2_ilp + d( nj )*s**2_ilp
                 d( nj ) = d( pj )*s**2_ilp + d( nj )*c**2_ilp
                 d( pj ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 90 continue
                 if( k2+i<=n ) then
                    if( d( pj )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = pj
                       i = i + 1_ilp
                       go to 90
                    else
                       indxp( k2+i-1 ) = pj
                    end if
                 else
                    indxp( k2+i-1 ) = pj
                 end if
                 pj = nj
              else
                 k = k + 1_ilp
                 dlamda( k ) = d( pj )
                 w( k ) = z( pj )
                 indxp( k ) = pj
                 pj = nj
              end if
           end if
           go to 80
           100 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           dlamda( k ) = d( pj )
           w( k ) = z( pj )
           indxp( k ) = pj
           ! count up the total number of the various types of columns, then
           ! form a permutation which positions the four column types into
           ! four uniform groups (although one or more of these groups may be
           ! empty).
           do j = 1, 4
              ctot( j ) = 0_ilp
           end do
           do j = 1, n
              ct = coltyp( j )
              ctot( ct ) = ctot( ct ) + 1_ilp
           end do
           ! psm(*) = position in submatrix (of types 1 through 4)
           psm( 1_ilp ) = 1_ilp
           psm( 2_ilp ) = 1_ilp + ctot( 1_ilp )
           psm( 3_ilp ) = psm( 2_ilp ) + ctot( 2_ilp )
           psm( 4_ilp ) = psm( 3_ilp ) + ctot( 3_ilp )
           k = n - ctot( 4_ilp )
           ! fill out the indxc array so that the permutation which it induces
           ! will place all type-1 columns first, all type-2 columns next,
           ! then all type-3's, and finally all type-4's.
           do j = 1, n
              js = indxp( j )
              ct = coltyp( js )
              indx( psm( ct ) ) = js
              indxc( psm( ct ) ) = j
              psm( ct ) = psm( ct ) + 1_ilp
           end do
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           i = 1_ilp
           iq1 = 1_ilp
           iq2 = 1_ilp + ( ctot( 1_ilp )+ctot( 2_ilp ) )*n1
           do j = 1, ctot( 1 )
              js = indx( i )
              call stdlib_dcopy( n1, q( 1_ilp, js ), 1_ilp, q2( iq1 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq1 = iq1 + n1
           end do
           do j = 1, ctot( 2 )
              js = indx( i )
              call stdlib_dcopy( n1, q( 1_ilp, js ), 1_ilp, q2( iq1 ), 1_ilp )
              call stdlib_dcopy( n2, q( n1+1, js ), 1_ilp, q2( iq2 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq1 = iq1 + n1
              iq2 = iq2 + n2
           end do
           do j = 1, ctot( 3 )
              js = indx( i )
              call stdlib_dcopy( n2, q( n1+1, js ), 1_ilp, q2( iq2 ), 1_ilp )
              z( i ) = d( js )
              i = i + 1_ilp
              iq2 = iq2 + n2
           end do
           iq1 = iq2
           do j = 1, ctot( 4 )
              js = indx( i )
              call stdlib_dcopy( n, q( 1_ilp, js ), 1_ilp, q2( iq2 ), 1_ilp )
              iq2 = iq2 + n
              z( i ) = d( js )
              i = i + 1_ilp
           end do
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              call stdlib_dlacpy( 'A', n, ctot( 4_ilp ), q2( iq1 ), n,q( 1_ilp, k+1 ), ldq )
              call stdlib_dcopy( n-k, z( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
           end if
           ! copy ctot into coltyp for referencing in stdlib_dlaed3.
           do j = 1, 4
              coltyp( j ) = ctot( j )
           end do
           190 continue
           return
     end subroutine stdlib_dlaed2




     pure module subroutine stdlib_slaed3( k, n, n1, d, q, ldq, rho, dlamda, q2, indx,ctot, w, s, info )
     !! SLAED3 finds the roots of the secular equation, as defined by the
     !! values in D, W, and RHO, between 1 and K.  It makes the
     !! appropriate calls to SLAED4 and then updates the eigenvectors by
     !! multiplying the matrix of eigenvectors of the pair of eigensystems
     !! being combined by the matrix of eigenvectors of the K-by-K system
     !! which is solved here.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, n, n1
           real(sp), intent(in) :: rho
           ! Array Arguments 
           integer(ilp), intent(in) :: ctot(*), indx(*)
           real(sp), intent(out) :: d(*), q(ldq,*), s(*)
           real(sp), intent(inout) :: dlamda(*), w(*)
           real(sp), intent(in) :: q2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, iq2, j, n12, n2, n23
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( k<0_ilp ) then
              info = -1_ilp
           else if( n<k ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED3', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! modify values dlamda(i) to make sure all dlamda(i)-dlamda(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dlamda(i) by 2*dlamda(i)-dlamda(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dlamda(i) if it is 1; this makes the subsequent
           ! subtractions dlamda(i)-dlamda(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dlamda(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dlamda(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dlamda( i ) = stdlib_slamc3( dlamda( i ), dlamda( i ) ) - dlamda( i )
           end do
           do j = 1, k
              call stdlib_slaed4( k, j, dlamda, w, q( 1_ilp, j ), rho, d( j ), info )
              ! if the zero finder fails, the computation is terminated.
              if( info/=0 )go to 120
           end do
           if( k==1 )go to 110
           if( k==2_ilp ) then
              do j = 1, k
                 w( 1_ilp ) = q( 1_ilp, j )
                 w( 2_ilp ) = q( 2_ilp, j )
                 ii = indx( 1_ilp )
                 q( 1_ilp, j ) = w( ii )
                 ii = indx( 2_ilp )
                 q( 2_ilp, j ) = w( ii )
              end do
              go to 110
           end if
           ! compute updated w.
           call stdlib_scopy( k, w, 1_ilp, s, 1_ilp )
           ! initialize w(i) = q(i,i)
           call stdlib_scopy( k, q, ldq+1, w, 1_ilp )
           do j = 1, k
              do i = 1, j - 1
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
              do i = j + 1, k
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
           end do
           do i = 1, k
              w( i ) = sign( sqrt( -w( i ) ), s( i ) )
           end do
           ! compute eigenvectors of the modified rank-1 modification.
           do j = 1, k
              do i = 1, k
                 s( i ) = w( i ) / q( i, j )
              end do
              temp = stdlib_snrm2( k, s, 1_ilp )
              do i = 1, k
                 ii = indx( i )
                 q( i, j ) = s( ii ) / temp
              end do
           end do
           ! compute the updated eigenvectors.
           110 continue
           n2 = n - n1
           n12 = ctot( 1_ilp ) + ctot( 2_ilp )
           n23 = ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_slacpy( 'A', n23, k, q( ctot( 1_ilp )+1_ilp, 1_ilp ), ldq, s, n23 )
           iq2 = n1*n12 + 1_ilp
           if( n23/=0_ilp ) then
              call stdlib_sgemm( 'N', 'N', n2, k, n23, one, q2( iq2 ), n2, s, n23,zero, q( n1+1, &
                        1_ilp ), ldq )
           else
              call stdlib_slaset( 'A', n2, k, zero, zero, q( n1+1, 1_ilp ), ldq )
           end if
           call stdlib_slacpy( 'A', n12, k, q, ldq, s, n12 )
           if( n12/=0_ilp ) then
              call stdlib_sgemm( 'N', 'N', n1, k, n12, one, q2, n1, s, n12, zero, q,ldq )
           else
              call stdlib_slaset( 'A', n1, k, zero, zero, q( 1_ilp, 1_ilp ), ldq )
           end if
           120 continue
           return
     end subroutine stdlib_slaed3

     pure module subroutine stdlib_dlaed3( k, n, n1, d, q, ldq, rho, dlamda, q2, indx,ctot, w, s, info )
     !! DLAED3 finds the roots of the secular equation, as defined by the
     !! values in D, W, and RHO, between 1 and K.  It makes the
     !! appropriate calls to DLAED4 and then updates the eigenvectors by
     !! multiplying the matrix of eigenvectors of the pair of eigensystems
     !! being combined by the matrix of eigenvectors of the K-by-K system
     !! which is solved here.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, n, n1
           real(dp), intent(in) :: rho
           ! Array Arguments 
           integer(ilp), intent(in) :: ctot(*), indx(*)
           real(dp), intent(out) :: d(*), q(ldq,*), s(*)
           real(dp), intent(inout) :: dlamda(*), w(*)
           real(dp), intent(in) :: q2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, iq2, j, n12, n2, n23
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( k<0_ilp ) then
              info = -1_ilp
           else if( n<k ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED3', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! modify values dlamda(i) to make sure all dlamda(i)-dlamda(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dlamda(i) by 2*dlamda(i)-dlamda(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dlamda(i) if it is 1; this makes the subsequent
           ! subtractions dlamda(i)-dlamda(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dlamda(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dlamda(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dlamda( i ) = stdlib_dlamc3( dlamda( i ), dlamda( i ) ) - dlamda( i )
           end do
           do j = 1, k
              call stdlib_dlaed4( k, j, dlamda, w, q( 1_ilp, j ), rho, d( j ), info )
              ! if the zero finder fails, the computation is terminated.
              if( info/=0 )go to 120
           end do
           if( k==1 )go to 110
           if( k==2_ilp ) then
              do j = 1, k
                 w( 1_ilp ) = q( 1_ilp, j )
                 w( 2_ilp ) = q( 2_ilp, j )
                 ii = indx( 1_ilp )
                 q( 1_ilp, j ) = w( ii )
                 ii = indx( 2_ilp )
                 q( 2_ilp, j ) = w( ii )
              end do
              go to 110
           end if
           ! compute updated w.
           call stdlib_dcopy( k, w, 1_ilp, s, 1_ilp )
           ! initialize w(i) = q(i,i)
           call stdlib_dcopy( k, q, ldq+1, w, 1_ilp )
           do j = 1, k
              do i = 1, j - 1
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
              do i = j + 1, k
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
           end do
           do i = 1, k
              w( i ) = sign( sqrt( -w( i ) ), s( i ) )
           end do
           ! compute eigenvectors of the modified rank-1 modification.
           do j = 1, k
              do i = 1, k
                 s( i ) = w( i ) / q( i, j )
              end do
              temp = stdlib_dnrm2( k, s, 1_ilp )
              do i = 1, k
                 ii = indx( i )
                 q( i, j ) = s( ii ) / temp
              end do
           end do
           ! compute the updated eigenvectors.
           110 continue
           n2 = n - n1
           n12 = ctot( 1_ilp ) + ctot( 2_ilp )
           n23 = ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_dlacpy( 'A', n23, k, q( ctot( 1_ilp )+1_ilp, 1_ilp ), ldq, s, n23 )
           iq2 = n1*n12 + 1_ilp
           if( n23/=0_ilp ) then
              call stdlib_dgemm( 'N', 'N', n2, k, n23, one, q2( iq2 ), n2, s, n23,zero, q( n1+1, &
                        1_ilp ), ldq )
           else
              call stdlib_dlaset( 'A', n2, k, zero, zero, q( n1+1, 1_ilp ), ldq )
           end if
           call stdlib_dlacpy( 'A', n12, k, q, ldq, s, n12 )
           if( n12/=0_ilp ) then
              call stdlib_dgemm( 'N', 'N', n1, k, n12, one, q2, n1, s, n12, zero, q,ldq )
           else
              call stdlib_dlaset( 'A', n1, k, zero, zero, q( 1_ilp, 1_ilp ), ldq )
           end if
           120 continue
           return
     end subroutine stdlib_dlaed3




     pure module subroutine stdlib_slaed4( n, i, d, z, delta, rho, dlam, info )
     !! This subroutine computes the I-th updated eigenvalue of a symmetric
     !! rank-one modification to a diagonal matrix whose elements are
     !! given in the array d, and that
     !! D(i) < D(j)  for  i < j
     !! and that RHO > 0.  This is arranged by the calling routine, and is
     !! no loss in generality.  The rank-one modified system is thus
     !! diag( D )  +  RHO * Z * Z_transpose.
     !! where we assume the Euclidean norm of Z is 1.
     !! The method consists of approximating the rational functions in the
     !! secular equation by simpler interpolating rational functions.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dlam
           real(sp), intent(in) :: rho
           ! Array Arguments 
           real(sp), intent(in) :: d(*), z(*)
           real(sp), intent(out) :: delta(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           logical(lk) :: orgati, swtch, swtch3
           integer(ilp) :: ii, iim1, iip1, ip1, iter, j, niter
           real(sp) :: a, b, c, del, dltlb, dltub, dphi, dpsi, dw, eps, erretm, eta, midpt, phi, &
                     prew, psi, rhoinv, tau, temp, temp1, w
           ! Local Arrays 
           real(sp) :: zz(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! since this routine is called in an inner loop, we do no argument
           ! checking.
           ! quick return for n=1 and 2.
           info = 0_ilp
           if( n==1_ilp ) then
               ! presumably, i=1 upon entry
              dlam = d( 1_ilp ) + rho*z( 1_ilp )*z( 1_ilp )
              delta( 1_ilp ) = one
              return
           end if
           if( n==2_ilp ) then
              call stdlib_slaed5( i, d, z, delta, rho, dlam )
              return
           end if
           ! compute machine epsilon
           eps = stdlib_slamch( 'EPSILON' )
           rhoinv = one / rho
           ! the case i = n
           if( i==n ) then
              ! initialize some basic variables
              ii = n - 1_ilp
              niter = 1_ilp
              ! calculate initial guess
              midpt = rho / two
              ! if ||z||_2 is not one, then temp should be set to
              ! rho * ||z||_2^2 / two
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - midpt
              end do
              psi = zero
              do j = 1, n - 2
                 psi = psi + z( j )*z( j ) / delta( j )
              end do
              c = rhoinv + psi
              w = c + z( ii )*z( ii ) / delta( ii ) +z( n )*z( n ) / delta( n )
              if( w<=zero ) then
                 temp = z( n-1 )*z( n-1 ) / ( d( n )-d( n-1 )+rho ) +z( n )*z( n ) / rho
                 if( c<=temp ) then
                    tau = rho
                 else
                    del = d( n ) - d( n-1 )
                    a = -c*del + z( n-1 )*z( n-1 ) + z( n )*z( n )
                    b = z( n )*z( n )*del
                    if( a<zero ) then
                       tau = two*b / ( sqrt( a*a+four*b*c )-a )
                    else
                       tau = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                    end if
                 end if
                 ! it can be proved that
                     ! d(n)+rho/2 <= lambda(n) < d(n)+tau <= d(n)+rho
                 dltlb = midpt
                 dltub = rho
              else
                 del = d( n ) - d( n-1 )
                 a = -c*del + z( n-1 )*z( n-1 ) + z( n )*z( n )
                 b = z( n )*z( n )*del
                 if( a<zero ) then
                    tau = two*b / ( sqrt( a*a+four*b*c )-a )
                 else
                    tau = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                 end if
                 ! it can be proved that
                     ! d(n) < d(n)+tau < lambda(n) < d(n)+rho/2
                 dltlb = zero
                 dltub = midpt
              end if
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - tau
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / delta( n )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                        
              w = rhoinv + phi + psi
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 dlam = d( i ) + tau
                 go to 250
              end if
              if( w<=zero ) then
                 dltlb = max( dltlb, tau )
              else
                 dltub = min( dltub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              c = w - delta( n-1 )*dpsi - delta( n )*dphi
              a = ( delta( n-1 )+delta( n ) )*w -delta( n-1 )*delta( n )*( dpsi+dphi )
              b = delta( n-1 )*delta( n )*w
              if( c<zero )c = abs( c )
              if( c==zero ) then
                ! eta = b/a
                 ! eta = rho - tau
                 eta = dltub - tau
              else if( a>=zero ) then
                 eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>zero )eta = -w / ( dpsi+dphi )
              temp = tau + eta
              if( temp>dltub .or. temp<dltlb ) then
                 if( w<zero ) then
                    eta = ( dltub-tau ) / two
                 else
                    eta = ( dltlb-tau ) / two
                 end if
              end if
              do j = 1, n
                 delta( j ) = delta( j ) - eta
              end do
              tau = tau + eta
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / delta( n )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                        
              w = rhoinv + phi + psi
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_90: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    dlam = d( i ) + tau
                    go to 250
                 end if
                 if( w<=zero ) then
                    dltlb = max( dltlb, tau )
                 else
                    dltub = min( dltub, tau )
                 end if
                 ! calculate the new step
                 c = w - delta( n-1 )*dpsi - delta( n )*dphi
                 a = ( delta( n-1 )+delta( n ) )*w -delta( n-1 )*delta( n )*( dpsi+dphi )
                 b = delta( n-1 )*delta( n )*w
                 if( a>=zero ) then
                    eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>zero )eta = -w / ( dpsi+dphi )
                 temp = tau + eta
                 if( temp>dltub .or. temp<dltlb ) then
                    if( w<zero ) then
                       eta = ( dltub-tau ) / two
                    else
                       eta = ( dltlb-tau ) / two
                    end if
                 end if
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                 end do
                 tau = tau + eta
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, ii
                    temp = z( j ) / delta( j )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 temp = z( n ) / delta( n )
                 phi = z( n )*temp
                 dphi = temp*temp
                 erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                           
                 w = rhoinv + phi + psi
              end do loop_90
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              dlam = d( i ) + tau
              go to 250
              ! end for the case i = n
           else
              ! the case for i < n
              niter = 1_ilp
              ip1 = i + 1_ilp
              ! calculate initial guess
              del = d( ip1 ) - d( i )
              midpt = del / two
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - midpt
              end do
              psi = zero
              do j = 1, i - 1
                 psi = psi + z( j )*z( j ) / delta( j )
              end do
              phi = zero
              do j = n, i + 2, -1
                 phi = phi + z( j )*z( j ) / delta( j )
              end do
              c = rhoinv + psi + phi
              w = c + z( i )*z( i ) / delta( i ) +z( ip1 )*z( ip1 ) / delta( ip1 )
              if( w>zero ) then
                 ! d(i)< the ith eigenvalue < (d(i)+d(i+1))/2
                 ! we choose d(i) as origin.
                 orgati = .true.
                 a = c*del + z( i )*z( i ) + z( ip1 )*z( ip1 )
                 b = z( i )*z( i )*del
                 if( a>zero ) then
                    tau = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 else
                    tau = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 end if
                 dltlb = zero
                 dltub = midpt
              else
                 ! (d(i)+d(i+1))/2 <= the ith eigenvalue < d(i+1)
                 ! we choose d(i+1) as origin.
                 orgati = .false.
                 a = c*del - z( i )*z( i ) - z( ip1 )*z( ip1 )
                 b = z( ip1 )*z( ip1 )*del
                 if( a<zero ) then
                    tau = two*b / ( a-sqrt( abs( a*a+four*b*c ) ) )
                 else
                    tau = -( a+sqrt( abs( a*a+four*b*c ) ) ) / ( two*c )
                 end if
                 dltlb = -midpt
                 dltub = zero
              end if
              if( orgati ) then
                 do j = 1, n
                    delta( j ) = ( d( j )-d( i ) ) - tau
                 end do
              else
                 do j = 1, n
                    delta( j ) = ( d( j )-d( ip1 ) ) - tau
                 end do
              end if
              if( orgati ) then
                 ii = i
              else
                 ii = i + 1_ilp
              end if
              iim1 = ii - 1_ilp
              iip1 = ii + 1_ilp
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / delta( j )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              w = rhoinv + phi + psi
              ! w is the value of the secular function with
              ! its ii-th element removed.
              swtch3 = .false.
              if( orgati ) then
                 if( w<zero )swtch3 = .true.
              else
                 if( w>zero )swtch3 = .true.
              end if
              if( ii==1_ilp .or. ii==n )swtch3 = .false.
              temp = z( ii ) / delta( ii )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = w + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau )&
                        *dw
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 if( orgati ) then
                    dlam = d( i ) + tau
                 else
                    dlam = d( ip1 ) + tau
                 end if
                 go to 250
              end if
              if( w<=zero ) then
                 dltlb = max( dltlb, tau )
              else
                 dltub = min( dltub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              if( .not.swtch3 ) then
                 if( orgati ) then
                    c = w - delta( ip1 )*dw - ( d( i )-d( ip1 ) )*( z( i ) / delta( i ) )&
                              **2_ilp
                 else
                    c = w - delta( i )*dw - ( d( ip1 )-d( i ) )*( z( ip1 ) / delta( ip1 ) )&
                              **2_ilp
                 end if
                 a = ( delta( i )+delta( ip1 ) )*w -delta( i )*delta( ip1 )*dw
                 b = delta( i )*delta( ip1 )*w
                 if( c==zero ) then
                    if( a==zero ) then
                       if( orgati ) then
                          a = z( i )*z( i ) + delta( ip1 )*delta( ip1 )*( dpsi+dphi )
                       else
                          a = z( ip1 )*z( ip1 ) + delta( i )*delta( i )*( dpsi+dphi )
                       end if
                    end if
                    eta = b / a
                 else if( a<=zero ) then
                    eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 end if
              else
                 ! interpolation using three most relevant poles
                 temp = rhoinv + psi + phi
                 if( orgati ) then
                    temp1 = z( iim1 ) / delta( iim1 )
                    temp1 = temp1*temp1
                    c = temp - delta( iip1 )*( dpsi+dphi ) -( d( iim1 )-d( iip1 ) )*temp1
                    zz( 1_ilp ) = z( iim1 )*z( iim1 )
                    zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*( ( dpsi-temp1 )+dphi )
                 else
                    temp1 = z( iip1 ) / delta( iip1 )
                    temp1 = temp1*temp1
                    c = temp - delta( iim1 )*( dpsi+dphi ) -( d( iip1 )-d( iim1 ) )*temp1
                    zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*( dpsi+( dphi-temp1 ) )
                    zz( 3_ilp ) = z( iip1 )*z( iip1 )
                 end if
                 zz( 2_ilp ) = z( ii )*z( ii )
                 call stdlib_slaed6( niter, orgati, c, delta( iim1 ), zz, w, eta,info )
                 if( info/=0 )go to 250
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>=zero )eta = -w / dw
              temp = tau + eta
              if( temp>dltub .or. temp<dltlb ) then
                 if( w<zero ) then
                    eta = ( dltub-tau ) / two
                 else
                    eta = ( dltlb-tau ) / two
                 end if
              end if
              prew = w
              do j = 1, n
                 delta( j ) = delta( j ) - eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / delta( j )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              temp = z( ii ) / delta( ii )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = rhoinv + phi + psi + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau+eta )&
                        *dw
              swtch = .false.
              if( orgati ) then
                 if( -w>abs( prew ) / ten )swtch = .true.
              else
                 if( w>abs( prew ) / ten )swtch = .true.
              end if
              tau = tau + eta
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_240: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    if( orgati ) then
                       dlam = d( i ) + tau
                    else
                       dlam = d( ip1 ) + tau
                    end if
                    go to 250
                 end if
                 if( w<=zero ) then
                    dltlb = max( dltlb, tau )
                 else
                    dltub = min( dltub, tau )
                 end if
                 ! calculate the new step
                 if( .not.swtch3 ) then
                    if( .not.swtch ) then
                       if( orgati ) then
                          c = w - delta( ip1 )*dw -( d( i )-d( ip1 ) )*( z( i ) / delta( i ) )&
                                    **2_ilp
                       else
                          c = w - delta( i )*dw - ( d( ip1 )-d( i ) )*( z( ip1 ) / delta( ip1 ) )&
                                    **2_ilp
                       end if
                    else
                       temp = z( ii ) / delta( ii )
                       if( orgati ) then
                          dpsi = dpsi + temp*temp
                       else
                          dphi = dphi + temp*temp
                       end if
                       c = w - delta( i )*dpsi - delta( ip1 )*dphi
                    end if
                    a = ( delta( i )+delta( ip1 ) )*w -delta( i )*delta( ip1 )*dw
                    b = delta( i )*delta( ip1 )*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( .not.swtch ) then
                             if( orgati ) then
                                a = z( i )*z( i ) + delta( ip1 )*delta( ip1 )*( dpsi+dphi )
                                          
                             else
                                a = z( ip1 )*z( ip1 ) +delta( i )*delta( i )*( dpsi+dphi )
                             end if
                          else
                             a = delta( i )*delta( i )*dpsi +delta( ip1 )*delta( ip1 )&
                                       *dphi
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 else
                    ! interpolation using three most relevant poles
                    temp = rhoinv + psi + phi
                    if( swtch ) then
                       c = temp - delta( iim1 )*dpsi - delta( iip1 )*dphi
                       zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*dpsi
                       zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*dphi
                    else
                       if( orgati ) then
                          temp1 = z( iim1 ) / delta( iim1 )
                          temp1 = temp1*temp1
                          c = temp - delta( iip1 )*( dpsi+dphi ) -( d( iim1 )-d( iip1 ) )&
                                    *temp1
                          zz( 1_ilp ) = z( iim1 )*z( iim1 )
                          zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*( ( dpsi-temp1 )+dphi )
                       else
                          temp1 = z( iip1 ) / delta( iip1 )
                          temp1 = temp1*temp1
                          c = temp - delta( iim1 )*( dpsi+dphi ) -( d( iip1 )-d( iim1 ) )&
                                    *temp1
                          zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*( dpsi+( dphi-temp1 ) )
                          zz( 3_ilp ) = z( iip1 )*z( iip1 )
                       end if
                    end if
                    call stdlib_slaed6( niter, orgati, c, delta( iim1 ), zz, w, eta,info )
                    if( info/=0 )go to 250
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>=zero )eta = -w / dw
                 temp = tau + eta
                 if( temp>dltub .or. temp<dltlb ) then
                    if( w<zero ) then
                       eta = ( dltub-tau ) / two
                    else
                       eta = ( dltlb-tau ) / two
                    end if
                 end if
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                 end do
                 tau = tau + eta
                 prew = w
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, iim1
                    temp = z( j ) / delta( j )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 dphi = zero
                 phi = zero
                 do j = n, iip1, -1
                    temp = z( j ) / delta( j )
                    phi = phi + z( j )*temp
                    dphi = dphi + temp*temp
                    erretm = erretm + phi
                 end do
                 temp = z( ii ) / delta( ii )
                 dw = dpsi + dphi + temp*temp
                 temp = z( ii )*temp
                 w = rhoinv + phi + psi + temp
                 erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau )&
                           *dw
                 if( w*prew>zero .and. abs( w )>abs( prew ) / ten )swtch = .not.swtch
              end do loop_240
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              if( orgati ) then
                 dlam = d( i ) + tau
              else
                 dlam = d( ip1 ) + tau
              end if
           end if
           250 continue
           return
     end subroutine stdlib_slaed4

     pure module subroutine stdlib_dlaed4( n, i, d, z, delta, rho, dlam, info )
     !! This subroutine computes the I-th updated eigenvalue of a symmetric
     !! rank-one modification to a diagonal matrix whose elements are
     !! given in the array d, and that
     !! D(i) < D(j)  for  i < j
     !! and that RHO > 0.  This is arranged by the calling routine, and is
     !! no loss in generality.  The rank-one modified system is thus
     !! diag( D )  +  RHO * Z * Z_transpose.
     !! where we assume the Euclidean norm of Z is 1.
     !! The method consists of approximating the rational functions in the
     !! secular equation by simpler interpolating rational functions.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dlam
           real(dp), intent(in) :: rho
           ! Array Arguments 
           real(dp), intent(in) :: d(*), z(*)
           real(dp), intent(out) :: delta(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           logical(lk) :: orgati, swtch, swtch3
           integer(ilp) :: ii, iim1, iip1, ip1, iter, j, niter
           real(dp) :: a, b, c, del, dltlb, dltub, dphi, dpsi, dw, eps, erretm, eta, midpt, phi, &
                     prew, psi, rhoinv, tau, temp, temp1, w
           ! Local Arrays 
           real(dp) :: zz(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! since this routine is called in an inner loop, we do no argument
           ! checking.
           ! quick return for n=1 and 2.
           info = 0_ilp
           if( n==1_ilp ) then
               ! presumably, i=1 upon entry
              dlam = d( 1_ilp ) + rho*z( 1_ilp )*z( 1_ilp )
              delta( 1_ilp ) = one
              return
           end if
           if( n==2_ilp ) then
              call stdlib_dlaed5( i, d, z, delta, rho, dlam )
              return
           end if
           ! compute machine epsilon
           eps = stdlib_dlamch( 'EPSILON' )
           rhoinv = one / rho
           ! the case i = n
           if( i==n ) then
              ! initialize some basic variables
              ii = n - 1_ilp
              niter = 1_ilp
              ! calculate initial guess
              midpt = rho / two
              ! if ||z||_2 is not one, then temp should be set to
              ! rho * ||z||_2^2 / two
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - midpt
              end do
              psi = zero
              do j = 1, n - 2
                 psi = psi + z( j )*z( j ) / delta( j )
              end do
              c = rhoinv + psi
              w = c + z( ii )*z( ii ) / delta( ii ) +z( n )*z( n ) / delta( n )
              if( w<=zero ) then
                 temp = z( n-1 )*z( n-1 ) / ( d( n )-d( n-1 )+rho ) +z( n )*z( n ) / rho
                 if( c<=temp ) then
                    tau = rho
                 else
                    del = d( n ) - d( n-1 )
                    a = -c*del + z( n-1 )*z( n-1 ) + z( n )*z( n )
                    b = z( n )*z( n )*del
                    if( a<zero ) then
                       tau = two*b / ( sqrt( a*a+four*b*c )-a )
                    else
                       tau = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                    end if
                 end if
                 ! it can be proved that
                     ! d(n)+rho/2 <= lambda(n) < d(n)+tau <= d(n)+rho
                 dltlb = midpt
                 dltub = rho
              else
                 del = d( n ) - d( n-1 )
                 a = -c*del + z( n-1 )*z( n-1 ) + z( n )*z( n )
                 b = z( n )*z( n )*del
                 if( a<zero ) then
                    tau = two*b / ( sqrt( a*a+four*b*c )-a )
                 else
                    tau = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                 end if
                 ! it can be proved that
                     ! d(n) < d(n)+tau < lambda(n) < d(n)+rho/2
                 dltlb = zero
                 dltub = midpt
              end if
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - tau
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / delta( n )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                        
              w = rhoinv + phi + psi
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 dlam = d( i ) + tau
                 go to 250
              end if
              if( w<=zero ) then
                 dltlb = max( dltlb, tau )
              else
                 dltub = min( dltub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              c = w - delta( n-1 )*dpsi - delta( n )*dphi
              a = ( delta( n-1 )+delta( n ) )*w -delta( n-1 )*delta( n )*( dpsi+dphi )
              b = delta( n-1 )*delta( n )*w
              if( c<zero )c = abs( c )
              if( c==zero ) then
                ! eta = b/a
                 ! eta = rho - tau
                 eta = dltub - tau
              else if( a>=zero ) then
                 eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>zero )eta = -w / ( dpsi+dphi )
              temp = tau + eta
              if( temp>dltub .or. temp<dltlb ) then
                 if( w<zero ) then
                    eta = ( dltub-tau ) / two
                 else
                    eta = ( dltlb-tau ) / two
                 end if
              end if
              do j = 1, n
                 delta( j ) = delta( j ) - eta
              end do
              tau = tau + eta
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / delta( n )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                        
              w = rhoinv + phi + psi
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_90: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    dlam = d( i ) + tau
                    go to 250
                 end if
                 if( w<=zero ) then
                    dltlb = max( dltlb, tau )
                 else
                    dltub = min( dltub, tau )
                 end if
                 ! calculate the new step
                 c = w - delta( n-1 )*dpsi - delta( n )*dphi
                 a = ( delta( n-1 )+delta( n ) )*w -delta( n-1 )*delta( n )*( dpsi+dphi )
                 b = delta( n-1 )*delta( n )*w
                 if( a>=zero ) then
                    eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>zero )eta = -w / ( dpsi+dphi )
                 temp = tau + eta
                 if( temp>dltub .or. temp<dltlb ) then
                    if( w<zero ) then
                       eta = ( dltub-tau ) / two
                    else
                       eta = ( dltlb-tau ) / two
                    end if
                 end if
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                 end do
                 tau = tau + eta
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, ii
                    temp = z( j ) / delta( j )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 temp = z( n ) / delta( n )
                 phi = z( n )*temp
                 dphi = temp*temp
                 erretm = eight*( -phi-psi ) + erretm - phi + rhoinv +abs( tau )*( dpsi+dphi )
                           
                 w = rhoinv + phi + psi
              end do loop_90
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              dlam = d( i ) + tau
              go to 250
              ! end for the case i = n
           else
              ! the case for i < n
              niter = 1_ilp
              ip1 = i + 1_ilp
              ! calculate initial guess
              del = d( ip1 ) - d( i )
              midpt = del / two
              do j = 1, n
                 delta( j ) = ( d( j )-d( i ) ) - midpt
              end do
              psi = zero
              do j = 1, i - 1
                 psi = psi + z( j )*z( j ) / delta( j )
              end do
              phi = zero
              do j = n, i + 2, -1
                 phi = phi + z( j )*z( j ) / delta( j )
              end do
              c = rhoinv + psi + phi
              w = c + z( i )*z( i ) / delta( i ) +z( ip1 )*z( ip1 ) / delta( ip1 )
              if( w>zero ) then
                 ! d(i)< the ith eigenvalue < (d(i)+d(i+1))/2
                 ! we choose d(i) as origin.
                 orgati = .true.
                 a = c*del + z( i )*z( i ) + z( ip1 )*z( ip1 )
                 b = z( i )*z( i )*del
                 if( a>zero ) then
                    tau = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 else
                    tau = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 end if
                 dltlb = zero
                 dltub = midpt
              else
                 ! (d(i)+d(i+1))/2 <= the ith eigenvalue < d(i+1)
                 ! we choose d(i+1) as origin.
                 orgati = .false.
                 a = c*del - z( i )*z( i ) - z( ip1 )*z( ip1 )
                 b = z( ip1 )*z( ip1 )*del
                 if( a<zero ) then
                    tau = two*b / ( a-sqrt( abs( a*a+four*b*c ) ) )
                 else
                    tau = -( a+sqrt( abs( a*a+four*b*c ) ) ) / ( two*c )
                 end if
                 dltlb = -midpt
                 dltub = zero
              end if
              if( orgati ) then
                 do j = 1, n
                    delta( j ) = ( d( j )-d( i ) ) - tau
                 end do
              else
                 do j = 1, n
                    delta( j ) = ( d( j )-d( ip1 ) ) - tau
                 end do
              end if
              if( orgati ) then
                 ii = i
              else
                 ii = i + 1_ilp
              end if
              iim1 = ii - 1_ilp
              iip1 = ii + 1_ilp
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / delta( j )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              w = rhoinv + phi + psi
              ! w is the value of the secular function with
              ! its ii-th element removed.
              swtch3 = .false.
              if( orgati ) then
                 if( w<zero )swtch3 = .true.
              else
                 if( w>zero )swtch3 = .true.
              end if
              if( ii==1_ilp .or. ii==n )swtch3 = .false.
              temp = z( ii ) / delta( ii )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = w + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau )&
                        *dw
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 if( orgati ) then
                    dlam = d( i ) + tau
                 else
                    dlam = d( ip1 ) + tau
                 end if
                 go to 250
              end if
              if( w<=zero ) then
                 dltlb = max( dltlb, tau )
              else
                 dltub = min( dltub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              if( .not.swtch3 ) then
                 if( orgati ) then
                    c = w - delta( ip1 )*dw - ( d( i )-d( ip1 ) )*( z( i ) / delta( i ) )&
                              **2_ilp
                 else
                    c = w - delta( i )*dw - ( d( ip1 )-d( i ) )*( z( ip1 ) / delta( ip1 ) )&
                              **2_ilp
                 end if
                 a = ( delta( i )+delta( ip1 ) )*w -delta( i )*delta( ip1 )*dw
                 b = delta( i )*delta( ip1 )*w
                 if( c==zero ) then
                    if( a==zero ) then
                       if( orgati ) then
                          a = z( i )*z( i ) + delta( ip1 )*delta( ip1 )*( dpsi+dphi )
                       else
                          a = z( ip1 )*z( ip1 ) + delta( i )*delta( i )*( dpsi+dphi )
                       end if
                    end if
                    eta = b / a
                 else if( a<=zero ) then
                    eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 end if
              else
                 ! interpolation using three most relevant poles
                 temp = rhoinv + psi + phi
                 if( orgati ) then
                    temp1 = z( iim1 ) / delta( iim1 )
                    temp1 = temp1*temp1
                    c = temp - delta( iip1 )*( dpsi+dphi ) -( d( iim1 )-d( iip1 ) )*temp1
                    zz( 1_ilp ) = z( iim1 )*z( iim1 )
                    zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*( ( dpsi-temp1 )+dphi )
                 else
                    temp1 = z( iip1 ) / delta( iip1 )
                    temp1 = temp1*temp1
                    c = temp - delta( iim1 )*( dpsi+dphi ) -( d( iip1 )-d( iim1 ) )*temp1
                    zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*( dpsi+( dphi-temp1 ) )
                    zz( 3_ilp ) = z( iip1 )*z( iip1 )
                 end if
                 zz( 2_ilp ) = z( ii )*z( ii )
                 call stdlib_dlaed6( niter, orgati, c, delta( iim1 ), zz, w, eta,info )
                 if( info/=0 )go to 250
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>=zero )eta = -w / dw
              temp = tau + eta
              if( temp>dltub .or. temp<dltlb ) then
                 if( w<zero ) then
                    eta = ( dltub-tau ) / two
                 else
                    eta = ( dltlb-tau ) / two
                 end if
              end if
              prew = w
              do j = 1, n
                 delta( j ) = delta( j ) - eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / delta( j )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / delta( j )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              temp = z( ii ) / delta( ii )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = rhoinv + phi + psi + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau+eta )&
                        *dw
              swtch = .false.
              if( orgati ) then
                 if( -w>abs( prew ) / ten )swtch = .true.
              else
                 if( w>abs( prew ) / ten )swtch = .true.
              end if
              tau = tau + eta
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_240: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    if( orgati ) then
                       dlam = d( i ) + tau
                    else
                       dlam = d( ip1 ) + tau
                    end if
                    go to 250
                 end if
                 if( w<=zero ) then
                    dltlb = max( dltlb, tau )
                 else
                    dltub = min( dltub, tau )
                 end if
                 ! calculate the new step
                 if( .not.swtch3 ) then
                    if( .not.swtch ) then
                       if( orgati ) then
                          c = w - delta( ip1 )*dw -( d( i )-d( ip1 ) )*( z( i ) / delta( i ) )&
                                    **2_ilp
                       else
                          c = w - delta( i )*dw - ( d( ip1 )-d( i ) )*( z( ip1 ) / delta( ip1 ) )&
                                    **2_ilp
                       end if
                    else
                       temp = z( ii ) / delta( ii )
                       if( orgati ) then
                          dpsi = dpsi + temp*temp
                       else
                          dphi = dphi + temp*temp
                       end if
                       c = w - delta( i )*dpsi - delta( ip1 )*dphi
                    end if
                    a = ( delta( i )+delta( ip1 ) )*w -delta( i )*delta( ip1 )*dw
                    b = delta( i )*delta( ip1 )*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( .not.swtch ) then
                             if( orgati ) then
                                a = z( i )*z( i ) + delta( ip1 )*delta( ip1 )*( dpsi+dphi )
                                          
                             else
                                a = z( ip1 )*z( ip1 ) +delta( i )*delta( i )*( dpsi+dphi )
                             end if
                          else
                             a = delta( i )*delta( i )*dpsi +delta( ip1 )*delta( ip1 )&
                                       *dphi
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 else
                    ! interpolation using three most relevant poles
                    temp = rhoinv + psi + phi
                    if( swtch ) then
                       c = temp - delta( iim1 )*dpsi - delta( iip1 )*dphi
                       zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*dpsi
                       zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*dphi
                    else
                       if( orgati ) then
                          temp1 = z( iim1 ) / delta( iim1 )
                          temp1 = temp1*temp1
                          c = temp - delta( iip1 )*( dpsi+dphi ) -( d( iim1 )-d( iip1 ) )&
                                    *temp1
                          zz( 1_ilp ) = z( iim1 )*z( iim1 )
                          zz( 3_ilp ) = delta( iip1 )*delta( iip1 )*( ( dpsi-temp1 )+dphi )
                       else
                          temp1 = z( iip1 ) / delta( iip1 )
                          temp1 = temp1*temp1
                          c = temp - delta( iim1 )*( dpsi+dphi ) -( d( iip1 )-d( iim1 ) )&
                                    *temp1
                          zz( 1_ilp ) = delta( iim1 )*delta( iim1 )*( dpsi+( dphi-temp1 ) )
                          zz( 3_ilp ) = z( iip1 )*z( iip1 )
                       end if
                    end if
                    call stdlib_dlaed6( niter, orgati, c, delta( iim1 ), zz, w, eta,info )
                    if( info/=0 )go to 250
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>=zero )eta = -w / dw
                 temp = tau + eta
                 if( temp>dltub .or. temp<dltlb ) then
                    if( w<zero ) then
                       eta = ( dltub-tau ) / two
                    else
                       eta = ( dltlb-tau ) / two
                    end if
                 end if
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                 end do
                 tau = tau + eta
                 prew = w
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, iim1
                    temp = z( j ) / delta( j )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 dphi = zero
                 phi = zero
                 do j = n, iip1, -1
                    temp = z( j ) / delta( j )
                    phi = phi + z( j )*temp
                    dphi = dphi + temp*temp
                    erretm = erretm + phi
                 end do
                 temp = z( ii ) / delta( ii )
                 dw = dpsi + dphi + temp*temp
                 temp = z( ii )*temp
                 w = rhoinv + phi + psi + temp
                 erretm = eight*( phi-psi ) + erretm + two*rhoinv +three*abs( temp ) + abs( tau )&
                           *dw
                 if( w*prew>zero .and. abs( w )>abs( prew ) / ten )swtch = .not.swtch
              end do loop_240
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              if( orgati ) then
                 dlam = d( i ) + tau
              else
                 dlam = d( ip1 ) + tau
              end if
           end if
           250 continue
           return
     end subroutine stdlib_dlaed4




     pure module subroutine stdlib_slaed5( i, d, z, delta, rho, dlam )
     !! This subroutine computes the I-th eigenvalue of a symmetric rank-one
     !! modification of a 2-by-2 diagonal matrix
     !! diag( D )  +  RHO * Z * transpose(Z) .
     !! The diagonal elements in the array D are assumed to satisfy
     !! D(i) < D(j)  for  i < j .
     !! We also assume RHO > 0 and that the Euclidean norm of the vector
     !! Z is one.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i
           real(sp), intent(out) :: dlam
           real(sp), intent(in) :: rho
           ! Array Arguments 
           real(sp), intent(in) :: d(2_ilp), z(2_ilp)
           real(sp), intent(out) :: delta(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: b, c, del, tau, temp, w
           ! Intrinsic Functions 
           ! Executable Statements 
           del = d( 2_ilp ) - d( 1_ilp )
           if( i==1_ilp ) then
              w = one + two*rho*( z( 2_ilp )*z( 2_ilp )-z( 1_ilp )*z( 1_ilp ) ) / del
              if( w>zero ) then
                 b = del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 1_ilp )*z( 1_ilp )*del
                 ! b > zero, always
                 tau = two*c / ( b+sqrt( abs( b*b-four*c ) ) )
                 dlam = d( 1_ilp ) + tau
                 delta( 1_ilp ) = -z( 1_ilp ) / tau
                 delta( 2_ilp ) = z( 2_ilp ) / ( del-tau )
              else
                 b = -del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 2_ilp )*z( 2_ilp )*del
                 if( b>zero ) then
                    tau = -two*c / ( b+sqrt( b*b+four*c ) )
                 else
                    tau = ( b-sqrt( b*b+four*c ) ) / two
                 end if
                 dlam = d( 2_ilp ) + tau
                 delta( 1_ilp ) = -z( 1_ilp ) / ( del+tau )
                 delta( 2_ilp ) = -z( 2_ilp ) / tau
              end if
              temp = sqrt( delta( 1_ilp )*delta( 1_ilp )+delta( 2_ilp )*delta( 2_ilp ) )
              delta( 1_ilp ) = delta( 1_ilp ) / temp
              delta( 2_ilp ) = delta( 2_ilp ) / temp
           else
           ! now i=2
              b = -del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
              c = rho*z( 2_ilp )*z( 2_ilp )*del
              if( b>zero ) then
                 tau = ( b+sqrt( b*b+four*c ) ) / two
              else
                 tau = two*c / ( -b+sqrt( b*b+four*c ) )
              end if
              dlam = d( 2_ilp ) + tau
              delta( 1_ilp ) = -z( 1_ilp ) / ( del+tau )
              delta( 2_ilp ) = -z( 2_ilp ) / tau
              temp = sqrt( delta( 1_ilp )*delta( 1_ilp )+delta( 2_ilp )*delta( 2_ilp ) )
              delta( 1_ilp ) = delta( 1_ilp ) / temp
              delta( 2_ilp ) = delta( 2_ilp ) / temp
           end if
           return
     end subroutine stdlib_slaed5

     pure module subroutine stdlib_dlaed5( i, d, z, delta, rho, dlam )
     !! This subroutine computes the I-th eigenvalue of a symmetric rank-one
     !! modification of a 2-by-2 diagonal matrix
     !! diag( D )  +  RHO * Z * transpose(Z) .
     !! The diagonal elements in the array D are assumed to satisfy
     !! D(i) < D(j)  for  i < j .
     !! We also assume RHO > 0 and that the Euclidean norm of the vector
     !! Z is one.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i
           real(dp), intent(out) :: dlam
           real(dp), intent(in) :: rho
           ! Array Arguments 
           real(dp), intent(in) :: d(2_ilp), z(2_ilp)
           real(dp), intent(out) :: delta(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: b, c, del, tau, temp, w
           ! Intrinsic Functions 
           ! Executable Statements 
           del = d( 2_ilp ) - d( 1_ilp )
           if( i==1_ilp ) then
              w = one + two*rho*( z( 2_ilp )*z( 2_ilp )-z( 1_ilp )*z( 1_ilp ) ) / del
              if( w>zero ) then
                 b = del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 1_ilp )*z( 1_ilp )*del
                 ! b > zero, always
                 tau = two*c / ( b+sqrt( abs( b*b-four*c ) ) )
                 dlam = d( 1_ilp ) + tau
                 delta( 1_ilp ) = -z( 1_ilp ) / tau
                 delta( 2_ilp ) = z( 2_ilp ) / ( del-tau )
              else
                 b = -del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 2_ilp )*z( 2_ilp )*del
                 if( b>zero ) then
                    tau = -two*c / ( b+sqrt( b*b+four*c ) )
                 else
                    tau = ( b-sqrt( b*b+four*c ) ) / two
                 end if
                 dlam = d( 2_ilp ) + tau
                 delta( 1_ilp ) = -z( 1_ilp ) / ( del+tau )
                 delta( 2_ilp ) = -z( 2_ilp ) / tau
              end if
              temp = sqrt( delta( 1_ilp )*delta( 1_ilp )+delta( 2_ilp )*delta( 2_ilp ) )
              delta( 1_ilp ) = delta( 1_ilp ) / temp
              delta( 2_ilp ) = delta( 2_ilp ) / temp
           else
           ! now i=2
              b = -del + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
              c = rho*z( 2_ilp )*z( 2_ilp )*del
              if( b>zero ) then
                 tau = ( b+sqrt( b*b+four*c ) ) / two
              else
                 tau = two*c / ( -b+sqrt( b*b+four*c ) )
              end if
              dlam = d( 2_ilp ) + tau
              delta( 1_ilp ) = -z( 1_ilp ) / ( del+tau )
              delta( 2_ilp ) = -z( 2_ilp ) / tau
              temp = sqrt( delta( 1_ilp )*delta( 1_ilp )+delta( 2_ilp )*delta( 2_ilp ) )
              delta( 1_ilp ) = delta( 1_ilp ) / temp
              delta( 2_ilp ) = delta( 2_ilp ) / temp
           end if
           return
     end subroutine stdlib_dlaed5




     pure module subroutine stdlib_slaed6( kniter, orgati, rho, d, z, finit, tau, info )
     !! SLAED6 computes the positive or negative root (closest to the origin)
     !! of
     !! z(1)        z(2)        z(3)
     !! f(x) =   rho + --------- + ---------- + ---------
     !! d(1)-x      d(2)-x      d(3)-x
     !! It is assumed that
     !! if ORGATI = .true. the root is between d(2) and d(3);
     !! otherwise it is between d(1) and d(2)
     !! This routine will be called by SLAED4 when necessary. In most cases,
     !! the root sought is the smallest in magnitude, though it might not be
     !! in some extremely rare situations.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: orgati
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kniter
           real(sp), intent(in) :: finit, rho
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(in) :: d(3_ilp), z(3_ilp)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           
           
           ! Local Arrays 
           real(sp) :: dscale(3_ilp), zscale(3_ilp)
           ! Local Scalars 
           logical(lk) :: scale
           integer(ilp) :: i, iter, niter
           real(sp) :: a, b, base, c, ddf, df, eps, erretm, eta, f, fc, sclfac, sclinv, small1, &
                     small2, sminv1, sminv2, temp, temp1, temp2, temp3, temp4, lbd, ubd
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( orgati ) then
              lbd = d(2_ilp)
              ubd = d(3_ilp)
           else
              lbd = d(1_ilp)
              ubd = d(2_ilp)
           end if
           if( finit < zero )then
              lbd = zero
           else
              ubd = zero
           end if
           niter = 1_ilp
           tau = zero
           if( kniter==2_ilp ) then
              if( orgati ) then
                 temp = ( d( 3_ilp )-d( 2_ilp ) ) / two
                 c = rho + z( 1_ilp ) / ( ( d( 1_ilp )-d( 2_ilp ) )-temp )
                 a = c*( d( 2_ilp )+d( 3_ilp ) ) + z( 2_ilp ) + z( 3_ilp )
                 b = c*d( 2_ilp )*d( 3_ilp ) + z( 2_ilp )*d( 3_ilp ) + z( 3_ilp )*d( 2_ilp )
              else
                 temp = ( d( 1_ilp )-d( 2_ilp ) ) / two
                 c = rho + z( 3_ilp ) / ( ( d( 3_ilp )-d( 2_ilp ) )-temp )
                 a = c*( d( 1_ilp )+d( 2_ilp ) ) + z( 1_ilp ) + z( 2_ilp )
                 b = c*d( 1_ilp )*d( 2_ilp ) + z( 1_ilp )*d( 2_ilp ) + z( 2_ilp )*d( 1_ilp )
              end if
              temp = max( abs( a ), abs( b ), abs( c ) )
              a = a / temp
              b = b / temp
              c = c / temp
              if( c==zero ) then
                 tau = b / a
              else if( a<=zero ) then
                 tau = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 tau = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
              end if
              if( tau < lbd .or. tau > ubd )tau = ( lbd+ubd )/two
              if( d(1_ilp)==tau .or. d(2_ilp)==tau .or. d(3_ilp)==tau ) then
                 tau = zero
              else
                 temp = finit + tau*z(1_ilp)/( d(1_ilp)*( d( 1_ilp )-tau ) ) +tau*z(2_ilp)/( d(2_ilp)*( d( 2_ilp )-tau ) )&
                            +tau*z(3_ilp)/( d(3_ilp)*( d( 3_ilp )-tau ) )
                 if( temp <= zero )then
                    lbd = tau
                 else
                    ubd = tau
                 end if
                 if( abs( finit )<=abs( temp ) )tau = zero
              end if
           end if
           ! get machine parameters for possible scaling to avoid overflow
           ! modified by sven: parameters small1, sminv1, small2,
           ! sminv2, eps are not saved anymore between one call to the
           ! others but recomputed at each call
           eps = stdlib_slamch( 'EPSILON' )
           base = stdlib_slamch( 'BASE' )
           small1 = base**( int( log( stdlib_slamch( 'SAFMIN' ) ) / log( base ) /three,KIND=ilp) )
                     
           sminv1 = one / small1
           small2 = small1*small1
           sminv2 = sminv1*sminv1
           ! determine if scaling of inputs necessary to avoid overflow
           ! when computing 1/temp**3
           if( orgati ) then
              temp = min( abs( d( 2_ilp )-tau ), abs( d( 3_ilp )-tau ) )
           else
              temp = min( abs( d( 1_ilp )-tau ), abs( d( 2_ilp )-tau ) )
           end if
           scale = .false.
           if( temp<=small1 ) then
              scale = .true.
              if( temp<=small2 ) then
              ! scale up by power of radix nearest 1/safmin**(2/3)
                 sclfac = sminv2
                 sclinv = small2
              else
              ! scale up by power of radix nearest 1/safmin**(1/3)
                 sclfac = sminv1
                 sclinv = small1
              end if
              ! scaling up safe because d, z, tau scaled elsewhere to be o(1)
              do i = 1, 3
                 dscale( i ) = d( i )*sclfac
                 zscale( i ) = z( i )*sclfac
              end do
              tau = tau*sclfac
              lbd = lbd*sclfac
              ubd = ubd*sclfac
           else
              ! copy d and z to dscale and zscale
              do i = 1, 3
                 dscale( i ) = d( i )
                 zscale( i ) = z( i )
              end do
           end if
           fc = zero
           df = zero
           ddf = zero
           do i = 1, 3
              temp = one / ( dscale( i )-tau )
              temp1 = zscale( i )*temp
              temp2 = temp1*temp
              temp3 = temp2*temp
              fc = fc + temp1 / dscale( i )
              df = df + temp2
              ddf = ddf + temp3
           end do
           f = finit + tau*fc
           if( abs( f )<=zero )go to 60
           if( f <= zero )then
              lbd = tau
           else
              ubd = tau
           end if
              ! iteration begins -- use gragg-thornton-warner cubic convergent
                                  ! scheme
           ! it is not hard to see that
                 ! 1) iterations will go up monotonically
                    ! if finit < 0;
                 ! 2) iterations will go down monotonically
                    ! if finit > 0.
           iter = niter + 1_ilp
           loop_50: do niter = iter, maxit
              if( orgati ) then
                 temp1 = dscale( 2_ilp ) - tau
                 temp2 = dscale( 3_ilp ) - tau
              else
                 temp1 = dscale( 1_ilp ) - tau
                 temp2 = dscale( 2_ilp ) - tau
              end if
              a = ( temp1+temp2 )*f - temp1*temp2*df
              b = temp1*temp2*f
              c = f - ( temp1+temp2 )*df + temp1*temp2*ddf
              temp = max( abs( a ), abs( b ), abs( c ) )
              a = a / temp
              b = b / temp
              c = c / temp
              if( c==zero ) then
                 eta = b / a
              else if( a<=zero ) then
                 eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
              end if
              if( f*eta>=zero ) then
                 eta = -f / df
              end if
              tau = tau + eta
              if( tau < lbd .or. tau > ubd )tau = ( lbd + ubd )/two
              fc = zero
              erretm = zero
              df = zero
              ddf = zero
              do i = 1, 3
                 if ( ( dscale( i )-tau )/=zero ) then
                    temp = one / ( dscale( i )-tau )
                    temp1 = zscale( i )*temp
                    temp2 = temp1*temp
                    temp3 = temp2*temp
                    temp4 = temp1 / dscale( i )
                    fc = fc + temp4
                    erretm = erretm + abs( temp4 )
                    df = df + temp2
                    ddf = ddf + temp3
                 else
                    go to 60
                 end if
              end do
              f = finit + tau*fc
              erretm = eight*( abs( finit )+abs( tau )*erretm ) +abs( tau )*df
              if( ( abs( f )<=four*eps*erretm ) .or.( (ubd-lbd)<=four*eps*abs(tau) )  )go to 60

              if( f <= zero )then
                 lbd = tau
              else
                 ubd = tau
              end if
           end do loop_50
           info = 1_ilp
           60 continue
           ! undo scaling
           if( scale )tau = tau*sclinv
           return
     end subroutine stdlib_slaed6

     pure module subroutine stdlib_dlaed6( kniter, orgati, rho, d, z, finit, tau, info )
     !! DLAED6 computes the positive or negative root (closest to the origin)
     !! of
     !! z(1)        z(2)        z(3)
     !! f(x) =   rho + --------- + ---------- + ---------
     !! d(1)-x      d(2)-x      d(3)-x
     !! It is assumed that
     !! if ORGATI = .true. the root is between d(2) and d(3);
     !! otherwise it is between d(1) and d(2)
     !! This routine will be called by DLAED4 when necessary. In most cases,
     !! the root sought is the smallest in magnitude, though it might not be
     !! in some extremely rare situations.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: orgati
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kniter
           real(dp), intent(in) :: finit, rho
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(in) :: d(3_ilp), z(3_ilp)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           
           
           ! Local Arrays 
           real(dp) :: dscale(3_ilp), zscale(3_ilp)
           ! Local Scalars 
           logical(lk) :: scale
           integer(ilp) :: i, iter, niter
           real(dp) :: a, b, base, c, ddf, df, eps, erretm, eta, f, fc, sclfac, sclinv, small1, &
                     small2, sminv1, sminv2, temp, temp1, temp2, temp3, temp4, lbd, ubd
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( orgati ) then
              lbd = d(2_ilp)
              ubd = d(3_ilp)
           else
              lbd = d(1_ilp)
              ubd = d(2_ilp)
           end if
           if( finit < zero )then
              lbd = zero
           else
              ubd = zero
           end if
           niter = 1_ilp
           tau = zero
           if( kniter==2_ilp ) then
              if( orgati ) then
                 temp = ( d( 3_ilp )-d( 2_ilp ) ) / two
                 c = rho + z( 1_ilp ) / ( ( d( 1_ilp )-d( 2_ilp ) )-temp )
                 a = c*( d( 2_ilp )+d( 3_ilp ) ) + z( 2_ilp ) + z( 3_ilp )
                 b = c*d( 2_ilp )*d( 3_ilp ) + z( 2_ilp )*d( 3_ilp ) + z( 3_ilp )*d( 2_ilp )
              else
                 temp = ( d( 1_ilp )-d( 2_ilp ) ) / two
                 c = rho + z( 3_ilp ) / ( ( d( 3_ilp )-d( 2_ilp ) )-temp )
                 a = c*( d( 1_ilp )+d( 2_ilp ) ) + z( 1_ilp ) + z( 2_ilp )
                 b = c*d( 1_ilp )*d( 2_ilp ) + z( 1_ilp )*d( 2_ilp ) + z( 2_ilp )*d( 1_ilp )
              end if
              temp = max( abs( a ), abs( b ), abs( c ) )
              a = a / temp
              b = b / temp
              c = c / temp
              if( c==zero ) then
                 tau = b / a
              else if( a<=zero ) then
                 tau = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 tau = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
              end if
              if( tau < lbd .or. tau > ubd )tau = ( lbd+ubd )/two
              if( d(1_ilp)==tau .or. d(2_ilp)==tau .or. d(3_ilp)==tau ) then
                 tau = zero
              else
                 temp = finit + tau*z(1_ilp)/( d(1_ilp)*( d( 1_ilp )-tau ) ) +tau*z(2_ilp)/( d(2_ilp)*( d( 2_ilp )-tau ) )&
                            +tau*z(3_ilp)/( d(3_ilp)*( d( 3_ilp )-tau ) )
                 if( temp <= zero )then
                    lbd = tau
                 else
                    ubd = tau
                 end if
                 if( abs( finit )<=abs( temp ) )tau = zero
              end if
           end if
           ! get machine parameters for possible scaling to avoid overflow
           ! modified by sven: parameters small1, sminv1, small2,
           ! sminv2, eps are not saved anymore between one call to the
           ! others but recomputed at each call
           eps = stdlib_dlamch( 'EPSILON' )
           base = stdlib_dlamch( 'BASE' )
           small1 = base**( int( log( stdlib_dlamch( 'SAFMIN' ) ) / log( base ) /three,KIND=ilp) )
                     
           sminv1 = one / small1
           small2 = small1*small1
           sminv2 = sminv1*sminv1
           ! determine if scaling of inputs necessary to avoid overflow
           ! when computing 1/temp**3
           if( orgati ) then
              temp = min( abs( d( 2_ilp )-tau ), abs( d( 3_ilp )-tau ) )
           else
              temp = min( abs( d( 1_ilp )-tau ), abs( d( 2_ilp )-tau ) )
           end if
           scale = .false.
           if( temp<=small1 ) then
              scale = .true.
              if( temp<=small2 ) then
              ! scale up by power of radix nearest 1/safmin**(2/3)
                 sclfac = sminv2
                 sclinv = small2
              else
              ! scale up by power of radix nearest 1/safmin**(1/3)
                 sclfac = sminv1
                 sclinv = small1
              end if
              ! scaling up safe because d, z, tau scaled elsewhere to be o(1)
              do i = 1, 3
                 dscale( i ) = d( i )*sclfac
                 zscale( i ) = z( i )*sclfac
              end do
              tau = tau*sclfac
              lbd = lbd*sclfac
              ubd = ubd*sclfac
           else
              ! copy d and z to dscale and zscale
              do i = 1, 3
                 dscale( i ) = d( i )
                 zscale( i ) = z( i )
              end do
           end if
           fc = zero
           df = zero
           ddf = zero
           do i = 1, 3
              temp = one / ( dscale( i )-tau )
              temp1 = zscale( i )*temp
              temp2 = temp1*temp
              temp3 = temp2*temp
              fc = fc + temp1 / dscale( i )
              df = df + temp2
              ddf = ddf + temp3
           end do
           f = finit + tau*fc
           if( abs( f )<=zero )go to 60
           if( f <= zero )then
              lbd = tau
           else
              ubd = tau
           end if
              ! iteration begins -- use gragg-thornton-warner cubic convergent
                                  ! scheme
           ! it is not hard to see that
                 ! 1) iterations will go up monotonically
                    ! if finit < 0;
                 ! 2) iterations will go down monotonically
                    ! if finit > 0.
           iter = niter + 1_ilp
           loop_50: do niter = iter, maxit
              if( orgati ) then
                 temp1 = dscale( 2_ilp ) - tau
                 temp2 = dscale( 3_ilp ) - tau
              else
                 temp1 = dscale( 1_ilp ) - tau
                 temp2 = dscale( 2_ilp ) - tau
              end if
              a = ( temp1+temp2 )*f - temp1*temp2*df
              b = temp1*temp2*f
              c = f - ( temp1+temp2 )*df + temp1*temp2*ddf
              temp = max( abs( a ), abs( b ), abs( c ) )
              a = a / temp
              b = b / temp
              c = c / temp
              if( c==zero ) then
                 eta = b / a
              else if( a<=zero ) then
                 eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
              end if
              if( f*eta>=zero ) then
                 eta = -f / df
              end if
              tau = tau + eta
              if( tau < lbd .or. tau > ubd )tau = ( lbd + ubd )/two
              fc = zero
              erretm = zero
              df = zero
              ddf = zero
              do i = 1, 3
                 if ( ( dscale( i )-tau )/=zero ) then
                    temp = one / ( dscale( i )-tau )
                    temp1 = zscale( i )*temp
                    temp2 = temp1*temp
                    temp3 = temp2*temp
                    temp4 = temp1 / dscale( i )
                    fc = fc + temp4
                    erretm = erretm + abs( temp4 )
                    df = df + temp2
                    ddf = ddf + temp3
                 else
                    go to 60
                 end if
              end do
              f = finit + tau*fc
              erretm = eight*( abs( finit )+abs( tau )*erretm ) +abs( tau )*df
              if( ( abs( f )<=four*eps*erretm ) .or.( (ubd-lbd)<=four*eps*abs(tau) )  ) go to 60
              if( f <= zero )then
                 lbd = tau
              else
                 ubd = tau
              end if
           end do loop_50
           info = 1_ilp
           60 continue
           ! undo scaling
           if( scale )tau = tau*sclinv
           return
     end subroutine stdlib_dlaed6




     pure module subroutine stdlib_slaed7( icompq, n, qsiz, tlvls, curlvl, curpbm, d, q,ldq, indxq, rho, &
     !! SLAED7 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix. This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and optionally eigenvectors of a dense symmetric matrix
     !! that has been reduced to tridiagonal form.  SLAED1 handles
     !! the case in which all eigenvalues and eigenvectors of a symmetric
     !! tridiagonal matrix are desired.
     !! T = Q(in) ( D(in) + RHO * Z*Z**T ) Q**T(in) = Q(out) * D(out) * Q**T(out)
     !! where Z = Q**Tu, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine SLAED8.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine SLAED4 (as called by SLAED9).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
               cutpnt, qstore, qptr, prmptr,perm, givptr, givcol, givnum, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, icompq, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(sp), intent(inout) :: d(*), givnum(2_ilp,*), q(ldq,*), qstore(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: coltyp, curr, i, idlmda, indx, indxc, indxp, iq2, is, iw, iz, k, ldq2, &
                     n1, n2, ptr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( icompq==1_ilp .and. qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( min( 1_ilp, n )>cutpnt .or. n<cutpnt ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED7', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_slaed8 and stdlib_slaed9.
           if( icompq==1_ilp ) then
              ldq2 = qsiz
           else
              ldq2 = n
           end if
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq2 = iw + n
           is = iq2 + n*ldq2
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           ptr = 1_ilp + 2_ilp**tlvls
           do i = 1, curlvl - 1
              ptr = ptr + 2_ilp**( tlvls-i )
           end do
           curr = ptr + curpbm
           call stdlib_slaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum, &
                     qstore, qptr, work( iz ),work( iz+n ), info )
           ! when solving the final problem, we no longer need the stored data,
           ! so we will overwrite the data from this level onto the previously
           ! used storage space.
           if( curlvl==tlvls ) then
              qptr( curr ) = 1_ilp
              prmptr( curr ) = 1_ilp
              givptr( curr ) = 1_ilp
           end if
           ! sort and deflate eigenvalues.
           call stdlib_slaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho, cutpnt,work( iz ), work(&
            idlmda ), work( iq2 ), ldq2,work( iw ), perm( prmptr( curr ) ), givptr( curr+1 ),&
            givcol( 1_ilp, givptr( curr ) ),givnum( 1_ilp, givptr( curr ) ), iwork( indxp ),iwork( indx ),&
                       info )
           prmptr( curr+1 ) = prmptr( curr ) + n
           givptr( curr+1 ) = givptr( curr+1 ) + givptr( curr )
           ! solve secular equation.
           if( k/=0_ilp ) then
              call stdlib_slaed9( k, 1_ilp, k, n, d, work( is ), k, rho, work( idlmda ),work( iw ), &
                        qstore( qptr( curr ) ), k, info )
              if( info/=0 )go to 30
              if( icompq==1_ilp ) then
                 call stdlib_sgemm( 'N', 'N', qsiz, k, k, one, work( iq2 ), ldq2,qstore( qptr( &
                           curr ) ), k, zero, q, ldq )
              end if
              qptr( curr+1 ) = qptr( curr ) + k**2_ilp
           ! prepare the indxq sorting permutation.
              n1 = k
              n2 = n - k
              call stdlib_slamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              qptr( curr+1 ) = qptr( curr )
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           30 continue
           return
     end subroutine stdlib_slaed7

     pure module subroutine stdlib_dlaed7( icompq, n, qsiz, tlvls, curlvl, curpbm, d, q,ldq, indxq, rho, &
     !! DLAED7 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix. This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and optionally eigenvectors of a dense symmetric matrix
     !! that has been reduced to tridiagonal form.  DLAED1 handles
     !! the case in which all eigenvalues and eigenvectors of a symmetric
     !! tridiagonal matrix are desired.
     !! T = Q(in) ( D(in) + RHO * Z*Z**T ) Q**T(in) = Q(out) * D(out) * Q**T(out)
     !! where Z = Q**Tu, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine DLAED8.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine DLAED4 (as called by DLAED9).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
               cutpnt, qstore, qptr, prmptr,perm, givptr, givcol, givnum, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, icompq, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(dp), intent(inout) :: d(*), givnum(2_ilp,*), q(ldq,*), qstore(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: coltyp, curr, i, idlmda, indx, indxc, indxp, iq2, is, iw, iz, k, ldq2, &
                     n1, n2, ptr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( icompq==1_ilp .and. qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( min( 1_ilp, n )>cutpnt .or. n<cutpnt ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED7', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_dlaed8 and stdlib_dlaed9.
           if( icompq==1_ilp ) then
              ldq2 = qsiz
           else
              ldq2 = n
           end if
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq2 = iw + n
           is = iq2 + n*ldq2
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           ptr = 1_ilp + 2_ilp**tlvls
           do i = 1, curlvl - 1
              ptr = ptr + 2_ilp**( tlvls-i )
           end do
           curr = ptr + curpbm
           call stdlib_dlaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum, &
                     qstore, qptr, work( iz ),work( iz+n ), info )
           ! when solving the final problem, we no longer need the stored data,
           ! so we will overwrite the data from this level onto the previously
           ! used storage space.
           if( curlvl==tlvls ) then
              qptr( curr ) = 1_ilp
              prmptr( curr ) = 1_ilp
              givptr( curr ) = 1_ilp
           end if
           ! sort and deflate eigenvalues.
           call stdlib_dlaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho, cutpnt,work( iz ), work(&
            idlmda ), work( iq2 ), ldq2,work( iw ), perm( prmptr( curr ) ), givptr( curr+1 ),&
            givcol( 1_ilp, givptr( curr ) ),givnum( 1_ilp, givptr( curr ) ), iwork( indxp ),iwork( indx ),&
                       info )
           prmptr( curr+1 ) = prmptr( curr ) + n
           givptr( curr+1 ) = givptr( curr+1 ) + givptr( curr )
           ! solve secular equation.
           if( k/=0_ilp ) then
              call stdlib_dlaed9( k, 1_ilp, k, n, d, work( is ), k, rho, work( idlmda ),work( iw ), &
                        qstore( qptr( curr ) ), k, info )
              if( info/=0 )go to 30
              if( icompq==1_ilp ) then
                 call stdlib_dgemm( 'N', 'N', qsiz, k, k, one, work( iq2 ), ldq2,qstore( qptr( &
                           curr ) ), k, zero, q, ldq )
              end if
              qptr( curr+1 ) = qptr( curr ) + k**2_ilp
           ! prepare the indxq sorting permutation.
              n1 = k
              n2 = n - k
              call stdlib_dlamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              qptr( curr+1 ) = qptr( curr )
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           30 continue
           return
     end subroutine stdlib_dlaed7


     pure module subroutine stdlib_claed7( n, cutpnt, qsiz, tlvls, curlvl, curpbm, d, q,ldq, rho, indxq, &
     !! CLAED7 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix. This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and optionally eigenvectors of a dense or banded
     !! Hermitian matrix that has been reduced to tridiagonal form.
     !! T = Q(in) ( D(in) + RHO * Z*Z**H ) Q**H(in) = Q(out) * D(out) * Q**H(out)
     !! where Z = Q**Hu, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine SLAED2.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine SLAED4 (as called by SLAED3).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
               qstore, qptr, prmptr, perm,givptr, givcol, givnum, work, rwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(sp), intent(inout) :: d(*), givnum(2_ilp,*), qstore(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: coltyp, curr, i, idlmda, indx, indxc, indxp, iq, iw, iz, k, n1, n2, &
                     ptr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! if( icompq<0 .or. icompq>1 ) then
              ! info = -1
           ! else if( n<0 ) then
           if( n<0_ilp ) then
              info = -1_ilp
           else if( min( 1_ilp, n )>cutpnt .or. n<cutpnt ) then
              info = -2_ilp
           else if( qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAED7', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_slaed2 and stdlib_slaed3.
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq = iw + n
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           ptr = 1_ilp + 2_ilp**tlvls
           do i = 1, curlvl - 1
              ptr = ptr + 2_ilp**( tlvls-i )
           end do
           curr = ptr + curpbm
           call stdlib_slaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum, &
                     qstore, qptr, rwork( iz ),rwork( iz+n ), info )
           ! when solving the final problem, we no longer need the stored data,
           ! so we will overwrite the data from this level onto the previously
           ! used storage space.
           if( curlvl==tlvls ) then
              qptr( curr ) = 1_ilp
              prmptr( curr ) = 1_ilp
              givptr( curr ) = 1_ilp
           end if
           ! sort and deflate eigenvalues.
           call stdlib_claed8( k, n, qsiz, q, ldq, d, rho, cutpnt, rwork( iz ),rwork( idlmda ), &
           work, qsiz, rwork( iw ),iwork( indxp ), iwork( indx ), indxq,perm( prmptr( curr ) ), &
           givptr( curr+1 ),givcol( 1_ilp, givptr( curr ) ),givnum( 1_ilp, givptr( curr ) ), info )
                     
           prmptr( curr+1 ) = prmptr( curr ) + n
           givptr( curr+1 ) = givptr( curr+1 ) + givptr( curr )
           ! solve secular equation.
           if( k/=0_ilp ) then
              call stdlib_slaed9( k, 1_ilp, k, n, d, rwork( iq ), k, rho,rwork( idlmda ), rwork( iw ),&
                        qstore( qptr( curr ) ), k, info )
              call stdlib_clacrm( qsiz, k, work, qsiz, qstore( qptr( curr ) ), k, q,ldq, rwork( &
                        iq ) )
              qptr( curr+1 ) = qptr( curr ) + k**2_ilp
              if( info/=0_ilp ) then
                 return
              end if
           ! prepare the indxq sorting premutation.
              n1 = k
              n2 = n - k
              call stdlib_slamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              qptr( curr+1 ) = qptr( curr )
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           return
     end subroutine stdlib_claed7

     pure module subroutine stdlib_zlaed7( n, cutpnt, qsiz, tlvls, curlvl, curpbm, d, q,ldq, rho, indxq, &
     !! ZLAED7 computes the updated eigensystem of a diagonal
     !! matrix after modification by a rank-one symmetric matrix. This
     !! routine is used only for the eigenproblem which requires all
     !! eigenvalues and optionally eigenvectors of a dense or banded
     !! Hermitian matrix that has been reduced to tridiagonal form.
     !! T = Q(in) ( D(in) + RHO * Z*Z**H ) Q**H(in) = Q(out) * D(out) * Q**H(out)
     !! where Z = Q**Hu, u is a vector of length N with ones in the
     !! CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
     !! The eigenvectors of the original matrix are stored in Q, and the
     !! eigenvalues are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple eigenvalues or if there is a zero in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine DLAED2.
     !! The second stage consists of calculating the updated
     !! eigenvalues. This is done by finding the roots of the secular
     !! equation via the routine DLAED4 (as called by SLAED3).
     !! This routine also calculates the eigenvectors of the current
     !! problem.
     !! The final stage consists of computing the updated eigenvectors
     !! directly using the updated eigenvalues.  The eigenvectors for
     !! the current problem are multiplied with the eigenvectors from
     !! the overall problem.
               qstore, qptr, prmptr, perm,givptr, givcol, givnum, work, rwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(dp), intent(inout) :: d(*), givnum(2_ilp,*), qstore(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: coltyp, curr, i, idlmda, indx, indxc, indxp, iq, iw, iz, k, n1, n2, &
                     ptr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! if( icompq<0 .or. icompq>1 ) then
              ! info = -1
           ! else if( n<0 ) then
           if( n<0_ilp ) then
              info = -1_ilp
           else if( min( 1_ilp, n )>cutpnt .or. n<cutpnt ) then
              info = -2_ilp
           else if( qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAED7', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_dlaed2 and stdlib_slaed3.
           iz = 1_ilp
           idlmda = iz + n
           iw = idlmda + n
           iq = iw + n
           indx = 1_ilp
           indxc = indx + n
           coltyp = indxc + n
           indxp = coltyp + n
           ! form the z-vector which consists of the last row of q_1 and the
           ! first row of q_2.
           ptr = 1_ilp + 2_ilp**tlvls
           do i = 1, curlvl - 1
              ptr = ptr + 2_ilp**( tlvls-i )
           end do
           curr = ptr + curpbm
           call stdlib_dlaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum, &
                     qstore, qptr, rwork( iz ),rwork( iz+n ), info )
           ! when solving the final problem, we no longer need the stored data,
           ! so we will overwrite the data from this level onto the previously
           ! used storage space.
           if( curlvl==tlvls ) then
              qptr( curr ) = 1_ilp
              prmptr( curr ) = 1_ilp
              givptr( curr ) = 1_ilp
           end if
           ! sort and deflate eigenvalues.
           call stdlib_zlaed8( k, n, qsiz, q, ldq, d, rho, cutpnt, rwork( iz ),rwork( idlmda ), &
           work, qsiz, rwork( iw ),iwork( indxp ), iwork( indx ), indxq,perm( prmptr( curr ) ), &
           givptr( curr+1 ),givcol( 1_ilp, givptr( curr ) ),givnum( 1_ilp, givptr( curr ) ), info )
                     
           prmptr( curr+1 ) = prmptr( curr ) + n
           givptr( curr+1 ) = givptr( curr+1 ) + givptr( curr )
           ! solve secular equation.
           if( k/=0_ilp ) then
              call stdlib_dlaed9( k, 1_ilp, k, n, d, rwork( iq ), k, rho,rwork( idlmda ), rwork( iw ),&
                        qstore( qptr( curr ) ), k, info )
              call stdlib_zlacrm( qsiz, k, work, qsiz, qstore( qptr( curr ) ), k, q,ldq, rwork( &
                        iq ) )
              qptr( curr+1 ) = qptr( curr ) + k**2_ilp
              if( info/=0_ilp ) then
                 return
              end if
           ! prepare the indxq sorting premutation.
              n1 = k
              n2 = n - k
              call stdlib_dlamrg( n1, n2, d, 1_ilp, -1_ilp, indxq )
           else
              qptr( curr+1 ) = qptr( curr )
              do i = 1, n
                 indxq( i ) = i
              end do
           end if
           return
     end subroutine stdlib_zlaed7




     pure module subroutine stdlib_slaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho,cutpnt, z, dlamda, &
     !! SLAED8 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny element in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
               q2, ldq2, w, perm, givptr,givcol, givnum, indxp, indx, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, icompq, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(sp), intent(out) :: dlamda(*), givnum(2_ilp,*), q2(ldq2,*), w(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: mone = -1.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, imax, j, jlam, jmax, jp, k2, n1, n1p1, n2
           real(sp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( icompq==1_ilp .and. qsiz<n ) then
              info = -4_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( cutpnt<min( 1_ilp, n ) .or. cutpnt>n ) then
              info = -10_ilp
           else if( ldq2<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED8', -info )
              return
           end if
           ! need to initialize givptr to o here in case of quick exit
           ! to prevent an unspecified code behavior (usually sigfault)
           ! when iwork array on entry to *stedc is not zeroed
           ! (or at least some iwork entries which used in *laed7 for givptr).
           givptr = 0_ilp
           ! quick return if possible
           if( n==0 )return
           n1 = cutpnt
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_sscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1
           t = one / sqrt( two )
           do j = 1, n
              indx( j ) = j
           end do
           call stdlib_sscal( n, t, z, 1_ilp )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = cutpnt + 1, n
              indxq( i ) = indxq( i ) + cutpnt
           end do
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
              w( i ) = z( indxq( i ) )
           end do
           i = 1_ilp
           j = cutpnt + 1_ilp
           call stdlib_slamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indx )
           do i = 1, n
              d( i ) = dlamda( indx( i ) )
              z( i ) = w( indx( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_isamax( n, z, 1_ilp )
           jmax = stdlib_isamax( n, d, 1_ilp )
           eps = stdlib_slamch( 'EPSILON' )
           tol = eight*eps*abs( d( jmax ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              if( icompq==0_ilp ) then
                 do j = 1, n
                    perm( j ) = indxq( indx( j ) )
                 end do
              else
                 do j = 1, n
                    perm( j ) = indxq( indx( j ) )
                    call stdlib_scopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
                 end do
                 call stdlib_slacpy( 'A', qsiz, n, q2( 1_ilp, 1_ilp ), ldq2, q( 1_ilp, 1_ilp ),ldq )
              end if
              return
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              if( rho*abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 indxp( k2 ) = j
                 if( j==n )go to 110
              else
                 jlam = j
                 go to 80
              end if
           end do
           80 continue
           j = j + 1_ilp
           if( j>n )go to 100
           if( rho*abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              indxp( k2 ) = j
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( jlam )
              c = z( j )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_slapy2( c, s )
              t = d( j ) - d( jlam )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( j ) = tau
                 z( jlam ) = zero
                 ! record the appropriate givens rotation
                 givptr = givptr + 1_ilp
                 givcol( 1_ilp, givptr ) = indxq( indx( jlam ) )
                 givcol( 2_ilp, givptr ) = indxq( indx( j ) )
                 givnum( 1_ilp, givptr ) = c
                 givnum( 2_ilp, givptr ) = s
                 if( icompq==1_ilp ) then
                    call stdlib_srot( qsiz, q( 1_ilp, indxq( indx( jlam ) ) ), 1_ilp,q( 1_ilp, indxq( indx( j &
                              ) ) ), 1_ilp, c, s )
                 end if
                 t = d( jlam )*c*c + d( j )*s*s
                 d( j ) = d( jlam )*s*s + d( j )*c*c
                 d( jlam ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 90 continue
                 if( k2+i<=n ) then
                    if( d( jlam )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = jlam
                       i = i + 1_ilp
                       go to 90
                    else
                       indxp( k2+i-1 ) = jlam
                    end if
                 else
                    indxp( k2+i-1 ) = jlam
                 end if
                 jlam = j
              else
                 k = k + 1_ilp
                 w( k ) = z( jlam )
                 dlamda( k ) = d( jlam )
                 indxp( k ) = jlam
                 jlam = j
              end if
           end if
           go to 80
           100 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           w( k ) = z( jlam )
           dlamda( k ) = d( jlam )
           indxp( k ) = jlam
           110 continue
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           if( icompq==0_ilp ) then
              do j = 1, n
                 jp = indxp( j )
                 dlamda( j ) = d( jp )
                 perm( j ) = indxq( indx( jp ) )
              end do
           else
              do j = 1, n
                 jp = indxp( j )
                 dlamda( j ) = d( jp )
                 perm( j ) = indxq( indx( jp ) )
                 call stdlib_scopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              if( icompq==0_ilp ) then
                 call stdlib_scopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              else
                 call stdlib_scopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
                 call stdlib_slacpy( 'A', qsiz, n-k, q2( 1_ilp, k+1 ), ldq2,q( 1_ilp, k+1 ), ldq )
              end if
           end if
           return
     end subroutine stdlib_slaed8

     pure module subroutine stdlib_dlaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho,cutpnt, z, dlamda, &
     !! DLAED8 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny element in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
               q2, ldq2, w, perm, givptr,givcol, givnum, indxp, indx, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, icompq, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(dp), intent(out) :: dlamda(*), givnum(2_ilp,*), q2(ldq2,*), w(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: mone = -1.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, imax, j, jlam, jmax, jp, k2, n1, n1p1, n2
           real(dp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( icompq<0_ilp .or. icompq>1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( icompq==1_ilp .and. qsiz<n ) then
              info = -4_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( cutpnt<min( 1_ilp, n ) .or. cutpnt>n ) then
              info = -10_ilp
           else if( ldq2<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED8', -info )
              return
           end if
           ! need to initialize givptr to o here in case of quick exit
           ! to prevent an unspecified code behavior (usually sigfault)
           ! when iwork array on entry to *stedc is not zeroed
           ! (or at least some iwork entries which used in *laed7 for givptr).
           givptr = 0_ilp
           ! quick return if possible
           if( n==0 )return
           n1 = cutpnt
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_dscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1
           t = one / sqrt( two )
           do j = 1, n
              indx( j ) = j
           end do
           call stdlib_dscal( n, t, z, 1_ilp )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = cutpnt + 1, n
              indxq( i ) = indxq( i ) + cutpnt
           end do
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
              w( i ) = z( indxq( i ) )
           end do
           i = 1_ilp
           j = cutpnt + 1_ilp
           call stdlib_dlamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indx )
           do i = 1, n
              d( i ) = dlamda( indx( i ) )
              z( i ) = w( indx( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_idamax( n, z, 1_ilp )
           jmax = stdlib_idamax( n, d, 1_ilp )
           eps = stdlib_dlamch( 'EPSILON' )
           tol = eight*eps*abs( d( jmax ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              if( icompq==0_ilp ) then
                 do j = 1, n
                    perm( j ) = indxq( indx( j ) )
                 end do
              else
                 do j = 1, n
                    perm( j ) = indxq( indx( j ) )
                    call stdlib_dcopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
                 end do
                 call stdlib_dlacpy( 'A', qsiz, n, q2( 1_ilp, 1_ilp ), ldq2, q( 1_ilp, 1_ilp ),ldq )
              end if
              return
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              if( rho*abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 indxp( k2 ) = j
                 if( j==n )go to 110
              else
                 jlam = j
                 go to 80
              end if
           end do
           80 continue
           j = j + 1_ilp
           if( j>n )go to 100
           if( rho*abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              indxp( k2 ) = j
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( jlam )
              c = z( j )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_dlapy2( c, s )
              t = d( j ) - d( jlam )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( j ) = tau
                 z( jlam ) = zero
                 ! record the appropriate givens rotation
                 givptr = givptr + 1_ilp
                 givcol( 1_ilp, givptr ) = indxq( indx( jlam ) )
                 givcol( 2_ilp, givptr ) = indxq( indx( j ) )
                 givnum( 1_ilp, givptr ) = c
                 givnum( 2_ilp, givptr ) = s
                 if( icompq==1_ilp ) then
                    call stdlib_drot( qsiz, q( 1_ilp, indxq( indx( jlam ) ) ), 1_ilp,q( 1_ilp, indxq( indx( j &
                              ) ) ), 1_ilp, c, s )
                 end if
                 t = d( jlam )*c*c + d( j )*s*s
                 d( j ) = d( jlam )*s*s + d( j )*c*c
                 d( jlam ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 90 continue
                 if( k2+i<=n ) then
                    if( d( jlam )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = jlam
                       i = i + 1_ilp
                       go to 90
                    else
                       indxp( k2+i-1 ) = jlam
                    end if
                 else
                    indxp( k2+i-1 ) = jlam
                 end if
                 jlam = j
              else
                 k = k + 1_ilp
                 w( k ) = z( jlam )
                 dlamda( k ) = d( jlam )
                 indxp( k ) = jlam
                 jlam = j
              end if
           end if
           go to 80
           100 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           w( k ) = z( jlam )
           dlamda( k ) = d( jlam )
           indxp( k ) = jlam
           110 continue
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           if( icompq==0_ilp ) then
              do j = 1, n
                 jp = indxp( j )
                 dlamda( j ) = d( jp )
                 perm( j ) = indxq( indx( jp ) )
              end do
           else
              do j = 1, n
                 jp = indxp( j )
                 dlamda( j ) = d( jp )
                 perm( j ) = indxq( indx( jp ) )
                 call stdlib_dcopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              if( icompq==0_ilp ) then
                 call stdlib_dcopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              else
                 call stdlib_dcopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
                 call stdlib_dlacpy( 'A', qsiz, n-k, q2( 1_ilp, k+1 ), ldq2,q( 1_ilp, k+1 ), ldq )
              end if
           end if
           return
     end subroutine stdlib_dlaed8


     pure module subroutine stdlib_claed8( k, n, qsiz, q, ldq, d, rho, cutpnt, z, dlamda,q2, ldq2, w, &
     !! CLAED8 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny element in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
               indxp, indx, indxq, perm, givptr,givcol, givnum, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(sp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), z(*)
           real(sp), intent(out) :: dlamda(*), givnum(2_ilp,*), w(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: q2(ldq2,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: mone = -1.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, imax, j, jlam, jmax, jp, k2, n1, n1p1, n2
           real(sp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( cutpnt<min( 1_ilp, n ) .or. cutpnt>n ) then
              info = -8_ilp
           else if( ldq2<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAED8', -info )
              return
           end if
           ! need to initialize givptr to o here in case of quick exit
           ! to prevent an unspecified code behavior (usually sigfault)
           ! when iwork array on entry to *stedc is not zeroed
           ! (or at least some iwork entries which used in *laed7 for givptr).
           givptr = 0_ilp
           ! quick return if possible
           if( n==0 )return
           n1 = cutpnt
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_sscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1
           t = one / sqrt( two )
           do j = 1, n
              indx( j ) = j
           end do
           call stdlib_sscal( n, t, z, 1_ilp )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = cutpnt + 1, n
              indxq( i ) = indxq( i ) + cutpnt
           end do
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
              w( i ) = z( indxq( i ) )
           end do
           i = 1_ilp
           j = cutpnt + 1_ilp
           call stdlib_slamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indx )
           do i = 1, n
              d( i ) = dlamda( indx( i ) )
              z( i ) = w( indx( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_isamax( n, z, 1_ilp )
           jmax = stdlib_isamax( n, d, 1_ilp )
           eps = stdlib_slamch( 'EPSILON' )
           tol = eight*eps*abs( d( jmax ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! -- except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              do j = 1, n
                 perm( j ) = indxq( indx( j ) )
                 call stdlib_ccopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
              end do
              call stdlib_clacpy( 'A', qsiz, n, q2( 1_ilp, 1_ilp ), ldq2, q( 1_ilp, 1_ilp ), ldq )
              return
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              if( rho*abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 indxp( k2 ) = j
                 if( j==n )go to 100
              else
                 jlam = j
                 go to 70
              end if
           end do
           70 continue
           j = j + 1_ilp
           if( j>n )go to 90
           if( rho*abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              indxp( k2 ) = j
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( jlam )
              c = z( j )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_slapy2( c, s )
              t = d( j ) - d( jlam )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( j ) = tau
                 z( jlam ) = zero
                 ! record the appropriate givens rotation
                 givptr = givptr + 1_ilp
                 givcol( 1_ilp, givptr ) = indxq( indx( jlam ) )
                 givcol( 2_ilp, givptr ) = indxq( indx( j ) )
                 givnum( 1_ilp, givptr ) = c
                 givnum( 2_ilp, givptr ) = s
                 call stdlib_csrot( qsiz, q( 1_ilp, indxq( indx( jlam ) ) ), 1_ilp,q( 1_ilp, indxq( indx( j ) &
                           ) ), 1_ilp, c, s )
                 t = d( jlam )*c*c + d( j )*s*s
                 d( j ) = d( jlam )*s*s + d( j )*c*c
                 d( jlam ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 80 continue
                 if( k2+i<=n ) then
                    if( d( jlam )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = jlam
                       i = i + 1_ilp
                       go to 80
                    else
                       indxp( k2+i-1 ) = jlam
                    end if
                 else
                    indxp( k2+i-1 ) = jlam
                 end if
                 jlam = j
              else
                 k = k + 1_ilp
                 w( k ) = z( jlam )
                 dlamda( k ) = d( jlam )
                 indxp( k ) = jlam
                 jlam = j
              end if
           end if
           go to 70
           90 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           w( k ) = z( jlam )
           dlamda( k ) = d( jlam )
           indxp( k ) = jlam
           100 continue
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           do j = 1, n
              jp = indxp( j )
              dlamda( j ) = d( jp )
              perm( j ) = indxq( indx( jp ) )
              call stdlib_ccopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
           end do
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              call stdlib_scopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              call stdlib_clacpy( 'A', qsiz, n-k, q2( 1_ilp, k+1 ), ldq2, q( 1_ilp, k+1 ),ldq )
           end if
           return
     end subroutine stdlib_claed8

     pure module subroutine stdlib_zlaed8( k, n, qsiz, q, ldq, d, rho, cutpnt, z, dlamda,q2, ldq2, w, &
     !! ZLAED8 merges the two sets of eigenvalues together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! eigenvalues are close together or if there is a tiny element in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
               indxp, indx, indxq, perm, givptr,givcol, givnum, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: cutpnt, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(dp), intent(inout) :: rho
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), z(*)
           real(dp), intent(out) :: dlamda(*), givnum(2_ilp,*), w(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: q2(ldq2,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: mone = -1.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, imax, j, jlam, jmax, jp, k2, n1, n1p1, n2
           real(dp) :: c, eps, s, t, tau, tol
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( qsiz<n ) then
              info = -3_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( cutpnt<min( 1_ilp, n ) .or. cutpnt>n ) then
              info = -8_ilp
           else if( ldq2<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAED8', -info )
              return
           end if
           ! need to initialize givptr to o here in case of quick exit
           ! to prevent an unspecified code behavior (usually sigfault)
           ! when iwork array on entry to *stedc is not zeroed
           ! (or at least some iwork entries which used in *laed7 for givptr).
           givptr = 0_ilp
           ! quick return if possible
           if( n==0 )return
           n1 = cutpnt
           n2 = n - n1
           n1p1 = n1 + 1_ilp
           if( rho<zero ) then
              call stdlib_dscal( n2, mone, z( n1p1 ), 1_ilp )
           end if
           ! normalize z so that norm(z) = 1
           t = one / sqrt( two )
           do j = 1, n
              indx( j ) = j
           end do
           call stdlib_dscal( n, t, z, 1_ilp )
           rho = abs( two*rho )
           ! sort the eigenvalues into increasing order
           do i = cutpnt + 1, n
              indxq( i ) = indxq( i ) + cutpnt
           end do
           do i = 1, n
              dlamda( i ) = d( indxq( i ) )
              w( i ) = z( indxq( i ) )
           end do
           i = 1_ilp
           j = cutpnt + 1_ilp
           call stdlib_dlamrg( n1, n2, dlamda, 1_ilp, 1_ilp, indx )
           do i = 1, n
              d( i ) = dlamda( indx( i ) )
              z( i ) = w( indx( i ) )
           end do
           ! calculate the allowable deflation tolerance
           imax = stdlib_idamax( n, z, 1_ilp )
           jmax = stdlib_idamax( n, d, 1_ilp )
           eps = stdlib_dlamch( 'EPSILON' )
           tol = eight*eps*abs( d( jmax ) )
           ! if the rank-1 modifier is small enough, no more needs to be done
           ! -- except to reorganize q so that its columns correspond with the
           ! elements in d.
           if( rho*abs( z( imax ) )<=tol ) then
              k = 0_ilp
              do j = 1, n
                 perm( j ) = indxq( indx( j ) )
                 call stdlib_zcopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
              end do
              call stdlib_zlacpy( 'A', qsiz, n, q2( 1_ilp, 1_ilp ), ldq2, q( 1_ilp, 1_ilp ), ldq )
              return
           end if
           ! if there are multiple eigenvalues then the problem deflates.  here
           ! the number of equal eigenvalues are found.  as each equal
           ! eigenvalue is found, an elementary reflector is computed to rotate
           ! the corresponding eigensubspace so that the corresponding
           ! components of z are zero in this new basis.
           k = 0_ilp
           k2 = n + 1_ilp
           do j = 1, n
              if( rho*abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 indxp( k2 ) = j
                 if( j==n )go to 100
              else
                 jlam = j
                 go to 70
              end if
           end do
           70 continue
           j = j + 1_ilp
           if( j>n )go to 90
           if( rho*abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              indxp( k2 ) = j
           else
              ! check if eigenvalues are close enough to allow deflation.
              s = z( jlam )
              c = z( j )
              ! find sqrt(a**2+b**2) without overflow or
              ! destructive underflow.
              tau = stdlib_dlapy2( c, s )
              t = d( j ) - d( jlam )
              c = c / tau
              s = -s / tau
              if( abs( t*c*s )<=tol ) then
                 ! deflation is possible.
                 z( j ) = tau
                 z( jlam ) = zero
                 ! record the appropriate givens rotation
                 givptr = givptr + 1_ilp
                 givcol( 1_ilp, givptr ) = indxq( indx( jlam ) )
                 givcol( 2_ilp, givptr ) = indxq( indx( j ) )
                 givnum( 1_ilp, givptr ) = c
                 givnum( 2_ilp, givptr ) = s
                 call stdlib_zdrot( qsiz, q( 1_ilp, indxq( indx( jlam ) ) ), 1_ilp,q( 1_ilp, indxq( indx( j ) &
                           ) ), 1_ilp, c, s )
                 t = d( jlam )*c*c + d( j )*s*s
                 d( j ) = d( jlam )*s*s + d( j )*c*c
                 d( jlam ) = t
                 k2 = k2 - 1_ilp
                 i = 1_ilp
                 80 continue
                 if( k2+i<=n ) then
                    if( d( jlam )<d( indxp( k2+i ) ) ) then
                       indxp( k2+i-1 ) = indxp( k2+i )
                       indxp( k2+i ) = jlam
                       i = i + 1_ilp
                       go to 80
                    else
                       indxp( k2+i-1 ) = jlam
                    end if
                 else
                    indxp( k2+i-1 ) = jlam
                 end if
                 jlam = j
              else
                 k = k + 1_ilp
                 w( k ) = z( jlam )
                 dlamda( k ) = d( jlam )
                 indxp( k ) = jlam
                 jlam = j
              end if
           end if
           go to 70
           90 continue
           ! record the last eigenvalue.
           k = k + 1_ilp
           w( k ) = z( jlam )
           dlamda( k ) = d( jlam )
           indxp( k ) = jlam
           100 continue
           ! sort the eigenvalues and corresponding eigenvectors into dlamda
           ! and q2 respectively.  the eigenvalues/vectors which were not
           ! deflated go into the first k slots of dlamda and q2 respectively,
           ! while those which were deflated go into the last n - k slots.
           do j = 1, n
              jp = indxp( j )
              dlamda( j ) = d( jp )
              perm( j ) = indxq( indx( jp ) )
              call stdlib_zcopy( qsiz, q( 1_ilp, perm( j ) ), 1_ilp, q2( 1_ilp, j ), 1_ilp )
           end do
           ! the deflated eigenvalues and their corresponding vectors go back
           ! into the last n - k slots of d and q respectively.
           if( k<n ) then
              call stdlib_dcopy( n-k, dlamda( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              call stdlib_zlacpy( 'A', qsiz, n-k, q2( 1_ilp, k+1 ), ldq2, q( 1_ilp, k+1 ),ldq )
           end if
           return
     end subroutine stdlib_zlaed8




     pure module subroutine stdlib_slaed9( k, kstart, kstop, n, d, q, ldq, rho, dlamda, w,s, lds, info )
     !! SLAED9 finds the roots of the secular equation, as defined by the
     !! values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
     !! appropriate calls to SLAED4 and then stores the new matrix of
     !! eigenvectors for use in calculating the next level of Z vectors.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, kstart, kstop, ldq, lds, n
           real(sp), intent(in) :: rho
           ! Array Arguments 
           real(sp), intent(out) :: d(*), q(ldq,*), s(lds,*)
           real(sp), intent(inout) :: dlamda(*), w(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( k<0_ilp ) then
              info = -1_ilp
           else if( kstart<1_ilp .or. kstart>max( 1_ilp, k ) ) then
              info = -2_ilp
           else if( max( 1_ilp, kstop )<kstart .or. kstop>max( 1_ilp, k ) )then
              info = -3_ilp
           else if( n<k ) then
              info = -4_ilp
           else if( ldq<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( lds<max( 1_ilp, k ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAED9', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! modify values dlamda(i) to make sure all dlamda(i)-dlamda(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dlamda(i) by 2*dlamda(i)-dlamda(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dlamda(i) if it is 1; this makes the subsequent
           ! subtractions dlamda(i)-dlamda(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dlamda(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dlamda(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, n
              dlamda( i ) = stdlib_slamc3( dlamda( i ), dlamda( i ) ) - dlamda( i )
           end do
           do j = kstart, kstop
              call stdlib_slaed4( k, j, dlamda, w, q( 1_ilp, j ), rho, d( j ), info )
              ! if the zero finder fails, the computation is terminated.
              if( info/=0 )go to 120
           end do
           if( k==1_ilp .or. k==2_ilp ) then
              do i = 1, k
                 do j = 1, k
                    s( j, i ) = q( j, i )
                 end do
              end do
              go to 120
           end if
           ! compute updated w.
           call stdlib_scopy( k, w, 1_ilp, s, 1_ilp )
           ! initialize w(i) = q(i,i)
           call stdlib_scopy( k, q, ldq+1, w, 1_ilp )
           do j = 1, k
              do i = 1, j - 1
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
              do i = j + 1, k
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
           end do
           do i = 1, k
              w( i ) = sign( sqrt( -w( i ) ), s( i, 1_ilp ) )
           end do
           ! compute eigenvectors of the modified rank-1 modification.
           do j = 1, k
              do i = 1, k
                 q( i, j ) = w( i ) / q( i, j )
              end do
              temp = stdlib_snrm2( k, q( 1_ilp, j ), 1_ilp )
              do i = 1, k
                 s( i, j ) = q( i, j ) / temp
              end do
           end do
           120 continue
           return
     end subroutine stdlib_slaed9

     pure module subroutine stdlib_dlaed9( k, kstart, kstop, n, d, q, ldq, rho, dlamda, w,s, lds, info )
     !! DLAED9 finds the roots of the secular equation, as defined by the
     !! values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
     !! appropriate calls to DLAED4 and then stores the new matrix of
     !! eigenvectors for use in calculating the next level of Z vectors.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, kstart, kstop, ldq, lds, n
           real(dp), intent(in) :: rho
           ! Array Arguments 
           real(dp), intent(out) :: d(*), q(ldq,*), s(lds,*)
           real(dp), intent(inout) :: dlamda(*), w(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( k<0_ilp ) then
              info = -1_ilp
           else if( kstart<1_ilp .or. kstart>max( 1_ilp, k ) ) then
              info = -2_ilp
           else if( max( 1_ilp, kstop )<kstart .or. kstop>max( 1_ilp, k ) )then
              info = -3_ilp
           else if( n<k ) then
              info = -4_ilp
           else if( ldq<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( lds<max( 1_ilp, k ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAED9', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! modify values dlamda(i) to make sure all dlamda(i)-dlamda(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dlamda(i) by 2*dlamda(i)-dlamda(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dlamda(i) if it is 1; this makes the subsequent
           ! subtractions dlamda(i)-dlamda(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dlamda(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dlamda(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, n
              dlamda( i ) = stdlib_dlamc3( dlamda( i ), dlamda( i ) ) - dlamda( i )
           end do
           do j = kstart, kstop
              call stdlib_dlaed4( k, j, dlamda, w, q( 1_ilp, j ), rho, d( j ), info )
              ! if the zero finder fails, the computation is terminated.
              if( info/=0 )go to 120
           end do
           if( k==1_ilp .or. k==2_ilp ) then
              do i = 1, k
                 do j = 1, k
                    s( j, i ) = q( j, i )
                 end do
              end do
              go to 120
           end if
           ! compute updated w.
           call stdlib_dcopy( k, w, 1_ilp, s, 1_ilp )
           ! initialize w(i) = q(i,i)
           call stdlib_dcopy( k, q, ldq+1, w, 1_ilp )
           do j = 1, k
              do i = 1, j - 1
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
              do i = j + 1, k
                 w( i ) = w( i )*( q( i, j ) / ( dlamda( i )-dlamda( j ) ) )
              end do
           end do
           do i = 1, k
              w( i ) = sign( sqrt( -w( i ) ), s( i, 1_ilp ) )
           end do
           ! compute eigenvectors of the modified rank-1 modification.
           do j = 1, k
              do i = 1, k
                 q( i, j ) = w( i ) / q( i, j )
              end do
              temp = stdlib_dnrm2( k, q( 1_ilp, j ), 1_ilp )
              do i = 1, k
                 s( i, j ) = q( i, j ) / temp
              end do
           end do
           120 continue
           return
     end subroutine stdlib_dlaed9



end submodule stdlib_lapack_eigv_tridiag
