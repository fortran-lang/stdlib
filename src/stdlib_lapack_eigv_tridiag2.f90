submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_tridiag2
  implicit none


  contains

     pure module subroutine stdlib_slamrg( n1, n2, a, strd1, strd2, index )
     !! SLAMRG will create a permutation list which will merge the elements
     !! of A (which is composed of two independently sorted sets) into a
     !! single set which is sorted in ascending order.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n1, n2, strd1, strd2
           ! Array Arguments 
           integer(ilp), intent(out) :: index(*)
           real(sp), intent(in) :: a(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ind1, ind2, n1sv, n2sv
           ! Executable Statements 
           n1sv = n1
           n2sv = n2
           if( strd1>0_ilp ) then
              ind1 = 1_ilp
           else
              ind1 = n1
           end if
           if( strd2>0_ilp ) then
              ind2 = 1_ilp + n1
           else
              ind2 = n1 + n2
           end if
           i = 1_ilp
           ! while ( (n1sv > 0)
           10 continue
           if( n1sv>0_ilp .and. n2sv>0_ilp ) then
              if( a( ind1 )<=a( ind2 ) ) then
                 index( i ) = ind1
                 i = i + 1_ilp
                 ind1 = ind1 + strd1
                 n1sv = n1sv - 1_ilp
              else
                 index( i ) = ind2
                 i = i + 1_ilp
                 ind2 = ind2 + strd2
                 n2sv = n2sv - 1_ilp
              end if
              go to 10
           end if
           ! end while
           if( n1sv==0_ilp ) then
              do n1sv = 1, n2sv
                 index( i ) = ind2
                 i = i + 1_ilp
                 ind2 = ind2 + strd2
              end do
           else
           ! n2sv == 0
              do n2sv = 1, n1sv
                 index( i ) = ind1
                 i = i + 1_ilp
                 ind1 = ind1 + strd1
              end do
           end if
           return
     end subroutine stdlib_slamrg

     pure module subroutine stdlib_dlamrg( n1, n2, a, dtrd1, dtrd2, index )
     !! DLAMRG will create a permutation list which will merge the elements
     !! of A (which is composed of two independently sorted sets) into a
     !! single set which is sorted in ascending order.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: dtrd1, dtrd2, n1, n2
           ! Array Arguments 
           integer(ilp), intent(out) :: index(*)
           real(dp), intent(in) :: a(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ind1, ind2, n1sv, n2sv
           ! Executable Statements 
           n1sv = n1
           n2sv = n2
           if( dtrd1>0_ilp ) then
              ind1 = 1_ilp
           else
              ind1 = n1
           end if
           if( dtrd2>0_ilp ) then
              ind2 = 1_ilp + n1
           else
              ind2 = n1 + n2
           end if
           i = 1_ilp
           ! while ( (n1sv > 0)
           10 continue
           if( n1sv>0_ilp .and. n2sv>0_ilp ) then
              if( a( ind1 )<=a( ind2 ) ) then
                 index( i ) = ind1
                 i = i + 1_ilp
                 ind1 = ind1 + dtrd1
                 n1sv = n1sv - 1_ilp
              else
                 index( i ) = ind2
                 i = i + 1_ilp
                 ind2 = ind2 + dtrd2
                 n2sv = n2sv - 1_ilp
              end if
              go to 10
           end if
           ! end while
           if( n1sv==0_ilp ) then
              do n1sv = 1, n2sv
                 index( i ) = ind2
                 i = i + 1_ilp
                 ind2 = ind2 + dtrd2
              end do
           else
           ! n2sv == 0
              do n2sv = 1, n1sv
                 index( i ) = ind1
                 i = i + 1_ilp
                 ind1 = ind1 + dtrd1
              end do
           end if
           return
     end subroutine stdlib_dlamrg




     pure module subroutine stdlib_slaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum,&
     !! SLAEDA computes the Z vector corresponding to the merge step in the
     !! CURLVLth step of the merge process with TLVLS steps for the CURPBMth
     !! problem.
                q, qptr, z, ztemp, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, n, tlvls
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
           real(sp), intent(in) :: givnum(2_ilp,*), q(*)
           real(sp), intent(out) :: z(*), ztemp(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: bsiz1, bsiz2, curr, i, k, mid, psiz1, psiz2, ptr, zptr1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAEDA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine location of first number in second half.
           mid = n / 2_ilp + 1_ilp
           ! gather last/first rows of appropriate eigenblocks into center of z
           ptr = 1_ilp
           ! determine location of lowest level subproblem in the full storage
           ! scheme
           curr = ptr + curpbm*2_ilp**curlvl + 2_ilp**( curlvl-1 ) - 1_ilp
           ! determine size of these matrices.  we add half to the value of
           ! the sqrt in case the machine underestimates one of these square
           ! roots.
           bsiz1 = int( half+sqrt( real( qptr( curr+1 )-qptr( curr ),KIND=sp) ),KIND=ilp)
           bsiz2 = int( half+sqrt( real( qptr( curr+2 )-qptr( curr+1 ),KIND=sp) ),KIND=ilp)
                     
           do k = 1, mid - bsiz1 - 1
              z( k ) = zero
           end do
           call stdlib_scopy( bsiz1, q( qptr( curr )+bsiz1-1 ), bsiz1,z( mid-bsiz1 ), 1_ilp )
           call stdlib_scopy( bsiz2, q( qptr( curr+1 ) ), bsiz2, z( mid ), 1_ilp )
           do k = mid + bsiz2, n
              z( k ) = zero
           end do
           ! loop through remaining levels 1 -> curlvl applying the givens
           ! rotations and permutation and then multiplying the center matrices
           ! against the current z.
           ptr = 2_ilp**tlvls + 1_ilp
           loop_70: do k = 1, curlvl - 1
              curr = ptr + curpbm*2_ilp**( curlvl-k ) + 2_ilp**( curlvl-k-1 ) - 1_ilp
              psiz1 = prmptr( curr+1 ) - prmptr( curr )
              psiz2 = prmptr( curr+2 ) - prmptr( curr+1 )
              zptr1 = mid - psiz1
             ! apply givens at curr and curr+1
              do i = givptr( curr ), givptr( curr+1 ) - 1
                 call stdlib_srot( 1_ilp, z( zptr1+givcol( 1_ilp, i )-1_ilp ), 1_ilp,z( zptr1+givcol( 2_ilp, i )-1_ilp ), &
                           1_ilp, givnum( 1_ilp, i ),givnum( 2_ilp, i ) )
              end do
              do i = givptr( curr+1 ), givptr( curr+2 ) - 1
                 call stdlib_srot( 1_ilp, z( mid-1+givcol( 1_ilp, i ) ), 1_ilp,z( mid-1+givcol( 2_ilp, i ) ), 1_ilp, &
                           givnum( 1_ilp, i ),givnum( 2_ilp, i ) )
              end do
              psiz1 = prmptr( curr+1 ) - prmptr( curr )
              psiz2 = prmptr( curr+2 ) - prmptr( curr+1 )
              do i = 0, psiz1 - 1
                 ztemp( i+1 ) = z( zptr1+perm( prmptr( curr )+i )-1_ilp )
              end do
              do i = 0, psiz2 - 1
                 ztemp( psiz1+i+1 ) = z( mid+perm( prmptr( curr+1 )+i )-1_ilp )
              end do
              ! multiply blocks at curr and curr+1
              ! determine size of these matrices.  we add half to the value of
              ! the sqrt in case the machine underestimates one of these
              ! square roots.
              bsiz1 = int( half+sqrt( real( qptr( curr+1 )-qptr( curr ),KIND=sp) ),KIND=ilp)
                        
              bsiz2 = int( half+sqrt( real( qptr( curr+2 )-qptr( curr+1 ),KIND=sp) ),KIND=ilp)
                        
              if( bsiz1>0_ilp ) then
                 call stdlib_sgemv( 'T', bsiz1, bsiz1, one, q( qptr( curr ) ),bsiz1, ztemp( 1_ilp ), &
                           1_ilp, zero, z( zptr1 ), 1_ilp )
              end if
              call stdlib_scopy( psiz1-bsiz1, ztemp( bsiz1+1 ), 1_ilp, z( zptr1+bsiz1 ),1_ilp )
              if( bsiz2>0_ilp ) then
                 call stdlib_sgemv( 'T', bsiz2, bsiz2, one, q( qptr( curr+1 ) ),bsiz2, ztemp( &
                           psiz1+1 ), 1_ilp, zero, z( mid ), 1_ilp )
              end if
              call stdlib_scopy( psiz2-bsiz2, ztemp( psiz1+bsiz2+1 ), 1_ilp,z( mid+bsiz2 ), 1_ilp )
                        
              ptr = ptr + 2_ilp**( tlvls-k )
           end do loop_70
           return
     end subroutine stdlib_slaeda

     pure module subroutine stdlib_dlaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum,&
     !! DLAEDA computes the Z vector corresponding to the merge step in the
     !! CURLVLth step of the merge process with TLVLS steps for the CURPBMth
     !! problem.
                q, qptr, z, ztemp, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: curlvl, curpbm, n, tlvls
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
           real(dp), intent(in) :: givnum(2_ilp,*), q(*)
           real(dp), intent(out) :: z(*), ztemp(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: bsiz1, bsiz2, curr, i, k, mid, psiz1, psiz2, ptr, zptr1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAEDA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine location of first number in second half.
           mid = n / 2_ilp + 1_ilp
           ! gather last/first rows of appropriate eigenblocks into center of z
           ptr = 1_ilp
           ! determine location of lowest level subproblem in the full storage
           ! scheme
           curr = ptr + curpbm*2_ilp**curlvl + 2_ilp**( curlvl-1 ) - 1_ilp
           ! determine size of these matrices.  we add half to the value of
           ! the sqrt in case the machine underestimates one of these square
           ! roots.
           bsiz1 = int( half+sqrt( real( qptr( curr+1 )-qptr( curr ),KIND=dp) ),KIND=ilp)
           bsiz2 = int( half+sqrt( real( qptr( curr+2 )-qptr( curr+1 ),KIND=dp) ),KIND=ilp)
                     
           do k = 1, mid - bsiz1 - 1
              z( k ) = zero
           end do
           call stdlib_dcopy( bsiz1, q( qptr( curr )+bsiz1-1 ), bsiz1,z( mid-bsiz1 ), 1_ilp )
           call stdlib_dcopy( bsiz2, q( qptr( curr+1 ) ), bsiz2, z( mid ), 1_ilp )
           do k = mid + bsiz2, n
              z( k ) = zero
           end do
           ! loop through remaining levels 1 -> curlvl applying the givens
           ! rotations and permutation and then multiplying the center matrices
           ! against the current z.
           ptr = 2_ilp**tlvls + 1_ilp
           loop_70: do k = 1, curlvl - 1
              curr = ptr + curpbm*2_ilp**( curlvl-k ) + 2_ilp**( curlvl-k-1 ) - 1_ilp
              psiz1 = prmptr( curr+1 ) - prmptr( curr )
              psiz2 = prmptr( curr+2 ) - prmptr( curr+1 )
              zptr1 = mid - psiz1
             ! apply givens at curr and curr+1
              do i = givptr( curr ), givptr( curr+1 ) - 1
                 call stdlib_drot( 1_ilp, z( zptr1+givcol( 1_ilp, i )-1_ilp ), 1_ilp,z( zptr1+givcol( 2_ilp, i )-1_ilp ), &
                           1_ilp, givnum( 1_ilp, i ),givnum( 2_ilp, i ) )
              end do
              do i = givptr( curr+1 ), givptr( curr+2 ) - 1
                 call stdlib_drot( 1_ilp, z( mid-1+givcol( 1_ilp, i ) ), 1_ilp,z( mid-1+givcol( 2_ilp, i ) ), 1_ilp, &
                           givnum( 1_ilp, i ),givnum( 2_ilp, i ) )
              end do
              psiz1 = prmptr( curr+1 ) - prmptr( curr )
              psiz2 = prmptr( curr+2 ) - prmptr( curr+1 )
              do i = 0, psiz1 - 1
                 ztemp( i+1 ) = z( zptr1+perm( prmptr( curr )+i )-1_ilp )
              end do
              do i = 0, psiz2 - 1
                 ztemp( psiz1+i+1 ) = z( mid+perm( prmptr( curr+1 )+i )-1_ilp )
              end do
              ! multiply blocks at curr and curr+1
              ! determine size of these matrices.  we add half to the value of
              ! the sqrt in case the machine underestimates one of these
              ! square roots.
              bsiz1 = int( half+sqrt( real( qptr( curr+1 )-qptr( curr ),KIND=dp) ),KIND=ilp)
                        
              bsiz2 = int( half+sqrt( real( qptr( curr+2 )-qptr( curr+1 ),KIND=dp) ),KIND=ilp)
                        
              if( bsiz1>0_ilp ) then
                 call stdlib_dgemv( 'T', bsiz1, bsiz1, one, q( qptr( curr ) ),bsiz1, ztemp( 1_ilp ), &
                           1_ilp, zero, z( zptr1 ), 1_ilp )
              end if
              call stdlib_dcopy( psiz1-bsiz1, ztemp( bsiz1+1 ), 1_ilp, z( zptr1+bsiz1 ),1_ilp )
              if( bsiz2>0_ilp ) then
                 call stdlib_dgemv( 'T', bsiz2, bsiz2, one, q( qptr( curr+1 ) ),bsiz2, ztemp( &
                           psiz1+1 ), 1_ilp, zero, z( mid ), 1_ilp )
              end if
              call stdlib_dcopy( psiz2-bsiz2, ztemp( psiz1+bsiz2+1 ), 1_ilp,z( mid+bsiz2 ), 1_ilp )
                        
              ptr = ptr + 2_ilp**( tlvls-k )
           end do loop_70
           return
     end subroutine stdlib_dlaeda




     pure module subroutine stdlib_slarra( n, d, e, e2, spltol, tnrm,nsplit, isplit, info )
     !! Compute the splitting points with threshold SPLTOL.
     !! SLARRA sets any "small" off-diagonal elements to zero.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, nsplit
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: spltol, tnrm
           ! Array Arguments 
           integer(ilp), intent(out) :: isplit(*)
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: e(*), e2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: eabs, tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! compute splitting points
           nsplit = 1_ilp
           if(spltol<zero) then
              ! criterion based on absolute off-diagonal value
              tmp1 = abs(spltol)* tnrm
              do i = 1, n-1
                 eabs = abs( e(i) )
                 if( eabs <= tmp1) then
                    e(i) = zero
                    e2(i) = zero
                    isplit( nsplit ) = i
                    nsplit = nsplit + 1_ilp
                 end if
              end do
           else
              ! criterion that guarantees relative accuracy
              do i = 1, n-1
                 eabs = abs( e(i) )
                 if( eabs <= spltol * sqrt(abs(d(i)))*sqrt(abs(d(i+1))) )then
                    e(i) = zero
                    e2(i) = zero
                    isplit( nsplit ) = i
                    nsplit = nsplit + 1_ilp
                 end if
              end do
           endif
           isplit( nsplit ) = n
           return
     end subroutine stdlib_slarra

     pure module subroutine stdlib_dlarra( n, d, e, e2, spltol, tnrm,nsplit, isplit, info )
     !! Compute the splitting points with threshold SPLTOL.
     !! DLARRA sets any "small" off-diagonal elements to zero.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, nsplit
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: spltol, tnrm
           ! Array Arguments 
           integer(ilp), intent(out) :: isplit(*)
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: e(*), e2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: eabs, tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! compute splitting points
           nsplit = 1_ilp
           if(spltol<zero) then
              ! criterion based on absolute off-diagonal value
              tmp1 = abs(spltol)* tnrm
              do i = 1, n-1
                 eabs = abs( e(i) )
                 if( eabs <= tmp1) then
                    e(i) = zero
                    e2(i) = zero
                    isplit( nsplit ) = i
                    nsplit = nsplit + 1_ilp
                 end if
              end do
           else
              ! criterion that guarantees relative accuracy
              do i = 1, n-1
                 eabs = abs( e(i) )
                 if( eabs <= spltol * sqrt(abs(d(i)))*sqrt(abs(d(i+1))) )then
                    e(i) = zero
                    e2(i) = zero
                    isplit( nsplit ) = i
                    nsplit = nsplit + 1_ilp
                 end if
              end do
           endif
           isplit( nsplit ) = n
           return
     end subroutine stdlib_dlarra




     pure module subroutine stdlib_slarrb( n, d, lld, ifirst, ilast, rtol1,rtol2, offset, w, wgap, werr, &
     !! Given the relatively robust representation(RRR) L D L^T, SLARRB:
     !! does "limited" bisection to refine the eigenvalues of L D L^T,
     !! W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
     !! guesses for these eigenvalues are input in W, the corresponding estimate
     !! of the error in these guesses and their gaps are input in WERR
     !! and WGAP, respectively. During bisection, intervals
     !! [left, right] are maintained by storing their mid-points and
     !! semi-widths in the arrays W and WERR respectively.
               work, iwork,pivmin, spdiam, twist, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ifirst, ilast, n, offset, twist
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: pivmin, rtol1, rtol2, spdiam
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: d(*), lld(*)
           real(sp), intent(inout) :: w(*), werr(*), wgap(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           integer(ilp) :: maxitr
           ! Local Scalars 
           integer(ilp) :: i, i1, ii, ip, iter, k, negcnt, next, nint, olnint, prev, r
           real(sp) :: back, cvrgd, gap, left, lgap, mid, mnwdth, rgap, right, tmp, width
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           maxitr = int( ( log( spdiam+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           mnwdth = two * pivmin
           r = twist
           if((r<1_ilp).or.(r>n)) r = n
           ! initialize unconverged intervals in [ work(2*i-1), work(2*i) ].
           ! the sturm count, count( work(2*i-1) ) is arranged to be i-1, while
           ! count( work(2*i) ) is stored in iwork( 2*i ). the integer iwork( 2*i-1 )
           ! for an unconverged interval is set to the index of the next unconverged
           ! interval, and is -1 or 0 for a converged interval. thus a linked
           ! list of unconverged intervals is set up.
           i1 = ifirst
           ! the number of unconverged intervals
           nint = 0_ilp
           ! the last unconverged interval found
           prev = 0_ilp
           rgap = wgap( i1-offset )
           loop_75: do i = i1, ilast
              k = 2_ilp*i
              ii = i - offset
              left = w( ii ) - werr( ii )
              right = w( ii ) + werr( ii )
              lgap = rgap
              rgap = wgap( ii )
              gap = min( lgap, rgap )
              ! make sure that [left,right] contains the desired eigenvalue
              ! compute negcount from dstqds facto l+d+l+^t = l d l^t - left
              ! do while( negcnt(left)>i-1 )
              back = werr( ii )
              20 continue
              negcnt = stdlib_slaneg( n, d, lld, left, pivmin, r )
              if( negcnt>i-1 ) then
                 left = left - back
                 back = two*back
                 go to 20
              end if
              ! do while( negcnt(right)<i )
              ! compute negcount from dstqds facto l+d+l+^t = l d l^t - right
              back = werr( ii )
              50 continue
              negcnt = stdlib_slaneg( n, d, lld, right, pivmin, r )
               if( negcnt<i ) then
                  right = right + back
                  back = two*back
                  go to 50
               end if
              width = half*abs( left - right )
              tmp = max( abs( left ), abs( right ) )
              cvrgd = max(rtol1*gap,rtol2*tmp)
              if( width<=cvrgd .or. width<=mnwdth ) then
                 ! this interval has already converged and does not need refinement.
                 ! (note that the gaps might change through refining the
                  ! eigenvalues, however, they can only get bigger.)
                 ! remove it from the list.
                 iwork( k-1 ) = -1_ilp
                 ! make sure that i1 always points to the first unconverged interval
                 if((i==i1).and.(i<ilast)) i1 = i + 1_ilp
                 if((prev>=i1).and.(i<=ilast)) iwork( 2_ilp*prev-1 ) = i + 1_ilp
              else
                 ! unconverged interval found
                 prev = i
                 nint = nint + 1_ilp
                 iwork( k-1 ) = i + 1_ilp
                 iwork( k ) = negcnt
              end if
              work( k-1 ) = left
              work( k ) = right
           end do loop_75
           ! do while( nint>0 ), i.e. there are still unconverged intervals
           ! and while (iter<maxitr)
           iter = 0_ilp
           80 continue
           prev = i1 - 1_ilp
           i = i1
           olnint = nint
           loop_100: do ip = 1, olnint
              k = 2_ilp*i
              ii = i - offset
              rgap = wgap( ii )
              lgap = rgap
              if(ii>1_ilp) lgap = wgap( ii-1 )
              gap = min( lgap, rgap )
              next = iwork( k-1 )
              left = work( k-1 )
              right = work( k )
              mid = half*( left + right )
              ! semiwidth of interval
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              cvrgd = max(rtol1*gap,rtol2*tmp)
              if( ( width<=cvrgd ) .or. ( width<=mnwdth ).or.( iter==maxitr ) )then
                 ! reduce number of unconverged intervals
                 nint = nint - 1_ilp
                 ! mark interval as converged.
                 iwork( k-1 ) = 0_ilp
                 if( i1==i ) then
                    i1 = next
                 else
                    ! prev holds the last unconverged interval previously examined
                    if(prev>=i1) iwork( 2_ilp*prev-1 ) = next
                 end if
                 i = next
                 cycle loop_100
              end if
              prev = i
              ! perform one bisection step
              negcnt = stdlib_slaneg( n, d, lld, mid, pivmin, r )
              if( negcnt<=i-1 ) then
                 work( k-1 ) = mid
              else
                 work( k ) = mid
              end if
              i = next
           end do loop_100
           iter = iter + 1_ilp
           ! do another loop if there are still unconverged intervals
           ! however, in the last iteration, all intervals are accepted
           ! since this is the best we can do.
           if( ( nint>0 ).and.(iter<=maxitr) ) go to 80
           ! at this point, all the intervals have converged
           do i = ifirst, ilast
              k = 2_ilp*i
              ii = i - offset
              ! all intervals marked by '0' have been refined.
              if( iwork( k-1 )==0_ilp ) then
                 w( ii ) = half*( work( k-1 )+work( k ) )
                 werr( ii ) = work( k ) - w( ii )
              end if
           end do
           do i = ifirst+1, ilast
              k = 2_ilp*i
              ii = i - offset
              wgap( ii-1 ) = max( zero,w(ii) - werr (ii) - w( ii-1 ) - werr( ii-1 ))
           end do
           return
     end subroutine stdlib_slarrb

     pure module subroutine stdlib_dlarrb( n, d, lld, ifirst, ilast, rtol1,rtol2, offset, w, wgap, werr, &
     !! Given the relatively robust representation(RRR) L D L^T, DLARRB:
     !! does "limited" bisection to refine the eigenvalues of L D L^T,
     !! W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
     !! guesses for these eigenvalues are input in W, the corresponding estimate
     !! of the error in these guesses and their gaps are input in WERR
     !! and WGAP, respectively. During bisection, intervals
     !! [left, right] are maintained by storing their mid-points and
     !! semi-widths in the arrays W and WERR respectively.
               work, iwork,pivmin, spdiam, twist, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ifirst, ilast, n, offset, twist
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: pivmin, rtol1, rtol2, spdiam
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: d(*), lld(*)
           real(dp), intent(inout) :: w(*), werr(*), wgap(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           integer(ilp) :: maxitr
           ! Local Scalars 
           integer(ilp) :: i, i1, ii, ip, iter, k, negcnt, next, nint, olnint, prev, r
           real(dp) :: back, cvrgd, gap, left, lgap, mid, mnwdth, rgap, right, tmp, width
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           maxitr = int( ( log( spdiam+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           mnwdth = two * pivmin
           r = twist
           if((r<1_ilp).or.(r>n)) r = n
           ! initialize unconverged intervals in [ work(2*i-1), work(2*i) ].
           ! the sturm count, count( work(2*i-1) ) is arranged to be i-1, while
           ! count( work(2*i) ) is stored in iwork( 2*i ). the integer iwork( 2*i-1 )
           ! for an unconverged interval is set to the index of the next unconverged
           ! interval, and is -1 or 0 for a converged interval. thus a linked
           ! list of unconverged intervals is set up.
           i1 = ifirst
           ! the number of unconverged intervals
           nint = 0_ilp
           ! the last unconverged interval found
           prev = 0_ilp
           rgap = wgap( i1-offset )
           loop_75: do i = i1, ilast
              k = 2_ilp*i
              ii = i - offset
              left = w( ii ) - werr( ii )
              right = w( ii ) + werr( ii )
              lgap = rgap
              rgap = wgap( ii )
              gap = min( lgap, rgap )
              ! make sure that [left,right] contains the desired eigenvalue
              ! compute negcount from dstqds facto l+d+l+^t = l d l^t - left
              ! do while( negcnt(left)>i-1 )
              back = werr( ii )
              20 continue
              negcnt = stdlib_dlaneg( n, d, lld, left, pivmin, r )
              if( negcnt>i-1 ) then
                 left = left - back
                 back = two*back
                 go to 20
              end if
              ! do while( negcnt(right)<i )
              ! compute negcount from dstqds facto l+d+l+^t = l d l^t - right
              back = werr( ii )
              50 continue
              negcnt = stdlib_dlaneg( n, d, lld, right, pivmin, r )
               if( negcnt<i ) then
                  right = right + back
                  back = two*back
                  go to 50
               end if
              width = half*abs( left - right )
              tmp = max( abs( left ), abs( right ) )
              cvrgd = max(rtol1*gap,rtol2*tmp)
              if( width<=cvrgd .or. width<=mnwdth ) then
                 ! this interval has already converged and does not need refinement.
                 ! (note that the gaps might change through refining the
                  ! eigenvalues, however, they can only get bigger.)
                 ! remove it from the list.
                 iwork( k-1 ) = -1_ilp
                 ! make sure that i1 always points to the first unconverged interval
                 if((i==i1).and.(i<ilast)) i1 = i + 1_ilp
                 if((prev>=i1).and.(i<=ilast)) iwork( 2_ilp*prev-1 ) = i + 1_ilp
              else
                 ! unconverged interval found
                 prev = i
                 nint = nint + 1_ilp
                 iwork( k-1 ) = i + 1_ilp
                 iwork( k ) = negcnt
              end if
              work( k-1 ) = left
              work( k ) = right
           end do loop_75
           ! do while( nint>0 ), i.e. there are still unconverged intervals
           ! and while (iter<maxitr)
           iter = 0_ilp
           80 continue
           prev = i1 - 1_ilp
           i = i1
           olnint = nint
           loop_100: do ip = 1, olnint
              k = 2_ilp*i
              ii = i - offset
              rgap = wgap( ii )
              lgap = rgap
              if(ii>1_ilp) lgap = wgap( ii-1 )
              gap = min( lgap, rgap )
              next = iwork( k-1 )
              left = work( k-1 )
              right = work( k )
              mid = half*( left + right )
              ! semiwidth of interval
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              cvrgd = max(rtol1*gap,rtol2*tmp)
              if( ( width<=cvrgd ) .or. ( width<=mnwdth ).or.( iter==maxitr ) )then
                 ! reduce number of unconverged intervals
                 nint = nint - 1_ilp
                 ! mark interval as converged.
                 iwork( k-1 ) = 0_ilp
                 if( i1==i ) then
                    i1 = next
                 else
                    ! prev holds the last unconverged interval previously examined
                    if(prev>=i1) iwork( 2_ilp*prev-1 ) = next
                 end if
                 i = next
                 cycle loop_100
              end if
              prev = i
              ! perform one bisection step
              negcnt = stdlib_dlaneg( n, d, lld, mid, pivmin, r )
              if( negcnt<=i-1 ) then
                 work( k-1 ) = mid
              else
                 work( k ) = mid
              end if
              i = next
           end do loop_100
           iter = iter + 1_ilp
           ! do another loop if there are still unconverged intervals
           ! however, in the last iteration, all intervals are accepted
           ! since this is the best we can do.
           if( ( nint>0 ).and.(iter<=maxitr) ) go to 80
           ! at this point, all the intervals have converged
           do i = ifirst, ilast
              k = 2_ilp*i
              ii = i - offset
              ! all intervals marked by '0' have been refined.
              if( iwork( k-1 )==0_ilp ) then
                 w( ii ) = half*( work( k-1 )+work( k ) )
                 werr( ii ) = work( k ) - w( ii )
              end if
           end do
           do i = ifirst+1, ilast
              k = 2_ilp*i
              ii = i - offset
              wgap( ii-1 ) = max( zero,w(ii) - werr (ii) - w( ii-1 ) - werr( ii-1 ))
           end do
           return
     end subroutine stdlib_dlarrb




     pure module subroutine stdlib_slarrc( jobt, n, vl, vu, d, e, pivmin,eigcnt, lcnt, rcnt, info )
     !! Find the number of eigenvalues of the symmetric tridiagonal matrix T
     !! that are in the interval (VL,VU] if JOBT = 'T', and of L D L^T
     !! if JOBT = 'L'.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobt
           integer(ilp), intent(out) :: eigcnt, info, lcnt, rcnt
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: pivmin, vl, vu
           ! Array Arguments 
           real(sp), intent(in) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           logical(lk) :: matt
           real(sp) :: lpivot, rpivot, sl, su, tmp, tmp2
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           lcnt = 0_ilp
           rcnt = 0_ilp
           eigcnt = 0_ilp
           matt = stdlib_lsame( jobt, 'T' )
           if (matt) then
              ! sturm sequence count on t
              lpivot = d( 1_ilp ) - vl
              rpivot = d( 1_ilp ) - vu
              if( lpivot<=zero ) then
                 lcnt = lcnt + 1_ilp
              endif
              if( rpivot<=zero ) then
                 rcnt = rcnt + 1_ilp
              endif
              do i = 1, n-1
                 tmp = e(i)**2_ilp
                 lpivot = ( d( i+1 )-vl ) - tmp/lpivot
                 rpivot = ( d( i+1 )-vu ) - tmp/rpivot
                 if( lpivot<=zero ) then
                    lcnt = lcnt + 1_ilp
                 endif
                 if( rpivot<=zero ) then
                    rcnt = rcnt + 1_ilp
                 endif
              end do
           else
              ! sturm sequence count on l d l^t
              sl = -vl
              su = -vu
              do i = 1, n - 1
                 lpivot = d( i ) + sl
                 rpivot = d( i ) + su
                 if( lpivot<=zero ) then
                    lcnt = lcnt + 1_ilp
                 endif
                 if( rpivot<=zero ) then
                    rcnt = rcnt + 1_ilp
                 endif
                 tmp = e(i) * d(i) * e(i)
                 tmp2 = tmp / lpivot
                 if( tmp2==zero ) then
                    sl =  tmp - vl
                 else
                    sl = sl*tmp2 - vl
                 end if
                 tmp2 = tmp / rpivot
                 if( tmp2==zero ) then
                    su =  tmp - vu
                 else
                    su = su*tmp2 - vu
                 end if
              end do
              lpivot = d( n ) + sl
              rpivot = d( n ) + su
              if( lpivot<=zero ) then
                 lcnt = lcnt + 1_ilp
              endif
              if( rpivot<=zero ) then
                 rcnt = rcnt + 1_ilp
              endif
           endif
           eigcnt = rcnt - lcnt
           return
     end subroutine stdlib_slarrc

     pure module subroutine stdlib_dlarrc( jobt, n, vl, vu, d, e, pivmin,eigcnt, lcnt, rcnt, info )
     !! Find the number of eigenvalues of the symmetric tridiagonal matrix T
     !! that are in the interval (VL,VU] if JOBT = 'T', and of L D L^T
     !! if JOBT = 'L'.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobt
           integer(ilp), intent(out) :: eigcnt, info, lcnt, rcnt
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: pivmin, vl, vu
           ! Array Arguments 
           real(dp), intent(in) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           logical(lk) :: matt
           real(dp) :: lpivot, rpivot, sl, su, tmp, tmp2
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           lcnt = 0_ilp
           rcnt = 0_ilp
           eigcnt = 0_ilp
           matt = stdlib_lsame( jobt, 'T' )
           if (matt) then
              ! sturm sequence count on t
              lpivot = d( 1_ilp ) - vl
              rpivot = d( 1_ilp ) - vu
              if( lpivot<=zero ) then
                 lcnt = lcnt + 1_ilp
              endif
              if( rpivot<=zero ) then
                 rcnt = rcnt + 1_ilp
              endif
              do i = 1, n-1
                 tmp = e(i)**2_ilp
                 lpivot = ( d( i+1 )-vl ) - tmp/lpivot
                 rpivot = ( d( i+1 )-vu ) - tmp/rpivot
                 if( lpivot<=zero ) then
                    lcnt = lcnt + 1_ilp
                 endif
                 if( rpivot<=zero ) then
                    rcnt = rcnt + 1_ilp
                 endif
              end do
           else
              ! sturm sequence count on l d l^t
              sl = -vl
              su = -vu
              do i = 1, n - 1
                 lpivot = d( i ) + sl
                 rpivot = d( i ) + su
                 if( lpivot<=zero ) then
                    lcnt = lcnt + 1_ilp
                 endif
                 if( rpivot<=zero ) then
                    rcnt = rcnt + 1_ilp
                 endif
                 tmp = e(i) * d(i) * e(i)
                 tmp2 = tmp / lpivot
                 if( tmp2==zero ) then
                    sl =  tmp - vl
                 else
                    sl = sl*tmp2 - vl
                 end if
                 tmp2 = tmp / rpivot
                 if( tmp2==zero ) then
                    su =  tmp - vu
                 else
                    su = su*tmp2 - vu
                 end if
              end do
              lpivot = d( n ) + sl
              rpivot = d( n ) + su
              if( lpivot<=zero ) then
                 lcnt = lcnt + 1_ilp
              endif
              if( rpivot<=zero ) then
                 rcnt = rcnt + 1_ilp
              endif
           endif
           eigcnt = rcnt - lcnt
           return
     end subroutine stdlib_dlarrc




     pure module subroutine stdlib_slarrd( range, order, n, vl, vu, il, iu, gers,reltol, d, e, e2, &
     !! SLARRD computes the eigenvalues of a symmetric tridiagonal
     !! matrix T to suitable accuracy. This is an auxiliary code to be
     !! called from SSTEMR.
     !! The user may ask for all eigenvalues, all eigenvalues
     !! in the half-open interval (VL, VU], or the IL-th through IU-th
     !! eigenvalues.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
               pivmin, nsplit, isplit,m, w, werr, wl, wu, iblock, indexw,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n, nsplit
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: pivmin, reltol, vl, vu
           real(sp), intent(out) :: wl, wu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), indexw(*), iwork(*)
           integer(ilp), intent(in) :: isplit(*)
           real(sp), intent(in) :: d(*), e(*), e2(*), gers(*)
           real(sp), intent(out) :: w(*), werr(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: fudge = two
           integer(ilp), parameter :: allrng = 1_ilp
           integer(ilp), parameter :: valrng = 2_ilp
           integer(ilp), parameter :: indrng = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: ncnvrg, toofew
           integer(ilp) :: i, ib, ibegin, idiscl, idiscu, ie, iend, iinfo, im, in, ioff, iout, &
                     irange, itmax, itmp1, itmp2, iw, iwoff, j, jblk, jdisc, je, jee, nb, nwl, nwu
           real(sp) :: atoli, eps, gl, gu, rtoli, tmp1, tmp2, tnorm, uflow, wkill, wlu, &
                     wul
           ! Local Arrays 
           integer(ilp) :: idumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = allrng
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = valrng
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = indrng
           else
              irange = 0_ilp
           end if
           ! check for errors
           if( irange<=0_ilp ) then
              info = -1_ilp
           else if( .not.(stdlib_lsame(order,'B').or.stdlib_lsame(order,'E')) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( irange==valrng ) then
              if( vl>=vu )info = -5_ilp
           else if( irange==indrng .and.( il<1_ilp .or. il>max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if( irange==indrng .and.( iu<min( n, il ) .or. iu>n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              return
           end if
           ! initialize error flags
           info = 0_ilp
           ncnvrg = .false.
           toofew = .false.
           ! quick return if possible
           m = 0_ilp
           if( n==0 ) return
           ! simplification:
           if( irange==indrng .and. il==1_ilp .and. iu==n ) irange = 1_ilp
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           uflow = stdlib_slamch( 'U' )
           ! special case when n=1
           ! treat case of 1x1 matrix for quick return
           if( n==1_ilp ) then
              if( (irange==allrng).or.((irange==valrng).and.(d(1_ilp)>vl).and.(d(1_ilp)<=vu)).or.((&
                        irange==indrng).and.(il==1_ilp).and.(iu==1_ilp)) ) then
                 m = 1_ilp
                 w(1_ilp) = d(1_ilp)
                 ! the computation error of the eigenvalue is zero
                 werr(1_ilp) = zero
                 iblock( 1_ilp ) = 1_ilp
                 indexw( 1_ilp ) = 1_ilp
              endif
              return
           end if
           ! nb is the minimum vector length for vector bisection, or 0
           ! if only scalar is to be done.
           nb = stdlib_ilaenv( 1_ilp, 'SSTEBZ', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp ) nb = 0_ilp
           ! find global spectral radius
           gl = d(1_ilp)
           gu = d(1_ilp)
           do i = 1,n
              gl =  min( gl, gers( 2_ilp*i - 1_ilp))
              gu = max( gu, gers(2_ilp*i) )
           end do
           ! compute global gerschgorin bounds and spectral diameter
           tnorm = max( abs( gl ), abs( gu ) )
           gl = gl - fudge*tnorm*eps*n - fudge*two*pivmin
           gu = gu + fudge*tnorm*eps*n + fudge*two*pivmin
           ! [jan/28/2009] remove the line below since spdiam variable not use
           ! spdiam = gu - gl
           ! input arguments for stdlib_slaebz:
           ! the relative tolerance.  an interval (a,b] lies within
           ! "relative tolerance" if  b-a < reltol*max(|a|,|b|),
           rtoli = reltol
           ! set the absolute tolerance for interval convergence to zero to force
           ! interval convergence based on relative size of the interval.
           ! this is dangerous because intervals might not converge when reltol is
           ! small. but at least a very small number should be selected so that for
           ! strongly graded matrices, the code can get relatively accurate
           ! eigenvalues.
           atoli = fudge*two*uflow + fudge*two*pivmin
           if( irange==indrng ) then
              ! range='i': compute an interval containing eigenvalues
              ! il through iu. the initial interval [gl,gu] from the global
              ! gerschgorin bounds gl and gu is refined by stdlib_slaebz.
              itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
              work( n+1 ) = gl
              work( n+2 ) = gl
              work( n+3 ) = gu
              work( n+4 ) = gu
              work( n+5 ) = gl
              work( n+6 ) = gu
              iwork( 1_ilp ) = -1_ilp
              iwork( 2_ilp ) = -1_ilp
              iwork( 3_ilp ) = n + 1_ilp
              iwork( 4_ilp ) = n + 1_ilp
              iwork( 5_ilp ) = il - 1_ilp
              iwork( 6_ilp ) = iu
              call stdlib_slaebz( 3_ilp, itmax, n, 2_ilp, 2_ilp, nb, atoli, rtoli, pivmin,d, e, e2, iwork( 5_ilp )&
                        , work( n+1 ), work( n+5 ), iout,iwork, w, iblock, iinfo )
              if( iinfo /= 0_ilp ) then
                 info = iinfo
                 return
              end if
              ! on exit, output intervals may not be ordered by ascending negcount
              if( iwork( 6_ilp )==iu ) then
                 wl = work( n+1 )
                 wlu = work( n+3 )
                 nwl = iwork( 1_ilp )
                 wu = work( n+4 )
                 wul = work( n+2 )
                 nwu = iwork( 4_ilp )
              else
                 wl = work( n+2 )
                 wlu = work( n+4 )
                 nwl = iwork( 2_ilp )
                 wu = work( n+3 )
                 wul = work( n+1 )
                 nwu = iwork( 3_ilp )
              end if
              ! on exit, the interval [wl, wlu] contains a value with negcount nwl,
              ! and [wul, wu] contains a value with negcount nwu.
              if( nwl<0_ilp .or. nwl>=n .or. nwu<1_ilp .or. nwu>n ) then
                 info = 4_ilp
                 return
              end if
           elseif( irange==valrng ) then
              wl = vl
              wu = vu
           elseif( irange==allrng ) then
              wl = gl
              wu = gu
           endif
           ! find eigenvalues -- loop over blocks and recompute nwl and nwu.
           ! nwl accumulates the number of eigenvalues .le. wl,
           ! nwu accumulates the number of eigenvalues .le. wu
           m = 0_ilp
           iend = 0_ilp
           info = 0_ilp
           nwl = 0_ilp
           nwu = 0_ilp
           loop_70: do jblk = 1, nsplit
              ioff = iend
              ibegin = ioff + 1_ilp
              iend = isplit( jblk )
              in = iend - ioff
              if( in==1_ilp ) then
                 ! 1x1 block
                 if( wl>=d( ibegin )-pivmin )nwl = nwl + 1_ilp
                 if( wu>=d( ibegin )-pivmin )nwu = nwu + 1_ilp
                 if( irange==allrng .or.( wl<d( ibegin )-pivmin.and. wu>= d( ibegin )-pivmin ) ) &
                           then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    werr(m) = zero
                    ! the gap for a single block doesn't matter for the later
                    ! algorithm and is assigned an arbitrary large value
                    iblock( m ) = jblk
                    indexw( m ) = 1_ilp
                 end if
              ! disabled 2x2 case because of a failure on the following matrix
              ! range = 'i', il = iu = 4
                ! original tridiagonal, d = [
                 ! -0.150102010615740e+00_sp
                 ! -0.849897989384260e+00_sp
                 ! -0.128208148052635e-15_sp
                  ! 0.128257718286320e-15_sp
                ! ];
                ! e = [
                 ! -0.357171383266986e+00_sp
                 ! -0.180411241501588e-15_sp
                 ! -0.175152352710251e-15_sp
                ! ];
               ! else if( in==2 ) then
      ! *           2x2 block
                  ! disc = sqrt( (half*(d(ibegin)-d(iend)))**2 + e(ibegin)**2 )
                  ! tmp1 = half*(d(ibegin)+d(iend))
                  ! l1 = tmp1 - disc
                  ! if( wl>= l1-pivmin )
           ! $         nwl = nwl + 1
                  ! if( wu>= l1-pivmin )
           ! $         nwu = nwu + 1
                  ! if( irange==allrng .or. ( wl<l1-pivmin .and. wu>=
           ! $          l1-pivmin ) ) then
                     ! m = m + 1
                     ! w( m ) = l1
      ! *              the uncertainty of eigenvalues of a 2x2 matrix is very small
                     ! werr( m ) = eps * abs( w( m ) ) * two
                     ! iblock( m ) = jblk
                     ! indexw( m ) = 1
                  ! endif
                  ! l2 = tmp1 + disc
                  ! if( wl>= l2-pivmin )
           ! $         nwl = nwl + 1
                  ! if( wu>= l2-pivmin )
           ! $         nwu = nwu + 1
                  ! if( irange==allrng .or. ( wl<l2-pivmin .and. wu>=
           ! $          l2-pivmin ) ) then
                     ! m = m + 1
                     ! w( m ) = l2
      ! *              the uncertainty of eigenvalues of a 2x2 matrix is very small
                     ! werr( m ) = eps * abs( w( m ) ) * two
                     ! iblock( m ) = jblk
                     ! indexw( m ) = 2
                  ! endif
              else
                 ! general case - block of size in >= 2
                 ! compute local gerschgorin interval and use it as the initial
                 ! interval for stdlib_slaebz
                 gu = d( ibegin )
                 gl = d( ibegin )
                 tmp1 = zero
                 do j = ibegin, iend
                    gl =  min( gl, gers( 2_ilp*j - 1_ilp))
                    gu = max( gu, gers(2_ilp*j) )
                 end do
                 ! [jan/28/2009]
                 ! change spdiam by tnorm in lines 2 and 3 thereafter
                 ! line 1: remove computation of spdiam (not useful anymore)
                 ! spdiam = gu - gl
                 ! gl = gl - fudge*spdiam*eps*in - fudge*pivmin
                 ! gu = gu + fudge*spdiam*eps*in + fudge*pivmin
                 gl = gl - fudge*tnorm*eps*in - fudge*pivmin
                 gu = gu + fudge*tnorm*eps*in + fudge*pivmin
                 if( irange>1_ilp ) then
                    if( gu<wl ) then
                       ! the local block contains none of the wanted eigenvalues
                       nwl = nwl + in
                       nwu = nwu + in
                       cycle loop_70
                    end if
                    ! refine search interval if possible, only range (wl,wu] matters
                    gl = max( gl, wl )
                    gu = min( gu, wu )
                    if( gl>=gu )cycle loop_70
                 end if
                 ! find negcount of initial interval boundaries gl and gu
                 work( n+1 ) = gl
                 work( n+in+1 ) = gu
                 call stdlib_slaebz( 1_ilp, 0_ilp, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e( &
                 ibegin ), e2( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), im,iwork, w( m+1 ),&
                            iblock( m+1 ), iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = iinfo
                    return
                 end if
                 nwl = nwl + iwork( 1_ilp )
                 nwu = nwu + iwork( in+1 )
                 iwoff = m - iwork( 1_ilp )
                 ! compute eigenvalues
                 itmax = int( ( log( gu-gl+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + &
                           2_ilp
                 call stdlib_slaebz( 2_ilp, itmax, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e(&
                  ibegin ), e2( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), iout,iwork, w( m+&
                            1_ilp ), iblock( m+1 ), iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = iinfo
                    return
                 end if
                 ! copy eigenvalues into w and iblock
                 ! use -jblk for block number for unconverged eigenvalues.
                 ! loop over the number of output intervals from stdlib_slaebz
                 do j = 1, iout
                    ! eigenvalue approximation is middle point of interval
                    tmp1 = half*( work( j+n )+work( j+in+n ) )
                    ! semi length of error interval
                    tmp2 = half*abs( work( j+n )-work( j+in+n ) )
                    if( j>iout-iinfo ) then
                       ! flag non-convergence.
                       ncnvrg = .true.
                       ib = -jblk
                    else
                       ib = jblk
                    end if
                    do je = iwork( j ) + 1 + iwoff,iwork( j+in ) + iwoff
                       w( je ) = tmp1
                       werr( je ) = tmp2
                       indexw( je ) = je - iwoff
                       iblock( je ) = ib
                    end do
                 end do
                 m = m + im
              end if
           end do loop_70
           ! if range='i', then (wl,wu) contains eigenvalues nwl+1,...,nwu
           ! if nwl+1 < il or nwu > iu, discard extra eigenvalues.
           if( irange==indrng ) then
              idiscl = il - 1_ilp - nwl
              idiscu = nwu - iu
              if( idiscl>0_ilp ) then
                 im = 0_ilp
                 do je = 1, m
                    ! remove some of the smallest eigenvalues from the left so that
                    ! at the end idiscl =0. move all eigenvalues up to the left.
                    if( w( je )<=wlu .and. idiscl>0_ilp ) then
                       idiscl = idiscl - 1_ilp
                    else
                       im = im + 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscu>0_ilp ) then
                 ! remove some of the largest eigenvalues from the right so that
                 ! at the end idiscu =0. move all eigenvalues up to the left.
                 im=m+1
                 do je = m, 1, -1
                    if( w( je )>=wul .and. idiscu>0_ilp ) then
                       idiscu = idiscu - 1_ilp
                    else
                       im = im - 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 jee = 0_ilp
                 do je = im, m
                    jee = jee + 1_ilp
                    w( jee ) = w( je )
                    werr( jee ) = werr( je )
                    indexw( jee ) = indexw( je )
                    iblock( jee ) = iblock( je )
                 end do
                 m = m-im+1
              end if
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 ! code to deal with effects of bad arithmetic. (if n(w) is
                 ! monotone non-decreasing, this should never happen.)
                 ! some low eigenvalues to be discarded are not in (wl,wlu],
                 ! or high eigenvalues to be discarded are not in (wul,wu]
                 ! so just kill off the smallest idiscl/largest idiscu
                 ! eigenvalues, by marking the corresponding iblock = 0
                 if( idiscl>0_ilp ) then
                    wkill = wu
                    do jdisc = 1, idiscl
                       iw = 0_ilp
                       do je = 1, m
                          if( iblock( je )/=0_ilp .and.( w( je )<wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 if( idiscu>0_ilp ) then
                    wkill = wl
                    do jdisc = 1, idiscu
                       iw = 0_ilp
                       do je = 1, m
                          if( iblock( je )/=0_ilp .and.( w( je )>=wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 ! now erase all eigenvalues with iblock set to zero
                 im = 0_ilp
                 do je = 1, m
                    if( iblock( je )/=0_ilp ) then
                       im = im + 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl<0_ilp .or. idiscu<0_ilp ) then
                 toofew = .true.
              end if
           end if
           if(( irange==allrng .and. m/=n ).or.( irange==indrng .and. m/=iu-il+1 ) ) then
              toofew = .true.
           end if
           ! if order='b', do nothing the eigenvalues are already sorted by
              ! block.
           ! if order='e', sort the eigenvalues from smallest to largest
           if( stdlib_lsame(order,'E') .and. nsplit>1_ilp ) then
              do je = 1, m - 1
                 ie = 0_ilp
                 tmp1 = w( je )
                 do j = je + 1, m
                    if( w( j )<tmp1 ) then
                       ie = j
                       tmp1 = w( j )
                    end if
                 end do
                 if( ie/=0_ilp ) then
                    tmp2 = werr( ie )
                    itmp1 = iblock( ie )
                    itmp2 = indexw( ie )
                    w( ie ) = w( je )
                    werr( ie ) = werr( je )
                    iblock( ie ) = iblock( je )
                    indexw( ie ) = indexw( je )
                    w( je ) = tmp1
                    werr( je ) = tmp2
                    iblock( je ) = itmp1
                    indexw( je ) = itmp2
                 end if
              end do
           end if
           info = 0_ilp
           if( ncnvrg )info = info + 1_ilp
           if( toofew )info = info + 2_ilp
           return
     end subroutine stdlib_slarrd

     pure module subroutine stdlib_dlarrd( range, order, n, vl, vu, il, iu, gers,reltol, d, e, e2, &
     !! DLARRD computes the eigenvalues of a symmetric tridiagonal
     !! matrix T to suitable accuracy. This is an auxiliary code to be
     !! called from DSTEMR.
     !! The user may ask for all eigenvalues, all eigenvalues
     !! in the half-open interval (VL, VU], or the IL-th through IU-th
     !! eigenvalues.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
               pivmin, nsplit, isplit,m, w, werr, wl, wu, iblock, indexw,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n, nsplit
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: pivmin, reltol, vl, vu
           real(dp), intent(out) :: wl, wu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), indexw(*), iwork(*)
           integer(ilp), intent(in) :: isplit(*)
           real(dp), intent(in) :: d(*), e(*), e2(*), gers(*)
           real(dp), intent(out) :: w(*), werr(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: fudge = two
           integer(ilp), parameter :: allrng = 1_ilp
           integer(ilp), parameter :: valrng = 2_ilp
           integer(ilp), parameter :: indrng = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: ncnvrg, toofew
           integer(ilp) :: i, ib, ibegin, idiscl, idiscu, ie, iend, iinfo, im, in, ioff, iout, &
                     irange, itmax, itmp1, itmp2, iw, iwoff, j, jblk, jdisc, je, jee, nb, nwl, nwu
           real(dp) :: atoli, eps, gl, gu, rtoli, tmp1, tmp2, tnorm, uflow, wkill, wlu, &
                     wul
           ! Local Arrays 
           integer(ilp) :: idumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = allrng
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = valrng
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = indrng
           else
              irange = 0_ilp
           end if
           ! check for errors
           if( irange<=0_ilp ) then
              info = -1_ilp
           else if( .not.(stdlib_lsame(order,'B').or.stdlib_lsame(order,'E')) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( irange==valrng ) then
              if( vl>=vu )info = -5_ilp
           else if( irange==indrng .and.( il<1_ilp .or. il>max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if( irange==indrng .and.( iu<min( n, il ) .or. iu>n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              return
           end if
           ! initialize error flags
           info = 0_ilp
           ncnvrg = .false.
           toofew = .false.
           ! quick return if possible
           m = 0_ilp
           if( n==0 ) return
           ! simplification:
           if( irange==indrng .and. il==1_ilp .and. iu==n ) irange = 1_ilp
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           uflow = stdlib_dlamch( 'U' )
           ! special case when n=1
           ! treat case of 1x1 matrix for quick return
           if( n==1_ilp ) then
              if( (irange==allrng).or.((irange==valrng).and.(d(1_ilp)>vl).and.(d(1_ilp)<=vu)).or.((&
                        irange==indrng).and.(il==1_ilp).and.(iu==1_ilp)) ) then
                 m = 1_ilp
                 w(1_ilp) = d(1_ilp)
                 ! the computation error of the eigenvalue is zero
                 werr(1_ilp) = zero
                 iblock( 1_ilp ) = 1_ilp
                 indexw( 1_ilp ) = 1_ilp
              endif
              return
           end if
           ! nb is the minimum vector length for vector bisection, or 0
           ! if only scalar is to be done.
           nb = stdlib_ilaenv( 1_ilp, 'DSTEBZ', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp ) nb = 0_ilp
           ! find global spectral radius
           gl = d(1_ilp)
           gu = d(1_ilp)
           do i = 1,n
              gl =  min( gl, gers( 2_ilp*i - 1_ilp))
              gu = max( gu, gers(2_ilp*i) )
           end do
           ! compute global gerschgorin bounds and spectral diameter
           tnorm = max( abs( gl ), abs( gu ) )
           gl = gl - fudge*tnorm*eps*n - fudge*two*pivmin
           gu = gu + fudge*tnorm*eps*n + fudge*two*pivmin
           ! [jan/28/2009] remove the line below since spdiam variable not use
           ! spdiam = gu - gl
           ! input arguments for stdlib_dlaebz:
           ! the relative tolerance.  an interval (a,b] lies within
           ! "relative tolerance" if  b-a < reltol*max(|a|,|b|),
           rtoli = reltol
           ! set the absolute tolerance for interval convergence to zero to force
           ! interval convergence based on relative size of the interval.
           ! this is dangerous because intervals might not converge when reltol is
           ! small. but at least a very small number should be selected so that for
           ! strongly graded matrices, the code can get relatively accurate
           ! eigenvalues.
           atoli = fudge*two*uflow + fudge*two*pivmin
           if( irange==indrng ) then
              ! range='i': compute an interval containing eigenvalues
              ! il through iu. the initial interval [gl,gu] from the global
              ! gerschgorin bounds gl and gu is refined by stdlib_dlaebz.
              itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
              work( n+1 ) = gl
              work( n+2 ) = gl
              work( n+3 ) = gu
              work( n+4 ) = gu
              work( n+5 ) = gl
              work( n+6 ) = gu
              iwork( 1_ilp ) = -1_ilp
              iwork( 2_ilp ) = -1_ilp
              iwork( 3_ilp ) = n + 1_ilp
              iwork( 4_ilp ) = n + 1_ilp
              iwork( 5_ilp ) = il - 1_ilp
              iwork( 6_ilp ) = iu
              call stdlib_dlaebz( 3_ilp, itmax, n, 2_ilp, 2_ilp, nb, atoli, rtoli, pivmin,d, e, e2, iwork( 5_ilp )&
                        , work( n+1 ), work( n+5 ), iout,iwork, w, iblock, iinfo )
              if( iinfo /= 0_ilp ) then
                 info = iinfo
                 return
              end if
              ! on exit, output intervals may not be ordered by ascending negcount
              if( iwork( 6_ilp )==iu ) then
                 wl = work( n+1 )
                 wlu = work( n+3 )
                 nwl = iwork( 1_ilp )
                 wu = work( n+4 )
                 wul = work( n+2 )
                 nwu = iwork( 4_ilp )
              else
                 wl = work( n+2 )
                 wlu = work( n+4 )
                 nwl = iwork( 2_ilp )
                 wu = work( n+3 )
                 wul = work( n+1 )
                 nwu = iwork( 3_ilp )
              end if
              ! on exit, the interval [wl, wlu] contains a value with negcount nwl,
              ! and [wul, wu] contains a value with negcount nwu.
              if( nwl<0_ilp .or. nwl>=n .or. nwu<1_ilp .or. nwu>n ) then
                 info = 4_ilp
                 return
              end if
           elseif( irange==valrng ) then
              wl = vl
              wu = vu
           elseif( irange==allrng ) then
              wl = gl
              wu = gu
           endif
           ! find eigenvalues -- loop over blocks and recompute nwl and nwu.
           ! nwl accumulates the number of eigenvalues .le. wl,
           ! nwu accumulates the number of eigenvalues .le. wu
           m = 0_ilp
           iend = 0_ilp
           info = 0_ilp
           nwl = 0_ilp
           nwu = 0_ilp
           loop_70: do jblk = 1, nsplit
              ioff = iend
              ibegin = ioff + 1_ilp
              iend = isplit( jblk )
              in = iend - ioff
              if( in==1_ilp ) then
                 ! 1x1 block
                 if( wl>=d( ibegin )-pivmin )nwl = nwl + 1_ilp
                 if( wu>=d( ibegin )-pivmin )nwu = nwu + 1_ilp
                 if( irange==allrng .or.( wl<d( ibegin )-pivmin.and. wu>= d( ibegin )-pivmin ) ) &
                           then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    werr(m) = zero
                    ! the gap for a single block doesn't matter for the later
                    ! algorithm and is assigned an arbitrary large value
                    iblock( m ) = jblk
                    indexw( m ) = 1_ilp
                 end if
              ! disabled 2x2 case because of a failure on the following matrix
              ! range = 'i', il = iu = 4
                ! original tridiagonal, d = [
                 ! -0.150102010615740e+00_dp
                 ! -0.849897989384260e+00_dp
                 ! -0.128208148052635e-15_dp
                  ! 0.128257718286320e-15_dp
                ! ];
                ! e = [
                 ! -0.357171383266986e+00_dp
                 ! -0.180411241501588e-15_dp
                 ! -0.175152352710251e-15_dp
                ! ];
               ! else if( in==2 ) then
      ! *           2x2 block
                  ! disc = sqrt( (half*(d(ibegin)-d(iend)))**2 + e(ibegin)**2 )
                  ! tmp1 = half*(d(ibegin)+d(iend))
                  ! l1 = tmp1 - disc
                  ! if( wl>= l1-pivmin )
           ! $         nwl = nwl + 1
                  ! if( wu>= l1-pivmin )
           ! $         nwu = nwu + 1
                  ! if( irange==allrng .or. ( wl<l1-pivmin .and. wu>=
           ! $          l1-pivmin ) ) then
                     ! m = m + 1
                     ! w( m ) = l1
      ! *              the uncertainty of eigenvalues of a 2x2 matrix is very small
                     ! werr( m ) = eps * abs( w( m ) ) * two
                     ! iblock( m ) = jblk
                     ! indexw( m ) = 1
                  ! endif
                  ! l2 = tmp1 + disc
                  ! if( wl>= l2-pivmin )
           ! $         nwl = nwl + 1
                  ! if( wu>= l2-pivmin )
           ! $         nwu = nwu + 1
                  ! if( irange==allrng .or. ( wl<l2-pivmin .and. wu>=
           ! $          l2-pivmin ) ) then
                     ! m = m + 1
                     ! w( m ) = l2
      ! *              the uncertainty of eigenvalues of a 2x2 matrix is very small
                     ! werr( m ) = eps * abs( w( m ) ) * two
                     ! iblock( m ) = jblk
                     ! indexw( m ) = 2
                  ! endif
              else
                 ! general case - block of size in >= 2
                 ! compute local gerschgorin interval and use it as the initial
                 ! interval for stdlib_dlaebz
                 gu = d( ibegin )
                 gl = d( ibegin )
                 tmp1 = zero
                 do j = ibegin, iend
                    gl =  min( gl, gers( 2_ilp*j - 1_ilp))
                    gu = max( gu, gers(2_ilp*j) )
                 end do
                 ! [jan/28/2009]
                 ! change spdiam by tnorm in lines 2 and 3 thereafter
                 ! line 1: remove computation of spdiam (not useful anymore)
                 ! spdiam = gu - gl
                 ! gl = gl - fudge*spdiam*eps*in - fudge*pivmin
                 ! gu = gu + fudge*spdiam*eps*in + fudge*pivmin
                 gl = gl - fudge*tnorm*eps*in - fudge*pivmin
                 gu = gu + fudge*tnorm*eps*in + fudge*pivmin
                 if( irange>1_ilp ) then
                    if( gu<wl ) then
                       ! the local block contains none of the wanted eigenvalues
                       nwl = nwl + in
                       nwu = nwu + in
                       cycle loop_70
                    end if
                    ! refine search interval if possible, only range (wl,wu] matters
                    gl = max( gl, wl )
                    gu = min( gu, wu )
                    if( gl>=gu )cycle loop_70
                 end if
                 ! find negcount of initial interval boundaries gl and gu
                 work( n+1 ) = gl
                 work( n+in+1 ) = gu
                 call stdlib_dlaebz( 1_ilp, 0_ilp, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e( &
                 ibegin ), e2( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), im,iwork, w( m+1 ),&
                            iblock( m+1 ), iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = iinfo
                    return
                 end if
                 nwl = nwl + iwork( 1_ilp )
                 nwu = nwu + iwork( in+1 )
                 iwoff = m - iwork( 1_ilp )
                 ! compute eigenvalues
                 itmax = int( ( log( gu-gl+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + &
                           2_ilp
                 call stdlib_dlaebz( 2_ilp, itmax, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e(&
                  ibegin ), e2( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), iout,iwork, w( m+&
                            1_ilp ), iblock( m+1 ), iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = iinfo
                    return
                 end if
                 ! copy eigenvalues into w and iblock
                 ! use -jblk for block number for unconverged eigenvalues.
                 ! loop over the number of output intervals from stdlib_dlaebz
                 do j = 1, iout
                    ! eigenvalue approximation is middle point of interval
                    tmp1 = half*( work( j+n )+work( j+in+n ) )
                    ! semi length of error interval
                    tmp2 = half*abs( work( j+n )-work( j+in+n ) )
                    if( j>iout-iinfo ) then
                       ! flag non-convergence.
                       ncnvrg = .true.
                       ib = -jblk
                    else
                       ib = jblk
                    end if
                    do je = iwork( j ) + 1 + iwoff,iwork( j+in ) + iwoff
                       w( je ) = tmp1
                       werr( je ) = tmp2
                       indexw( je ) = je - iwoff
                       iblock( je ) = ib
                    end do
                 end do
                 m = m + im
              end if
           end do loop_70
           ! if range='i', then (wl,wu) contains eigenvalues nwl+1,...,nwu
           ! if nwl+1 < il or nwu > iu, discard extra eigenvalues.
           if( irange==indrng ) then
              idiscl = il - 1_ilp - nwl
              idiscu = nwu - iu
              if( idiscl>0_ilp ) then
                 im = 0_ilp
                 do je = 1, m
                    ! remove some of the smallest eigenvalues from the left so that
                    ! at the end idiscl =0. move all eigenvalues up to the left.
                    if( w( je )<=wlu .and. idiscl>0_ilp ) then
                       idiscl = idiscl - 1_ilp
                    else
                       im = im + 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscu>0_ilp ) then
                 ! remove some of the largest eigenvalues from the right so that
                 ! at the end idiscu =0. move all eigenvalues up to the left.
                 im=m+1
                 do je = m, 1, -1
                    if( w( je )>=wul .and. idiscu>0_ilp ) then
                       idiscu = idiscu - 1_ilp
                    else
                       im = im - 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 jee = 0_ilp
                 do je = im, m
                    jee = jee + 1_ilp
                    w( jee ) = w( je )
                    werr( jee ) = werr( je )
                    indexw( jee ) = indexw( je )
                    iblock( jee ) = iblock( je )
                 end do
                 m = m-im+1
              end if
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 ! code to deal with effects of bad arithmetic. (if n(w) is
                 ! monotone non-decreasing, this should never happen.)
                 ! some low eigenvalues to be discarded are not in (wl,wlu],
                 ! or high eigenvalues to be discarded are not in (wul,wu]
                 ! so just kill off the smallest idiscl/largest idiscu
                 ! eigenvalues, by marking the corresponding iblock = 0
                 if( idiscl>0_ilp ) then
                    wkill = wu
                    do jdisc = 1, idiscl
                       iw = 0_ilp
                       do je = 1, m
                          if( iblock( je )/=0_ilp .and.( w( je )<wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 if( idiscu>0_ilp ) then
                    wkill = wl
                    do jdisc = 1, idiscu
                       iw = 0_ilp
                       do je = 1, m
                          if( iblock( je )/=0_ilp .and.( w( je )>=wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 ! now erase all eigenvalues with iblock set to zero
                 im = 0_ilp
                 do je = 1, m
                    if( iblock( je )/=0_ilp ) then
                       im = im + 1_ilp
                       w( im ) = w( je )
                       werr( im ) = werr( je )
                       indexw( im ) = indexw( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl<0_ilp .or. idiscu<0_ilp ) then
                 toofew = .true.
              end if
           end if
           if(( irange==allrng .and. m/=n ).or.( irange==indrng .and. m/=iu-il+1 ) ) then
              toofew = .true.
           end if
           ! if order='b', do nothing the eigenvalues are already sorted by
              ! block.
           ! if order='e', sort the eigenvalues from smallest to largest
           if( stdlib_lsame(order,'E') .and. nsplit>1_ilp ) then
              do je = 1, m - 1
                 ie = 0_ilp
                 tmp1 = w( je )
                 do j = je + 1, m
                    if( w( j )<tmp1 ) then
                       ie = j
                       tmp1 = w( j )
                    end if
                 end do
                 if( ie/=0_ilp ) then
                    tmp2 = werr( ie )
                    itmp1 = iblock( ie )
                    itmp2 = indexw( ie )
                    w( ie ) = w( je )
                    werr( ie ) = werr( je )
                    iblock( ie ) = iblock( je )
                    indexw( ie ) = indexw( je )
                    w( je ) = tmp1
                    werr( je ) = tmp2
                    iblock( je ) = itmp1
                    indexw( je ) = itmp2
                 end if
              end do
           end if
           info = 0_ilp
           if( ncnvrg )info = info + 1_ilp
           if( toofew )info = info + 2_ilp
           return
     end subroutine stdlib_dlarrd




     pure module subroutine stdlib_slarre( range, n, vl, vu, il, iu, d, e, e2,rtol1, rtol2, spltol, &
     !! To find the desired eigenvalues of a given real symmetric
     !! tridiagonal matrix T, SLARRE: sets any "small" off-diagonal
     !! elements to zero, and for each unreduced block T_i, it finds
     !! (a) a suitable shift at one end of the block's spectrum,
     !! (b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
     !! (c) eigenvalues of each L_i D_i L_i^T.
     !! The representations and eigenvalues found are then used by
     !! SSTEMR to compute the eigenvectors of T.
     !! The accuracy varies depending on whether bisection is used to
     !! find a few eigenvalues or the dqds algorithm (subroutine SLASQ2) to
     !! conpute all and then discard any unwanted one.
     !! As an added benefit, SLARRE also outputs the n
     !! Gerschgorin intervals for the matrices L_i D_i L_i^T.
               nsplit, isplit, m,w, werr, wgap, iblock, indexw, gers, pivmin,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(sp), intent(out) :: pivmin
           real(sp), intent(in) :: rtol1, rtol2, spltol
           real(sp), intent(inout) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*), indexw(*)
           real(sp), intent(inout) :: d(*), e(*), e2(*)
           real(sp), intent(out) :: gers(*), w(*), werr(*), wgap(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: hndrd = 100.0_sp
           real(sp), parameter :: pert = 4.0_sp
           real(sp), parameter :: fourth = one/four
           real(sp), parameter :: fac = half
           real(sp), parameter :: maxgrowth = 64.0_sp
           real(sp), parameter :: fudge = two
           integer(ilp), parameter :: maxtry = 6_ilp
           integer(ilp), parameter :: allrng = 1_ilp
           integer(ilp), parameter :: indrng = 2_ilp
           integer(ilp), parameter :: valrng = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: forceb, norep, usedqd
           integer(ilp) :: cnt, cnt1, cnt2, i, ibegin, idum, iend, iinfo, in, indl, indu, irange, &
                     j, jblk, mb, mm, wbegin, wend
           real(sp) :: avgap, bsrtol, clwdth, dmax, dpivot, eabs, emax, eold, eps, gl, gu, isleft,&
                      isrght, rtl, rtol, s1, s2, safmin, sgndef, sigma, spdiam, tau, tmp, tmp1
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = allrng
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = valrng
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = indrng
           end if
           m = 0_ilp
           ! get machine constants
           safmin = stdlib_slamch( 'S' )
           eps = stdlib_slamch( 'P' )
           ! set parameters
           rtl = hndrd*eps
           ! if one were ever to ask for less initial precision in bsrtol,
           ! one should keep in mind that for the subset case, the extremal
           ! eigenvalues must be at least as accurate as the current setting
           ! (eigenvalues in the middle need not as much accuracy)
           bsrtol = sqrt(eps)*(0.5e-3_sp)
           ! treat case of 1x1 matrix for quick return
           if( n==1_ilp ) then
              if( (irange==allrng).or.((irange==valrng).and.(d(1_ilp)>vl).and.(d(1_ilp)<=vu)).or.((&
                        irange==indrng).and.(il==1_ilp).and.(iu==1_ilp)) ) then
                 m = 1_ilp
                 w(1_ilp) = d(1_ilp)
                 ! the computation error of the eigenvalue is zero
                 werr(1_ilp) = zero
                 wgap(1_ilp) = zero
                 iblock( 1_ilp ) = 1_ilp
                 indexw( 1_ilp ) = 1_ilp
                 gers(1_ilp) = d( 1_ilp )
                 gers(2_ilp) = d( 1_ilp )
              endif
              ! store the shift for the initial rrr, which is zero in this case
              e(1_ilp) = zero
              return
           end if
           ! general case: tridiagonal matrix of order > 1
           ! init werr, wgap. compute gerschgorin intervals and spectral diameter.
           ! compute maximum off-diagonal entry and pivmin.
           gl = d(1_ilp)
           gu = d(1_ilp)
           eold = zero
           emax = zero
           e(n) = zero
           do i = 1,n
              werr(i) = zero
              wgap(i) = zero
              eabs = abs( e(i) )
              if( eabs >= emax ) then
                 emax = eabs
              end if
              tmp1 = eabs + eold
              gers( 2_ilp*i-1) = d(i) - tmp1
              gl =  min( gl, gers( 2_ilp*i - 1_ilp))
              gers( 2_ilp*i ) = d(i) + tmp1
              gu = max( gu, gers(2_ilp*i) )
              eold  = eabs
           end do
           ! the minimum pivot allowed in the sturm sequence for t
           pivmin = safmin * max( one, emax**2_ilp )
           ! compute spectral diameter. the gerschgorin bounds give an
           ! estimate that is wrong by at most a factor of sqrt(2)
           spdiam = gu - gl
           ! compute splitting points
           call stdlib_slarra( n, d, e, e2, spltol, spdiam,nsplit, isplit, iinfo )
           ! can force use of bisection instead of faster dqds.
           ! option left in the code for future multisection work.
           forceb = .false.
           ! initialize usedqd, dqds should be used for allrng unless someone
           ! explicitly wants bisection.
           usedqd = (( irange==allrng ) .and. (.not.forceb))
           if( (irange==allrng) .and. (.not. forceb) ) then
              ! set interval [vl,vu] that contains all eigenvalues
              vl = gl
              vu = gu
           else
              ! we call stdlib_slarrd to find crude approximations to the eigenvalues
              ! in the desired range. in case irange = indrng, we also obtain the
              ! interval (vl,vu] that contains all the wanted eigenvalues.
              ! an interval [left,right] has converged if
              ! right-left<rtol*max(abs(left),abs(right))
              ! stdlib_slarrd needs a work of size 4*n, iwork of size 3*n
              call stdlib_slarrd( range, 'B', n, vl, vu, il, iu, gers,bsrtol, d, e, e2, pivmin, &
                        nsplit, isplit,mm, w, werr, vl, vu, iblock, indexw,work, iwork, iinfo )
              if( iinfo/=0_ilp ) then
                 info = -1_ilp
                 return
              endif
              ! make sure that the entries m+1 to n in w, werr, iblock, indexw are 0
              do i = mm+1,n
                 w( i ) = zero
                 werr( i ) = zero
                 iblock( i ) = 0_ilp
                 indexw( i ) = 0_ilp
              end do
           end if
      ! **
           ! loop over unreduced blocks
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, nsplit
              iend = isplit( jblk )
              in = iend - ibegin + 1_ilp
              ! 1 x 1 block
              if( in==1_ilp ) then
                 if( (irange==allrng).or.( (irange==valrng).and.( d( ibegin )>vl ).and.( d( &
                           ibegin )<=vu ) ).or. ( (irange==indrng).and.(iblock(wbegin)==jblk))) then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    werr(m) = zero
                    ! the gap for a single block doesn't matter for the later
                    ! algorithm and is assigned an arbitrary large value
                    wgap(m) = zero
                    iblock( m ) = jblk
                    indexw( m ) = 1_ilp
                    wbegin = wbegin + 1_ilp
                 endif
                 ! e( iend ) holds the shift for the initial rrr
                 e( iend ) = zero
                 ibegin = iend + 1_ilp
                 cycle loop_170
              end if
              ! blocks of size larger than 1x1
              ! e( iend ) will hold the shift for the initial rrr, for now set it =0
              e( iend ) = zero
              ! find local outer bounds gl,gu for the block
              gl = d(ibegin)
              gu = d(ibegin)
              do i = ibegin , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              if(.not. ((irange==allrng).and.(.not.forceb)) ) then
                 ! count the number of eigenvalues in the current block.
                 mb = 0_ilp
                 do i = wbegin,mm
                    if( iblock(i)==jblk ) then
                       mb = mb+1
                    else
                       goto 21
                    endif
                 end do
                 21 continue
                 if( mb==0_ilp) then
                    ! no eigenvalue in the current block lies in the desired range
                    ! e( iend ) holds the shift for the initial rrr
                    e( iend ) = zero
                    ibegin = iend + 1_ilp
                    cycle loop_170
                 else
                    ! decide whether dqds or bisection is more efficient
                    usedqd = ( (mb > fac*in) .and. (.not.forceb) )
                    wend = wbegin + mb - 1_ilp
                    ! calculate gaps for the current block
                    ! in later stages, when representations for individual
                    ! eigenvalues are different, we use sigma = e( iend ).
                    sigma = zero
                    do i = wbegin, wend - 1
                       wgap( i ) = max( zero,w(i+1)-werr(i+1) - (w(i)+werr(i)) )
                    end do
                    wgap( wend ) = max( zero,vu - sigma - (w( wend )+werr( wend )))
                    ! find local index of the first and last desired evalue.
                    indl = indexw(wbegin)
                    indu = indexw( wend )
                 endif
              endif
              if(( (irange==allrng) .and. (.not. forceb) ).or.usedqd) then
                 ! case of dqds
                 ! find approximations to the extremal eigenvalues of the block
                 call stdlib_slarrk( in, 1_ilp, gl, gu, d(ibegin),e2(ibegin), pivmin, rtl, tmp, tmp1, &
                           iinfo )
                 if( iinfo/=0_ilp ) then
                    info = -1_ilp
                    return
                 endif
                 isleft = max(gl, tmp - tmp1- hndrd * eps* abs(tmp - tmp1))
                 call stdlib_slarrk( in, in, gl, gu, d(ibegin),e2(ibegin), pivmin, rtl, tmp, tmp1,&
                            iinfo )
                 if( iinfo/=0_ilp ) then
                    info = -1_ilp
                    return
                 endif
                 isrght = min(gu, tmp + tmp1+ hndrd * eps * abs(tmp + tmp1))
                 ! improve the estimate of the spectral diameter
                 spdiam = isrght - isleft
              else
                 ! case of bisection
                 ! find approximations to the wanted extremal eigenvalues
                 isleft = max(gl, w(wbegin) - werr(wbegin)- hndrd * eps*abs(w(wbegin)- werr(&
                           wbegin) ))
                 isrght = min(gu,w(wend) + werr(wend)+ hndrd * eps * abs(w(wend)+ werr(wend)))
                           
              endif
              ! decide whether the base representation for the current block
              ! l_jblk d_jblk l_jblk^t = t_jblk - sigma_jblk i
              ! should be on the left or the right end of the current block.
              ! the strategy is to shift to the end which is "more populated"
              ! furthermore, decide whether to use dqds for the computation of
              ! dqds is chosen if all eigenvalues are desired or the number of
              ! eigenvalues to be computed is large compared to the blocksize.
              if( ( irange==allrng ) .and. (.not.forceb) ) then
                 ! if all the eigenvalues have to be computed, we use dqd
                 usedqd = .true.
                 ! indl is the local index of the first eigenvalue to compute
                 indl = 1_ilp
                 indu = in
                 ! mb =  number of eigenvalues to compute
                 mb = in
                 wend = wbegin + mb - 1_ilp
                 ! define 1/4 and 3/4 points of the spectrum
                 s1 = isleft + fourth * spdiam
                 s2 = isrght - fourth * spdiam
              else
                 ! stdlib_slarrd has computed iblock and indexw for each eigenvalue
                 ! approximation.
                 ! choose sigma
                 if( usedqd ) then
                    s1 = isleft + fourth * spdiam
                    s2 = isrght - fourth * spdiam
                 else
                    tmp = min(isrght,vu) -  max(isleft,vl)
                    s1 =  max(isleft,vl) + fourth * tmp
                    s2 =  min(isrght,vu) - fourth * tmp
                 endif
              endif
              ! compute the negcount at the 1/4 and 3/4 points
              if(mb>1_ilp) then
                 call stdlib_slarrc( 'T', in, s1, s2, d(ibegin),e(ibegin), pivmin, cnt, cnt1, &
                           cnt2, iinfo)
              endif
              if(mb==1_ilp) then
                 sigma = gl
                 sgndef = one
              elseif( cnt1 - indl >= indu - cnt2 ) then
                 if( ( irange==allrng ) .and. (.not.forceb) ) then
                    sigma = max(isleft,gl)
                 elseif( usedqd ) then
                    ! use gerschgorin bound as shift to get pos def matrix
                    ! for dqds
                    sigma = isleft
                 else
                    ! use approximation of the first desired eigenvalue of the
                    ! block as shift
                    sigma = max(isleft,vl)
                 endif
                 sgndef = one
              else
                 if( ( irange==allrng ) .and. (.not.forceb) ) then
                    sigma = min(isrght,gu)
                 elseif( usedqd ) then
                    ! use gerschgorin bound as shift to get neg def matrix
                    ! for dqds
                    sigma = isrght
                 else
                    ! use approximation of the first desired eigenvalue of the
                    ! block as shift
                    sigma = min(isrght,vu)
                 endif
                 sgndef = -one
              endif
              ! an initial sigma has been chosen that will be used for computing
              ! t - sigma i = l d l^t
              ! define the increment tau of the shift in case the initial shift
              ! needs to be refined to obtain a factorization with not too much
              ! element growth.
              if( usedqd ) then
                 ! the initial sigma was to the outer end of the spectrum
                 ! the matrix is definite and we need not retreat.
                 tau = spdiam*eps*n + two*pivmin
                 tau = max( tau,two*eps*abs(sigma) )
              else
                 if(mb>1_ilp) then
                    clwdth = w(wend) + werr(wend) - w(wbegin) - werr(wbegin)
                    avgap = abs(clwdth / real(wend-wbegin,KIND=sp))
                    if( sgndef==one ) then
                       tau = half*max(wgap(wbegin),avgap)
                       tau = max(tau,werr(wbegin))
                    else
                       tau = half*max(wgap(wend-1),avgap)
                       tau = max(tau,werr(wend))
                    endif
                 else
                    tau = werr(wbegin)
                 endif
              endif
              loop_80: do idum = 1, maxtry
                 ! compute l d l^t factorization of tridiagonal matrix t - sigma i.
                 ! store d in work(1:in), l in work(in+1:2*in), and reciprocals of
                 ! pivots in work(2*in+1:3*in)
                 dpivot = d( ibegin ) - sigma
                 work( 1_ilp ) = dpivot
                 dmax = abs( work(1_ilp) )
                 j = ibegin
                 do i = 1, in - 1
                    work( 2_ilp*in+i ) = one / work( i )
                    tmp = e( j )*work( 2_ilp*in+i )
                    work( in+i ) = tmp
                    dpivot = ( d( j+1 )-sigma ) - tmp*e( j )
                    work( i+1 ) = dpivot
                    dmax = max( dmax, abs(dpivot) )
                    j = j + 1_ilp
                 end do
                 ! check for element growth
                 if( dmax > maxgrowth*spdiam ) then
                    norep = .true.
                 else
                    norep = .false.
                 endif
                 if( usedqd .and. .not.norep ) then
                    ! ensure the definiteness of the representation
                    ! all entries of d (of l d l^t) must have the same sign
                    do i = 1, in
                       tmp = sgndef*work( i )
                       if( tmp<zero ) norep = .true.
                    end do
                 endif
                 if(norep) then
                    ! note that in the case of irange=allrng, we use the gerschgorin
                    ! shift which makes the matrix definite. so we should end up
                    ! here really only in the case of irange = valrng or indrng.
                    if( idum==maxtry-1 ) then
                       if( sgndef==one ) then
                          ! the fudged gerschgorin shift should succeed
                          sigma =gl - fudge*spdiam*eps*n - fudge*two*pivmin
                       else
                          sigma =gu + fudge*spdiam*eps*n + fudge*two*pivmin
                       end if
                    else
                       sigma = sigma - sgndef * tau
                       tau = two * tau
                    end if
                 else
                    ! an initial rrr is found
                    go to 83
                 end if
              end do loop_80
              ! if the program reaches this point, no base representation could be
              ! found in maxtry iterations.
              info = 2_ilp
              return
              83 continue
              ! at this point, we have found an initial base representation
              ! t - sigma i = l d l^t with not too much element growth.
              ! store the shift.
              e( iend ) = sigma
              ! store d and l.
              call stdlib_scopy( in, work, 1_ilp, d( ibegin ), 1_ilp )
              call stdlib_scopy( in-1, work( in+1 ), 1_ilp, e( ibegin ), 1_ilp )
              if(mb>1_ilp ) then
                 ! perturb each entry of the base representation by a small
                 ! (but random) relative amount to overcome difficulties with
                 ! glued matrices.
                 do i = 1, 4
                    iseed( i ) = 1_ilp
                 end do
                 call stdlib_slarnv(2_ilp, iseed, 2_ilp*in-1, work(1_ilp))
                 do i = 1,in-1
                    d(ibegin+i-1) = d(ibegin+i-1)*(one+eps*pert*work(i))
                    e(ibegin+i-1) = e(ibegin+i-1)*(one+eps*pert*work(in+i))
                 end do
                 d(iend) = d(iend)*(one+eps*four*work(in))
              endif
              ! don't update the gerschgorin intervals because keeping track
              ! of the updates would be too much work in stdlib_slarrv.
              ! we update w instead and use it to locate the proper gerschgorin
              ! intervals.
              ! compute the required eigenvalues of l d l' by bisection or dqds
              if ( .not.usedqd ) then
                 ! if stdlib_slarrd has been used, shift the eigenvalue approximations
                 ! according to their representation. this is necessary for
                 ! a uniform stdlib_slarrv since dqds computes eigenvalues of the
                 ! shifted representation. in stdlib_slarrv, w will always hold the
                 ! unshifted eigenvalue approximation.
                 do j=wbegin,wend
                    w(j) = w(j) - sigma
                    werr(j) = werr(j) + abs(w(j)) * eps
                 end do
                 ! call stdlib_slarrb to reduce eigenvalue error of the approximations
                 ! from stdlib_slarrd
                 do i = ibegin, iend-1
                    work( i ) = d( i ) * e( i )**2_ilp
                 end do
                 ! use bisection to find ev from indl to indu
                 call stdlib_slarrb(in, d(ibegin), work(ibegin),indl, indu, rtol1, rtol2, indl-1,&
                 w(wbegin), wgap(wbegin), werr(wbegin),work( 2_ilp*n+1 ), iwork, pivmin, spdiam,in, &
                           iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = -4_ilp
                    return
                 end if
                 ! stdlib_slarrb computes all gaps correctly except for the last one
                 ! record distance to vu/gu
                 wgap( wend ) = max( zero,( vu-sigma ) - ( w( wend ) + werr( wend ) ) )
                 do i = indl, indu
                    m = m + 1_ilp
                    iblock(m) = jblk
                    indexw(m) = i
                 end do
              else
                 ! call dqds to get all eigs (and then possibly delete unwanted
                 ! eigenvalues).
                 ! note that dqds finds the eigenvalues of the l d l^t representation
                 ! of t to high relative accuracy. high relative accuracy
                 ! might be lost when the shift of the rrr is subtracted to obtain
                 ! the eigenvalues of t. however, t is not guaranteed to define its
                 ! eigenvalues to high relative accuracy anyway.
                 ! set rtol to the order of the tolerance used in stdlib_slasq2
                 ! this is an estimated error, the worst case bound is 4*n*eps
                 ! which is usually too large and requires unnecessary work to be
                 ! done by bisection when computing the eigenvectors
                 rtol = log(real(in,KIND=sp)) * four * eps
                 j = ibegin
                 do i = 1, in - 1
                    work( 2_ilp*i-1 ) = abs( d( j ) )
                    work( 2_ilp*i ) = e( j )*e( j )*work( 2_ilp*i-1 )
                    j = j + 1_ilp
                 end do
                 work( 2_ilp*in-1 ) = abs( d( iend ) )
                 work( 2_ilp*in ) = zero
                 call stdlib_slasq2( in, work, iinfo )
                 if( iinfo /= 0_ilp ) then
                    ! if iinfo = -5 then an index is part of a tight cluster
                    ! and should be changed. the index is in iwork(1) and the
                    ! gap is in work(n+1)
                    info = -5_ilp
                    return
                 else
                    ! test that all eigenvalues are positive as expected
                    do i = 1, in
                       if( work( i )<zero ) then
                          info = -6_ilp
                          return
                       endif
                    end do
                 end if
                 if( sgndef>zero ) then
                    do i = indl, indu
                       m = m + 1_ilp
                       w( m ) = work( in-i+1 )
                       iblock( m ) = jblk
                       indexw( m ) = i
                    end do
                 else
                    do i = indl, indu
                       m = m + 1_ilp
                       w( m ) = -work( i )
                       iblock( m ) = jblk
                       indexw( m ) = i
                    end do
                 end if
                 do i = m - mb + 1, m
                    ! the value of rtol below should be the tolerance in stdlib_slasq2
                    werr( i ) = rtol * abs( w(i) )
                 end do
                 do i = m - mb + 1, m - 1
                    ! compute the right gap between the intervals
                    wgap( i ) = max( zero,w(i+1)-werr(i+1) - (w(i)+werr(i)) )
                 end do
                 wgap( m ) = max( zero,( vu-sigma ) - ( w( m ) + werr( m ) ) )
              end if
              ! proceed with next block
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_slarre

     pure module subroutine stdlib_dlarre( range, n, vl, vu, il, iu, d, e, e2,rtol1, rtol2, spltol, &
     !! To find the desired eigenvalues of a given real symmetric
     !! tridiagonal matrix T, DLARRE: sets any "small" off-diagonal
     !! elements to zero, and for each unreduced block T_i, it finds
     !! (a) a suitable shift at one end of the block's spectrum,
     !! (b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
     !! (c) eigenvalues of each L_i D_i L_i^T.
     !! The representations and eigenvalues found are then used by
     !! DSTEMR to compute the eigenvectors of T.
     !! The accuracy varies depending on whether bisection is used to
     !! find a few eigenvalues or the dqds algorithm (subroutine DLASQ2) to
     !! conpute all and then discard any unwanted one.
     !! As an added benefit, DLARRE also outputs the n
     !! Gerschgorin intervals for the matrices L_i D_i L_i^T.
               nsplit, isplit, m,w, werr, wgap, iblock, indexw, gers, pivmin,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(dp), intent(out) :: pivmin
           real(dp), intent(in) :: rtol1, rtol2, spltol
           real(dp), intent(inout) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*), indexw(*)
           real(dp), intent(inout) :: d(*), e(*), e2(*)
           real(dp), intent(out) :: gers(*), w(*), werr(*), wgap(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: hndrd = 100.0_dp
           real(dp), parameter :: pert = 8.0_dp
           real(dp), parameter :: fourth = one/four
           real(dp), parameter :: fac = half
           real(dp), parameter :: maxgrowth = 64.0_dp
           real(dp), parameter :: fudge = two
           integer(ilp), parameter :: maxtry = 6_ilp
           integer(ilp), parameter :: allrng = 1_ilp
           integer(ilp), parameter :: indrng = 2_ilp
           integer(ilp), parameter :: valrng = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: forceb, norep, usedqd
           integer(ilp) :: cnt, cnt1, cnt2, i, ibegin, idum, iend, iinfo, in, indl, indu, irange, &
                     j, jblk, mb, mm, wbegin, wend
           real(dp) :: avgap, bsrtol, clwdth, dmax, dpivot, eabs, emax, eold, eps, gl, gu, isleft,&
                      isrght, rtl, rtol, s1, s2, safmin, sgndef, sigma, spdiam, tau, tmp, tmp1
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = allrng
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = valrng
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = indrng
           end if
           m = 0_ilp
           ! get machine constants
           safmin = stdlib_dlamch( 'S' )
           eps = stdlib_dlamch( 'P' )
           ! set parameters
           rtl = sqrt(eps)
           bsrtol = sqrt(eps)
           ! treat case of 1x1 matrix for quick return
           if( n==1_ilp ) then
              if( (irange==allrng).or.((irange==valrng).and.(d(1_ilp)>vl).and.(d(1_ilp)<=vu)).or.((&
                        irange==indrng).and.(il==1_ilp).and.(iu==1_ilp)) ) then
                 m = 1_ilp
                 w(1_ilp) = d(1_ilp)
                 ! the computation error of the eigenvalue is zero
                 werr(1_ilp) = zero
                 wgap(1_ilp) = zero
                 iblock( 1_ilp ) = 1_ilp
                 indexw( 1_ilp ) = 1_ilp
                 gers(1_ilp) = d( 1_ilp )
                 gers(2_ilp) = d( 1_ilp )
              endif
              ! store the shift for the initial rrr, which is zero in this case
              e(1_ilp) = zero
              return
           end if
           ! general case: tridiagonal matrix of order > 1
           ! init werr, wgap. compute gerschgorin intervals and spectral diameter.
           ! compute maximum off-diagonal entry and pivmin.
           gl = d(1_ilp)
           gu = d(1_ilp)
           eold = zero
           emax = zero
           e(n) = zero
           do i = 1,n
              werr(i) = zero
              wgap(i) = zero
              eabs = abs( e(i) )
              if( eabs >= emax ) then
                 emax = eabs
              end if
              tmp1 = eabs + eold
              gers( 2_ilp*i-1) = d(i) - tmp1
              gl =  min( gl, gers( 2_ilp*i - 1_ilp))
              gers( 2_ilp*i ) = d(i) + tmp1
              gu = max( gu, gers(2_ilp*i) )
              eold  = eabs
           end do
           ! the minimum pivot allowed in the sturm sequence for t
           pivmin = safmin * max( one, emax**2_ilp )
           ! compute spectral diameter. the gerschgorin bounds give an
           ! estimate that is wrong by at most a factor of sqrt(2)
           spdiam = gu - gl
           ! compute splitting points
           call stdlib_dlarra( n, d, e, e2, spltol, spdiam,nsplit, isplit, iinfo )
           ! can force use of bisection instead of faster dqds.
           ! option left in the code for future multisection work.
           forceb = .false.
           ! initialize usedqd, dqds should be used for allrng unless someone
           ! explicitly wants bisection.
           usedqd = (( irange==allrng ) .and. (.not.forceb))
           if( (irange==allrng) .and. (.not. forceb) ) then
              ! set interval [vl,vu] that contains all eigenvalues
              vl = gl
              vu = gu
           else
              ! we call stdlib_dlarrd to find crude approximations to the eigenvalues
              ! in the desired range. in case irange = indrng, we also obtain the
              ! interval (vl,vu] that contains all the wanted eigenvalues.
              ! an interval [left,right] has converged if
              ! right-left<rtol*max(abs(left),abs(right))
              ! stdlib_dlarrd needs a work of size 4*n, iwork of size 3*n
              call stdlib_dlarrd( range, 'B', n, vl, vu, il, iu, gers,bsrtol, d, e, e2, pivmin, &
                        nsplit, isplit,mm, w, werr, vl, vu, iblock, indexw,work, iwork, iinfo )
              if( iinfo/=0_ilp ) then
                 info = -1_ilp
                 return
              endif
              ! make sure that the entries m+1 to n in w, werr, iblock, indexw are 0
              do i = mm+1,n
                 w( i ) = zero
                 werr( i ) = zero
                 iblock( i ) = 0_ilp
                 indexw( i ) = 0_ilp
              end do
           end if
      ! **
           ! loop over unreduced blocks
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, nsplit
              iend = isplit( jblk )
              in = iend - ibegin + 1_ilp
              ! 1 x 1 block
              if( in==1_ilp ) then
                 if( (irange==allrng).or.( (irange==valrng).and.( d( ibegin )>vl ).and.( d( &
                           ibegin )<=vu ) ).or. ( (irange==indrng).and.(iblock(wbegin)==jblk))) then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    werr(m) = zero
                    ! the gap for a single block doesn't matter for the later
                    ! algorithm and is assigned an arbitrary large value
                    wgap(m) = zero
                    iblock( m ) = jblk
                    indexw( m ) = 1_ilp
                    wbegin = wbegin + 1_ilp
                 endif
                 ! e( iend ) holds the shift for the initial rrr
                 e( iend ) = zero
                 ibegin = iend + 1_ilp
                 cycle loop_170
              end if
              ! blocks of size larger than 1x1
              ! e( iend ) will hold the shift for the initial rrr, for now set it =0
              e( iend ) = zero
              ! find local outer bounds gl,gu for the block
              gl = d(ibegin)
              gu = d(ibegin)
              do i = ibegin , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              if(.not. ((irange==allrng).and.(.not.forceb)) ) then
                 ! count the number of eigenvalues in the current block.
                 mb = 0_ilp
                 do i = wbegin,mm
                    if( iblock(i)==jblk ) then
                       mb = mb+1
                    else
                       goto 21
                    endif
                 end do
                 21 continue
                 if( mb==0_ilp) then
                    ! no eigenvalue in the current block lies in the desired range
                    ! e( iend ) holds the shift for the initial rrr
                    e( iend ) = zero
                    ibegin = iend + 1_ilp
                    cycle loop_170
                 else
                    ! decide whether dqds or bisection is more efficient
                    usedqd = ( (mb > fac*in) .and. (.not.forceb) )
                    wend = wbegin + mb - 1_ilp
                    ! calculate gaps for the current block
                    ! in later stages, when representations for individual
                    ! eigenvalues are different, we use sigma = e( iend ).
                    sigma = zero
                    do i = wbegin, wend - 1
                       wgap( i ) = max( zero,w(i+1)-werr(i+1) - (w(i)+werr(i)) )
                    end do
                    wgap( wend ) = max( zero,vu - sigma - (w( wend )+werr( wend )))
                    ! find local index of the first and last desired evalue.
                    indl = indexw(wbegin)
                    indu = indexw( wend )
                 endif
              endif
              if(( (irange==allrng) .and. (.not. forceb) ).or.usedqd) then
                 ! case of dqds
                 ! find approximations to the extremal eigenvalues of the block
                 call stdlib_dlarrk( in, 1_ilp, gl, gu, d(ibegin),e2(ibegin), pivmin, rtl, tmp, tmp1, &
                           iinfo )
                 if( iinfo/=0_ilp ) then
                    info = -1_ilp
                    return
                 endif
                 isleft = max(gl, tmp - tmp1- hndrd * eps* abs(tmp - tmp1))
                 call stdlib_dlarrk( in, in, gl, gu, d(ibegin),e2(ibegin), pivmin, rtl, tmp, tmp1,&
                            iinfo )
                 if( iinfo/=0_ilp ) then
                    info = -1_ilp
                    return
                 endif
                 isrght = min(gu, tmp + tmp1+ hndrd * eps * abs(tmp + tmp1))
                 ! improve the estimate of the spectral diameter
                 spdiam = isrght - isleft
              else
                 ! case of bisection
                 ! find approximations to the wanted extremal eigenvalues
                 isleft = max(gl, w(wbegin) - werr(wbegin)- hndrd * eps*abs(w(wbegin)- werr(&
                           wbegin) ))
                 isrght = min(gu,w(wend) + werr(wend)+ hndrd * eps * abs(w(wend)+ werr(wend)))
                           
              endif
              ! decide whether the base representation for the current block
              ! l_jblk d_jblk l_jblk^t = t_jblk - sigma_jblk i
              ! should be on the left or the right end of the current block.
              ! the strategy is to shift to the end which is "more populated"
              ! furthermore, decide whether to use dqds for the computation of
              ! dqds is chosen if all eigenvalues are desired or the number of
              ! eigenvalues to be computed is large compared to the blocksize.
              if( ( irange==allrng ) .and. (.not.forceb) ) then
                 ! if all the eigenvalues have to be computed, we use dqd
                 usedqd = .true.
                 ! indl is the local index of the first eigenvalue to compute
                 indl = 1_ilp
                 indu = in
                 ! mb =  number of eigenvalues to compute
                 mb = in
                 wend = wbegin + mb - 1_ilp
                 ! define 1/4 and 3/4 points of the spectrum
                 s1 = isleft + fourth * spdiam
                 s2 = isrght - fourth * spdiam
              else
                 ! stdlib_dlarrd has computed iblock and indexw for each eigenvalue
                 ! approximation.
                 ! choose sigma
                 if( usedqd ) then
                    s1 = isleft + fourth * spdiam
                    s2 = isrght - fourth * spdiam
                 else
                    tmp = min(isrght,vu) -  max(isleft,vl)
                    s1 =  max(isleft,vl) + fourth * tmp
                    s2 =  min(isrght,vu) - fourth * tmp
                 endif
              endif
              ! compute the negcount at the 1/4 and 3/4 points
              if(mb>1_ilp) then
                 call stdlib_dlarrc( 'T', in, s1, s2, d(ibegin),e(ibegin), pivmin, cnt, cnt1, &
                           cnt2, iinfo)
              endif
              if(mb==1_ilp) then
                 sigma = gl
                 sgndef = one
              elseif( cnt1 - indl >= indu - cnt2 ) then
                 if( ( irange==allrng ) .and. (.not.forceb) ) then
                    sigma = max(isleft,gl)
                 elseif( usedqd ) then
                    ! use gerschgorin bound as shift to get pos def matrix
                    ! for dqds
                    sigma = isleft
                 else
                    ! use approximation of the first desired eigenvalue of the
                    ! block as shift
                    sigma = max(isleft,vl)
                 endif
                 sgndef = one
              else
                 if( ( irange==allrng ) .and. (.not.forceb) ) then
                    sigma = min(isrght,gu)
                 elseif( usedqd ) then
                    ! use gerschgorin bound as shift to get neg def matrix
                    ! for dqds
                    sigma = isrght
                 else
                    ! use approximation of the first desired eigenvalue of the
                    ! block as shift
                    sigma = min(isrght,vu)
                 endif
                 sgndef = -one
              endif
              ! an initial sigma has been chosen that will be used for computing
              ! t - sigma i = l d l^t
              ! define the increment tau of the shift in case the initial shift
              ! needs to be refined to obtain a factorization with not too much
              ! element growth.
              if( usedqd ) then
                 ! the initial sigma was to the outer end of the spectrum
                 ! the matrix is definite and we need not retreat.
                 tau = spdiam*eps*n + two*pivmin
                 tau = max( tau,two*eps*abs(sigma) )
              else
                 if(mb>1_ilp) then
                    clwdth = w(wend) + werr(wend) - w(wbegin) - werr(wbegin)
                    avgap = abs(clwdth / real(wend-wbegin,KIND=dp))
                    if( sgndef==one ) then
                       tau = half*max(wgap(wbegin),avgap)
                       tau = max(tau,werr(wbegin))
                    else
                       tau = half*max(wgap(wend-1),avgap)
                       tau = max(tau,werr(wend))
                    endif
                 else
                    tau = werr(wbegin)
                 endif
              endif
              loop_80: do idum = 1, maxtry
                 ! compute l d l^t factorization of tridiagonal matrix t - sigma i.
                 ! store d in work(1:in), l in work(in+1:2*in), and reciprocals of
                 ! pivots in work(2*in+1:3*in)
                 dpivot = d( ibegin ) - sigma
                 work( 1_ilp ) = dpivot
                 dmax = abs( work(1_ilp) )
                 j = ibegin
                 do i = 1, in - 1
                    work( 2_ilp*in+i ) = one / work( i )
                    tmp = e( j )*work( 2_ilp*in+i )
                    work( in+i ) = tmp
                    dpivot = ( d( j+1 )-sigma ) - tmp*e( j )
                    work( i+1 ) = dpivot
                    dmax = max( dmax, abs(dpivot) )
                    j = j + 1_ilp
                 end do
                 ! check for element growth
                 if( dmax > maxgrowth*spdiam ) then
                    norep = .true.
                 else
                    norep = .false.
                 endif
                 if( usedqd .and. .not.norep ) then
                    ! ensure the definiteness of the representation
                    ! all entries of d (of l d l^t) must have the same sign
                    do i = 1, in
                       tmp = sgndef*work( i )
                       if( tmp<zero ) norep = .true.
                    end do
                 endif
                 if(norep) then
                    ! note that in the case of irange=allrng, we use the gerschgorin
                    ! shift which makes the matrix definite. so we should end up
                    ! here really only in the case of irange = valrng or indrng.
                    if( idum==maxtry-1 ) then
                       if( sgndef==one ) then
                          ! the fudged gerschgorin shift should succeed
                          sigma =gl - fudge*spdiam*eps*n - fudge*two*pivmin
                       else
                          sigma =gu + fudge*spdiam*eps*n + fudge*two*pivmin
                       end if
                    else
                       sigma = sigma - sgndef * tau
                       tau = two * tau
                    end if
                 else
                    ! an initial rrr is found
                    go to 83
                 end if
              end do loop_80
              ! if the program reaches this point, no base representation could be
              ! found in maxtry iterations.
              info = 2_ilp
              return
              83 continue
              ! at this point, we have found an initial base representation
              ! t - sigma i = l d l^t with not too much element growth.
              ! store the shift.
              e( iend ) = sigma
              ! store d and l.
              call stdlib_dcopy( in, work, 1_ilp, d( ibegin ), 1_ilp )
              call stdlib_dcopy( in-1, work( in+1 ), 1_ilp, e( ibegin ), 1_ilp )
              if(mb>1_ilp ) then
                 ! perturb each entry of the base representation by a small
                 ! (but random) relative amount to overcome difficulties with
                 ! glued matrices.
                 do i = 1, 4
                    iseed( i ) = 1_ilp
                 end do
                 call stdlib_dlarnv(2_ilp, iseed, 2_ilp*in-1, work(1_ilp))
                 do i = 1,in-1
                    d(ibegin+i-1) = d(ibegin+i-1)*(one+eps*pert*work(i))
                    e(ibegin+i-1) = e(ibegin+i-1)*(one+eps*pert*work(in+i))
                 end do
                 d(iend) = d(iend)*(one+eps*four*work(in))
              endif
              ! don't update the gerschgorin intervals because keeping track
              ! of the updates would be too much work in stdlib_dlarrv.
              ! we update w instead and use it to locate the proper gerschgorin
              ! intervals.
              ! compute the required eigenvalues of l d l' by bisection or dqds
              if ( .not.usedqd ) then
                 ! if stdlib_dlarrd has been used, shift the eigenvalue approximations
                 ! according to their representation. this is necessary for
                 ! a uniform stdlib_dlarrv since dqds computes eigenvalues of the
                 ! shifted representation. in stdlib_dlarrv, w will always hold the
                 ! unshifted eigenvalue approximation.
                 do j=wbegin,wend
                    w(j) = w(j) - sigma
                    werr(j) = werr(j) + abs(w(j)) * eps
                 end do
                 ! call stdlib_dlarrb to reduce eigenvalue error of the approximations
                 ! from stdlib_dlarrd
                 do i = ibegin, iend-1
                    work( i ) = d( i ) * e( i )**2_ilp
                 end do
                 ! use bisection to find ev from indl to indu
                 call stdlib_dlarrb(in, d(ibegin), work(ibegin),indl, indu, rtol1, rtol2, indl-1,&
                 w(wbegin), wgap(wbegin), werr(wbegin),work( 2_ilp*n+1 ), iwork, pivmin, spdiam,in, &
                           iinfo )
                 if( iinfo /= 0_ilp ) then
                    info = -4_ilp
                    return
                 end if
                 ! stdlib_dlarrb computes all gaps correctly except for the last one
                 ! record distance to vu/gu
                 wgap( wend ) = max( zero,( vu-sigma ) - ( w( wend ) + werr( wend ) ) )
                 do i = indl, indu
                    m = m + 1_ilp
                    iblock(m) = jblk
                    indexw(m) = i
                 end do
              else
                 ! call dqds to get all eigs (and then possibly delete unwanted
                 ! eigenvalues).
                 ! note that dqds finds the eigenvalues of the l d l^t representation
                 ! of t to high relative accuracy. high relative accuracy
                 ! might be lost when the shift of the rrr is subtracted to obtain
                 ! the eigenvalues of t. however, t is not guaranteed to define its
                 ! eigenvalues to high relative accuracy anyway.
                 ! set rtol to the order of the tolerance used in stdlib_dlasq2
                 ! this is an estimated error, the worst case bound is 4*n*eps
                 ! which is usually too large and requires unnecessary work to be
                 ! done by bisection when computing the eigenvectors
                 rtol = log(real(in,KIND=dp)) * four * eps
                 j = ibegin
                 do i = 1, in - 1
                    work( 2_ilp*i-1 ) = abs( d( j ) )
                    work( 2_ilp*i ) = e( j )*e( j )*work( 2_ilp*i-1 )
                    j = j + 1_ilp
                 end do
                 work( 2_ilp*in-1 ) = abs( d( iend ) )
                 work( 2_ilp*in ) = zero
                 call stdlib_dlasq2( in, work, iinfo )
                 if( iinfo /= 0_ilp ) then
                    ! if iinfo = -5 then an index is part of a tight cluster
                    ! and should be changed. the index is in iwork(1) and the
                    ! gap is in work(n+1)
                    info = -5_ilp
                    return
                 else
                    ! test that all eigenvalues are positive as expected
                    do i = 1, in
                       if( work( i )<zero ) then
                          info = -6_ilp
                          return
                       endif
                    end do
                 end if
                 if( sgndef>zero ) then
                    do i = indl, indu
                       m = m + 1_ilp
                       w( m ) = work( in-i+1 )
                       iblock( m ) = jblk
                       indexw( m ) = i
                    end do
                 else
                    do i = indl, indu
                       m = m + 1_ilp
                       w( m ) = -work( i )
                       iblock( m ) = jblk
                       indexw( m ) = i
                    end do
                 end if
                 do i = m - mb + 1, m
                    ! the value of rtol below should be the tolerance in stdlib_dlasq2
                    werr( i ) = rtol * abs( w(i) )
                 end do
                 do i = m - mb + 1, m - 1
                    ! compute the right gap between the intervals
                    wgap( i ) = max( zero,w(i+1)-werr(i+1) - (w(i)+werr(i)) )
                 end do
                 wgap( m ) = max( zero,( vu-sigma ) - ( w( m ) + werr( m ) ) )
              end if
              ! proceed with next block
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_dlarre




     pure module subroutine stdlib_slarrf( n, d, l, ld, clstrt, clend,w, wgap, werr,spdiam, clgapl, &
     !! Given the initial representation L D L^T and its cluster of close
     !! eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
     !! W( CLEND ), SLARRF: finds a new relatively robust representation
     !! L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
     !! eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
               clgapr, pivmin, sigma,dplus, lplus, work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: clstrt, clend, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: clgapl, clgapr, pivmin, spdiam
           real(sp), intent(out) :: sigma
           ! Array Arguments 
           real(sp), intent(in) :: d(*), l(*), ld(*), w(*), werr(*)
           real(sp), intent(out) :: dplus(*), lplus(*), work(*)
           real(sp), intent(inout) :: wgap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: quart = 0.25_sp
           real(sp), parameter :: maxgrowth1 = 8._sp
           real(sp), parameter :: maxgrowth2 = 8._sp
           integer(ilp), parameter :: ktrymax = 1_ilp
           integer(ilp), parameter :: sleft = 1_ilp
           integer(ilp), parameter :: sright = 2_ilp
           
           ! Local Scalars 
           logical(lk) :: dorrr1, forcer, nofail, sawnan1, sawnan2, tryrrr1
           integer(ilp) :: i, indx, ktry,    shift
           real(sp) :: avgap, bestshift, clwdth, eps, fact, fail, fail2, growthbound, ldelta, &
           ldmax, lsigma, max1, max2, mingap, oldp, prod, rdelta, rdmax, rrr1, rrr2, rsigma, s, &
                     smlgrowth, tmp, znm2
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           fact = real(2_ilp**ktrymax,KIND=sp)
           eps = stdlib_slamch( 'PRECISION' )
           shift = 0_ilp
           forcer = .false.
           ! note that we cannot guarantee that for any of the shifts tried,
           ! the factorization has a small or even moderate element growth.
           ! there could be ritz values at both ends of the cluster and despite
           ! backing off, there are examples where all factorizations tried
           ! (in ieee mode, allowing zero pivots
           ! element growth.
           ! for this reason, we should use pivmin in this subroutine so that at
           ! least the l d l^t factorization exists. it can be checked afterwards
           ! whether the element growth caused bad residuals/orthogonality.
           ! decide whether the code should accept the best among all
           ! representations despite large element growth or signal info=1
           ! setting nofail to .false. for quick fix for bug 113
           nofail = .false.
           ! compute the average gap length of the cluster
           clwdth = abs(w(clend)-w(clstrt)) + werr(clend) + werr(clstrt)
           avgap = clwdth / real(clend-clstrt,KIND=sp)
           mingap = min(clgapl, clgapr)
           ! initial values for shifts to both ends of cluster
           lsigma = min(w( clstrt ),w( clend )) - werr( clstrt )
           rsigma = max(w( clstrt ),w( clend )) + werr( clend )
           ! use a small fudge to make sure that we really shift to the outside
           lsigma = lsigma - abs(lsigma)* two * eps
           rsigma = rsigma + abs(rsigma)* two * eps
           ! compute upper bounds for how much to back off the initial shifts
           ldmax = quart * mingap + two * pivmin
           rdmax = quart * mingap + two * pivmin
           ldelta = max(avgap,wgap( clstrt ))/fact
           rdelta = max(avgap,wgap( clend-1 ))/fact
           ! initialize the record of the best representation found
           s = stdlib_slamch( 'S' )
           smlgrowth = one / s
           fail = real(n-1,KIND=sp)*mingap/(spdiam*eps)
           fail2 = real(n-1,KIND=sp)*mingap/(spdiam*sqrt(eps))
           bestshift = lsigma
           ! while (ktry <= ktrymax)
           ktry = 0_ilp
           growthbound = maxgrowth1*spdiam
           5 continue
           sawnan1 = .false.
           sawnan2 = .false.
           ! ensure that we do not back off too much of the initial shifts
           ldelta = min(ldmax,ldelta)
           rdelta = min(rdmax,rdelta)
           ! compute the element growth when shifting to both ends of the cluster
           ! accept the shift if there is no element growth at one of the two ends
           ! left end
           s = -lsigma
           dplus( 1_ilp ) = d( 1_ilp ) + s
           if(abs(dplus(1_ilp))<pivmin) then
              dplus(1_ilp) = -pivmin
              ! need to set sawnan1 because refined rrr test should not be used
              ! in this case
              sawnan1 = .true.
           endif
           max1 = abs( dplus( 1_ilp ) )
           do i = 1, n - 1
              lplus( i ) = ld( i ) / dplus( i )
              s = s*lplus( i )*l( i ) - lsigma
              dplus( i+1 ) = d( i+1 ) + s
              if(abs(dplus(i+1))<pivmin) then
                 dplus(i+1) = -pivmin
                 ! need to set sawnan1 because refined rrr test should not be used
                 ! in this case
                 sawnan1 = .true.
              endif
              max1 = max( max1,abs(dplus(i+1)) )
           end do
           sawnan1 = sawnan1 .or.  stdlib_sisnan( max1 )
           if( forcer .or.(max1<=growthbound .and. .not.sawnan1 ) ) then
              sigma = lsigma
              shift = sleft
              goto 100
           endif
           ! right end
           s = -rsigma
           work( 1_ilp ) = d( 1_ilp ) + s
           if(abs(work(1_ilp))<pivmin) then
              work(1_ilp) = -pivmin
              ! need to set sawnan2 because refined rrr test should not be used
              ! in this case
              sawnan2 = .true.
           endif
           max2 = abs( work( 1_ilp ) )
           do i = 1, n - 1
              work( n+i ) = ld( i ) / work( i )
              s = s*work( n+i )*l( i ) - rsigma
              work( i+1 ) = d( i+1 ) + s
              if(abs(work(i+1))<pivmin) then
                 work(i+1) = -pivmin
                 ! need to set sawnan2 because refined rrr test should not be used
                 ! in this case
                 sawnan2 = .true.
              endif
              max2 = max( max2,abs(work(i+1)) )
           end do
           sawnan2 = sawnan2 .or.  stdlib_sisnan( max2 )
           if( forcer .or.(max2<=growthbound .and. .not.sawnan2 ) ) then
              sigma = rsigma
              shift = sright
              goto 100
           endif
           ! if we are at this point, both shifts led to too much element growth
           ! record the better of the two shifts (provided it didn't lead to nan)
           if(sawnan1.and.sawnan2) then
              ! both max1 and max2 are nan
              goto 50
           else
              if( .not.sawnan1 ) then
                 indx = 1_ilp
                 if(max1<=smlgrowth) then
                    smlgrowth = max1
                    bestshift = lsigma
                 endif
              endif
              if( .not.sawnan2 ) then
                 if(sawnan1 .or. max2<=max1) indx = 2_ilp
                 if(max2<=smlgrowth) then
                    smlgrowth = max2
                    bestshift = rsigma
                 endif
              endif
           endif
           ! if we are here, both the left and the right shift led to
           ! element growth. if the element growth is moderate, then
           ! we may still accept the representation, if it passes a
           ! refined test for rrr. this test supposes that no nan occurred.
           ! moreover, we use the refined rrr test only for isolated clusters.
           if((clwdth<mingap/real(128_ilp,KIND=sp)) .and.(min(max1,max2)<fail2).and.(.not.sawnan1)&
                     .and.(.not.sawnan2)) then
              dorrr1 = .true.
           else
              dorrr1 = .false.
           endif
           tryrrr1 = .true.
           if( tryrrr1 .and. dorrr1 ) then
           if(indx==1_ilp) then
              tmp = abs( dplus( n ) )
              znm2 = one
              prod = one
              oldp = one
              do i = n-1, 1, -1
                 if( prod <= eps ) then
                    prod =((dplus(i+1)*work(n+i+1))/(dplus(i)*work(n+i)))*oldp
                 else
                    prod = prod*abs(work(n+i))
                 end if
                 oldp = prod
                 znm2 = znm2 + prod**2_ilp
                 tmp = max( tmp, abs( dplus( i ) * prod ))
              end do
              rrr1 = tmp/( spdiam * sqrt( znm2 ) )
              if (rrr1<=maxgrowth2) then
                 sigma = lsigma
                 shift = sleft
                 goto 100
              endif
           else if(indx==2_ilp) then
              tmp = abs( work( n ) )
              znm2 = one
              prod = one
              oldp = one
              do i = n-1, 1, -1
                 if( prod <= eps ) then
                    prod = ((work(i+1)*lplus(i+1))/(work(i)*lplus(i)))*oldp
                 else
                    prod = prod*abs(lplus(i))
                 end if
                 oldp = prod
                 znm2 = znm2 + prod**2_ilp
                 tmp = max( tmp, abs( work( i ) * prod ))
              end do
              rrr2 = tmp/( spdiam * sqrt( znm2 ) )
              if (rrr2<=maxgrowth2) then
                 sigma = rsigma
                 shift = sright
                 goto 100
              endif
           end if
           endif
           50 continue
           if (ktry<ktrymax) then
              ! if we are here, both shifts failed also the rrr test.
              ! back off to the outside
              lsigma = max( lsigma - ldelta,lsigma - ldmax)
              rsigma = min( rsigma + rdelta,rsigma + rdmax )
              ldelta = two * ldelta
              rdelta = two * rdelta
              ktry = ktry + 1_ilp
              goto 5
           else
              ! none of the representations investigated satisfied our
              ! criteria. take the best one we found.
              if((smlgrowth<fail).or.nofail) then
                 lsigma = bestshift
                 rsigma = bestshift
                 forcer = .true.
                 goto 5
              else
                 info = 1_ilp
                 return
              endif
           end if
           100 continue
           if (shift==sleft) then
           elseif (shift==sright) then
              ! store new l and d back into dplus, lplus
              call stdlib_scopy( n, work, 1_ilp, dplus, 1_ilp )
              call stdlib_scopy( n-1, work(n+1), 1_ilp, lplus, 1_ilp )
           endif
           return
     end subroutine stdlib_slarrf

     pure module subroutine stdlib_dlarrf( n, d, l, ld, clstrt, clend,w, wgap, werr,spdiam, clgapl, &
     !! Given the initial representation L D L^T and its cluster of close
     !! eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
     !! W( CLEND ), DLARRF: finds a new relatively robust representation
     !! L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
     !! eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
               clgapr, pivmin, sigma,dplus, lplus, work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: clstrt, clend, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: clgapl, clgapr, pivmin, spdiam
           real(dp), intent(out) :: sigma
           ! Array Arguments 
           real(dp), intent(in) :: d(*), l(*), ld(*), w(*), werr(*)
           real(dp), intent(out) :: dplus(*), lplus(*), work(*)
           real(dp), intent(inout) :: wgap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: quart = 0.25_dp
           real(dp), parameter :: maxgrowth1 = 8._dp
           real(dp), parameter :: maxgrowth2 = 8._dp
           integer(ilp), parameter :: ktrymax = 1_ilp
           integer(ilp), parameter :: sleft = 1_ilp
           integer(ilp), parameter :: sright = 2_ilp
           
           ! Local Scalars 
           logical(lk) :: dorrr1, forcer, nofail, sawnan1, sawnan2, tryrrr1
           integer(ilp) :: i, indx, ktry,    shift
           real(dp) :: avgap, bestshift, clwdth, eps, fact, fail, fail2, growthbound, ldelta, &
           ldmax, lsigma, max1, max2, mingap, oldp, prod, rdelta, rdmax, rrr1, rrr2, rsigma, s, &
                     smlgrowth, tmp, znm2
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           fact = real(2_ilp**ktrymax,KIND=dp)
           eps = stdlib_dlamch( 'PRECISION' )
           shift = 0_ilp
           forcer = .false.
           ! note that we cannot guarantee that for any of the shifts tried,
           ! the factorization has a small or even moderate element growth.
           ! there could be ritz values at both ends of the cluster and despite
           ! backing off, there are examples where all factorizations tried
           ! (in ieee mode, allowing zero pivots
           ! element growth.
           ! for this reason, we should use pivmin in this subroutine so that at
           ! least the l d l^t factorization exists. it can be checked afterwards
           ! whether the element growth caused bad residuals/orthogonality.
           ! decide whether the code should accept the best among all
           ! representations despite large element growth or signal info=1
           ! setting nofail to .false. for quick fix for bug 113
           nofail = .false.
           ! compute the average gap length of the cluster
           clwdth = abs(w(clend)-w(clstrt)) + werr(clend) + werr(clstrt)
           avgap = clwdth / real(clend-clstrt,KIND=dp)
           mingap = min(clgapl, clgapr)
           ! initial values for shifts to both ends of cluster
           lsigma = min(w( clstrt ),w( clend )) - werr( clstrt )
           rsigma = max(w( clstrt ),w( clend )) + werr( clend )
           ! use a small fudge to make sure that we really shift to the outside
           lsigma = lsigma - abs(lsigma)* four * eps
           rsigma = rsigma + abs(rsigma)* four * eps
           ! compute upper bounds for how much to back off the initial shifts
           ldmax = quart * mingap + two * pivmin
           rdmax = quart * mingap + two * pivmin
           ldelta = max(avgap,wgap( clstrt ))/fact
           rdelta = max(avgap,wgap( clend-1 ))/fact
           ! initialize the record of the best representation found
           s = stdlib_dlamch( 'S' )
           smlgrowth = one / s
           fail = real(n-1,KIND=dp)*mingap/(spdiam*eps)
           fail2 = real(n-1,KIND=dp)*mingap/(spdiam*sqrt(eps))
           bestshift = lsigma
           ! while (ktry <= ktrymax)
           ktry = 0_ilp
           growthbound = maxgrowth1*spdiam
           5 continue
           sawnan1 = .false.
           sawnan2 = .false.
           ! ensure that we do not back off too much of the initial shifts
           ldelta = min(ldmax,ldelta)
           rdelta = min(rdmax,rdelta)
           ! compute the element growth when shifting to both ends of the cluster
           ! accept the shift if there is no element growth at one of the two ends
           ! left end
           s = -lsigma
           dplus( 1_ilp ) = d( 1_ilp ) + s
           if(abs(dplus(1_ilp))<pivmin) then
              dplus(1_ilp) = -pivmin
              ! need to set sawnan1 because refined rrr test should not be used
              ! in this case
              sawnan1 = .true.
           endif
           max1 = abs( dplus( 1_ilp ) )
           do i = 1, n - 1
              lplus( i ) = ld( i ) / dplus( i )
              s = s*lplus( i )*l( i ) - lsigma
              dplus( i+1 ) = d( i+1 ) + s
              if(abs(dplus(i+1))<pivmin) then
                 dplus(i+1) = -pivmin
                 ! need to set sawnan1 because refined rrr test should not be used
                 ! in this case
                 sawnan1 = .true.
              endif
              max1 = max( max1,abs(dplus(i+1)) )
           end do
           sawnan1 = sawnan1 .or.  stdlib_disnan( max1 )
           if( forcer .or.(max1<=growthbound .and. .not.sawnan1 ) ) then
              sigma = lsigma
              shift = sleft
              goto 100
           endif
           ! right end
           s = -rsigma
           work( 1_ilp ) = d( 1_ilp ) + s
           if(abs(work(1_ilp))<pivmin) then
              work(1_ilp) = -pivmin
              ! need to set sawnan2 because refined rrr test should not be used
              ! in this case
              sawnan2 = .true.
           endif
           max2 = abs( work( 1_ilp ) )
           do i = 1, n - 1
              work( n+i ) = ld( i ) / work( i )
              s = s*work( n+i )*l( i ) - rsigma
              work( i+1 ) = d( i+1 ) + s
              if(abs(work(i+1))<pivmin) then
                 work(i+1) = -pivmin
                 ! need to set sawnan2 because refined rrr test should not be used
                 ! in this case
                 sawnan2 = .true.
              endif
              max2 = max( max2,abs(work(i+1)) )
           end do
           sawnan2 = sawnan2 .or.  stdlib_disnan( max2 )
           if( forcer .or.(max2<=growthbound .and. .not.sawnan2 ) ) then
              sigma = rsigma
              shift = sright
              goto 100
           endif
           ! if we are at this point, both shifts led to too much element growth
           ! record the better of the two shifts (provided it didn't lead to nan)
           if(sawnan1.and.sawnan2) then
              ! both max1 and max2 are nan
              goto 50
           else
              if( .not.sawnan1 ) then
                 indx = 1_ilp
                 if(max1<=smlgrowth) then
                    smlgrowth = max1
                    bestshift = lsigma
                 endif
              endif
              if( .not.sawnan2 ) then
                 if(sawnan1 .or. max2<=max1) indx = 2_ilp
                 if(max2<=smlgrowth) then
                    smlgrowth = max2
                    bestshift = rsigma
                 endif
              endif
           endif
           ! if we are here, both the left and the right shift led to
           ! element growth. if the element growth is moderate, then
           ! we may still accept the representation, if it passes a
           ! refined test for rrr. this test supposes that no nan occurred.
           ! moreover, we use the refined rrr test only for isolated clusters.
           if((clwdth<mingap/real(128_ilp,KIND=dp)) .and.(min(max1,max2)<fail2).and.(.not.sawnan1)&
                     .and.(.not.sawnan2)) then
              dorrr1 = .true.
           else
              dorrr1 = .false.
           endif
           tryrrr1 = .true.
           if( tryrrr1 .and. dorrr1 ) then
           if(indx==1_ilp) then
              tmp = abs( dplus( n ) )
              znm2 = one
              prod = one
              oldp = one
              do i = n-1, 1, -1
                 if( prod <= eps ) then
                    prod =((dplus(i+1)*work(n+i+1))/(dplus(i)*work(n+i)))*oldp
                 else
                    prod = prod*abs(work(n+i))
                 end if
                 oldp = prod
                 znm2 = znm2 + prod**2_ilp
                 tmp = max( tmp, abs( dplus( i ) * prod ))
              end do
              rrr1 = tmp/( spdiam * sqrt( znm2 ) )
              if (rrr1<=maxgrowth2) then
                 sigma = lsigma
                 shift = sleft
                 goto 100
              endif
           else if(indx==2_ilp) then
              tmp = abs( work( n ) )
              znm2 = one
              prod = one
              oldp = one
              do i = n-1, 1, -1
                 if( prod <= eps ) then
                    prod = ((work(i+1)*lplus(i+1))/(work(i)*lplus(i)))*oldp
                 else
                    prod = prod*abs(lplus(i))
                 end if
                 oldp = prod
                 znm2 = znm2 + prod**2_ilp
                 tmp = max( tmp, abs( work( i ) * prod ))
              end do
              rrr2 = tmp/( spdiam * sqrt( znm2 ) )
              if (rrr2<=maxgrowth2) then
                 sigma = rsigma
                 shift = sright
                 goto 100
              endif
           end if
           endif
           50 continue
           if (ktry<ktrymax) then
              ! if we are here, both shifts failed also the rrr test.
              ! back off to the outside
              lsigma = max( lsigma - ldelta,lsigma - ldmax)
              rsigma = min( rsigma + rdelta,rsigma + rdmax )
              ldelta = two * ldelta
              rdelta = two * rdelta
              ktry = ktry + 1_ilp
              goto 5
           else
              ! none of the representations investigated satisfied our
              ! criteria. take the best one we found.
              if((smlgrowth<fail).or.nofail) then
                 lsigma = bestshift
                 rsigma = bestshift
                 forcer = .true.
                 goto 5
              else
                 info = 1_ilp
                 return
              endif
           end if
           100 continue
           if (shift==sleft) then
           elseif (shift==sright) then
              ! store new l and d back into dplus, lplus
              call stdlib_dcopy( n, work, 1_ilp, dplus, 1_ilp )
              call stdlib_dcopy( n-1, work(n+1), 1_ilp, lplus, 1_ilp )
           endif
           return
     end subroutine stdlib_dlarrf




     pure module subroutine stdlib_slarrj( n, d, e2, ifirst, ilast,rtol, offset, w, werr, work, iwork,&
     !! Given the initial eigenvalue approximations of T, SLARRJ:
     !! does  bisection to refine the eigenvalues of T,
     !! W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
     !! guesses for these eigenvalues are input in W, the corresponding estimate
     !! of the error in these guesses in WERR. During bisection, intervals
     !! [left, right] are maintained by storing their mid-points and
     !! semi-widths in the arrays W and WERR respectively.
               pivmin, spdiam, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ifirst, ilast, n, offset
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: pivmin, rtol, spdiam
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: d(*), e2(*)
           real(sp), intent(inout) :: w(*), werr(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           integer(ilp) :: maxitr
           ! Local Scalars 
           integer(ilp) :: cnt, i, i1, i2, ii, iter, j, k, next, nint, olnint, p, prev, &
                     savi1
           real(sp) :: dplus, fac, left, mid, right, s, tmp, width
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           maxitr = int( ( log( spdiam+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           ! initialize unconverged intervals in [ work(2*i-1), work(2*i) ].
           ! the sturm count, count( work(2*i-1) ) is arranged to be i-1, while
           ! count( work(2*i) ) is stored in iwork( 2*i ). the integer iwork( 2*i-1 )
           ! for an unconverged interval is set to the index of the next unconverged
           ! interval, and is -1 or 0 for a converged interval. thus a linked
           ! list of unconverged intervals is set up.
           i1 = ifirst
           i2 = ilast
           ! the number of unconverged intervals
           nint = 0_ilp
           ! the last unconverged interval found
           prev = 0_ilp
           loop_75: do i = i1, i2
              k = 2_ilp*i
              ii = i - offset
              left = w( ii ) - werr( ii )
              mid = w(ii)
              right = w( ii ) + werr( ii )
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              ! the following test prevents the test of converged intervals
              if( width<rtol*tmp ) then
                 ! this interval has already converged and does not need refinement.
                 ! (note that the gaps might change through refining the
                  ! eigenvalues, however, they can only get bigger.)
                 ! remove it from the list.
                 iwork( k-1 ) = -1_ilp
                 ! make sure that i1 always points to the first unconverged interval
                 if((i==i1).and.(i<i2)) i1 = i + 1_ilp
                 if((prev>=i1).and.(i<=i2)) iwork( 2_ilp*prev-1 ) = i + 1_ilp
              else
                 ! unconverged interval found
                 prev = i
                 ! make sure that [left,right] contains the desired eigenvalue
                 ! do while( cnt(left)>i-1 )
                 fac = one
                 20 continue
                 cnt = 0_ilp
                 s = left
                 dplus = d( 1_ilp ) - s
                 if( dplus<zero ) cnt = cnt + 1_ilp
                 do j = 2, n
                    dplus = d( j ) - s - e2( j-1 )/dplus
                    if( dplus<zero ) cnt = cnt + 1_ilp
                 end do
                 if( cnt>i-1 ) then
                    left = left - werr( ii )*fac
                    fac = two*fac
                    go to 20
                 end if
                 ! do while( cnt(right)<i )
                 fac = one
                 50 continue
                 cnt = 0_ilp
                 s = right
                 dplus = d( 1_ilp ) - s
                 if( dplus<zero ) cnt = cnt + 1_ilp
                 do j = 2, n
                    dplus = d( j ) - s - e2( j-1 )/dplus
                    if( dplus<zero ) cnt = cnt + 1_ilp
                 end do
                 if( cnt<i ) then
                    right = right + werr( ii )*fac
                    fac = two*fac
                    go to 50
                 end if
                 nint = nint + 1_ilp
                 iwork( k-1 ) = i + 1_ilp
                 iwork( k ) = cnt
              end if
              work( k-1 ) = left
              work( k ) = right
           end do loop_75
           savi1 = i1
           ! do while( nint>0 ), i.e. there are still unconverged intervals
           ! and while (iter<maxitr)
           iter = 0_ilp
           80 continue
           prev = i1 - 1_ilp
           i = i1
           olnint = nint
           loop_100: do p = 1, olnint
              k = 2_ilp*i
              ii = i - offset
              next = iwork( k-1 )
              left = work( k-1 )
              right = work( k )
              mid = half*( left + right )
              ! semiwidth of interval
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              if( ( width<rtol*tmp ) .or.(iter==maxitr) )then
                 ! reduce number of unconverged intervals
                 nint = nint - 1_ilp
                 ! mark interval as converged.
                 iwork( k-1 ) = 0_ilp
                 if( i1==i ) then
                    i1 = next
                 else
                    ! prev holds the last unconverged interval previously examined
                    if(prev>=i1) iwork( 2_ilp*prev-1 ) = next
                 end if
                 i = next
                 cycle loop_100
              end if
              prev = i
              ! perform one bisection step
              cnt = 0_ilp
              s = mid
              dplus = d( 1_ilp ) - s
              if( dplus<zero ) cnt = cnt + 1_ilp
              do j = 2, n
                 dplus = d( j ) - s - e2( j-1 )/dplus
                 if( dplus<zero ) cnt = cnt + 1_ilp
              end do
              if( cnt<=i-1 ) then
                 work( k-1 ) = mid
              else
                 work( k ) = mid
              end if
              i = next
           end do loop_100
           iter = iter + 1_ilp
           ! do another loop if there are still unconverged intervals
           ! however, in the last iteration, all intervals are accepted
           ! since this is the best we can do.
           if( ( nint>0 ).and.(iter<=maxitr) ) go to 80
           ! at this point, all the intervals have converged
           do i = savi1, ilast
              k = 2_ilp*i
              ii = i - offset
              ! all intervals marked by '0' have been refined.
              if( iwork( k-1 )==0_ilp ) then
                 w( ii ) = half*( work( k-1 )+work( k ) )
                 werr( ii ) = work( k ) - w( ii )
              end if
           end do
           return
     end subroutine stdlib_slarrj

     pure module subroutine stdlib_dlarrj( n, d, e2, ifirst, ilast,rtol, offset, w, werr, work, iwork,&
     !! Given the initial eigenvalue approximations of T, DLARRJ:
     !! does  bisection to refine the eigenvalues of T,
     !! W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
     !! guesses for these eigenvalues are input in W, the corresponding estimate
     !! of the error in these guesses in WERR. During bisection, intervals
     !! [left, right] are maintained by storing their mid-points and
     !! semi-widths in the arrays W and WERR respectively.
               pivmin, spdiam, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ifirst, ilast, n, offset
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: pivmin, rtol, spdiam
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: d(*), e2(*)
           real(dp), intent(inout) :: w(*), werr(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           integer(ilp) :: maxitr
           ! Local Scalars 
           integer(ilp) :: cnt, i, i1, i2, ii, iter, j, k, next, nint, olnint, p, prev, &
                     savi1
           real(dp) :: dplus, fac, left, mid, right, s, tmp, width
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           maxitr = int( ( log( spdiam+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           ! initialize unconverged intervals in [ work(2*i-1), work(2*i) ].
           ! the sturm count, count( work(2*i-1) ) is arranged to be i-1, while
           ! count( work(2*i) ) is stored in iwork( 2*i ). the integer iwork( 2*i-1 )
           ! for an unconverged interval is set to the index of the next unconverged
           ! interval, and is -1 or 0 for a converged interval. thus a linked
           ! list of unconverged intervals is set up.
           i1 = ifirst
           i2 = ilast
           ! the number of unconverged intervals
           nint = 0_ilp
           ! the last unconverged interval found
           prev = 0_ilp
           loop_75: do i = i1, i2
              k = 2_ilp*i
              ii = i - offset
              left = w( ii ) - werr( ii )
              mid = w(ii)
              right = w( ii ) + werr( ii )
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              ! the following test prevents the test of converged intervals
              if( width<rtol*tmp ) then
                 ! this interval has already converged and does not need refinement.
                 ! (note that the gaps might change through refining the
                  ! eigenvalues, however, they can only get bigger.)
                 ! remove it from the list.
                 iwork( k-1 ) = -1_ilp
                 ! make sure that i1 always points to the first unconverged interval
                 if((i==i1).and.(i<i2)) i1 = i + 1_ilp
                 if((prev>=i1).and.(i<=i2)) iwork( 2_ilp*prev-1 ) = i + 1_ilp
              else
                 ! unconverged interval found
                 prev = i
                 ! make sure that [left,right] contains the desired eigenvalue
                 ! do while( cnt(left)>i-1 )
                 fac = one
                 20 continue
                 cnt = 0_ilp
                 s = left
                 dplus = d( 1_ilp ) - s
                 if( dplus<zero ) cnt = cnt + 1_ilp
                 do j = 2, n
                    dplus = d( j ) - s - e2( j-1 )/dplus
                    if( dplus<zero ) cnt = cnt + 1_ilp
                 end do
                 if( cnt>i-1 ) then
                    left = left - werr( ii )*fac
                    fac = two*fac
                    go to 20
                 end if
                 ! do while( cnt(right)<i )
                 fac = one
                 50 continue
                 cnt = 0_ilp
                 s = right
                 dplus = d( 1_ilp ) - s
                 if( dplus<zero ) cnt = cnt + 1_ilp
                 do j = 2, n
                    dplus = d( j ) - s - e2( j-1 )/dplus
                    if( dplus<zero ) cnt = cnt + 1_ilp
                 end do
                 if( cnt<i ) then
                    right = right + werr( ii )*fac
                    fac = two*fac
                    go to 50
                 end if
                 nint = nint + 1_ilp
                 iwork( k-1 ) = i + 1_ilp
                 iwork( k ) = cnt
              end if
              work( k-1 ) = left
              work( k ) = right
           end do loop_75
           savi1 = i1
           ! do while( nint>0 ), i.e. there are still unconverged intervals
           ! and while (iter<maxitr)
           iter = 0_ilp
           80 continue
           prev = i1 - 1_ilp
           i = i1
           olnint = nint
           loop_100: do p = 1, olnint
              k = 2_ilp*i
              ii = i - offset
              next = iwork( k-1 )
              left = work( k-1 )
              right = work( k )
              mid = half*( left + right )
              ! semiwidth of interval
              width = right - mid
              tmp = max( abs( left ), abs( right ) )
              if( ( width<rtol*tmp ) .or.(iter==maxitr) )then
                 ! reduce number of unconverged intervals
                 nint = nint - 1_ilp
                 ! mark interval as converged.
                 iwork( k-1 ) = 0_ilp
                 if( i1==i ) then
                    i1 = next
                 else
                    ! prev holds the last unconverged interval previously examined
                    if(prev>=i1) iwork( 2_ilp*prev-1 ) = next
                 end if
                 i = next
                 cycle loop_100
              end if
              prev = i
              ! perform one bisection step
              cnt = 0_ilp
              s = mid
              dplus = d( 1_ilp ) - s
              if( dplus<zero ) cnt = cnt + 1_ilp
              do j = 2, n
                 dplus = d( j ) - s - e2( j-1 )/dplus
                 if( dplus<zero ) cnt = cnt + 1_ilp
              end do
              if( cnt<=i-1 ) then
                 work( k-1 ) = mid
              else
                 work( k ) = mid
              end if
              i = next
           end do loop_100
           iter = iter + 1_ilp
           ! do another loop if there are still unconverged intervals
           ! however, in the last iteration, all intervals are accepted
           ! since this is the best we can do.
           if( ( nint>0 ).and.(iter<=maxitr) ) go to 80
           ! at this point, all the intervals have converged
           do i = savi1, ilast
              k = 2_ilp*i
              ii = i - offset
              ! all intervals marked by '0' have been refined.
              if( iwork( k-1 )==0_ilp ) then
                 w( ii ) = half*( work( k-1 )+work( k ) )
                 werr( ii ) = work( k ) - w( ii )
              end if
           end do
           return
     end subroutine stdlib_dlarrj




     pure module subroutine stdlib_slarrk( n, iw, gl, gu,d, e2, pivmin, reltol, w, werr, info)
     !! SLARRK computes one eigenvalue of a symmetric tridiagonal
     !! matrix T to suitable accuracy. This is an auxiliary code to be
     !! called from SSTEMR.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: iw, n
           real(sp), intent(in) :: pivmin, reltol, gl, gu
           real(sp), intent(out) :: w, werr
           ! Array Arguments 
           real(sp), intent(in) :: d(*), e2(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: fudge = two
           
           ! Local Scalars 
           integer(ilp) :: i, it, itmax, negcnt
           real(sp) :: atoli, eps, left, mid, right, rtoli, tmp1, tmp2, tnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              info = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           tnorm = max( abs( gl ), abs( gu ) )
           rtoli = reltol
           atoli = fudge*two*pivmin
           itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           info = -1_ilp
           left = gl - fudge*tnorm*eps*n - fudge*two*pivmin
           right = gu + fudge*tnorm*eps*n + fudge*two*pivmin
           it = 0_ilp
           10 continue
           ! check if interval converged or maximum number of iterations reached
           tmp1 = abs( right - left )
           tmp2 = max( abs(right), abs(left) )
           if( tmp1<max( atoli, pivmin, rtoli*tmp2 ) ) then
              info = 0_ilp
              goto 30
           endif
           if(it>itmax)goto 30
           ! count number of negative pivots for mid-point
           it = it + 1_ilp
           mid = half * (left + right)
           negcnt = 0_ilp
           tmp1 = d( 1_ilp ) - mid
           if( abs( tmp1 )<pivmin )tmp1 = -pivmin
           if( tmp1<=zero )negcnt = negcnt + 1_ilp
           do i = 2, n
              tmp1 = d( i ) - e2( i-1 ) / tmp1 - mid
              if( abs( tmp1 )<pivmin )tmp1 = -pivmin
              if( tmp1<=zero )negcnt = negcnt + 1_ilp
           end do
           if(negcnt>=iw) then
              right = mid
           else
              left = mid
           endif
           goto 10
           30 continue
           ! converged or maximum number of iterations reached
           w = half * (left + right)
           werr = half * abs( right - left )
           return
     end subroutine stdlib_slarrk

     pure module subroutine stdlib_dlarrk( n, iw, gl, gu,d, e2, pivmin, reltol, w, werr, info)
     !! DLARRK computes one eigenvalue of a symmetric tridiagonal
     !! matrix T to suitable accuracy. This is an auxiliary code to be
     !! called from DSTEMR.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: iw, n
           real(dp), intent(in) :: pivmin, reltol, gl, gu
           real(dp), intent(out) :: w, werr
           ! Array Arguments 
           real(dp), intent(in) :: d(*), e2(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: fudge = two
           
           ! Local Scalars 
           integer(ilp) :: i, it, itmax, negcnt
           real(dp) :: atoli, eps, left, mid, right, rtoli, tmp1, tmp2, tnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              info = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           tnorm = max( abs( gl ), abs( gu ) )
           rtoli = reltol
           atoli = fudge*two*pivmin
           itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
           info = -1_ilp
           left = gl - fudge*tnorm*eps*n - fudge*two*pivmin
           right = gu + fudge*tnorm*eps*n + fudge*two*pivmin
           it = 0_ilp
           10 continue
           ! check if interval converged or maximum number of iterations reached
           tmp1 = abs( right - left )
           tmp2 = max( abs(right), abs(left) )
           if( tmp1<max( atoli, pivmin, rtoli*tmp2 ) ) then
              info = 0_ilp
              goto 30
           endif
           if(it>itmax)goto 30
           ! count number of negative pivots for mid-point
           it = it + 1_ilp
           mid = half * (left + right)
           negcnt = 0_ilp
           tmp1 = d( 1_ilp ) - mid
           if( abs( tmp1 )<pivmin )tmp1 = -pivmin
           if( tmp1<=zero )negcnt = negcnt + 1_ilp
           do i = 2, n
              tmp1 = d( i ) - e2( i-1 ) / tmp1 - mid
              if( abs( tmp1 )<pivmin )tmp1 = -pivmin
              if( tmp1<=zero )negcnt = negcnt + 1_ilp
           end do
           if(negcnt>=iw) then
              right = mid
           else
              left = mid
           endif
           goto 10
           30 continue
           ! converged or maximum number of iterations reached
           w = half * (left + right)
           werr = half * abs( right - left )
           return
     end subroutine stdlib_dlarrk




     pure module subroutine stdlib_slarrr( n, d, e, info )
     !! Perform tests to decide whether the symmetric tridiagonal matrix T
     !! warrants expensive computations which guarantee high relative accuracy
     !! in the eigenvalues.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: relcond = 0.999_sp
           
           ! Local Scalars 
           integer(ilp) :: i
           logical(lk) :: yesrel
           real(sp) :: eps, safmin, smlnum, rmin, tmp, tmp2, offdig, offdig2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              info = 0_ilp
              return
           end if
           ! as a default, do not go for relative-accuracy preserving computations.
           info = 1_ilp
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           rmin = sqrt( smlnum )
           ! tests for relative accuracy
           ! test for scaled diagonal dominance
           ! scale the diagonal entries to one and check whether the sum of the
           ! off-diagonals is less than one
           ! the sdd relative error bounds have a 1/(1- 2*x) factor in them,
           ! x = max(offdig + offdig2), so when x is close to 1/2, no relative
           ! accuracy is promised.  in the notation of the code fragment below,
           ! 1/(1 - (offdig + offdig2)) is the condition number.
           ! we don't think it is worth going into "sdd mode" unless the relative
           ! condition number is reasonable, not 1/macheps.
           ! the threshold should be compatible with other thresholds used in the
           ! code. we set  offdig + offdig2 <= .999_sp =: relcond, it corresponds
           ! to losing at most 3 decimal digits: 1 / (1 - (offdig + offdig2)) <= 1000
           ! instead of the current offdig + offdig2 < 1
           yesrel = .true.
           offdig = zero
           tmp = sqrt(abs(d(1_ilp)))
           if (tmp<rmin) yesrel = .false.
           if(.not.yesrel) goto 11
           do i = 2, n
              tmp2 = sqrt(abs(d(i)))
              if (tmp2<rmin) yesrel = .false.
              if(.not.yesrel) goto 11
              offdig2 = abs(e(i-1))/(tmp*tmp2)
              if(offdig+offdig2>=relcond) yesrel = .false.
              if(.not.yesrel) goto 11
              tmp = tmp2
              offdig = offdig2
           end do
           11 continue
           if( yesrel ) then
              info = 0_ilp
              return
           else
           endif
           ! *** more to be implemented ***
           ! test if the lower bidiagonal matrix l from t = l d l^t
           ! (zero shift facto) is well conditioned
           ! test if the upper bidiagonal matrix u from t = u d u^t
           ! (zero shift facto) is well conditioned.
           ! in this case, the matrix needs to be flipped and, at the end
           ! of the eigenvector computation, the flip needs to be applied
           ! to the computed eigenvectors (and the support)
           return
     end subroutine stdlib_slarrr

     pure module subroutine stdlib_dlarrr( n, d, e, info )
     !! Perform tests to decide whether the symmetric tridiagonal matrix T
     !! warrants expensive computations which guarantee high relative accuracy
     !! in the eigenvalues.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: relcond = 0.999_dp
           
           ! Local Scalars 
           integer(ilp) :: i
           logical(lk) :: yesrel
           real(dp) :: eps, safmin, smlnum, rmin, tmp, tmp2, offdig, offdig2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              info = 0_ilp
              return
           end if
           ! as a default, do not go for relative-accuracy preserving computations.
           info = 1_ilp
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           rmin = sqrt( smlnum )
           ! tests for relative accuracy
           ! test for scaled diagonal dominance
           ! scale the diagonal entries to one and check whether the sum of the
           ! off-diagonals is less than one
           ! the sdd relative error bounds have a 1/(1- 2*x) factor in them,
           ! x = max(offdig + offdig2), so when x is close to 1/2, no relative
           ! accuracy is promised.  in the notation of the code fragment below,
           ! 1/(1 - (offdig + offdig2)) is the condition number.
           ! we don't think it is worth going into "sdd mode" unless the relative
           ! condition number is reasonable, not 1/macheps.
           ! the threshold should be compatible with other thresholds used in the
           ! code. we set  offdig + offdig2 <= .999_dp =: relcond, it corresponds
           ! to losing at most 3 decimal digits: 1 / (1 - (offdig + offdig2)) <= 1000
           ! instead of the current offdig + offdig2 < 1
           yesrel = .true.
           offdig = zero
           tmp = sqrt(abs(d(1_ilp)))
           if (tmp<rmin) yesrel = .false.
           if(.not.yesrel) goto 11
           do i = 2, n
              tmp2 = sqrt(abs(d(i)))
              if (tmp2<rmin) yesrel = .false.
              if(.not.yesrel) goto 11
              offdig2 = abs(e(i-1))/(tmp*tmp2)
              if(offdig+offdig2>=relcond) yesrel = .false.
              if(.not.yesrel) goto 11
              tmp = tmp2
              offdig = offdig2
           end do
           11 continue
           if( yesrel ) then
              info = 0_ilp
              return
           else
           endif
           ! *** more to be implemented ***
           ! test if the lower bidiagonal matrix l from t = l d l^t
           ! (zero shift facto) is well conditioned
           ! test if the upper bidiagonal matrix u from t = u d u^t
           ! (zero shift facto) is well conditioned.
           ! in this case, the matrix needs to be flipped and, at the end
           ! of the eigenvector computation, the flip needs to be applied
           ! to the computed eigenvectors (and the support)
           return
     end subroutine stdlib_dlarrr




     pure module subroutine stdlib_slarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
     !! SLARRV computes the eigenvectors of the tridiagonal matrix
     !! T = L D L**T given L, D and APPROXIMATIONS to the eigenvalues of L D L**T.
     !! The input eigenvalues should have been computed by SLARRE.
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: minrgp, pivmin, vl, vu
           real(sp), intent(inout) :: rtol1, rtol2
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(sp), intent(in) :: gers(*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 10_ilp
           
           
           ! Local Scalars 
           logical(lk) :: eskip, needbs, stp2ii, tryrqc, usedbs, usedrq
           integer(ilp) :: done, i, ibegin, idone, iend, ii, iindc1, iindc2, iindr, iindwk, iinfo,&
            im, in, indeig, indld, indlld, indwrk, isupmn, isupmx, iter, itmp1, j, jblk, k, &
            miniwsize, minwsize, nclus, ndepth, negcnt, newcls, newfst, newftt, newlst, newsiz, &
            offset, oldcls, oldfst, oldien, oldlst, oldncl, p, parity, q, wbegin, wend, windex, &
                      windmn, windpl, zfrom, zto, zusedl, zusedu, zusedw
           real(sp) :: bstres, bstw, eps, fudge, gap, gaptol, gl, gu, lambda, left, lgap, mingma, &
           nrminv, resid, rgap, right, rqcorr, rqtol, savgap, sgndef, sigma, spdiam, ssigma, tau, &
                     tmp, tol, ztz
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( (n<=0_ilp).or.(m<=0_ilp) ) then
              return
           end if
           ! the first n entries of work are reserved for the eigenvalues
           indld = n+1
           indlld= 2_ilp*n+1
           indwrk= 3_ilp*n+1
           minwsize = 12_ilp * n
           do i= 1,minwsize
              work( i ) = zero
           end do
           ! iwork(iindr+1:iindr+n) hold the twist indices r for the
           ! factorization used to compute the fp vector
           iindr = 0_ilp
           ! iwork(iindc1+1:iinc2+n) are used to store the clusters of the current
           ! layer and the one above.
           iindc1 = n
           iindc2 = 2_ilp*n
           iindwk = 3_ilp*n + 1_ilp
           miniwsize = 7_ilp * n
           do i= 1,miniwsize
              iwork( i ) = 0_ilp
           end do
           zusedl = 1_ilp
           if(dol>1_ilp) then
              ! set lower bound for use of z
              zusedl = dol-1
           endif
           zusedu = m
           if(dou<m) then
              ! set lower bound for use of z
              zusedu = dou+1
           endif
           ! the width of the part of z that is used
           zusedw = zusedu - zusedl + 1_ilp
           call stdlib_slaset( 'FULL', n, zusedw, zero, zero,z(1_ilp,zusedl), ldz )
           eps = stdlib_slamch( 'PRECISION' )
           rqtol = two * eps
           ! set expert flags for standard code.
           tryrqc = .true.
           if((dol==1_ilp).and.(dou==m)) then
           else
              ! only selected eigenpairs are computed. since the other evalues
              ! are not refined by rq iteration, bisection has to compute to full
              ! accuracy.
              rtol1 = four * eps
              rtol2 = four * eps
           endif
           ! the entries wbegin:wend in w, werr, wgap correspond to the
           ! desired eigenvalues. the support of the nonzero eigenvector
           ! entries is contained in the interval ibegin:iend.
           ! remark that if k eigenpairs are desired, then the eigenvectors
           ! are stored in k contiguous columns of z.
           ! done is the number of eigenvectors already computed
           done = 0_ilp
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, iblock( m )
              iend = isplit( jblk )
              sigma = l( iend )
              ! find the eigenvectors of the submatrix indexed ibegin
              ! through iend.
              wend = wbegin - 1_ilp
              15 continue
              if( wend<m ) then
                 if( iblock( wend+1 )==jblk ) then
                    wend = wend + 1_ilp
                    go to 15
                 end if
              end if
              if( wend<wbegin ) then
                 ibegin = iend + 1_ilp
                 cycle loop_170
              elseif( (wend<dol).or.(wbegin>dou) ) then
                 ibegin = iend + 1_ilp
                 wbegin = wend + 1_ilp
                 cycle loop_170
              end if
              ! find local spectral diameter of the block
              gl = gers( 2_ilp*ibegin-1 )
              gu = gers( 2_ilp*ibegin )
              do i = ibegin+1 , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              ! oldien is the last index of the previous block
              oldien = ibegin - 1_ilp
              ! calculate the size of the current block
              in = iend - ibegin + 1_ilp
              ! the number of eigenvalues in the current block
              im = wend - wbegin + 1_ilp
              ! this is for a 1x1 block
              if( ibegin==iend ) then
                 done = done+1
                 z( ibegin, wbegin ) = one
                 isuppz( 2_ilp*wbegin-1 ) = ibegin
                 isuppz( 2_ilp*wbegin ) = ibegin
                 w( wbegin ) = w( wbegin ) + sigma
                 work( wbegin ) = w( wbegin )
                 ibegin = iend + 1_ilp
                 wbegin = wbegin + 1_ilp
                 cycle loop_170
              end if
              ! the desired (shifted) eigenvalues are stored in w(wbegin:wend)
              ! note that these can be approximations, in this case, the corresp.
              ! entries of werr give the size of the uncertainty interval.
              ! the eigenvalue approximations will be refined when necessary as
              ! high relative accuracy is required for the computation of the
              ! corresponding eigenvectors.
              call stdlib_scopy( im, w( wbegin ), 1_ilp,work( wbegin ), 1_ilp )
              ! we store in w the eigenvalue approximations w.r.t. the original
              ! matrix t.
              do i=1,im
                 w(wbegin+i-1) = w(wbegin+i-1)+sigma
              end do
              ! ndepth is the current depth of the representation tree
              ndepth = 0_ilp
              ! parity is either 1 or 0
              parity = 1_ilp
              ! nclus is the number of clusters for the next level of the
              ! representation tree, we start with nclus = 1 for the root
              nclus = 1_ilp
              iwork( iindc1+1 ) = 1_ilp
              iwork( iindc1+2 ) = im
              ! idone is the number of eigenvectors already computed in the current
              ! block
              idone = 0_ilp
              ! loop while( idone<im )
              ! generate the representation tree for the current block and
              ! compute the eigenvectors
              40 continue
              if( idone<im ) then
                 ! this is a crude protection against infinitely deep trees
                 if( ndepth>m ) then
                    info = -2_ilp
                    return
                 endif
                 ! breadth first processing of the current level of the representation
                 ! tree: oldncl = number of clusters on current level
                 oldncl = nclus
                 ! reset nclus to count the number of child clusters
                 nclus = 0_ilp
                 parity = 1_ilp - parity
                 if( parity==0_ilp ) then
                    oldcls = iindc1
                    newcls = iindc2
                 else
                    oldcls = iindc2
                    newcls = iindc1
                 end if
                 ! process the clusters on the current level
                 loop_150: do i = 1, oldncl
                    j = oldcls + 2_ilp*i
                    ! oldfst, oldlst = first, last index of current cluster.
                                     ! cluster indices start with 1 and are relative
                                     ! to wbegin when accessing w, wgap, werr, z
                    oldfst = iwork( j-1 )
                    oldlst = iwork( j )
                    if( ndepth>0_ilp ) then
                       ! retrieve relatively robust representation (rrr) of cluster
                       ! that has been computed at the previous level
                       ! the rrr is stored in z and overwritten once the eigenvectors
                       ! have been computed or when the cluster is refined
                       if((dol==1_ilp).and.(dou==m)) then
                          ! get representation from location of the leftmost evalue
                          ! of the cluster
                          j = wbegin + oldfst - 1_ilp
                       else
                          if(wbegin+oldfst-1<dol) then
                             ! get representation from the left end of z array
                             j = dol - 1_ilp
                          elseif(wbegin+oldfst-1>dou) then
                             ! get representation from the right end of z array
                             j = dou
                          else
                             j = wbegin + oldfst - 1_ilp
                          endif
                       endif
                       call stdlib_scopy( in, z( ibegin, j ), 1_ilp, d( ibegin ), 1_ilp )
                       call stdlib_scopy( in-1, z( ibegin, j+1 ), 1_ilp, l( ibegin ),1_ilp )
                       sigma = z( iend, j+1 )
                       ! set the corresponding entries in z to zero
                       call stdlib_slaset( 'FULL', in, 2_ilp, zero, zero,z( ibegin, j), ldz )
                    end if
                    ! compute dl and dll of current rrr
                    do j = ibegin, iend-1
                       tmp = d( j )*l( j )
                       work( indld-1+j ) = tmp
                       work( indlld-1+j ) = tmp*l( j )
                    end do
                    if( ndepth>0_ilp ) then
                       ! p and q are index of the first and last eigenvalue to compute
                       ! within the current block
                       p = indexw( wbegin-1+oldfst )
                       q = indexw( wbegin-1+oldlst )
                       ! offset for the arrays work, wgap and werr, i.e., the p-offset
                       ! through the q-offset elements of these arrays are to be used.
                        ! offset = p-oldfst
                       offset = indexw( wbegin ) - 1_ilp
                       ! perform limited bisection (if necessary) to get approximate
                       ! eigenvalues to the precision needed.
                       call stdlib_slarrb( in, d( ibegin ),work(indlld+ibegin-1),p, q, rtol1, &
                       rtol2, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ), iwork(&
                                  iindwk ),pivmin, spdiam, in, iinfo )
                       if( iinfo/=0_ilp ) then
                          info = -1_ilp
                          return
                       endif
                       ! we also recompute the extremal gaps. w holds all eigenvalues
                       ! of the unshifted matrix and must be used for computation
                       ! of wgap, the entries of work might stem from rrrs with
                       ! different shifts. the gaps from wbegin-1+oldfst to
                       ! wbegin-1+oldlst are correctly computed in stdlib_slarrb.
                       ! however, we only allow the gaps to become greater since
                       ! this is what should happen when we decrease werr
                       if( oldfst>1_ilp) then
                          wgap( wbegin+oldfst-2 ) =max(wgap(wbegin+oldfst-2),w(wbegin+oldfst-1)-&
                          werr(wbegin+oldfst-1)- w(wbegin+oldfst-2)-werr(wbegin+oldfst-2) )
                                    
                       endif
                       if( wbegin + oldlst -1_ilp < wend ) then
                          wgap( wbegin+oldlst-1 ) =max(wgap(wbegin+oldlst-1),w(wbegin+oldlst)-&
                                    werr(wbegin+oldlst)- w(wbegin+oldlst-1)-werr(wbegin+oldlst-1) )
                       endif
                       ! each time the eigenvalues in work get refined, we store
                       ! the newly found approximation with all shifts applied in w
                       do j=oldfst,oldlst
                          w(wbegin+j-1) = work(wbegin+j-1)+sigma
                       end do
                    end if
                    ! process the current node.
                    newfst = oldfst
                    loop_140: do j = oldfst, oldlst
                       if( j==oldlst ) then
                          ! we are at the right end of the cluster, this is also the
                          ! boundary of the child cluster
                          newlst = j
                       else if ( wgap( wbegin + j -1_ilp)>=minrgp* abs( work(wbegin + j -1_ilp) ) ) &
                                 then
                          ! the right relative gap is big enough, the child cluster
                          ! (newfst,..,newlst) is well separated from the following
                          newlst = j
                        else
                          ! inside a child cluster, the relative gap is not
                          ! big enough.
                          cycle loop_140
                       end if
                       ! compute size of child cluster found
                       newsiz = newlst - newfst + 1_ilp
                       ! newftt is the place in z where the new rrr or the computed
                       ! eigenvector is to be stored
                       if((dol==1_ilp).and.(dou==m)) then
                          ! store representation at location of the leftmost evalue
                          ! of the cluster
                          newftt = wbegin + newfst - 1_ilp
                       else
                          if(wbegin+newfst-1<dol) then
                             ! store representation at the left end of z array
                             newftt = dol - 1_ilp
                          elseif(wbegin+newfst-1>dou) then
                             ! store representation at the right end of z array
                             newftt = dou
                          else
                             newftt = wbegin + newfst - 1_ilp
                          endif
                       endif
                       if( newsiz>1_ilp) then
                          ! current child is not a singleton but a cluster.
                          ! compute and store new representation of child.
                          ! compute left and right cluster gap.
                          ! lgap and rgap are not computed from work because
                          ! the eigenvalue approximations may stem from rrrs
                          ! different shifts. however, w hold all eigenvalues
                          ! of the unshifted matrix. still, the entries in wgap
                          ! have to be computed from work since the entries
                          ! in w might be of the same order so that gaps are not
                          ! exhibited correctly for very close eigenvalues.
                          if( newfst==1_ilp ) then
                             lgap = max( zero,w(wbegin)-werr(wbegin) - vl )
                         else
                             lgap = wgap( wbegin+newfst-2 )
                          endif
                          rgap = wgap( wbegin+newlst-1 )
                          ! compute left- and rightmost eigenvalue of child
                          ! to high precision in order to shift as close
                          ! as possible and obtain as large relative gaps
                          ! as possible
                          do k =1,2
                             if(k==1_ilp) then
                                p = indexw( wbegin-1+newfst )
                             else
                                p = indexw( wbegin-1+newlst )
                             endif
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_slarrb( in, d(ibegin),work( indlld+ibegin-1 ),p,p,rqtol, &
                             rqtol, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ),&
                                       iwork( iindwk ), pivmin, spdiam,in, iinfo )
                          end do
                          if((wbegin+newlst-1<dol).or.(wbegin+newfst-1>dou)) then
                             ! if the cluster contains no desired eigenvalues
                             ! skip the computation of that branch of the rep. tree
                             ! we could skip before the refinement of the extremal
                             ! eigenvalues of the child, but then the representation
                             ! tree could be different from the one when nothing is
                             ! skipped. for this reason we skip at this place.
                             idone = idone + newlst - newfst + 1_ilp
                             goto 139
                          endif
                          ! compute rrr of child cluster.
                          ! note that the new rrr is stored in z
                          ! stdlib_slarrf needs lwork = 2*n
                          call stdlib_slarrf( in, d( ibegin ), l( ibegin ),work(indld+ibegin-1),&
                          newfst, newlst, work(wbegin),wgap(wbegin), werr(wbegin),spdiam, lgap, &
                          rgap, pivmin, tau,z(ibegin, newftt),z(ibegin, newftt+1),work( indwrk ), &
                                    iinfo )
                          if( iinfo==0_ilp ) then
                             ! a new rrr for the cluster was found by stdlib_slarrf
                             ! update shift and store it
                             ssigma = sigma + tau
                             z( iend, newftt+1 ) = ssigma
                             ! work() are the midpoints and werr() the semi-width
                             ! note that the entries in w are unchanged.
                             do k = newfst, newlst
                                fudge =three*eps*abs(work(wbegin+k-1))
                                work( wbegin + k - 1_ilp ) =work( wbegin + k - 1_ilp) - tau
                                fudge = fudge +four*eps*abs(work(wbegin+k-1))
                                ! fudge errors
                                werr( wbegin + k - 1_ilp ) =werr( wbegin + k - 1_ilp ) + fudge
                                ! gaps are not fudged. provided that werr is small
                                ! when eigenvalues are close, a zero gap indicates
                                ! that a new representation is needed for resolving
                                ! the cluster. a fudge could lead to a wrong decision
                                ! of judging eigenvalues 'separated' which in
                                ! reality are not. this could have a negative impact
                                ! on the orthogonality of the computed eigenvectors.
                             end do
                             nclus = nclus + 1_ilp
                             k = newcls + 2_ilp*nclus
                             iwork( k-1 ) = newfst
                             iwork( k ) = newlst
                          else
                             info = -2_ilp
                             return
                          endif
                       else
                          ! compute eigenvector of singleton
                          iter = 0_ilp
                          tol = four * log(real(in,KIND=sp)) * eps
                          k = newfst
                          windex = wbegin + k - 1_ilp
                          windmn = max(windex - 1_ilp,1_ilp)
                          windpl = min(windex + 1_ilp,m)
                          lambda = work( windex )
                          done = done + 1_ilp
                          ! check if eigenvector computation is to be skipped
                          if((windex<dol).or.(windex>dou)) then
                             eskip = .true.
                             goto 125
                          else
                             eskip = .false.
                          endif
                          left = work( windex ) - werr( windex )
                          right = work( windex ) + werr( windex )
                          indeig = indexw( windex )
                          ! note that since we compute the eigenpairs for a child,
                          ! all eigenvalue approximations are w.r.t the same shift.
                          ! in this case, the entries in work should be used for
                          ! computing the gaps since they exhibit even very small
                          ! differences in the eigenvalues, as opposed to the
                          ! entries in w which might "look" the same.
                          if( k == 1_ilp) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vl, the formula
                             ! lgap = max( zero, (sigma - vl) + lambda )
                             ! can lead to an overestimation of the left gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small left gap.
                             lgap = eps*max(abs(left),abs(right))
                          else
                             lgap = wgap(windmn)
                          endif
                          if( k == im) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vu, the formula
                             ! can lead to an overestimation of the right gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small right gap.
                             rgap = eps*max(abs(left),abs(right))
                          else
                             rgap = wgap(windex)
                          endif
                          gap = min( lgap, rgap )
                          if(( k == 1_ilp).or.(k == im)) then
                             ! the eigenvector support can become wrong
                             ! because significant entries could be cut off due to a
                             ! large gaptol parameter in lar1v. prevent this.
                             gaptol = zero
                          else
                             gaptol = gap * eps
                          endif
                          isupmn = in
                          isupmx = 1_ilp
                          ! update wgap so that it holds the minimum gap
                          ! to the left or the right. this is crucial in the
                          ! case where bisection is used to ensure that the
                          ! eigenvalue is refined up to the required precision.
                          ! the correct value is restored afterwards.
                          savgap = wgap(windex)
                          wgap(windex) = gap
                          ! we want to use the rayleigh quotient correction
                          ! as often as possible since it converges quadratically
                          ! when we are close enough to the desired eigenvalue.
                          ! however, the rayleigh quotient can have the wrong sign
                          ! and lead us away from the desired eigenvalue. in this
                          ! case, the best we can do is to use bisection.
                          usedbs = .false.
                          usedrq = .false.
                          ! bisection is initially turned off unless it is forced
                          needbs =  .not.tryrqc
                          120 continue
                          ! check if bisection should be used to refine eigenvalue
                          if(needbs) then
                             ! take the bisection as new iterate
                             usedbs = .true.
                             itmp1 = iwork( iindr+windex )
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_slarrb( in, d(ibegin),work(indlld+ibegin-1),indeig,&
                             indeig,zero, two*eps, offset,work(wbegin),wgap(wbegin),werr(wbegin),&
                                       work( indwrk ),iwork( iindwk ), pivmin, spdiam,itmp1, iinfo )
                             if( iinfo/=0_ilp ) then
                                info = -3_ilp
                                return
                             endif
                             lambda = work( windex )
                             ! reset twist index from inaccurate lambda to
                             ! force computation of true mingma
                             iwork( iindr+windex ) = 0_ilp
                          endif
                          ! given lambda, compute the eigenvector.
                          call stdlib_slar1v( in, 1_ilp, in, lambda, d( ibegin ),l( ibegin ), work(&
                          indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( ibegin, windex &
                          ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+windex ), isuppz( &
                                    2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk ) )
                          if(iter == 0_ilp) then
                             bstres = resid
                             bstw = lambda
                          elseif(resid<bstres) then
                             bstres = resid
                             bstw = lambda
                          endif
                          isupmn = min(isupmn,isuppz( 2_ilp*windex-1 ))
                          isupmx = max(isupmx,isuppz( 2_ilp*windex ))
                          iter = iter + 1_ilp
                          ! sin alpha <= |resid|/gap
                          ! note that both the residual and the gap are
                          ! proportional to the matrix, so ||t|| doesn't play
                          ! a role in the quotient
                          ! convergence test for rayleigh-quotient iteration
                          ! (omitted when bisection has been used)
                          if( resid>tol*gap .and. abs( rqcorr )>rqtol*abs( lambda ) .and. .not. &
                                    usedbs)then
                             ! we need to check that the rqcorr update doesn't
                             ! move the eigenvalue away from the desired one and
                             ! towards a neighbor. -> protection with bisection
                             if(indeig<=negcnt) then
                                ! the wanted eigenvalue lies to the left
                                sgndef = -one
                             else
                                ! the wanted eigenvalue lies to the right
                                sgndef = one
                             endif
                             ! we only use the rqcorr if it improves the
                             ! the iterate reasonably.
                             if( ( rqcorr*sgndef>=zero ).and.( lambda + rqcorr<= right).and.( &
                                       lambda + rqcorr>= left)) then
                                usedrq = .true.
                                ! store new midpoint of bisection interval in work
                                if(sgndef==one) then
                                   ! the current lambda is on the left of the true
                                   ! eigenvalue
                                   left = lambda
                                   ! we prefer to assume that the error estimate
                                   ! is correct. we could make the interval not
                                   ! as a bracket but to be modified if the rqcorr
                                   ! chooses to. in this case, the right side should
                                   ! be modified as follows:
                                    ! right = max(right, lambda + rqcorr)
                                else
                                   ! the current lambda is on the right of the true
                                   ! eigenvalue
                                   right = lambda
                                   ! see comment about assuming the error estimate is
                                   ! correct above.
                                    ! left = min(left, lambda + rqcorr)
                                endif
                                work( windex ) =half * (right + left)
                                ! take rqcorr since it has the correct sign and
                                ! improves the iterate reasonably
                                lambda = lambda + rqcorr
                                ! update width of error interval
                                werr( windex ) =half * (right-left)
                             else
                                needbs = .true.
                             endif
                             if(right-left<rqtol*abs(lambda)) then
                                   ! the eigenvalue is computed to bisection accuracy
                                   ! compute eigenvector and stop
                                usedbs = .true.
                                goto 120
                             elseif( iter<maxitr ) then
                                goto 120
                             elseif( iter==maxitr ) then
                                needbs = .true.
                                goto 120
                             else
                                info = 5_ilp
                                return
                             end if
                          else
                             stp2ii = .false.
             if(usedrq .and. usedbs .and.bstres<=resid) then
                                lambda = bstw
                                stp2ii = .true.
                             endif
                             if (stp2ii) then
                                ! improve error angle by second step
                                call stdlib_slar1v( in, 1_ilp, in, lambda,d( ibegin ), l( ibegin ),&
                                work(indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( &
                                ibegin, windex ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+&
                                windex ),isuppz( 2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk &
                                          ) )
                             endif
                             work( windex ) = lambda
                          end if
                          ! compute fp-vector support w.r.t. whole matrix
                          isuppz( 2_ilp*windex-1 ) = isuppz( 2_ilp*windex-1 )+oldien
                          isuppz( 2_ilp*windex ) = isuppz( 2_ilp*windex )+oldien
                          zfrom = isuppz( 2_ilp*windex-1 )
                          zto = isuppz( 2_ilp*windex )
                          isupmn = isupmn + oldien
                          isupmx = isupmx + oldien
                          ! ensure vector is ok if support in the rqi has changed
                          if(isupmn<zfrom) then
                             do ii = isupmn,zfrom-1
                                z( ii, windex ) = zero
                             end do
                          endif
                          if(isupmx>zto) then
                             do ii = zto+1,isupmx
                                z( ii, windex ) = zero
                             end do
                          endif
                          call stdlib_sscal( zto-zfrom+1, nrminv,z( zfrom, windex ), 1_ilp )
                          125 continue
                          ! update w
                          w( windex ) = lambda+sigma
                          ! recompute the gaps on the left and right
                          ! but only allow them to become larger and not
                          ! smaller (which can only happen through "bad"
                          ! cancellation and doesn't reflect the theory
                          ! where the initial gaps are underestimated due
                          ! to werr being too crude.)
                          if(.not.eskip) then
                             if( k>1_ilp) then
                                wgap( windmn ) = max( wgap(windmn),w(windex)-werr(windex)- w(&
                                          windmn)-werr(windmn) )
                             endif
                             if( windex<wend ) then
                                wgap( windex ) = max( savgap,w( windpl )-werr( windpl )- w( &
                                          windex )-werr( windex) )
                             endif
                          endif
                          idone = idone + 1_ilp
                       endif
                       ! here ends the code for the current child
                       139 continue
                       ! proceed to any remaining child nodes
                       newfst = j + 1_ilp
                    end do loop_140
                 end do loop_150
                 ndepth = ndepth + 1_ilp
                 go to 40
              end if
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_slarrv

     pure module subroutine stdlib_dlarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
     !! DLARRV computes the eigenvectors of the tridiagonal matrix
     !! T = L D L**T given L, D and APPROXIMATIONS to the eigenvalues of L D L**T.
     !! The input eigenvalues should have been computed by DLARRE.
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: minrgp, pivmin, vl, vu
           real(dp), intent(inout) :: rtol1, rtol2
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(dp), intent(in) :: gers(*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 10_ilp
           
           
           ! Local Scalars 
           logical(lk) :: eskip, needbs, stp2ii, tryrqc, usedbs, usedrq
           integer(ilp) :: done, i, ibegin, idone, iend, ii, iindc1, iindc2, iindr, iindwk, iinfo,&
            im, in, indeig, indld, indlld, indwrk, isupmn, isupmx, iter, itmp1, j, jblk, k, &
            miniwsize, minwsize, nclus, ndepth, negcnt, newcls, newfst, newftt, newlst, newsiz, &
            offset, oldcls, oldfst, oldien, oldlst, oldncl, p, parity, q, wbegin, wend, windex, &
                      windmn, windpl, zfrom, zto, zusedl, zusedu, zusedw
           real(dp) :: bstres, bstw, eps, fudge, gap, gaptol, gl, gu, lambda, left, lgap, mingma, &
           nrminv, resid, rgap, right, rqcorr, rqtol, savgap, sgndef, sigma, spdiam, ssigma, tau, &
                     tmp, tol, ztz
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( (n<=0_ilp).or.(m<=0_ilp) ) then
              return
           end if
           ! the first n entries of work are reserved for the eigenvalues
           indld = n+1
           indlld= 2_ilp*n+1
           indwrk= 3_ilp*n+1
           minwsize = 12_ilp * n
           do i= 1,minwsize
              work( i ) = zero
           end do
           ! iwork(iindr+1:iindr+n) hold the twist indices r for the
           ! factorization used to compute the fp vector
           iindr = 0_ilp
           ! iwork(iindc1+1:iinc2+n) are used to store the clusters of the current
           ! layer and the one above.
           iindc1 = n
           iindc2 = 2_ilp*n
           iindwk = 3_ilp*n + 1_ilp
           miniwsize = 7_ilp * n
           do i= 1,miniwsize
              iwork( i ) = 0_ilp
           end do
           zusedl = 1_ilp
           if(dol>1_ilp) then
              ! set lower bound for use of z
              zusedl = dol-1
           endif
           zusedu = m
           if(dou<m) then
              ! set lower bound for use of z
              zusedu = dou+1
           endif
           ! the width of the part of z that is used
           zusedw = zusedu - zusedl + 1_ilp
           call stdlib_dlaset( 'FULL', n, zusedw, zero, zero,z(1_ilp,zusedl), ldz )
           eps = stdlib_dlamch( 'PRECISION' )
           rqtol = two * eps
           ! set expert flags for standard code.
           tryrqc = .true.
           if((dol==1_ilp).and.(dou==m)) then
           else
              ! only selected eigenpairs are computed. since the other evalues
              ! are not refined by rq iteration, bisection has to compute to full
              ! accuracy.
              rtol1 = four * eps
              rtol2 = four * eps
           endif
           ! the entries wbegin:wend in w, werr, wgap correspond to the
           ! desired eigenvalues. the support of the nonzero eigenvector
           ! entries is contained in the interval ibegin:iend.
           ! remark that if k eigenpairs are desired, then the eigenvectors
           ! are stored in k contiguous columns of z.
           ! done is the number of eigenvectors already computed
           done = 0_ilp
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, iblock( m )
              iend = isplit( jblk )
              sigma = l( iend )
              ! find the eigenvectors of the submatrix indexed ibegin
              ! through iend.
              wend = wbegin - 1_ilp
              15 continue
              if( wend<m ) then
                 if( iblock( wend+1 )==jblk ) then
                    wend = wend + 1_ilp
                    go to 15
                 end if
              end if
              if( wend<wbegin ) then
                 ibegin = iend + 1_ilp
                 cycle loop_170
              elseif( (wend<dol).or.(wbegin>dou) ) then
                 ibegin = iend + 1_ilp
                 wbegin = wend + 1_ilp
                 cycle loop_170
              end if
              ! find local spectral diameter of the block
              gl = gers( 2_ilp*ibegin-1 )
              gu = gers( 2_ilp*ibegin )
              do i = ibegin+1 , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              ! oldien is the last index of the previous block
              oldien = ibegin - 1_ilp
              ! calculate the size of the current block
              in = iend - ibegin + 1_ilp
              ! the number of eigenvalues in the current block
              im = wend - wbegin + 1_ilp
              ! this is for a 1x1 block
              if( ibegin==iend ) then
                 done = done+1
                 z( ibegin, wbegin ) = one
                 isuppz( 2_ilp*wbegin-1 ) = ibegin
                 isuppz( 2_ilp*wbegin ) = ibegin
                 w( wbegin ) = w( wbegin ) + sigma
                 work( wbegin ) = w( wbegin )
                 ibegin = iend + 1_ilp
                 wbegin = wbegin + 1_ilp
                 cycle loop_170
              end if
              ! the desired (shifted) eigenvalues are stored in w(wbegin:wend)
              ! note that these can be approximations, in this case, the corresp.
              ! entries of werr give the size of the uncertainty interval.
              ! the eigenvalue approximations will be refined when necessary as
              ! high relative accuracy is required for the computation of the
              ! corresponding eigenvectors.
              call stdlib_dcopy( im, w( wbegin ), 1_ilp,work( wbegin ), 1_ilp )
              ! we store in w the eigenvalue approximations w.r.t. the original
              ! matrix t.
              do i=1,im
                 w(wbegin+i-1) = w(wbegin+i-1)+sigma
              end do
              ! ndepth is the current depth of the representation tree
              ndepth = 0_ilp
              ! parity is either 1 or 0
              parity = 1_ilp
              ! nclus is the number of clusters for the next level of the
              ! representation tree, we start with nclus = 1 for the root
              nclus = 1_ilp
              iwork( iindc1+1 ) = 1_ilp
              iwork( iindc1+2 ) = im
              ! idone is the number of eigenvectors already computed in the current
              ! block
              idone = 0_ilp
              ! loop while( idone<im )
              ! generate the representation tree for the current block and
              ! compute the eigenvectors
              40 continue
              if( idone<im ) then
                 ! this is a crude protection against infinitely deep trees
                 if( ndepth>m ) then
                    info = -2_ilp
                    return
                 endif
                 ! breadth first processing of the current level of the representation
                 ! tree: oldncl = number of clusters on current level
                 oldncl = nclus
                 ! reset nclus to count the number of child clusters
                 nclus = 0_ilp
                 parity = 1_ilp - parity
                 if( parity==0_ilp ) then
                    oldcls = iindc1
                    newcls = iindc2
                 else
                    oldcls = iindc2
                    newcls = iindc1
                 end if
                 ! process the clusters on the current level
                 loop_150: do i = 1, oldncl
                    j = oldcls + 2_ilp*i
                    ! oldfst, oldlst = first, last index of current cluster.
                                     ! cluster indices start with 1 and are relative
                                     ! to wbegin when accessing w, wgap, werr, z
                    oldfst = iwork( j-1 )
                    oldlst = iwork( j )
                    if( ndepth>0_ilp ) then
                       ! retrieve relatively robust representation (rrr) of cluster
                       ! that has been computed at the previous level
                       ! the rrr is stored in z and overwritten once the eigenvectors
                       ! have been computed or when the cluster is refined
                       if((dol==1_ilp).and.(dou==m)) then
                          ! get representation from location of the leftmost evalue
                          ! of the cluster
                          j = wbegin + oldfst - 1_ilp
                       else
                          if(wbegin+oldfst-1<dol) then
                             ! get representation from the left end of z array
                             j = dol - 1_ilp
                          elseif(wbegin+oldfst-1>dou) then
                             ! get representation from the right end of z array
                             j = dou
                          else
                             j = wbegin + oldfst - 1_ilp
                          endif
                       endif
                       call stdlib_dcopy( in, z( ibegin, j ), 1_ilp, d( ibegin ), 1_ilp )
                       call stdlib_dcopy( in-1, z( ibegin, j+1 ), 1_ilp, l( ibegin ),1_ilp )
                       sigma = z( iend, j+1 )
                       ! set the corresponding entries in z to zero
                       call stdlib_dlaset( 'FULL', in, 2_ilp, zero, zero,z( ibegin, j), ldz )
                    end if
                    ! compute dl and dll of current rrr
                    do j = ibegin, iend-1
                       tmp = d( j )*l( j )
                       work( indld-1+j ) = tmp
                       work( indlld-1+j ) = tmp*l( j )
                    end do
                    if( ndepth>0_ilp ) then
                       ! p and q are index of the first and last eigenvalue to compute
                       ! within the current block
                       p = indexw( wbegin-1+oldfst )
                       q = indexw( wbegin-1+oldlst )
                       ! offset for the arrays work, wgap and werr, i.e., the p-offset
                       ! through the q-offset elements of these arrays are to be used.
                        ! offset = p-oldfst
                       offset = indexw( wbegin ) - 1_ilp
                       ! perform limited bisection (if necessary) to get approximate
                       ! eigenvalues to the precision needed.
                       call stdlib_dlarrb( in, d( ibegin ),work(indlld+ibegin-1),p, q, rtol1, &
                       rtol2, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ), iwork(&
                                  iindwk ),pivmin, spdiam, in, iinfo )
                       if( iinfo/=0_ilp ) then
                          info = -1_ilp
                          return
                       endif
                       ! we also recompute the extremal gaps. w holds all eigenvalues
                       ! of the unshifted matrix and must be used for computation
                       ! of wgap, the entries of work might stem from rrrs with
                       ! different shifts. the gaps from wbegin-1+oldfst to
                       ! wbegin-1+oldlst are correctly computed in stdlib_dlarrb.
                       ! however, we only allow the gaps to become greater since
                       ! this is what should happen when we decrease werr
                       if( oldfst>1_ilp) then
                          wgap( wbegin+oldfst-2 ) =max(wgap(wbegin+oldfst-2),w(wbegin+oldfst-1)-&
                          werr(wbegin+oldfst-1)- w(wbegin+oldfst-2)-werr(wbegin+oldfst-2) )
                                    
                       endif
                       if( wbegin + oldlst -1_ilp < wend ) then
                          wgap( wbegin+oldlst-1 ) =max(wgap(wbegin+oldlst-1),w(wbegin+oldlst)-&
                                    werr(wbegin+oldlst)- w(wbegin+oldlst-1)-werr(wbegin+oldlst-1) )
                       endif
                       ! each time the eigenvalues in work get refined, we store
                       ! the newly found approximation with all shifts applied in w
                       do j=oldfst,oldlst
                          w(wbegin+j-1) = work(wbegin+j-1)+sigma
                       end do
                    end if
                    ! process the current node.
                    newfst = oldfst
                    loop_140: do j = oldfst, oldlst
                       if( j==oldlst ) then
                          ! we are at the right end of the cluster, this is also the
                          ! boundary of the child cluster
                          newlst = j
                       else if ( wgap( wbegin + j -1_ilp)>=minrgp* abs( work(wbegin + j -1_ilp) ) ) &
                                 then
                          ! the right relative gap is big enough, the child cluster
                          ! (newfst,..,newlst) is well separated from the following
                          newlst = j
                        else
                          ! inside a child cluster, the relative gap is not
                          ! big enough.
                          cycle loop_140
                       end if
                       ! compute size of child cluster found
                       newsiz = newlst - newfst + 1_ilp
                       ! newftt is the place in z where the new rrr or the computed
                       ! eigenvector is to be stored
                       if((dol==1_ilp).and.(dou==m)) then
                          ! store representation at location of the leftmost evalue
                          ! of the cluster
                          newftt = wbegin + newfst - 1_ilp
                       else
                          if(wbegin+newfst-1<dol) then
                             ! store representation at the left end of z array
                             newftt = dol - 1_ilp
                          elseif(wbegin+newfst-1>dou) then
                             ! store representation at the right end of z array
                             newftt = dou
                          else
                             newftt = wbegin + newfst - 1_ilp
                          endif
                       endif
                       if( newsiz>1_ilp) then
                          ! current child is not a singleton but a cluster.
                          ! compute and store new representation of child.
                          ! compute left and right cluster gap.
                          ! lgap and rgap are not computed from work because
                          ! the eigenvalue approximations may stem from rrrs
                          ! different shifts. however, w hold all eigenvalues
                          ! of the unshifted matrix. still, the entries in wgap
                          ! have to be computed from work since the entries
                          ! in w might be of the same order so that gaps are not
                          ! exhibited correctly for very close eigenvalues.
                          if( newfst==1_ilp ) then
                             lgap = max( zero,w(wbegin)-werr(wbegin) - vl )
                         else
                             lgap = wgap( wbegin+newfst-2 )
                          endif
                          rgap = wgap( wbegin+newlst-1 )
                          ! compute left- and rightmost eigenvalue of child
                          ! to high precision in order to shift as close
                          ! as possible and obtain as large relative gaps
                          ! as possible
                          do k =1,2
                             if(k==1_ilp) then
                                p = indexw( wbegin-1+newfst )
                             else
                                p = indexw( wbegin-1+newlst )
                             endif
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_dlarrb( in, d(ibegin),work( indlld+ibegin-1 ),p,p,rqtol, &
                             rqtol, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ),&
                                       iwork( iindwk ), pivmin, spdiam,in, iinfo )
                          end do
                          if((wbegin+newlst-1<dol).or.(wbegin+newfst-1>dou)) then
                             ! if the cluster contains no desired eigenvalues
                             ! skip the computation of that branch of the rep. tree
                             ! we could skip before the refinement of the extremal
                             ! eigenvalues of the child, but then the representation
                             ! tree could be different from the one when nothing is
                             ! skipped. for this reason we skip at this place.
                             idone = idone + newlst - newfst + 1_ilp
                             goto 139
                          endif
                          ! compute rrr of child cluster.
                          ! note that the new rrr is stored in z
                          ! stdlib_dlarrf needs lwork = 2*n
                          call stdlib_dlarrf( in, d( ibegin ), l( ibegin ),work(indld+ibegin-1),&
                          newfst, newlst, work(wbegin),wgap(wbegin), werr(wbegin),spdiam, lgap, &
                          rgap, pivmin, tau,z(ibegin, newftt),z(ibegin, newftt+1),work( indwrk ), &
                                    iinfo )
                          if( iinfo==0_ilp ) then
                             ! a new rrr for the cluster was found by stdlib_dlarrf
                             ! update shift and store it
                             ssigma = sigma + tau
                             z( iend, newftt+1 ) = ssigma
                             ! work() are the midpoints and werr() the semi-width
                             ! note that the entries in w are unchanged.
                             do k = newfst, newlst
                                fudge =three*eps*abs(work(wbegin+k-1))
                                work( wbegin + k - 1_ilp ) =work( wbegin + k - 1_ilp) - tau
                                fudge = fudge +four*eps*abs(work(wbegin+k-1))
                                ! fudge errors
                                werr( wbegin + k - 1_ilp ) =werr( wbegin + k - 1_ilp ) + fudge
                                ! gaps are not fudged. provided that werr is small
                                ! when eigenvalues are close, a zero gap indicates
                                ! that a new representation is needed for resolving
                                ! the cluster. a fudge could lead to a wrong decision
                                ! of judging eigenvalues 'separated' which in
                                ! reality are not. this could have a negative impact
                                ! on the orthogonality of the computed eigenvectors.
                             end do
                             nclus = nclus + 1_ilp
                             k = newcls + 2_ilp*nclus
                             iwork( k-1 ) = newfst
                             iwork( k ) = newlst
                          else
                             info = -2_ilp
                             return
                          endif
                       else
                          ! compute eigenvector of singleton
                          iter = 0_ilp
                          tol = four * log(real(in,KIND=dp)) * eps
                          k = newfst
                          windex = wbegin + k - 1_ilp
                          windmn = max(windex - 1_ilp,1_ilp)
                          windpl = min(windex + 1_ilp,m)
                          lambda = work( windex )
                          done = done + 1_ilp
                          ! check if eigenvector computation is to be skipped
                          if((windex<dol).or.(windex>dou)) then
                             eskip = .true.
                             goto 125
                          else
                             eskip = .false.
                          endif
                          left = work( windex ) - werr( windex )
                          right = work( windex ) + werr( windex )
                          indeig = indexw( windex )
                          ! note that since we compute the eigenpairs for a child,
                          ! all eigenvalue approximations are w.r.t the same shift.
                          ! in this case, the entries in work should be used for
                          ! computing the gaps since they exhibit even very small
                          ! differences in the eigenvalues, as opposed to the
                          ! entries in w which might "look" the same.
                          if( k == 1_ilp) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vl, the formula
                             ! lgap = max( zero, (sigma - vl) + lambda )
                             ! can lead to an overestimation of the left gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small left gap.
                             lgap = eps*max(abs(left),abs(right))
                          else
                             lgap = wgap(windmn)
                          endif
                          if( k == im) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vu, the formula
                             ! can lead to an overestimation of the right gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small right gap.
                             rgap = eps*max(abs(left),abs(right))
                          else
                             rgap = wgap(windex)
                          endif
                          gap = min( lgap, rgap )
                          if(( k == 1_ilp).or.(k == im)) then
                             ! the eigenvector support can become wrong
                             ! because significant entries could be cut off due to a
                             ! large gaptol parameter in lar1v. prevent this.
                             gaptol = zero
                          else
                             gaptol = gap * eps
                          endif
                          isupmn = in
                          isupmx = 1_ilp
                          ! update wgap so that it holds the minimum gap
                          ! to the left or the right. this is crucial in the
                          ! case where bisection is used to ensure that the
                          ! eigenvalue is refined up to the required precision.
                          ! the correct value is restored afterwards.
                          savgap = wgap(windex)
                          wgap(windex) = gap
                          ! we want to use the rayleigh quotient correction
                          ! as often as possible since it converges quadratically
                          ! when we are close enough to the desired eigenvalue.
                          ! however, the rayleigh quotient can have the wrong sign
                          ! and lead us away from the desired eigenvalue. in this
                          ! case, the best we can do is to use bisection.
                          usedbs = .false.
                          usedrq = .false.
                          ! bisection is initially turned off unless it is forced
                          needbs =  .not.tryrqc
                          120 continue
                          ! check if bisection should be used to refine eigenvalue
                          if(needbs) then
                             ! take the bisection as new iterate
                             usedbs = .true.
                             itmp1 = iwork( iindr+windex )
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_dlarrb( in, d(ibegin),work(indlld+ibegin-1),indeig,&
                             indeig,zero, two*eps, offset,work(wbegin),wgap(wbegin),werr(wbegin),&
                                       work( indwrk ),iwork( iindwk ), pivmin, spdiam,itmp1, iinfo )
                             if( iinfo/=0_ilp ) then
                                info = -3_ilp
                                return
                             endif
                             lambda = work( windex )
                             ! reset twist index from inaccurate lambda to
                             ! force computation of true mingma
                             iwork( iindr+windex ) = 0_ilp
                          endif
                          ! given lambda, compute the eigenvector.
                          call stdlib_dlar1v( in, 1_ilp, in, lambda, d( ibegin ),l( ibegin ), work(&
                          indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( ibegin, windex &
                          ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+windex ), isuppz( &
                                    2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk ) )
                          if(iter == 0_ilp) then
                             bstres = resid
                             bstw = lambda
                          elseif(resid<bstres) then
                             bstres = resid
                             bstw = lambda
                          endif
                          isupmn = min(isupmn,isuppz( 2_ilp*windex-1 ))
                          isupmx = max(isupmx,isuppz( 2_ilp*windex ))
                          iter = iter + 1_ilp
                          ! sin alpha <= |resid|/gap
                          ! note that both the residual and the gap are
                          ! proportional to the matrix, so ||t|| doesn't play
                          ! a role in the quotient
                          ! convergence test for rayleigh-quotient iteration
                          ! (omitted when bisection has been used)
                          if( resid>tol*gap .and. abs( rqcorr )>rqtol*abs( lambda ) .and. .not. &
                                    usedbs)then
                             ! we need to check that the rqcorr update doesn't
                             ! move the eigenvalue away from the desired one and
                             ! towards a neighbor. -> protection with bisection
                             if(indeig<=negcnt) then
                                ! the wanted eigenvalue lies to the left
                                sgndef = -one
                             else
                                ! the wanted eigenvalue lies to the right
                                sgndef = one
                             endif
                             ! we only use the rqcorr if it improves the
                             ! the iterate reasonably.
                             if( ( rqcorr*sgndef>=zero ).and.( lambda + rqcorr<= right).and.( &
                                       lambda + rqcorr>= left)) then
                                usedrq = .true.
                                ! store new midpoint of bisection interval in work
                                if(sgndef==one) then
                                   ! the current lambda is on the left of the true
                                   ! eigenvalue
                                   left = lambda
                                   ! we prefer to assume that the error estimate
                                   ! is correct. we could make the interval not
                                   ! as a bracket but to be modified if the rqcorr
                                   ! chooses to. in this case, the right side should
                                   ! be modified as follows:
                                    ! right = max(right, lambda + rqcorr)
                                else
                                   ! the current lambda is on the right of the true
                                   ! eigenvalue
                                   right = lambda
                                   ! see comment about assuming the error estimate is
                                   ! correct above.
                                    ! left = min(left, lambda + rqcorr)
                                endif
                                work( windex ) =half * (right + left)
                                ! take rqcorr since it has the correct sign and
                                ! improves the iterate reasonably
                                lambda = lambda + rqcorr
                                ! update width of error interval
                                werr( windex ) =half * (right-left)
                             else
                                needbs = .true.
                             endif
                             if(right-left<rqtol*abs(lambda)) then
                                   ! the eigenvalue is computed to bisection accuracy
                                   ! compute eigenvector and stop
                                usedbs = .true.
                                goto 120
                             elseif( iter<maxitr ) then
                                goto 120
                             elseif( iter==maxitr ) then
                                needbs = .true.
                                goto 120
                             else
                                info = 5_ilp
                                return
                             end if
                          else
                             stp2ii = .false.
             if(usedrq .and. usedbs .and.bstres<=resid) then
                                lambda = bstw
                                stp2ii = .true.
                             endif
                             if (stp2ii) then
                                ! improve error angle by second step
                                call stdlib_dlar1v( in, 1_ilp, in, lambda,d( ibegin ), l( ibegin ),&
                                work(indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( &
                                ibegin, windex ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+&
                                windex ),isuppz( 2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk &
                                          ) )
                             endif
                             work( windex ) = lambda
                          end if
                          ! compute fp-vector support w.r.t. whole matrix
                          isuppz( 2_ilp*windex-1 ) = isuppz( 2_ilp*windex-1 )+oldien
                          isuppz( 2_ilp*windex ) = isuppz( 2_ilp*windex )+oldien
                          zfrom = isuppz( 2_ilp*windex-1 )
                          zto = isuppz( 2_ilp*windex )
                          isupmn = isupmn + oldien
                          isupmx = isupmx + oldien
                          ! ensure vector is ok if support in the rqi has changed
                          if(isupmn<zfrom) then
                             do ii = isupmn,zfrom-1
                                z( ii, windex ) = zero
                             end do
                          endif
                          if(isupmx>zto) then
                             do ii = zto+1,isupmx
                                z( ii, windex ) = zero
                             end do
                          endif
                          call stdlib_dscal( zto-zfrom+1, nrminv,z( zfrom, windex ), 1_ilp )
                          125 continue
                          ! update w
                          w( windex ) = lambda+sigma
                          ! recompute the gaps on the left and right
                          ! but only allow them to become larger and not
                          ! smaller (which can only happen through "bad"
                          ! cancellation and doesn't reflect the theory
                          ! where the initial gaps are underestimated due
                          ! to werr being too crude.)
                          if(.not.eskip) then
                             if( k>1_ilp) then
                                wgap( windmn ) = max( wgap(windmn),w(windex)-werr(windex)- w(&
                                          windmn)-werr(windmn) )
                             endif
                             if( windex<wend ) then
                                wgap( windex ) = max( savgap,w( windpl )-werr( windpl )- w( &
                                          windex )-werr( windex) )
                             endif
                          endif
                          idone = idone + 1_ilp
                       endif
                       ! here ends the code for the current child
                       139 continue
                       ! proceed to any remaining child nodes
                       newfst = j + 1_ilp
                    end do loop_140
                 end do loop_150
                 ndepth = ndepth + 1_ilp
                 go to 40
              end if
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_dlarrv


     pure module subroutine stdlib_clarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
     !! CLARRV computes the eigenvectors of the tridiagonal matrix
     !! T = L D L**T given L, D and APPROXIMATIONS to the eigenvalues of L D L**T.
     !! The input eigenvalues should have been computed by SLARRE.
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: minrgp, pivmin, vl, vu
           real(sp), intent(inout) :: rtol1, rtol2
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(sp), intent(in) :: gers(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 10_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: eskip, needbs, stp2ii, tryrqc, usedbs, usedrq
           integer(ilp) :: done, i, ibegin, idone, iend, ii, iindc1, iindc2, iindr, iindwk, iinfo,&
            im, in, indeig, indld, indlld, indwrk, isupmn, isupmx, iter, itmp1, j, jblk, k, &
            miniwsize, minwsize, nclus, ndepth, negcnt, newcls, newfst, newftt, newlst, newsiz, &
            offset, oldcls, oldfst, oldien, oldlst, oldncl, p, parity, q, wbegin, wend, windex, &
                      windmn, windpl, zfrom, zto, zusedl, zusedu, zusedw
           integer(ilp) :: indin1, indin2
           real(sp) :: bstres, bstw, eps, fudge, gap, gaptol, gl, gu, lambda, left, lgap, mingma, &
           nrminv, resid, rgap, right, rqcorr, rqtol, savgap, sgndef, sigma, spdiam, ssigma, tau, &
                     tmp, tol, ztz
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( (n<=0_ilp).or.(m<=0_ilp) ) then
              return
           end if
           ! the first n entries of work are reserved for the eigenvalues
           indld = n+1
           indlld= 2_ilp*n+1
           indin1 = 3_ilp*n + 1_ilp
           indin2 = 4_ilp*n + 1_ilp
           indwrk = 5_ilp*n + 1_ilp
           minwsize = 12_ilp * n
           do i= 1,minwsize
              work( i ) = zero
           end do
           ! iwork(iindr+1:iindr+n) hold the twist indices r for the
           ! factorization used to compute the fp vector
           iindr = 0_ilp
           ! iwork(iindc1+1:iinc2+n) are used to store the clusters of the current
           ! layer and the one above.
           iindc1 = n
           iindc2 = 2_ilp*n
           iindwk = 3_ilp*n + 1_ilp
           miniwsize = 7_ilp * n
           do i= 1,miniwsize
              iwork( i ) = 0_ilp
           end do
           zusedl = 1_ilp
           if(dol>1_ilp) then
              ! set lower bound for use of z
              zusedl = dol-1
           endif
           zusedu = m
           if(dou<m) then
              ! set lower bound for use of z
              zusedu = dou+1
           endif
           ! the width of the part of z that is used
           zusedw = zusedu - zusedl + 1_ilp
           call stdlib_claset( 'FULL', n, zusedw, czero, czero,z(1_ilp,zusedl), ldz )
           eps = stdlib_slamch( 'PRECISION' )
           rqtol = two * eps
           ! set expert flags for standard code.
           tryrqc = .true.
           if((dol==1_ilp).and.(dou==m)) then
           else
              ! only selected eigenpairs are computed. since the other evalues
              ! are not refined by rq iteration, bisection has to compute to full
              ! accuracy.
              rtol1 = four * eps
              rtol2 = four * eps
           endif
           ! the entries wbegin:wend in w, werr, wgap correspond to the
           ! desired eigenvalues. the support of the nonzero eigenvector
           ! entries is contained in the interval ibegin:iend.
           ! remark that if k eigenpairs are desired, then the eigenvectors
           ! are stored in k contiguous columns of z.
           ! done is the number of eigenvectors already computed
           done = 0_ilp
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, iblock( m )
              iend = isplit( jblk )
              sigma = l( iend )
              ! find the eigenvectors of the submatrix indexed ibegin
              ! through iend.
              wend = wbegin - 1_ilp
              15 continue
              if( wend<m ) then
                 if( iblock( wend+1 )==jblk ) then
                    wend = wend + 1_ilp
                    go to 15
                 end if
              end if
              if( wend<wbegin ) then
                 ibegin = iend + 1_ilp
                 cycle loop_170
              elseif( (wend<dol).or.(wbegin>dou) ) then
                 ibegin = iend + 1_ilp
                 wbegin = wend + 1_ilp
                 cycle loop_170
              end if
              ! find local spectral diameter of the block
              gl = gers( 2_ilp*ibegin-1 )
              gu = gers( 2_ilp*ibegin )
              do i = ibegin+1 , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              ! oldien is the last index of the previous block
              oldien = ibegin - 1_ilp
              ! calculate the size of the current block
              in = iend - ibegin + 1_ilp
              ! the number of eigenvalues in the current block
              im = wend - wbegin + 1_ilp
              ! this is for a 1x1 block
              if( ibegin==iend ) then
                 done = done+1
                 z( ibegin, wbegin ) = cmplx( one, zero,KIND=sp)
                 isuppz( 2_ilp*wbegin-1 ) = ibegin
                 isuppz( 2_ilp*wbegin ) = ibegin
                 w( wbegin ) = w( wbegin ) + sigma
                 work( wbegin ) = w( wbegin )
                 ibegin = iend + 1_ilp
                 wbegin = wbegin + 1_ilp
                 cycle loop_170
              end if
              ! the desired (shifted) eigenvalues are stored in w(wbegin:wend)
              ! note that these can be approximations, in this case, the corresp.
              ! entries of werr give the size of the uncertainty interval.
              ! the eigenvalue approximations will be refined when necessary as
              ! high relative accuracy is required for the computation of the
              ! corresponding eigenvectors.
              call stdlib_scopy( im, w( wbegin ), 1_ilp,work( wbegin ), 1_ilp )
              ! we store in w the eigenvalue approximations w.r.t. the original
              ! matrix t.
              do i=1,im
                 w(wbegin+i-1) = w(wbegin+i-1)+sigma
              end do
              ! ndepth is the current depth of the representation tree
              ndepth = 0_ilp
              ! parity is either 1 or 0
              parity = 1_ilp
              ! nclus is the number of clusters for the next level of the
              ! representation tree, we start with nclus = 1 for the root
              nclus = 1_ilp
              iwork( iindc1+1 ) = 1_ilp
              iwork( iindc1+2 ) = im
              ! idone is the number of eigenvectors already computed in the current
              ! block
              idone = 0_ilp
              ! loop while( idone<im )
              ! generate the representation tree for the current block and
              ! compute the eigenvectors
              40 continue
              if( idone<im ) then
                 ! this is a crude protection against infinitely deep trees
                 if( ndepth>m ) then
                    info = -2_ilp
                    return
                 endif
                 ! breadth first processing of the current level of the representation
                 ! tree: oldncl = number of clusters on current level
                 oldncl = nclus
                 ! reset nclus to count the number of child clusters
                 nclus = 0_ilp
                 parity = 1_ilp - parity
                 if( parity==0_ilp ) then
                    oldcls = iindc1
                    newcls = iindc2
                 else
                    oldcls = iindc2
                    newcls = iindc1
                 end if
                 ! process the clusters on the current level
                 loop_150: do i = 1, oldncl
                    j = oldcls + 2_ilp*i
                    ! oldfst, oldlst = first, last index of current cluster.
                                     ! cluster indices start with 1 and are relative
                                     ! to wbegin when accessing w, wgap, werr, z
                    oldfst = iwork( j-1 )
                    oldlst = iwork( j )
                    if( ndepth>0_ilp ) then
                       ! retrieve relatively robust representation (rrr) of cluster
                       ! that has been computed at the previous level
                       ! the rrr is stored in z and overwritten once the eigenvectors
                       ! have been computed or when the cluster is refined
                       if((dol==1_ilp).and.(dou==m)) then
                          ! get representation from location of the leftmost evalue
                          ! of the cluster
                          j = wbegin + oldfst - 1_ilp
                       else
                          if(wbegin+oldfst-1<dol) then
                             ! get representation from the left end of z array
                             j = dol - 1_ilp
                          elseif(wbegin+oldfst-1>dou) then
                             ! get representation from the right end of z array
                             j = dou
                          else
                             j = wbegin + oldfst - 1_ilp
                          endif
                       endif
                       do k = 1, in - 1
                          d( ibegin+k-1 ) = real( z( ibegin+k-1,j ),KIND=sp)
                          l( ibegin+k-1 ) = real( z( ibegin+k-1,j+1 ),KIND=sp)
                       end do
                       d( iend ) = real( z( iend, j ),KIND=sp)
                       sigma = real( z( iend, j+1 ),KIND=sp)
                       ! set the corresponding entries in z to zero
                       call stdlib_claset( 'FULL', in, 2_ilp, czero, czero,z( ibegin, j), ldz )
                                 
                    end if
                    ! compute dl and dll of current rrr
                    do j = ibegin, iend-1
                       tmp = d( j )*l( j )
                       work( indld-1+j ) = tmp
                       work( indlld-1+j ) = tmp*l( j )
                    end do
                    if( ndepth>0_ilp ) then
                       ! p and q are index of the first and last eigenvalue to compute
                       ! within the current block
                       p = indexw( wbegin-1+oldfst )
                       q = indexw( wbegin-1+oldlst )
                       ! offset for the arrays work, wgap and werr, i.e., the p-offset
                       ! through the q-offset elements of these arrays are to be used.
                        ! offset = p-oldfst
                       offset = indexw( wbegin ) - 1_ilp
                       ! perform limited bisection (if necessary) to get approximate
                       ! eigenvalues to the precision needed.
                       call stdlib_slarrb( in, d( ibegin ),work(indlld+ibegin-1),p, q, rtol1, &
                       rtol2, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ), iwork(&
                                  iindwk ),pivmin, spdiam, in, iinfo )
                       if( iinfo/=0_ilp ) then
                          info = -1_ilp
                          return
                       endif
                       ! we also recompute the extremal gaps. w holds all eigenvalues
                       ! of the unshifted matrix and must be used for computation
                       ! of wgap, the entries of work might stem from rrrs with
                       ! different shifts. the gaps from wbegin-1+oldfst to
                       ! wbegin-1+oldlst are correctly computed in stdlib_slarrb.
                       ! however, we only allow the gaps to become greater since
                       ! this is what should happen when we decrease werr
                       if( oldfst>1_ilp) then
                          wgap( wbegin+oldfst-2 ) =max(wgap(wbegin+oldfst-2),w(wbegin+oldfst-1)-&
                          werr(wbegin+oldfst-1)- w(wbegin+oldfst-2)-werr(wbegin+oldfst-2) )
                                    
                       endif
                       if( wbegin + oldlst -1_ilp < wend ) then
                          wgap( wbegin+oldlst-1 ) =max(wgap(wbegin+oldlst-1),w(wbegin+oldlst)-&
                                    werr(wbegin+oldlst)- w(wbegin+oldlst-1)-werr(wbegin+oldlst-1) )
                       endif
                       ! each time the eigenvalues in work get refined, we store
                       ! the newly found approximation with all shifts applied in w
                       do j=oldfst,oldlst
                          w(wbegin+j-1) = work(wbegin+j-1)+sigma
                       end do
                    end if
                    ! process the current node.
                    newfst = oldfst
                    loop_140: do j = oldfst, oldlst
                       if( j==oldlst ) then
                          ! we are at the right end of the cluster, this is also the
                          ! boundary of the child cluster
                          newlst = j
                       else if ( wgap( wbegin + j -1_ilp)>=minrgp* abs( work(wbegin + j -1_ilp) ) ) &
                                 then
                          ! the right relative gap is big enough, the child cluster
                          ! (newfst,..,newlst) is well separated from the following
                          newlst = j
                        else
                          ! inside a child cluster, the relative gap is not
                          ! big enough.
                          cycle loop_140
                       end if
                       ! compute size of child cluster found
                       newsiz = newlst - newfst + 1_ilp
                       ! newftt is the place in z where the new rrr or the computed
                       ! eigenvector is to be stored
                       if((dol==1_ilp).and.(dou==m)) then
                          ! store representation at location of the leftmost evalue
                          ! of the cluster
                          newftt = wbegin + newfst - 1_ilp
                       else
                          if(wbegin+newfst-1<dol) then
                             ! store representation at the left end of z array
                             newftt = dol - 1_ilp
                          elseif(wbegin+newfst-1>dou) then
                             ! store representation at the right end of z array
                             newftt = dou
                          else
                             newftt = wbegin + newfst - 1_ilp
                          endif
                       endif
                       if( newsiz>1_ilp) then
                          ! current child is not a singleton but a cluster.
                          ! compute and store new representation of child.
                          ! compute left and right cluster gap.
                          ! lgap and rgap are not computed from work because
                          ! the eigenvalue approximations may stem from rrrs
                          ! different shifts. however, w hold all eigenvalues
                          ! of the unshifted matrix. still, the entries in wgap
                          ! have to be computed from work since the entries
                          ! in w might be of the same order so that gaps are not
                          ! exhibited correctly for very close eigenvalues.
                          if( newfst==1_ilp ) then
                             lgap = max( zero,w(wbegin)-werr(wbegin) - vl )
                         else
                             lgap = wgap( wbegin+newfst-2 )
                          endif
                          rgap = wgap( wbegin+newlst-1 )
                          ! compute left- and rightmost eigenvalue of child
                          ! to high precision in order to shift as close
                          ! as possible and obtain as large relative gaps
                          ! as possible
                          do k =1,2
                             if(k==1_ilp) then
                                p = indexw( wbegin-1+newfst )
                             else
                                p = indexw( wbegin-1+newlst )
                             endif
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_slarrb( in, d(ibegin),work( indlld+ibegin-1 ),p,p,rqtol, &
                             rqtol, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ),&
                                       iwork( iindwk ), pivmin, spdiam,in, iinfo )
                          end do
                          if((wbegin+newlst-1<dol).or.(wbegin+newfst-1>dou)) then
                             ! if the cluster contains no desired eigenvalues
                             ! skip the computation of that branch of the rep. tree
                             ! we could skip before the refinement of the extremal
                             ! eigenvalues of the child, but then the representation
                             ! tree could be different from the one when nothing is
                             ! skipped. for this reason we skip at this place.
                             idone = idone + newlst - newfst + 1_ilp
                             goto 139
                          endif
                          ! compute rrr of child cluster.
                          ! note that the new rrr is stored in z
                          ! stdlib_slarrf needs lwork = 2*n
                          call stdlib_slarrf( in, d( ibegin ), l( ibegin ),work(indld+ibegin-1),&
                          newfst, newlst, work(wbegin),wgap(wbegin), werr(wbegin),spdiam, lgap, &
                          rgap, pivmin, tau,work( indin1 ), work( indin2 ),work( indwrk ), iinfo )
                                    
                          ! in the complex case, stdlib_slarrf cannot write
                          ! the new rrr directly into z and needs an intermediate
                          ! workspace
                          do k = 1, in-1
                             z( ibegin+k-1, newftt ) =cmplx( work( indin1+k-1 ), zero,KIND=sp)
                                       
                             z( ibegin+k-1, newftt+1 ) =cmplx( work( indin2+k-1 ), zero,KIND=sp)
                                       
                          end do
                          z( iend, newftt ) =cmplx( work( indin1+in-1 ), zero,KIND=sp)
                          if( iinfo==0_ilp ) then
                             ! a new rrr for the cluster was found by stdlib_slarrf
                             ! update shift and store it
                             ssigma = sigma + tau
                             z( iend, newftt+1 ) = cmplx( ssigma, zero,KIND=sp)
                             ! work() are the midpoints and werr() the semi-width
                             ! note that the entries in w are unchanged.
                             do k = newfst, newlst
                                fudge =three*eps*abs(work(wbegin+k-1))
                                work( wbegin + k - 1_ilp ) =work( wbegin + k - 1_ilp) - tau
                                fudge = fudge +four*eps*abs(work(wbegin+k-1))
                                ! fudge errors
                                werr( wbegin + k - 1_ilp ) =werr( wbegin + k - 1_ilp ) + fudge
                                ! gaps are not fudged. provided that werr is small
                                ! when eigenvalues are close, a zero gap indicates
                                ! that a new representation is needed for resolving
                                ! the cluster. a fudge could lead to a wrong decision
                                ! of judging eigenvalues 'separated' which in
                                ! reality are not. this could have a negative impact
                                ! on the orthogonality of the computed eigenvectors.
                             end do
                             nclus = nclus + 1_ilp
                             k = newcls + 2_ilp*nclus
                             iwork( k-1 ) = newfst
                             iwork( k ) = newlst
                          else
                             info = -2_ilp
                             return
                          endif
                       else
                          ! compute eigenvector of singleton
                          iter = 0_ilp
                          tol = four * log(real(in,KIND=sp)) * eps
                          k = newfst
                          windex = wbegin + k - 1_ilp
                          windmn = max(windex - 1_ilp,1_ilp)
                          windpl = min(windex + 1_ilp,m)
                          lambda = work( windex )
                          done = done + 1_ilp
                          ! check if eigenvector computation is to be skipped
                          if((windex<dol).or.(windex>dou)) then
                             eskip = .true.
                             goto 125
                          else
                             eskip = .false.
                          endif
                          left = work( windex ) - werr( windex )
                          right = work( windex ) + werr( windex )
                          indeig = indexw( windex )
                          ! note that since we compute the eigenpairs for a child,
                          ! all eigenvalue approximations are w.r.t the same shift.
                          ! in this case, the entries in work should be used for
                          ! computing the gaps since they exhibit even very small
                          ! differences in the eigenvalues, as opposed to the
                          ! entries in w which might "look" the same.
                          if( k == 1_ilp) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vl, the formula
                             ! lgap = max( zero, (sigma - vl) + lambda )
                             ! can lead to an overestimation of the left gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small left gap.
                             lgap = eps*max(abs(left),abs(right))
                          else
                             lgap = wgap(windmn)
                          endif
                          if( k == im) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vu, the formula
                             ! can lead to an overestimation of the right gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small right gap.
                             rgap = eps*max(abs(left),abs(right))
                          else
                             rgap = wgap(windex)
                          endif
                          gap = min( lgap, rgap )
                          if(( k == 1_ilp).or.(k == im)) then
                             ! the eigenvector support can become wrong
                             ! because significant entries could be cut off due to a
                             ! large gaptol parameter in lar1v. prevent this.
                             gaptol = zero
                          else
                             gaptol = gap * eps
                          endif
                          isupmn = in
                          isupmx = 1_ilp
                          ! update wgap so that it holds the minimum gap
                          ! to the left or the right. this is crucial in the
                          ! case where bisection is used to ensure that the
                          ! eigenvalue is refined up to the required precision.
                          ! the correct value is restored afterwards.
                          savgap = wgap(windex)
                          wgap(windex) = gap
                          ! we want to use the rayleigh quotient correction
                          ! as often as possible since it converges quadratically
                          ! when we are close enough to the desired eigenvalue.
                          ! however, the rayleigh quotient can have the wrong sign
                          ! and lead us away from the desired eigenvalue. in this
                          ! case, the best we can do is to use bisection.
                          usedbs = .false.
                          usedrq = .false.
                          ! bisection is initially turned off unless it is forced
                          needbs =  .not.tryrqc
                          120 continue
                          ! check if bisection should be used to refine eigenvalue
                          if(needbs) then
                             ! take the bisection as new iterate
                             usedbs = .true.
                             itmp1 = iwork( iindr+windex )
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_slarrb( in, d(ibegin),work(indlld+ibegin-1),indeig,&
                             indeig,zero, two*eps, offset,work(wbegin),wgap(wbegin),werr(wbegin),&
                                       work( indwrk ),iwork( iindwk ), pivmin, spdiam,itmp1, iinfo )
                             if( iinfo/=0_ilp ) then
                                info = -3_ilp
                                return
                             endif
                             lambda = work( windex )
                             ! reset twist index from inaccurate lambda to
                             ! force computation of true mingma
                             iwork( iindr+windex ) = 0_ilp
                          endif
                          ! given lambda, compute the eigenvector.
                          call stdlib_clar1v( in, 1_ilp, in, lambda, d( ibegin ),l( ibegin ), work(&
                          indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( ibegin, windex &
                          ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+windex ), isuppz( &
                                    2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk ) )
                          if(iter == 0_ilp) then
                             bstres = resid
                             bstw = lambda
                          elseif(resid<bstres) then
                             bstres = resid
                             bstw = lambda
                          endif
                          isupmn = min(isupmn,isuppz( 2_ilp*windex-1 ))
                          isupmx = max(isupmx,isuppz( 2_ilp*windex ))
                          iter = iter + 1_ilp
                          ! sin alpha <= |resid|/gap
                          ! note that both the residual and the gap are
                          ! proportional to the matrix, so ||t|| doesn't play
                          ! a role in the quotient
                          ! convergence test for rayleigh-quotient iteration
                          ! (omitted when bisection has been used)
                          if( resid>tol*gap .and. abs( rqcorr )>rqtol*abs( lambda ) .and. .not. &
                                    usedbs)then
                             ! we need to check that the rqcorr update doesn't
                             ! move the eigenvalue away from the desired one and
                             ! towards a neighbor. -> protection with bisection
                             if(indeig<=negcnt) then
                                ! the wanted eigenvalue lies to the left
                                sgndef = -one
                             else
                                ! the wanted eigenvalue lies to the right
                                sgndef = one
                             endif
                             ! we only use the rqcorr if it improves the
                             ! the iterate reasonably.
                             if( ( rqcorr*sgndef>=zero ).and.( lambda + rqcorr<= right).and.( &
                                       lambda + rqcorr>= left)) then
                                usedrq = .true.
                                ! store new midpoint of bisection interval in work
                                if(sgndef==one) then
                                   ! the current lambda is on the left of the true
                                   ! eigenvalue
                                   left = lambda
                                   ! we prefer to assume that the error estimate
                                   ! is correct. we could make the interval not
                                   ! as a bracket but to be modified if the rqcorr
                                   ! chooses to. in this case, the right side should
                                   ! be modified as follows:
                                    ! right = max(right, lambda + rqcorr)
                                else
                                   ! the current lambda is on the right of the true
                                   ! eigenvalue
                                   right = lambda
                                   ! see comment about assuming the error estimate is
                                   ! correct above.
                                    ! left = min(left, lambda + rqcorr)
                                endif
                                work( windex ) =half * (right + left)
                                ! take rqcorr since it has the correct sign and
                                ! improves the iterate reasonably
                                lambda = lambda + rqcorr
                                ! update width of error interval
                                werr( windex ) =half * (right-left)
                             else
                                needbs = .true.
                             endif
                             if(right-left<rqtol*abs(lambda)) then
                                   ! the eigenvalue is computed to bisection accuracy
                                   ! compute eigenvector and stop
                                usedbs = .true.
                                goto 120
                             elseif( iter<maxitr ) then
                                goto 120
                             elseif( iter==maxitr ) then
                                needbs = .true.
                                goto 120
                             else
                                info = 5_ilp
                                return
                             end if
                          else
                             stp2ii = .false.
             if(usedrq .and. usedbs .and.bstres<=resid) then
                                lambda = bstw
                                stp2ii = .true.
                             endif
                             if (stp2ii) then
                                ! improve error angle by second step
                                call stdlib_clar1v( in, 1_ilp, in, lambda,d( ibegin ), l( ibegin ),&
                                work(indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( &
                                ibegin, windex ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+&
                                windex ),isuppz( 2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk &
                                          ) )
                             endif
                             work( windex ) = lambda
                          end if
                          ! compute fp-vector support w.r.t. whole matrix
                          isuppz( 2_ilp*windex-1 ) = isuppz( 2_ilp*windex-1 )+oldien
                          isuppz( 2_ilp*windex ) = isuppz( 2_ilp*windex )+oldien
                          zfrom = isuppz( 2_ilp*windex-1 )
                          zto = isuppz( 2_ilp*windex )
                          isupmn = isupmn + oldien
                          isupmx = isupmx + oldien
                          ! ensure vector is ok if support in the rqi has changed
                          if(isupmn<zfrom) then
                             do ii = isupmn,zfrom-1
                                z( ii, windex ) = zero
                             end do
                          endif
                          if(isupmx>zto) then
                             do ii = zto+1,isupmx
                                z( ii, windex ) = zero
                             end do
                          endif
                          call stdlib_csscal( zto-zfrom+1, nrminv,z( zfrom, windex ), 1_ilp )
                          125 continue
                          ! update w
                          w( windex ) = lambda+sigma
                          ! recompute the gaps on the left and right
                          ! but only allow them to become larger and not
                          ! smaller (which can only happen through "bad"
                          ! cancellation and doesn't reflect the theory
                          ! where the initial gaps are underestimated due
                          ! to werr being too crude.)
                          if(.not.eskip) then
                             if( k>1_ilp) then
                                wgap( windmn ) = max( wgap(windmn),w(windex)-werr(windex)- w(&
                                          windmn)-werr(windmn) )
                             endif
                             if( windex<wend ) then
                                wgap( windex ) = max( savgap,w( windpl )-werr( windpl )- w( &
                                          windex )-werr( windex) )
                             endif
                          endif
                          idone = idone + 1_ilp
                       endif
                       ! here ends the code for the current child
                       139 continue
                       ! proceed to any remaining child nodes
                       newfst = j + 1_ilp
                    end do loop_140
                 end do loop_150
                 ndepth = ndepth + 1_ilp
                 go to 40
              end if
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_clarrv

     pure module subroutine stdlib_zlarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
     !! ZLARRV computes the eigenvectors of the tridiagonal matrix
     !! T = L D L**T given L, D and APPROXIMATIONS to the eigenvalues of L D L**T.
     !! The input eigenvalues should have been computed by DLARRE.
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: minrgp, pivmin, vl, vu
           real(dp), intent(inout) :: rtol1, rtol2
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(dp), intent(in) :: gers(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxitr = 10_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: eskip, needbs, stp2ii, tryrqc, usedbs, usedrq
           integer(ilp) :: done, i, ibegin, idone, iend, ii, iindc1, iindc2, iindr, iindwk, iinfo,&
            im, in, indeig, indld, indlld, indwrk, isupmn, isupmx, iter, itmp1, j, jblk, k, &
            miniwsize, minwsize, nclus, ndepth, negcnt, newcls, newfst, newftt, newlst, newsiz, &
            offset, oldcls, oldfst, oldien, oldlst, oldncl, p, parity, q, wbegin, wend, windex, &
                      windmn, windpl, zfrom, zto, zusedl, zusedu, zusedw
           integer(ilp) :: indin1, indin2
           real(dp) :: bstres, bstw, eps, fudge, gap, gaptol, gl, gu, lambda, left, lgap, mingma, &
           nrminv, resid, rgap, right, rqcorr, rqtol, savgap, sgndef, sigma, spdiam, ssigma, tau, &
                     tmp, tol, ztz
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( (n<=0_ilp).or.(m<=0_ilp) ) then
              return
           end if
           ! the first n entries of work are reserved for the eigenvalues
           indld = n+1
           indlld= 2_ilp*n+1
           indin1 = 3_ilp*n + 1_ilp
           indin2 = 4_ilp*n + 1_ilp
           indwrk = 5_ilp*n + 1_ilp
           minwsize = 12_ilp * n
           do i= 1,minwsize
              work( i ) = zero
           end do
           ! iwork(iindr+1:iindr+n) hold the twist indices r for the
           ! factorization used to compute the fp vector
           iindr = 0_ilp
           ! iwork(iindc1+1:iinc2+n) are used to store the clusters of the current
           ! layer and the one above.
           iindc1 = n
           iindc2 = 2_ilp*n
           iindwk = 3_ilp*n + 1_ilp
           miniwsize = 7_ilp * n
           do i= 1,miniwsize
              iwork( i ) = 0_ilp
           end do
           zusedl = 1_ilp
           if(dol>1_ilp) then
              ! set lower bound for use of z
              zusedl = dol-1
           endif
           zusedu = m
           if(dou<m) then
              ! set lower bound for use of z
              zusedu = dou+1
           endif
           ! the width of the part of z that is used
           zusedw = zusedu - zusedl + 1_ilp
           call stdlib_zlaset( 'FULL', n, zusedw, czero, czero,z(1_ilp,zusedl), ldz )
           eps = stdlib_dlamch( 'PRECISION' )
           rqtol = two * eps
           ! set expert flags for standard code.
           tryrqc = .true.
           if((dol==1_ilp).and.(dou==m)) then
           else
              ! only selected eigenpairs are computed. since the other evalues
              ! are not refined by rq iteration, bisection has to compute to full
              ! accuracy.
              rtol1 = four * eps
              rtol2 = four * eps
           endif
           ! the entries wbegin:wend in w, werr, wgap correspond to the
           ! desired eigenvalues. the support of the nonzero eigenvector
           ! entries is contained in the interval ibegin:iend.
           ! remark that if k eigenpairs are desired, then the eigenvectors
           ! are stored in k contiguous columns of z.
           ! done is the number of eigenvectors already computed
           done = 0_ilp
           ibegin = 1_ilp
           wbegin = 1_ilp
           loop_170: do jblk = 1, iblock( m )
              iend = isplit( jblk )
              sigma = l( iend )
              ! find the eigenvectors of the submatrix indexed ibegin
              ! through iend.
              wend = wbegin - 1_ilp
              15 continue
              if( wend<m ) then
                 if( iblock( wend+1 )==jblk ) then
                    wend = wend + 1_ilp
                    go to 15
                 end if
              end if
              if( wend<wbegin ) then
                 ibegin = iend + 1_ilp
                 cycle loop_170
              elseif( (wend<dol).or.(wbegin>dou) ) then
                 ibegin = iend + 1_ilp
                 wbegin = wend + 1_ilp
                 cycle loop_170
              end if
              ! find local spectral diameter of the block
              gl = gers( 2_ilp*ibegin-1 )
              gu = gers( 2_ilp*ibegin )
              do i = ibegin+1 , iend
                 gl = min( gers( 2_ilp*i-1 ), gl )
                 gu = max( gers( 2_ilp*i ), gu )
              end do
              spdiam = gu - gl
              ! oldien is the last index of the previous block
              oldien = ibegin - 1_ilp
              ! calculate the size of the current block
              in = iend - ibegin + 1_ilp
              ! the number of eigenvalues in the current block
              im = wend - wbegin + 1_ilp
              ! this is for a 1x1 block
              if( ibegin==iend ) then
                 done = done+1
                 z( ibegin, wbegin ) = cmplx( one, zero,KIND=dp)
                 isuppz( 2_ilp*wbegin-1 ) = ibegin
                 isuppz( 2_ilp*wbegin ) = ibegin
                 w( wbegin ) = w( wbegin ) + sigma
                 work( wbegin ) = w( wbegin )
                 ibegin = iend + 1_ilp
                 wbegin = wbegin + 1_ilp
                 cycle loop_170
              end if
              ! the desired (shifted) eigenvalues are stored in w(wbegin:wend)
              ! note that these can be approximations, in this case, the corresp.
              ! entries of werr give the size of the uncertainty interval.
              ! the eigenvalue approximations will be refined when necessary as
              ! high relative accuracy is required for the computation of the
              ! corresponding eigenvectors.
              call stdlib_dcopy( im, w( wbegin ), 1_ilp,work( wbegin ), 1_ilp )
              ! we store in w the eigenvalue approximations w.r.t. the original
              ! matrix t.
              do i=1,im
                 w(wbegin+i-1) = w(wbegin+i-1)+sigma
              end do
              ! ndepth is the current depth of the representation tree
              ndepth = 0_ilp
              ! parity is either 1 or 0
              parity = 1_ilp
              ! nclus is the number of clusters for the next level of the
              ! representation tree, we start with nclus = 1 for the root
              nclus = 1_ilp
              iwork( iindc1+1 ) = 1_ilp
              iwork( iindc1+2 ) = im
              ! idone is the number of eigenvectors already computed in the current
              ! block
              idone = 0_ilp
              ! loop while( idone<im )
              ! generate the representation tree for the current block and
              ! compute the eigenvectors
              40 continue
              if( idone<im ) then
                 ! this is a crude protection against infinitely deep trees
                 if( ndepth>m ) then
                    info = -2_ilp
                    return
                 endif
                 ! breadth first processing of the current level of the representation
                 ! tree: oldncl = number of clusters on current level
                 oldncl = nclus
                 ! reset nclus to count the number of child clusters
                 nclus = 0_ilp
                 parity = 1_ilp - parity
                 if( parity==0_ilp ) then
                    oldcls = iindc1
                    newcls = iindc2
                 else
                    oldcls = iindc2
                    newcls = iindc1
                 end if
                 ! process the clusters on the current level
                 loop_150: do i = 1, oldncl
                    j = oldcls + 2_ilp*i
                    ! oldfst, oldlst = first, last index of current cluster.
                                     ! cluster indices start with 1 and are relative
                                     ! to wbegin when accessing w, wgap, werr, z
                    oldfst = iwork( j-1 )
                    oldlst = iwork( j )
                    if( ndepth>0_ilp ) then
                       ! retrieve relatively robust representation (rrr) of cluster
                       ! that has been computed at the previous level
                       ! the rrr is stored in z and overwritten once the eigenvectors
                       ! have been computed or when the cluster is refined
                       if((dol==1_ilp).and.(dou==m)) then
                          ! get representation from location of the leftmost evalue
                          ! of the cluster
                          j = wbegin + oldfst - 1_ilp
                       else
                          if(wbegin+oldfst-1<dol) then
                             ! get representation from the left end of z array
                             j = dol - 1_ilp
                          elseif(wbegin+oldfst-1>dou) then
                             ! get representation from the right end of z array
                             j = dou
                          else
                             j = wbegin + oldfst - 1_ilp
                          endif
                       endif
                       do k = 1, in - 1
                          d( ibegin+k-1 ) = real( z( ibegin+k-1,j ),KIND=dp)
                          l( ibegin+k-1 ) = real( z( ibegin+k-1,j+1 ),KIND=dp)
                       end do
                       d( iend ) = real( z( iend, j ),KIND=dp)
                       sigma = real( z( iend, j+1 ),KIND=dp)
                       ! set the corresponding entries in z to zero
                       call stdlib_zlaset( 'FULL', in, 2_ilp, czero, czero,z( ibegin, j), ldz )
                                 
                    end if
                    ! compute dl and dll of current rrr
                    do j = ibegin, iend-1
                       tmp = d( j )*l( j )
                       work( indld-1+j ) = tmp
                       work( indlld-1+j ) = tmp*l( j )
                    end do
                    if( ndepth>0_ilp ) then
                       ! p and q are index of the first and last eigenvalue to compute
                       ! within the current block
                       p = indexw( wbegin-1+oldfst )
                       q = indexw( wbegin-1+oldlst )
                       ! offset for the arrays work, wgap and werr, i.e., the p-offset
                       ! through the q-offset elements of these arrays are to be used.
                        ! offset = p-oldfst
                       offset = indexw( wbegin ) - 1_ilp
                       ! perform limited bisection (if necessary) to get approximate
                       ! eigenvalues to the precision needed.
                       call stdlib_dlarrb( in, d( ibegin ),work(indlld+ibegin-1),p, q, rtol1, &
                       rtol2, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ), iwork(&
                                  iindwk ),pivmin, spdiam, in, iinfo )
                       if( iinfo/=0_ilp ) then
                          info = -1_ilp
                          return
                       endif
                       ! we also recompute the extremal gaps. w holds all eigenvalues
                       ! of the unshifted matrix and must be used for computation
                       ! of wgap, the entries of work might stem from rrrs with
                       ! different shifts. the gaps from wbegin-1+oldfst to
                       ! wbegin-1+oldlst are correctly computed in stdlib_dlarrb.
                       ! however, we only allow the gaps to become greater since
                       ! this is what should happen when we decrease werr
                       if( oldfst>1_ilp) then
                          wgap( wbegin+oldfst-2 ) =max(wgap(wbegin+oldfst-2),w(wbegin+oldfst-1)-&
                          werr(wbegin+oldfst-1)- w(wbegin+oldfst-2)-werr(wbegin+oldfst-2) )
                                    
                       endif
                       if( wbegin + oldlst -1_ilp < wend ) then
                          wgap( wbegin+oldlst-1 ) =max(wgap(wbegin+oldlst-1),w(wbegin+oldlst)-&
                                    werr(wbegin+oldlst)- w(wbegin+oldlst-1)-werr(wbegin+oldlst-1) )
                       endif
                       ! each time the eigenvalues in work get refined, we store
                       ! the newly found approximation with all shifts applied in w
                       do j=oldfst,oldlst
                          w(wbegin+j-1) = work(wbegin+j-1)+sigma
                       end do
                    end if
                    ! process the current node.
                    newfst = oldfst
                    loop_140: do j = oldfst, oldlst
                       if( j==oldlst ) then
                          ! we are at the right end of the cluster, this is also the
                          ! boundary of the child cluster
                          newlst = j
                       else if ( wgap( wbegin + j -1_ilp)>=minrgp* abs( work(wbegin + j -1_ilp) ) ) &
                                 then
                          ! the right relative gap is big enough, the child cluster
                          ! (newfst,..,newlst) is well separated from the following
                          newlst = j
                        else
                          ! inside a child cluster, the relative gap is not
                          ! big enough.
                          cycle loop_140
                       end if
                       ! compute size of child cluster found
                       newsiz = newlst - newfst + 1_ilp
                       ! newftt is the place in z where the new rrr or the computed
                       ! eigenvector is to be stored
                       if((dol==1_ilp).and.(dou==m)) then
                          ! store representation at location of the leftmost evalue
                          ! of the cluster
                          newftt = wbegin + newfst - 1_ilp
                       else
                          if(wbegin+newfst-1<dol) then
                             ! store representation at the left end of z array
                             newftt = dol - 1_ilp
                          elseif(wbegin+newfst-1>dou) then
                             ! store representation at the right end of z array
                             newftt = dou
                          else
                             newftt = wbegin + newfst - 1_ilp
                          endif
                       endif
                       if( newsiz>1_ilp) then
                          ! current child is not a singleton but a cluster.
                          ! compute and store new representation of child.
                          ! compute left and right cluster gap.
                          ! lgap and rgap are not computed from work because
                          ! the eigenvalue approximations may stem from rrrs
                          ! different shifts. however, w hold all eigenvalues
                          ! of the unshifted matrix. still, the entries in wgap
                          ! have to be computed from work since the entries
                          ! in w might be of the same order so that gaps are not
                          ! exhibited correctly for very close eigenvalues.
                          if( newfst==1_ilp ) then
                             lgap = max( zero,w(wbegin)-werr(wbegin) - vl )
                         else
                             lgap = wgap( wbegin+newfst-2 )
                          endif
                          rgap = wgap( wbegin+newlst-1 )
                          ! compute left- and rightmost eigenvalue of child
                          ! to high precision in order to shift as close
                          ! as possible and obtain as large relative gaps
                          ! as possible
                          do k =1,2
                             if(k==1_ilp) then
                                p = indexw( wbegin-1+newfst )
                             else
                                p = indexw( wbegin-1+newlst )
                             endif
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_dlarrb( in, d(ibegin),work( indlld+ibegin-1 ),p,p,rqtol, &
                             rqtol, offset,work(wbegin),wgap(wbegin),werr(wbegin),work( indwrk ),&
                                       iwork( iindwk ), pivmin, spdiam,in, iinfo )
                          end do
                          if((wbegin+newlst-1<dol).or.(wbegin+newfst-1>dou)) then
                             ! if the cluster contains no desired eigenvalues
                             ! skip the computation of that branch of the rep. tree
                             ! we could skip before the refinement of the extremal
                             ! eigenvalues of the child, but then the representation
                             ! tree could be different from the one when nothing is
                             ! skipped. for this reason we skip at this place.
                             idone = idone + newlst - newfst + 1_ilp
                             goto 139
                          endif
                          ! compute rrr of child cluster.
                          ! note that the new rrr is stored in z
                          ! stdlib_dlarrf needs lwork = 2*n
                          call stdlib_dlarrf( in, d( ibegin ), l( ibegin ),work(indld+ibegin-1),&
                          newfst, newlst, work(wbegin),wgap(wbegin), werr(wbegin),spdiam, lgap, &
                          rgap, pivmin, tau,work( indin1 ), work( indin2 ),work( indwrk ), iinfo )
                                    
                          ! in the complex case, stdlib_dlarrf cannot write
                          ! the new rrr directly into z and needs an intermediate
                          ! workspace
                          do k = 1, in-1
                             z( ibegin+k-1, newftt ) =cmplx( work( indin1+k-1 ), zero,KIND=dp)
                                       
                             z( ibegin+k-1, newftt+1 ) =cmplx( work( indin2+k-1 ), zero,KIND=dp)
                                       
                          end do
                          z( iend, newftt ) =cmplx( work( indin1+in-1 ), zero,KIND=dp)
                          if( iinfo==0_ilp ) then
                             ! a new rrr for the cluster was found by stdlib_dlarrf
                             ! update shift and store it
                             ssigma = sigma + tau
                             z( iend, newftt+1 ) = cmplx( ssigma, zero,KIND=dp)
                             ! work() are the midpoints and werr() the semi-width
                             ! note that the entries in w are unchanged.
                             do k = newfst, newlst
                                fudge =three*eps*abs(work(wbegin+k-1))
                                work( wbegin + k - 1_ilp ) =work( wbegin + k - 1_ilp) - tau
                                fudge = fudge +four*eps*abs(work(wbegin+k-1))
                                ! fudge errors
                                werr( wbegin + k - 1_ilp ) =werr( wbegin + k - 1_ilp ) + fudge
                                ! gaps are not fudged. provided that werr is small
                                ! when eigenvalues are close, a zero gap indicates
                                ! that a new representation is needed for resolving
                                ! the cluster. a fudge could lead to a wrong decision
                                ! of judging eigenvalues 'separated' which in
                                ! reality are not. this could have a negative impact
                                ! on the orthogonality of the computed eigenvectors.
                             end do
                             nclus = nclus + 1_ilp
                             k = newcls + 2_ilp*nclus
                             iwork( k-1 ) = newfst
                             iwork( k ) = newlst
                          else
                             info = -2_ilp
                             return
                          endif
                       else
                          ! compute eigenvector of singleton
                          iter = 0_ilp
                          tol = four * log(real(in,KIND=dp)) * eps
                          k = newfst
                          windex = wbegin + k - 1_ilp
                          windmn = max(windex - 1_ilp,1_ilp)
                          windpl = min(windex + 1_ilp,m)
                          lambda = work( windex )
                          done = done + 1_ilp
                          ! check if eigenvector computation is to be skipped
                          if((windex<dol).or.(windex>dou)) then
                             eskip = .true.
                             goto 125
                          else
                             eskip = .false.
                          endif
                          left = work( windex ) - werr( windex )
                          right = work( windex ) + werr( windex )
                          indeig = indexw( windex )
                          ! note that since we compute the eigenpairs for a child,
                          ! all eigenvalue approximations are w.r.t the same shift.
                          ! in this case, the entries in work should be used for
                          ! computing the gaps since they exhibit even very small
                          ! differences in the eigenvalues, as opposed to the
                          ! entries in w which might "look" the same.
                          if( k == 1_ilp) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vl, the formula
                             ! lgap = max( zero, (sigma - vl) + lambda )
                             ! can lead to an overestimation of the left gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small left gap.
                             lgap = eps*max(abs(left),abs(right))
                          else
                             lgap = wgap(windmn)
                          endif
                          if( k == im) then
                             ! in the case range='i' and with not much initial
                             ! accuracy in lambda and vu, the formula
                             ! can lead to an overestimation of the right gap and
                             ! thus to inadequately early rqi 'convergence'.
                             ! prevent this by forcing a small right gap.
                             rgap = eps*max(abs(left),abs(right))
                          else
                             rgap = wgap(windex)
                          endif
                          gap = min( lgap, rgap )
                          if(( k == 1_ilp).or.(k == im)) then
                             ! the eigenvector support can become wrong
                             ! because significant entries could be cut off due to a
                             ! large gaptol parameter in lar1v. prevent this.
                             gaptol = zero
                          else
                             gaptol = gap * eps
                          endif
                          isupmn = in
                          isupmx = 1_ilp
                          ! update wgap so that it holds the minimum gap
                          ! to the left or the right. this is crucial in the
                          ! case where bisection is used to ensure that the
                          ! eigenvalue is refined up to the required precision.
                          ! the correct value is restored afterwards.
                          savgap = wgap(windex)
                          wgap(windex) = gap
                          ! we want to use the rayleigh quotient correction
                          ! as often as possible since it converges quadratically
                          ! when we are close enough to the desired eigenvalue.
                          ! however, the rayleigh quotient can have the wrong sign
                          ! and lead us away from the desired eigenvalue. in this
                          ! case, the best we can do is to use bisection.
                          usedbs = .false.
                          usedrq = .false.
                          ! bisection is initially turned off unless it is forced
                          needbs =  .not.tryrqc
                          120 continue
                          ! check if bisection should be used to refine eigenvalue
                          if(needbs) then
                             ! take the bisection as new iterate
                             usedbs = .true.
                             itmp1 = iwork( iindr+windex )
                             offset = indexw( wbegin ) - 1_ilp
                             call stdlib_dlarrb( in, d(ibegin),work(indlld+ibegin-1),indeig,&
                             indeig,zero, two*eps, offset,work(wbegin),wgap(wbegin),werr(wbegin),&
                                       work( indwrk ),iwork( iindwk ), pivmin, spdiam,itmp1, iinfo )
                             if( iinfo/=0_ilp ) then
                                info = -3_ilp
                                return
                             endif
                             lambda = work( windex )
                             ! reset twist index from inaccurate lambda to
                             ! force computation of true mingma
                             iwork( iindr+windex ) = 0_ilp
                          endif
                          ! given lambda, compute the eigenvector.
                          call stdlib_zlar1v( in, 1_ilp, in, lambda, d( ibegin ),l( ibegin ), work(&
                          indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( ibegin, windex &
                          ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+windex ), isuppz( &
                                    2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk ) )
                          if(iter == 0_ilp) then
                             bstres = resid
                             bstw = lambda
                          elseif(resid<bstres) then
                             bstres = resid
                             bstw = lambda
                          endif
                          isupmn = min(isupmn,isuppz( 2_ilp*windex-1 ))
                          isupmx = max(isupmx,isuppz( 2_ilp*windex ))
                          iter = iter + 1_ilp
                          ! sin alpha <= |resid|/gap
                          ! note that both the residual and the gap are
                          ! proportional to the matrix, so ||t|| doesn't play
                          ! a role in the quotient
                          ! convergence test for rayleigh-quotient iteration
                          ! (omitted when bisection has been used)
                          if( resid>tol*gap .and. abs( rqcorr )>rqtol*abs( lambda ) .and. .not. &
                                    usedbs)then
                             ! we need to check that the rqcorr update doesn't
                             ! move the eigenvalue away from the desired one and
                             ! towards a neighbor. -> protection with bisection
                             if(indeig<=negcnt) then
                                ! the wanted eigenvalue lies to the left
                                sgndef = -one
                             else
                                ! the wanted eigenvalue lies to the right
                                sgndef = one
                             endif
                             ! we only use the rqcorr if it improves the
                             ! the iterate reasonably.
                             if( ( rqcorr*sgndef>=zero ).and.( lambda + rqcorr<= right).and.( &
                                       lambda + rqcorr>= left)) then
                                usedrq = .true.
                                ! store new midpoint of bisection interval in work
                                if(sgndef==one) then
                                   ! the current lambda is on the left of the true
                                   ! eigenvalue
                                   left = lambda
                                   ! we prefer to assume that the error estimate
                                   ! is correct. we could make the interval not
                                   ! as a bracket but to be modified if the rqcorr
                                   ! chooses to. in this case, the right side should
                                   ! be modified as follows:
                                    ! right = max(right, lambda + rqcorr)
                                else
                                   ! the current lambda is on the right of the true
                                   ! eigenvalue
                                   right = lambda
                                   ! see comment about assuming the error estimate is
                                   ! correct above.
                                    ! left = min(left, lambda + rqcorr)
                                endif
                                work( windex ) =half * (right + left)
                                ! take rqcorr since it has the correct sign and
                                ! improves the iterate reasonably
                                lambda = lambda + rqcorr
                                ! update width of error interval
                                werr( windex ) =half * (right-left)
                             else
                                needbs = .true.
                             endif
                             if(right-left<rqtol*abs(lambda)) then
                                   ! the eigenvalue is computed to bisection accuracy
                                   ! compute eigenvector and stop
                                usedbs = .true.
                                goto 120
                             elseif( iter<maxitr ) then
                                goto 120
                             elseif( iter==maxitr ) then
                                needbs = .true.
                                goto 120
                             else
                                info = 5_ilp
                                return
                             end if
                          else
                             stp2ii = .false.
             if(usedrq .and. usedbs .and.bstres<=resid) then
                                lambda = bstw
                                stp2ii = .true.
                             endif
                             if (stp2ii) then
                                ! improve error angle by second step
                                call stdlib_zlar1v( in, 1_ilp, in, lambda,d( ibegin ), l( ibegin ),&
                                work(indld+ibegin-1),work(indlld+ibegin-1),pivmin, gaptol, z( &
                                ibegin, windex ),.not.usedbs, negcnt, ztz, mingma,iwork( iindr+&
                                windex ),isuppz( 2_ilp*windex-1 ),nrminv, resid, rqcorr, work( indwrk &
                                          ) )
                             endif
                             work( windex ) = lambda
                          end if
                          ! compute fp-vector support w.r.t. whole matrix
                          isuppz( 2_ilp*windex-1 ) = isuppz( 2_ilp*windex-1 )+oldien
                          isuppz( 2_ilp*windex ) = isuppz( 2_ilp*windex )+oldien
                          zfrom = isuppz( 2_ilp*windex-1 )
                          zto = isuppz( 2_ilp*windex )
                          isupmn = isupmn + oldien
                          isupmx = isupmx + oldien
                          ! ensure vector is ok if support in the rqi has changed
                          if(isupmn<zfrom) then
                             do ii = isupmn,zfrom-1
                                z( ii, windex ) = zero
                             end do
                          endif
                          if(isupmx>zto) then
                             do ii = zto+1,isupmx
                                z( ii, windex ) = zero
                             end do
                          endif
                          call stdlib_zdscal( zto-zfrom+1, nrminv,z( zfrom, windex ), 1_ilp )
                          125 continue
                          ! update w
                          w( windex ) = lambda+sigma
                          ! recompute the gaps on the left and right
                          ! but only allow them to become larger and not
                          ! smaller (which can only happen through "bad"
                          ! cancellation and doesn't reflect the theory
                          ! where the initial gaps are underestimated due
                          ! to werr being too crude.)
                          if(.not.eskip) then
                             if( k>1_ilp) then
                                wgap( windmn ) = max( wgap(windmn),w(windex)-werr(windex)- w(&
                                          windmn)-werr(windmn) )
                             endif
                             if( windex<wend ) then
                                wgap( windex ) = max( savgap,w( windpl )-werr( windpl )- w( &
                                          windex )-werr( windex) )
                             endif
                          endif
                          idone = idone + 1_ilp
                       endif
                       ! here ends the code for the current child
                       139 continue
                       ! proceed to any remaining child nodes
                       newfst = j + 1_ilp
                    end do loop_140
                 end do loop_150
                 ndepth = ndepth + 1_ilp
                 go to 40
              end if
              ibegin = iend + 1_ilp
              wbegin = wend + 1_ilp
           end do loop_170
           return
     end subroutine stdlib_zlarrv




     pure module subroutine stdlib_slar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
     !! SLAR1V computes the (scaled) r-th column of the inverse of
     !! the sumbmatrix in rows B1 through BN of the tridiagonal matrix
     !! L D L**T - sigma I. When sigma is close to an eigenvalue, the
     !! computed vector is an accurate eigenvector. Usually, r corresponds
     !! to the index where the eigenvector is largest in magnitude.
     !! The following steps accomplish this computation :
     !! (a) Stationary qd transform,  L D L**T - sigma I = L(+) D(+) L(+)**T,
     !! (b) Progressive qd transform, L D L**T - sigma I = U(-) D(-) U(-)**T,
     !! (c) Computation of the diagonal elements of the inverse of
     !! L D L**T - sigma I by combining the above transforms, and choosing
     !! r as the index where the diagonal of the inverse is (one of the)
     !! largest in magnitude.
     !! (d) Computation of the (scaled) r-th column of the inverse using the
     !! twisted factorization obtained by combining the top part of the
     !! the stationary and the bottom part of the progressive transform.
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(sp), intent(in) :: gaptol, lambda, pivmin
           real(sp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*)
           real(sp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: sawnan1, sawnan2
           integer(ilp) :: i, indlpl, indp, inds, indumn, neg1, neg2, r1, r2
           real(sp) :: dminus, dplus, eps, s, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_slamch( 'PRECISION' )
           if( r==0_ilp ) then
              r1 = b1
              r2 = bn
           else
              r1 = r
              r2 = r
           end if
           ! storage for lplus
           indlpl = 0_ilp
           ! storage for uminus
           indumn = n
           inds = 2_ilp*n + 1_ilp
           indp = 3_ilp*n + 1_ilp
           if( b1==1_ilp ) then
              work( inds ) = zero
           else
              work( inds+b1-1 ) = lld( b1-1 )
           end if
           ! compute the stationary transform (using the differential form)
           ! until the index r2.
           sawnan1 = .false.
           neg1 = 0_ilp
           s = work( inds+b1-1 ) - lambda
           do i = b1, r1 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              if(dplus<zero) neg1 = neg1 + 1_ilp
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_sisnan( s )
           if( sawnan1 ) goto 60
           do i = r1, r2 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_sisnan( s )
           60 continue
           if( sawnan1 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg1 = 0_ilp
              s = work( inds+b1-1 ) - lambda
              do i = b1, r1 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 if(dplus<zero) neg1 = neg1 + 1_ilp
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
              do i = r1, r2 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
           end if
           ! compute the progressive transform (using the differential form)
           ! until the index r1
           sawnan2 = .false.
           neg2 = 0_ilp
           work( indp+bn-1 ) = d( bn ) - lambda
           do i = bn - 1, r1, -1
              dminus = lld( i ) + work( indp+i )
              tmp = d( i ) / dminus
              if(dminus<zero) neg2 = neg2 + 1_ilp
              work( indumn+i ) = l( i )*tmp
              work( indp+i-1 ) = work( indp+i )*tmp - lambda
           end do
           tmp = work( indp+r1-1 )
           sawnan2 = stdlib_sisnan( tmp )
           if( sawnan2 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg2 = 0_ilp
              do i = bn-1, r1, -1
                 dminus = lld( i ) + work( indp+i )
                 if(abs(dminus)<pivmin) dminus = -pivmin
                 tmp = d( i ) / dminus
                 if(dminus<zero) neg2 = neg2 + 1_ilp
                 work( indumn+i ) = l( i )*tmp
                 work( indp+i-1 ) = work( indp+i )*tmp - lambda
                 if( tmp==zero )work( indp+i-1 ) = d( i ) - lambda
              end do
           end if
           ! find the index (from r1 to r2) of the largest (in magnitude)
           ! diagonal element of the inverse
           mingma = work( inds+r1-1 ) + work( indp+r1-1 )
           if( mingma<zero ) neg1 = neg1 + 1_ilp
           if( wantnc ) then
              negcnt = neg1 + neg2
           else
              negcnt = -1_ilp
           endif
           if( abs(mingma)==zero )mingma = eps*work( inds+r1-1 )
           r = r1
           do i = r1, r2 - 1
              tmp = work( inds+i ) + work( indp+i )
              if( tmp==zero )tmp = eps*work( inds+i )
              if( abs( tmp )<=abs( mingma ) ) then
                 mingma = tmp
                 r = i + 1_ilp
              end if
           end do
           ! compute the fp vector: solve n^t v = e_r
           isuppz( 1_ilp ) = b1
           isuppz( 2_ilp ) = bn
           z( r ) = one
           ztz = one
           ! compute the fp vector upwards from r
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r-1, b1, -1
                 z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    goto 220
                 endif
                 ztz = ztz + z( i )*z( i )
              end do
              220 continue
           else
              ! run slower loop if nan occurred.
              do i = r - 1, b1, -1
                 if( z( i+1 )==zero ) then
                    z( i ) = -( ld( i+1 ) / ld( i ) )*z( i+2 )
                 else
                    z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    go to 240
                 end if
                 ztz = ztz + z( i )*z( i )
              end do
              240 continue
           endif
           ! compute the fp vector downwards from r in blocks of size blksiz
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r, bn-1
                 z( i+1 ) = -( work( indumn+i )*z( i ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 260
                 end if
                 ztz = ztz + z( i+1 )*z( i+1 )
              end do
              260 continue
           else
              ! run slower loop if nan occurred.
              do i = r, bn - 1
                 if( z( i )==zero ) then
                    z( i+1 ) = -( ld( i-1 ) / ld( i ) )*z( i-1 )
                 else
                    z( i+1 ) = -( work( indumn+i )*z( i ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 280
                 end if
                 ztz = ztz + z( i+1 )*z( i+1 )
              end do
              280 continue
           end if
           ! compute quantities for convergence test
           tmp = one / ztz
           nrminv = sqrt( tmp )
           resid = abs( mingma )*nrminv
           rqcorr = mingma*tmp
           return
     end subroutine stdlib_slar1v

     pure module subroutine stdlib_dlar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
     !! DLAR1V computes the (scaled) r-th column of the inverse of
     !! the sumbmatrix in rows B1 through BN of the tridiagonal matrix
     !! L D L**T - sigma I. When sigma is close to an eigenvalue, the
     !! computed vector is an accurate eigenvector. Usually, r corresponds
     !! to the index where the eigenvector is largest in magnitude.
     !! The following steps accomplish this computation :
     !! (a) Stationary qd transform,  L D L**T - sigma I = L(+) D(+) L(+)**T,
     !! (b) Progressive qd transform, L D L**T - sigma I = U(-) D(-) U(-)**T,
     !! (c) Computation of the diagonal elements of the inverse of
     !! L D L**T - sigma I by combining the above transforms, and choosing
     !! r as the index where the diagonal of the inverse is (one of the)
     !! largest in magnitude.
     !! (d) Computation of the (scaled) r-th column of the inverse using the
     !! twisted factorization obtained by combining the top part of the
     !! the stationary and the bottom part of the progressive transform.
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(dp), intent(in) :: gaptol, lambda, pivmin
           real(dp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*)
           real(dp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: sawnan1, sawnan2
           integer(ilp) :: i, indlpl, indp, inds, indumn, neg1, neg2, r1, r2
           real(dp) :: dminus, dplus, eps, s, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_dlamch( 'PRECISION' )
           if( r==0_ilp ) then
              r1 = b1
              r2 = bn
           else
              r1 = r
              r2 = r
           end if
           ! storage for lplus
           indlpl = 0_ilp
           ! storage for uminus
           indumn = n
           inds = 2_ilp*n + 1_ilp
           indp = 3_ilp*n + 1_ilp
           if( b1==1_ilp ) then
              work( inds ) = zero
           else
              work( inds+b1-1 ) = lld( b1-1 )
           end if
           ! compute the stationary transform (using the differential form)
           ! until the index r2.
           sawnan1 = .false.
           neg1 = 0_ilp
           s = work( inds+b1-1 ) - lambda
           do i = b1, r1 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              if(dplus<zero) neg1 = neg1 + 1_ilp
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_disnan( s )
           if( sawnan1 ) goto 60
           do i = r1, r2 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_disnan( s )
           60 continue
           if( sawnan1 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg1 = 0_ilp
              s = work( inds+b1-1 ) - lambda
              do i = b1, r1 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 if(dplus<zero) neg1 = neg1 + 1_ilp
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
              do i = r1, r2 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
           end if
           ! compute the progressive transform (using the differential form)
           ! until the index r1
           sawnan2 = .false.
           neg2 = 0_ilp
           work( indp+bn-1 ) = d( bn ) - lambda
           do i = bn - 1, r1, -1
              dminus = lld( i ) + work( indp+i )
              tmp = d( i ) / dminus
              if(dminus<zero) neg2 = neg2 + 1_ilp
              work( indumn+i ) = l( i )*tmp
              work( indp+i-1 ) = work( indp+i )*tmp - lambda
           end do
           tmp = work( indp+r1-1 )
           sawnan2 = stdlib_disnan( tmp )
           if( sawnan2 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg2 = 0_ilp
              do i = bn-1, r1, -1
                 dminus = lld( i ) + work( indp+i )
                 if(abs(dminus)<pivmin) dminus = -pivmin
                 tmp = d( i ) / dminus
                 if(dminus<zero) neg2 = neg2 + 1_ilp
                 work( indumn+i ) = l( i )*tmp
                 work( indp+i-1 ) = work( indp+i )*tmp - lambda
                 if( tmp==zero )work( indp+i-1 ) = d( i ) - lambda
              end do
           end if
           ! find the index (from r1 to r2) of the largest (in magnitude)
           ! diagonal element of the inverse
           mingma = work( inds+r1-1 ) + work( indp+r1-1 )
           if( mingma<zero ) neg1 = neg1 + 1_ilp
           if( wantnc ) then
              negcnt = neg1 + neg2
           else
              negcnt = -1_ilp
           endif
           if( abs(mingma)==zero )mingma = eps*work( inds+r1-1 )
           r = r1
           do i = r1, r2 - 1
              tmp = work( inds+i ) + work( indp+i )
              if( tmp==zero )tmp = eps*work( inds+i )
              if( abs( tmp )<=abs( mingma ) ) then
                 mingma = tmp
                 r = i + 1_ilp
              end if
           end do
           ! compute the fp vector: solve n^t v = e_r
           isuppz( 1_ilp ) = b1
           isuppz( 2_ilp ) = bn
           z( r ) = one
           ztz = one
           ! compute the fp vector upwards from r
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r-1, b1, -1
                 z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    goto 220
                 endif
                 ztz = ztz + z( i )*z( i )
              end do
              220 continue
           else
              ! run slower loop if nan occurred.
              do i = r - 1, b1, -1
                 if( z( i+1 )==zero ) then
                    z( i ) = -( ld( i+1 ) / ld( i ) )*z( i+2 )
                 else
                    z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    go to 240
                 end if
                 ztz = ztz + z( i )*z( i )
              end do
              240 continue
           endif
           ! compute the fp vector downwards from r in blocks of size blksiz
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r, bn-1
                 z( i+1 ) = -( work( indumn+i )*z( i ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 260
                 end if
                 ztz = ztz + z( i+1 )*z( i+1 )
              end do
              260 continue
           else
              ! run slower loop if nan occurred.
              do i = r, bn - 1
                 if( z( i )==zero ) then
                    z( i+1 ) = -( ld( i-1 ) / ld( i ) )*z( i-1 )
                 else
                    z( i+1 ) = -( work( indumn+i )*z( i ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 280
                 end if
                 ztz = ztz + z( i+1 )*z( i+1 )
              end do
              280 continue
           end if
           ! compute quantities for convergence test
           tmp = one / ztz
           nrminv = sqrt( tmp )
           resid = abs( mingma )*nrminv
           rqcorr = mingma*tmp
           return
     end subroutine stdlib_dlar1v


     pure module subroutine stdlib_clar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
     !! CLAR1V computes the (scaled) r-th column of the inverse of
     !! the sumbmatrix in rows B1 through BN of the tridiagonal matrix
     !! L D L**T - sigma I. When sigma is close to an eigenvalue, the
     !! computed vector is an accurate eigenvector. Usually, r corresponds
     !! to the index where the eigenvector is largest in magnitude.
     !! The following steps accomplish this computation :
     !! (a) Stationary qd transform,  L D L**T - sigma I = L(+) D(+) L(+)**T,
     !! (b) Progressive qd transform, L D L**T - sigma I = U(-) D(-) U(-)**T,
     !! (c) Computation of the diagonal elements of the inverse of
     !! L D L**T - sigma I by combining the above transforms, and choosing
     !! r as the index where the diagonal of the inverse is (one of the)
     !! largest in magnitude.
     !! (d) Computation of the (scaled) r-th column of the inverse using the
     !! twisted factorization obtained by combining the top part of the
     !! the stationary and the bottom part of the progressive transform.
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(sp), intent(in) :: gaptol, lambda, pivmin
           real(sp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*)
           real(sp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: sawnan1, sawnan2
           integer(ilp) :: i, indlpl, indp, inds, indumn, neg1, neg2, r1, r2
           real(sp) :: dminus, dplus, eps, s, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_slamch( 'PRECISION' )
           if( r==0_ilp ) then
              r1 = b1
              r2 = bn
           else
              r1 = r
              r2 = r
           end if
           ! storage for lplus
           indlpl = 0_ilp
           ! storage for uminus
           indumn = n
           inds = 2_ilp*n + 1_ilp
           indp = 3_ilp*n + 1_ilp
           if( b1==1_ilp ) then
              work( inds ) = zero
           else
              work( inds+b1-1 ) = lld( b1-1 )
           end if
           ! compute the stationary transform (using the differential form)
           ! until the index r2.
           sawnan1 = .false.
           neg1 = 0_ilp
           s = work( inds+b1-1 ) - lambda
           do i = b1, r1 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              if(dplus<zero) neg1 = neg1 + 1_ilp
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_sisnan( s )
           if( sawnan1 ) goto 60
           do i = r1, r2 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_sisnan( s )
           60 continue
           if( sawnan1 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg1 = 0_ilp
              s = work( inds+b1-1 ) - lambda
              do i = b1, r1 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 if(dplus<zero) neg1 = neg1 + 1_ilp
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
              do i = r1, r2 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
           end if
           ! compute the progressive transform (using the differential form)
           ! until the index r1
           sawnan2 = .false.
           neg2 = 0_ilp
           work( indp+bn-1 ) = d( bn ) - lambda
           do i = bn - 1, r1, -1
              dminus = lld( i ) + work( indp+i )
              tmp = d( i ) / dminus
              if(dminus<zero) neg2 = neg2 + 1_ilp
              work( indumn+i ) = l( i )*tmp
              work( indp+i-1 ) = work( indp+i )*tmp - lambda
           end do
           tmp = work( indp+r1-1 )
           sawnan2 = stdlib_sisnan( tmp )
           if( sawnan2 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg2 = 0_ilp
              do i = bn-1, r1, -1
                 dminus = lld( i ) + work( indp+i )
                 if(abs(dminus)<pivmin) dminus = -pivmin
                 tmp = d( i ) / dminus
                 if(dminus<zero) neg2 = neg2 + 1_ilp
                 work( indumn+i ) = l( i )*tmp
                 work( indp+i-1 ) = work( indp+i )*tmp - lambda
                 if( tmp==zero )work( indp+i-1 ) = d( i ) - lambda
              end do
           end if
           ! find the index (from r1 to r2) of the largest (in magnitude)
           ! diagonal element of the inverse
           mingma = work( inds+r1-1 ) + work( indp+r1-1 )
           if( mingma<zero ) neg1 = neg1 + 1_ilp
           if( wantnc ) then
              negcnt = neg1 + neg2
           else
              negcnt = -1_ilp
           endif
           if( abs(mingma)==zero )mingma = eps*work( inds+r1-1 )
           r = r1
           do i = r1, r2 - 1
              tmp = work( inds+i ) + work( indp+i )
              if( tmp==zero )tmp = eps*work( inds+i )
              if( abs( tmp )<=abs( mingma ) ) then
                 mingma = tmp
                 r = i + 1_ilp
              end if
           end do
           ! compute the fp vector: solve n^t v = e_r
           isuppz( 1_ilp ) = b1
           isuppz( 2_ilp ) = bn
           z( r ) = cone
           ztz = one
           ! compute the fp vector upwards from r
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r-1, b1, -1
                 z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    goto 220
                 endif
                 ztz = ztz + real( z( i )*z( i ),KIND=sp)
              end do
              220 continue
           else
              ! run slower loop if nan occurred.
              do i = r - 1, b1, -1
                 if( z( i+1 )==zero ) then
                    z( i ) = -( ld( i+1 ) / ld( i ) )*z( i+2 )
                 else
                    z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    go to 240
                 end if
                 ztz = ztz + real( z( i )*z( i ),KIND=sp)
              end do
              240 continue
           endif
           ! compute the fp vector downwards from r in blocks of size blksiz
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r, bn-1
                 z( i+1 ) = -( work( indumn+i )*z( i ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 260
                 end if
                 ztz = ztz + real( z( i+1 )*z( i+1 ),KIND=sp)
              end do
              260 continue
           else
              ! run slower loop if nan occurred.
              do i = r, bn - 1
                 if( z( i )==zero ) then
                    z( i+1 ) = -( ld( i-1 ) / ld( i ) )*z( i-1 )
                 else
                    z( i+1 ) = -( work( indumn+i )*z( i ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 280
                 end if
                 ztz = ztz + real( z( i+1 )*z( i+1 ),KIND=sp)
              end do
              280 continue
           end if
           ! compute quantities for convergence test
           tmp = one / ztz
           nrminv = sqrt( tmp )
           resid = abs( mingma )*nrminv
           rqcorr = mingma*tmp
           return
     end subroutine stdlib_clar1v

     pure module subroutine stdlib_zlar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
     !! ZLAR1V computes the (scaled) r-th column of the inverse of
     !! the sumbmatrix in rows B1 through BN of the tridiagonal matrix
     !! L D L**T - sigma I. When sigma is close to an eigenvalue, the
     !! computed vector is an accurate eigenvector. Usually, r corresponds
     !! to the index where the eigenvector is largest in magnitude.
     !! The following steps accomplish this computation :
     !! (a) Stationary qd transform,  L D L**T - sigma I = L(+) D(+) L(+)**T,
     !! (b) Progressive qd transform, L D L**T - sigma I = U(-) D(-) U(-)**T,
     !! (c) Computation of the diagonal elements of the inverse of
     !! L D L**T - sigma I by combining the above transforms, and choosing
     !! r as the index where the diagonal of the inverse is (one of the)
     !! largest in magnitude.
     !! (d) Computation of the (scaled) r-th column of the inverse using the
     !! twisted factorization obtained by combining the top part of the
     !! the stationary and the bottom part of the progressive transform.
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(dp), intent(in) :: gaptol, lambda, pivmin
           real(dp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*)
           real(dp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: sawnan1, sawnan2
           integer(ilp) :: i, indlpl, indp, inds, indumn, neg1, neg2, r1, r2
           real(dp) :: dminus, dplus, eps, s, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           eps = stdlib_dlamch( 'PRECISION' )
           if( r==0_ilp ) then
              r1 = b1
              r2 = bn
           else
              r1 = r
              r2 = r
           end if
           ! storage for lplus
           indlpl = 0_ilp
           ! storage for uminus
           indumn = n
           inds = 2_ilp*n + 1_ilp
           indp = 3_ilp*n + 1_ilp
           if( b1==1_ilp ) then
              work( inds ) = zero
           else
              work( inds+b1-1 ) = lld( b1-1 )
           end if
           ! compute the stationary transform (using the differential form)
           ! until the index r2.
           sawnan1 = .false.
           neg1 = 0_ilp
           s = work( inds+b1-1 ) - lambda
           do i = b1, r1 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              if(dplus<zero) neg1 = neg1 + 1_ilp
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_disnan( s )
           if( sawnan1 ) goto 60
           do i = r1, r2 - 1
              dplus = d( i ) + s
              work( indlpl+i ) = ld( i ) / dplus
              work( inds+i ) = s*work( indlpl+i )*l( i )
              s = work( inds+i ) - lambda
           end do
           sawnan1 = stdlib_disnan( s )
           60 continue
           if( sawnan1 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg1 = 0_ilp
              s = work( inds+b1-1 ) - lambda
              do i = b1, r1 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 if(dplus<zero) neg1 = neg1 + 1_ilp
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
              do i = r1, r2 - 1
                 dplus = d( i ) + s
                 if(abs(dplus)<pivmin) dplus = -pivmin
                 work( indlpl+i ) = ld( i ) / dplus
                 work( inds+i ) = s*work( indlpl+i )*l( i )
                 if( work( indlpl+i )==zero )work( inds+i ) = lld( i )
                 s = work( inds+i ) - lambda
              end do
           end if
           ! compute the progressive transform (using the differential form)
           ! until the index r1
           sawnan2 = .false.
           neg2 = 0_ilp
           work( indp+bn-1 ) = d( bn ) - lambda
           do i = bn - 1, r1, -1
              dminus = lld( i ) + work( indp+i )
              tmp = d( i ) / dminus
              if(dminus<zero) neg2 = neg2 + 1_ilp
              work( indumn+i ) = l( i )*tmp
              work( indp+i-1 ) = work( indp+i )*tmp - lambda
           end do
           tmp = work( indp+r1-1 )
           sawnan2 = stdlib_disnan( tmp )
           if( sawnan2 ) then
              ! runs a slower version of the above loop if a nan is detected
              neg2 = 0_ilp
              do i = bn-1, r1, -1
                 dminus = lld( i ) + work( indp+i )
                 if(abs(dminus)<pivmin) dminus = -pivmin
                 tmp = d( i ) / dminus
                 if(dminus<zero) neg2 = neg2 + 1_ilp
                 work( indumn+i ) = l( i )*tmp
                 work( indp+i-1 ) = work( indp+i )*tmp - lambda
                 if( tmp==zero )work( indp+i-1 ) = d( i ) - lambda
              end do
           end if
           ! find the index (from r1 to r2) of the largest (in magnitude)
           ! diagonal element of the inverse
           mingma = work( inds+r1-1 ) + work( indp+r1-1 )
           if( mingma<zero ) neg1 = neg1 + 1_ilp
           if( wantnc ) then
              negcnt = neg1 + neg2
           else
              negcnt = -1_ilp
           endif
           if( abs(mingma)==zero )mingma = eps*work( inds+r1-1 )
           r = r1
           do i = r1, r2 - 1
              tmp = work( inds+i ) + work( indp+i )
              if( tmp==zero )tmp = eps*work( inds+i )
              if( abs( tmp )<=abs( mingma ) ) then
                 mingma = tmp
                 r = i + 1_ilp
              end if
           end do
           ! compute the fp vector: solve n^t v = e_r
           isuppz( 1_ilp ) = b1
           isuppz( 2_ilp ) = bn
           z( r ) = cone
           ztz = one
           ! compute the fp vector upwards from r
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r-1, b1, -1
                 z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    goto 220
                 endif
                 ztz = ztz + real( z( i )*z( i ),KIND=dp)
              end do
              220 continue
           else
              ! run slower loop if nan occurred.
              do i = r - 1, b1, -1
                 if( z( i+1 )==zero ) then
                    z( i ) = -( ld( i+1 ) / ld( i ) )*z( i+2 )
                 else
                    z( i ) = -( work( indlpl+i )*z( i+1 ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i ) = zero
                    isuppz( 1_ilp ) = i + 1_ilp
                    go to 240
                 end if
                 ztz = ztz + real( z( i )*z( i ),KIND=dp)
              end do
              240 continue
           endif
           ! compute the fp vector downwards from r in blocks of size blksiz
           if( .not.sawnan1 .and. .not.sawnan2 ) then
              do i = r, bn-1
                 z( i+1 ) = -( work( indumn+i )*z( i ) )
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 260
                 end if
                 ztz = ztz + real( z( i+1 )*z( i+1 ),KIND=dp)
              end do
              260 continue
           else
              ! run slower loop if nan occurred.
              do i = r, bn - 1
                 if( z( i )==zero ) then
                    z( i+1 ) = -( ld( i-1 ) / ld( i ) )*z( i-1 )
                 else
                    z( i+1 ) = -( work( indumn+i )*z( i ) )
                 end if
                 if( (abs(z(i))+abs(z(i+1)))* abs(ld(i))<gaptol )then
                    z( i+1 ) = zero
                    isuppz( 2_ilp ) = i
                    go to 280
                 end if
                 ztz = ztz + real( z( i+1 )*z( i+1 ),KIND=dp)
              end do
              280 continue
           end if
           ! compute quantities for convergence test
           tmp = one / ztz
           nrminv = sqrt( tmp )
           resid = abs( mingma )*nrminv
           rqcorr = mingma*tmp
           return
     end subroutine stdlib_zlar1v



end submodule stdlib_lapack_eigv_tridiag2
