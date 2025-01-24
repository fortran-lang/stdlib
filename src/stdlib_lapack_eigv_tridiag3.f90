submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_tridiag3
  implicit none


  contains

     pure module subroutine stdlib_sstev( jobz, n, d, e, z, ldz, work, info )
     !! SSTEV computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric tridiagonal matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: imax, iscale
           real(sp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           tnrm = stdlib_slanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( n, sigma, d, 1_ilp )
              call stdlib_sscal( n-1, sigma, e( 1_ilp ), 1_ilp )
           end if
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvalues and
           ! eigenvectors, call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, d, e, info )
           else
              call stdlib_ssteqr( 'I', n, d, e, z, ldz, work, info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, d, 1_ilp )
           end if
           return
     end subroutine stdlib_sstev

     pure module subroutine stdlib_dstev( jobz, n, d, e, z, ldz, work, info )
     !! DSTEV computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric tridiagonal matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: imax, iscale
           real(dp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           tnrm = stdlib_dlanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( n, sigma, d, 1_ilp )
              call stdlib_dscal( n-1, sigma, e( 1_ilp ), 1_ilp )
           end if
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvalues and
           ! eigenvectors, call stdlib_dsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, d, e, info )
           else
              call stdlib_dsteqr( 'I', n, d, e, z, ldz, work, info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, d, 1_ilp )
           end if
           return
     end subroutine stdlib_dstev




     pure module subroutine stdlib_sstevd( jobz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
     !! SSTEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric tridiagonal matrix. If eigenvectors are desired, it
     !! uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iscale, liwmin, lwmin
           real(sp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           liwmin = 1_ilp
           lwmin = 1_ilp
           if( n>1_ilp .and. wantz ) then
              lwmin = 1_ilp + 4_ilp*n + n**2_ilp
              liwmin = 3_ilp + 5_ilp*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           tnrm = stdlib_slanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( n, sigma, d, 1_ilp )
              call stdlib_sscal( n-1, sigma, e( 1_ilp ), 1_ilp )
           end if
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvalues and
           ! eigenvectors, call stdlib_sstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, d, e, info )
           else
              call stdlib_sstedc( 'I', n, d, e, z, ldz, work, lwork, iwork, liwork,info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_sscal( n, one / sigma, d, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sstevd

     pure module subroutine stdlib_dstevd( jobz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
     !! DSTEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric tridiagonal matrix. If eigenvectors are desired, it
     !! uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iscale, liwmin, lwmin
           real(dp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           liwmin = 1_ilp
           lwmin = 1_ilp
           if( n>1_ilp .and. wantz ) then
              lwmin = 1_ilp + 4_ilp*n + n**2_ilp
              liwmin = 3_ilp + 5_ilp*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           tnrm = stdlib_dlanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( n, sigma, d, 1_ilp )
              call stdlib_dscal( n-1, sigma, e( 1_ilp ), 1_ilp )
           end if
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvalues and
           ! eigenvectors, call stdlib_dstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, d, e, info )
           else
              call stdlib_dstedc( 'I', n, d, e, z, ldz, work, lwork, iwork, liwork,info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_dscal( n, one / sigma, d, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dstevd




     pure module subroutine stdlib_sstevr( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
     !! SSTEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T.  Eigenvalues and
     !! eigenvectors can be selected by specifying either a range of values
     !! or a range of indices for the desired eigenvalues.
     !! Whenever possible, SSTEVR calls SSTEMR to compute the
     !! eigenspectrum using Relatively Robust Representations.  SSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows. For the i-th
     !! unreduced block of T,
     !! (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
     !! is a relatively robust representation,
     !! (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
     !! relative accuracy by the dqds algorithm,
     !! (c) If there is a cluster of close eigenvalues, "choose" sigma_i
     !! close to the cluster, and go to step (a),
     !! (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
     !! compute the corresponding eigenvector by forming a
     !! rank-revealing twisted factorization.
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,
     !! Computer Science Division Technical Report No. UCB//CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Note 1 : SSTEVR calls SSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! SSTEVR calls SSTEBZ and SSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of SSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, lquery, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, imax, indibl, indifl, indisp, indiwo, iscale, j, jj, liwmin,&
                      lwmin, nsplit
           real(sp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, tnrm, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'SSTEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( liwork==-1_ilp ) )
           lwmin = max( 1_ilp, 20_ilp*n )
           liwmin = max(1_ilp, 10_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -7_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -8_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -9_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -14_ilp
              end if
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -17_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( vl<d( 1_ilp ) .and. vu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           tnrm = stdlib_slanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( n, sigma, d, 1_ilp )
              call stdlib_sscal( n-1, sigma, e( 1_ilp ), 1_ilp )
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: these indices are used only
           ! if stdlib_ssterf or stdlib_sstemr fail.
           ! iwork(indibl:indibl+m-1) corresponds to iblock in stdlib_sstebz and
           ! stores the block indices of each of the m<=n eigenvalues.
           indibl = 1_ilp
           ! iwork(indisp:indisp+nsplit-1) corresponds to isplit in stdlib_sstebz and
           ! stores the starting and finishing indices of each block.
           indisp = indibl + n
           ! iwork(indifl:indifl+n-1) stores the indices of eigenvectors
           ! that corresponding to eigenvectors that fail to converge in
           ! stdlib_sstein.  this information is discarded; if any fail, the driver
           ! returns info > 0.
           indifl = indisp + n
           ! indiwo is the offset of the remaining integer workspace.
           indiwo = indisp + n
           ! if all eigenvalues are desired, then
           ! call stdlib_ssterf or stdlib_sstemr.  if this fails for some eigenvalue, then
           ! try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ieeeok==1_ilp ) then
              call stdlib_scopy( n-1, e( 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_scopy( n, d, 1_ilp, w, 1_ilp )
                 call stdlib_ssterf( n, w, work, info )
              else
                 call stdlib_scopy( n, d, 1_ilp, work( n+1 ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_sstemr( jobz, 'A', n, work( n+1 ), work, vl, vu, il,iu, m, w, z, ldz,&
                            n, isuppz, tryrac,work( 2_ilp*n+1 ), lwork-2*n, iwork, liwork, info )
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 10
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstol, d, e, m,nsplit, w, &
                     iwork( indibl ), iwork( indisp ), work,iwork( indiwo ), info )
           if( wantz ) then
              call stdlib_sstein( n, d, e, m, w, iwork( indibl ), iwork( indisp ),z, ldz, work, &
                        iwork( indiwo ), iwork( indifl ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           10 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    w( i ) = w( j )
                    w( j ) = tmp1
                    call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                 end if
              end do
           end if
            ! causes problems with tests 19
            ! if (wantz .and. indeig ) z( 1,1) = z(1,1) / 1.002_sp + .002
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sstevr

     pure module subroutine stdlib_dstevr( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
     !! DSTEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T.  Eigenvalues and
     !! eigenvectors can be selected by specifying either a range of values
     !! or a range of indices for the desired eigenvalues.
     !! Whenever possible, DSTEVR calls DSTEMR to compute the
     !! eigenspectrum using Relatively Robust Representations.  DSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows. For the i-th
     !! unreduced block of T,
     !! (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
     !! is a relatively robust representation,
     !! (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
     !! relative accuracy by the dqds algorithm,
     !! (c) If there is a cluster of close eigenvalues, "choose" sigma_i
     !! close to the cluster, and go to step (a),
     !! (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
     !! compute the corresponding eigenvector by forming a
     !! rank-revealing twisted factorization.
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,
     !! Computer Science Division Technical Report No. UCB//CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Note 1 : DSTEVR calls DSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! DSTEVR calls DSTEBZ and DSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of DSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, lquery, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, imax, indibl, indifl, indisp, indiwo, iscale, itmp1, j, jj, &
                     liwmin, lwmin, nsplit
           real(dp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, tnrm, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'DSTEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( liwork==-1_ilp ) )
           lwmin = max( 1_ilp, 20_ilp*n )
           liwmin = max( 1_ilp, 10_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -7_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -8_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -9_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -14_ilp
              end if
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -17_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( vl<d( 1_ilp ) .and. vu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           tnrm = stdlib_dlanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( n, sigma, d, 1_ilp )
              call stdlib_dscal( n-1, sigma, e( 1_ilp ), 1_ilp )
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: these indices are used only
           ! if stdlib_dsterf or stdlib_dstemr fail.
           ! iwork(indibl:indibl+m-1) corresponds to iblock in stdlib_dstebz and
           ! stores the block indices of each of the m<=n eigenvalues.
           indibl = 1_ilp
           ! iwork(indisp:indisp+nsplit-1) corresponds to isplit in stdlib_dstebz and
           ! stores the starting and finishing indices of each block.
           indisp = indibl + n
           ! iwork(indifl:indifl+n-1) stores the indices of eigenvectors
           ! that corresponding to eigenvectors that fail to converge in
           ! stdlib_dstein.  this information is discarded; if any fail, the driver
           ! returns info > 0.
           indifl = indisp + n
           ! indiwo is the offset of the remaining integer workspace.
           indiwo = indisp + n
           ! if all eigenvalues are desired, then
           ! call stdlib_dsterf or stdlib_dstemr.  if this fails for some eigenvalue, then
           ! try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ieeeok==1_ilp ) then
              call stdlib_dcopy( n-1, e( 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_dcopy( n, d, 1_ilp, w, 1_ilp )
                 call stdlib_dsterf( n, w, work, info )
              else
                 call stdlib_dcopy( n, d, 1_ilp, work( n+1 ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_dstemr( jobz, 'A', n, work( n+1 ), work, vl, vu, il,iu, m, w, z, ldz,&
                            n, isuppz, tryrac,work( 2_ilp*n+1 ), lwork-2*n, iwork, liwork, info )
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 10
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_dstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstol, d, e, m,nsplit, w, &
                     iwork( indibl ), iwork( indisp ), work,iwork( indiwo ), info )
           if( wantz ) then
              call stdlib_dstein( n, d, e, m, w, iwork( indibl ), iwork( indisp ),z, ldz, work, &
                        iwork( indiwo ), iwork( indifl ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           10 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( i )
                    w( i ) = w( j )
                    iwork( i ) = iwork( j )
                    w( j ) = tmp1
                    iwork( j ) = itmp1
                    call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                 end if
              end do
           end if
            ! causes problems with tests 19
            ! if (wantz .and. indeig ) z( 1,1) = z(1,1) / 1.002_dp + .002
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dstevr




     pure module subroutine stdlib_sstevx( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
     !! SSTEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix A.  Eigenvalues and
     !! eigenvectors can be selected by specifying either a range of values
     !! or a range of indices for the desired eigenvalues.
               work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, imax, indibl, indisp, indiwo, indwrk, iscale, itmp1, j, jj, &
                     nsplit
           real(sp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, tnrm, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -7_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -8_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -9_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( vl<d( 1_ilp ) .and. vu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           if ( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           endif
           tnrm = stdlib_slanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( n, sigma, d, 1_ilp )
              call stdlib_sscal( n-1, sigma, e( 1_ilp ), 1_ilp )
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! if all eigenvalues are desired and abstol is less than zero, then
           ! call stdlib_ssterf or stdlib_ssteqr.  if this fails for some eigenvalue, then
           ! try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_scopy( n, d, 1_ilp, w, 1_ilp )
              call stdlib_scopy( n-1, e( 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              indwrk = n + 1_ilp
              if( .not.wantz ) then
                 call stdlib_ssterf( n, w, work, info )
              else
                 call stdlib_ssteqr( 'I', n, w, work, z, ldz, work( indwrk ), info )
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 20
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indwrk = 1_ilp
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstol, d, e, m,nsplit, w, &
                     iwork( indibl ), iwork( indisp ),work( indwrk ), iwork( indiwo ), info )
           if( wantz ) then
              call stdlib_sstein( n, d, e, m, w, iwork( indibl ), iwork( indisp ),z, ldz, work( &
                        indwrk ), iwork( indiwo ), ifail,info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           20 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_sstevx

     pure module subroutine stdlib_dstevx( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
     !! DSTEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix A.  Eigenvalues and
     !! eigenvectors can be selected by specifying either a range of values
     !! or a range of indices for the desired eigenvalues.
               work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, imax, indibl, indisp, indiwo, indwrk, iscale, itmp1, j, jj, &
                     nsplit
           real(dp) :: bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, tnrm, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -7_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -8_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -9_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( vl<d( 1_ilp ) .and. vu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           ! scale matrix to allowable range, if necessary.
           iscale = 0_ilp
           if( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           end if
           tnrm = stdlib_dlanst( 'M', n, d, e )
           if( tnrm>zero .and. tnrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / tnrm
           else if( tnrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / tnrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( n, sigma, d, 1_ilp )
              call stdlib_dscal( n-1, sigma, e( 1_ilp ), 1_ilp )
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! if all eigenvalues are desired and abstol is less than zero, then
           ! call stdlib_dsterf or stdlib_ssteqr.  if this fails for some eigenvalue, then
           ! try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_dcopy( n, d, 1_ilp, w, 1_ilp )
              call stdlib_dcopy( n-1, e( 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              indwrk = n + 1_ilp
              if( .not.wantz ) then
                 call stdlib_dsterf( n, w, work, info )
              else
                 call stdlib_dsteqr( 'I', n, w, work, z, ldz, work( indwrk ), info )
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 20
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indwrk = 1_ilp
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstol, d, e, m,nsplit, w, &
                     iwork( indibl ), iwork( indisp ),work( indwrk ), iwork( indiwo ), info )
           if( wantz ) then
              call stdlib_dstein( n, d, e, m, w, iwork( indibl ), iwork( indisp ),z, ldz, work( &
                        indwrk ), iwork( indiwo ), ifail,info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           20 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_dstevx




     pure module subroutine stdlib_spteqr( compz, n, d, e, z, ldz, work, info )
     !! SPTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric positive definite tridiagonal matrix by first factoring the
     !! matrix using SPTTRF, and then calling SBDSQR to compute the singular
     !! values of the bidiagonal factor.
     !! This routine computes the eigenvalues of the positive definite
     !! tridiagonal matrix to high relative accuracy.  This means that if the
     !! eigenvalues range over many orders of magnitude in size, then the
     !! small eigenvalues and corresponding eigenvectors will be computed
     !! more accurately than, for example, with the standard QR method.
     !! The eigenvectors of a full or band symmetric positive definite matrix
     !! can also be found if SSYTRD, SSPTRD, or SSBTRD has been used to
     !! reduce this matrix to tridiagonal form. (The reduction to tridiagonal
     !! form, however, may preclude the possibility of obtaining high
     !! relative accuracy in the small eigenvalues of the original matrix, if
     !! these eigenvalues range over many orders of magnitude.)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Arrays 
           real(sp) :: c(1_ilp,1_ilp), vt(1_ilp,1_ilp)
           ! Local Scalars 
           integer(ilp) :: i, icompz, nru
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz>0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           if( icompz==2_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
           ! call stdlib_spttrf to factor the matrix.
           call stdlib_spttrf( n, d, e, info )
           if( info/=0 )return
           do i = 1, n
              d( i ) = sqrt( d( i ) )
           end do
           do i = 1, n - 1
              e( i ) = e( i )*d( i )
           end do
           ! call stdlib_sbdsqr to compute the singular values/vectors of the
           ! bidiagonal factor.
           if( icompz>0_ilp ) then
              nru = n
           else
              nru = 0_ilp
           end if
           call stdlib_sbdsqr( 'LOWER', n, 0_ilp, nru, 0_ilp, d, e, vt, 1_ilp, z, ldz, c, 1_ilp,work, info )
                     
           ! square the singular values.
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = d( i )*d( i )
              end do
           else
              info = n + info
           end if
           return
     end subroutine stdlib_spteqr

     pure module subroutine stdlib_dpteqr( compz, n, d, e, z, ldz, work, info )
     !! DPTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric positive definite tridiagonal matrix by first factoring the
     !! matrix using DPTTRF, and then calling DBDSQR to compute the singular
     !! values of the bidiagonal factor.
     !! This routine computes the eigenvalues of the positive definite
     !! tridiagonal matrix to high relative accuracy.  This means that if the
     !! eigenvalues range over many orders of magnitude in size, then the
     !! small eigenvalues and corresponding eigenvectors will be computed
     !! more accurately than, for example, with the standard QR method.
     !! The eigenvectors of a full or band symmetric positive definite matrix
     !! can also be found if DSYTRD, DSPTRD, or DSBTRD has been used to
     !! reduce this matrix to tridiagonal form. (The reduction to tridiagonal
     !! form, however, may preclude the possibility of obtaining high
     !! relative accuracy in the small eigenvalues of the original matrix, if
     !! these eigenvalues range over many orders of magnitude.)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Arrays 
           real(dp) :: c(1_ilp,1_ilp), vt(1_ilp,1_ilp)
           ! Local Scalars 
           integer(ilp) :: i, icompz, nru
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz>0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           if( icompz==2_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
           ! call stdlib_dpttrf to factor the matrix.
           call stdlib_dpttrf( n, d, e, info )
           if( info/=0 )return
           do i = 1, n
              d( i ) = sqrt( d( i ) )
           end do
           do i = 1, n - 1
              e( i ) = e( i )*d( i )
           end do
           ! call stdlib_dbdsqr to compute the singular values/vectors of the
           ! bidiagonal factor.
           if( icompz>0_ilp ) then
              nru = n
           else
              nru = 0_ilp
           end if
           call stdlib_dbdsqr( 'LOWER', n, 0_ilp, nru, 0_ilp, d, e, vt, 1_ilp, z, ldz, c, 1_ilp,work, info )
                     
           ! square the singular values.
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = d( i )*d( i )
              end do
           else
              info = n + info
           end if
           return
     end subroutine stdlib_dpteqr


     pure module subroutine stdlib_cpteqr( compz, n, d, e, z, ldz, work, info )
     !! CPTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric positive definite tridiagonal matrix by first factoring the
     !! matrix using SPTTRF and then calling CBDSQR to compute the singular
     !! values of the bidiagonal factor.
     !! This routine computes the eigenvalues of the positive definite
     !! tridiagonal matrix to high relative accuracy.  This means that if the
     !! eigenvalues range over many orders of magnitude in size, then the
     !! small eigenvalues and corresponding eigenvectors will be computed
     !! more accurately than, for example, with the standard QR method.
     !! The eigenvectors of a full or band positive definite Hermitian matrix
     !! can also be found if CHETRD, CHPTRD, or CHBTRD has been used to
     !! reduce this matrix to tridiagonal form.  (The reduction to
     !! tridiagonal form, however, may preclude the possibility of obtaining
     !! high relative accuracy in the small eigenvalues of the original
     !! matrix, if these eigenvalues range over many orders of magnitude.)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
        ! ====================================================================
           
           ! Local Arrays 
           complex(sp) :: c(1_ilp,1_ilp), vt(1_ilp,1_ilp)
           ! Local Scalars 
           integer(ilp) :: i, icompz, nru
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz>0_ilp )z( 1_ilp, 1_ilp ) = cone
              return
           end if
           if( icompz==2_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, z, ldz )
           ! call stdlib_spttrf to factor the matrix.
           call stdlib_spttrf( n, d, e, info )
           if( info/=0 )return
           do i = 1, n
              d( i ) = sqrt( d( i ) )
           end do
           do i = 1, n - 1
              e( i ) = e( i )*d( i )
           end do
           ! call stdlib_cbdsqr to compute the singular values/vectors of the
           ! bidiagonal factor.
           if( icompz>0_ilp ) then
              nru = n
           else
              nru = 0_ilp
           end if
           call stdlib_cbdsqr( 'LOWER', n, 0_ilp, nru, 0_ilp, d, e, vt, 1_ilp, z, ldz, c, 1_ilp,work, info )
                     
           ! square the singular values.
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = d( i )*d( i )
              end do
           else
              info = n + info
           end if
           return
     end subroutine stdlib_cpteqr

     pure module subroutine stdlib_zpteqr( compz, n, d, e, z, ldz, work, info )
     !! ZPTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric positive definite tridiagonal matrix by first factoring the
     !! matrix using DPTTRF and then calling ZBDSQR to compute the singular
     !! values of the bidiagonal factor.
     !! This routine computes the eigenvalues of the positive definite
     !! tridiagonal matrix to high relative accuracy.  This means that if the
     !! eigenvalues range over many orders of magnitude in size, then the
     !! small eigenvalues and corresponding eigenvectors will be computed
     !! more accurately than, for example, with the standard QR method.
     !! The eigenvectors of a full or band positive definite Hermitian matrix
     !! can also be found if ZHETRD, ZHPTRD, or ZHBTRD has been used to
     !! reduce this matrix to tridiagonal form.  (The reduction to
     !! tridiagonal form, however, may preclude the possibility of obtaining
     !! high relative accuracy in the small eigenvalues of the original
     !! matrix, if these eigenvalues range over many orders of magnitude.)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
        ! ====================================================================
           
           ! Local Arrays 
           complex(dp) :: c(1_ilp,1_ilp), vt(1_ilp,1_ilp)
           ! Local Scalars 
           integer(ilp) :: i, icompz, nru
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz>0_ilp )z( 1_ilp, 1_ilp ) = cone
              return
           end if
           if( icompz==2_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, z, ldz )
           ! call stdlib_dpttrf to factor the matrix.
           call stdlib_dpttrf( n, d, e, info )
           if( info/=0 )return
           do i = 1, n
              d( i ) = sqrt( d( i ) )
           end do
           do i = 1, n - 1
              e( i ) = e( i )*d( i )
           end do
           ! call stdlib_zbdsqr to compute the singular values/vectors of the
           ! bidiagonal factor.
           if( icompz>0_ilp ) then
              nru = n
           else
              nru = 0_ilp
           end if
           call stdlib_zbdsqr( 'LOWER', n, 0_ilp, nru, 0_ilp, d, e, vt, 1_ilp, z, ldz, c, 1_ilp,work, info )
                     
           ! square the singular values.
           if( info==0_ilp ) then
              do i = 1, n
                 d( i ) = d( i )*d( i )
              end do
           else
              info = n + info
           end if
           return
     end subroutine stdlib_zpteqr




     pure module subroutine stdlib_sstebz( range, order, n, vl, vu, il, iu, abstol, d, e,m, nsplit, w, &
     !! SSTEBZ computes the eigenvalues of a symmetric tridiagonal
     !! matrix T.  The user may ask for all eigenvalues, all eigenvalues
     !! in the half-open interval (VL, VU], or the IL-th through IU-th
     !! eigenvalues.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
               iblock, isplit, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: fudge = 2.1_sp
           real(sp), parameter :: relfac = two
           
           
           ! Local Scalars 
           logical(lk) :: ncnvrg, toofew
           integer(ilp) :: ib, ibegin, idiscl, idiscu, ie, iend, iinfo, im, in, ioff, iorder, &
                     iout, irange, itmax, itmp1, iw, iwoff, j, jb, jdisc, je, nb, nwl, nwu
           real(sp) :: atoli, bnorm, gl, gu, pivmin, rtoli, safemn, tmp1, tmp2, tnorm, ulp, wkill,&
                      wl, wlu, wu, wul
           ! Local Arrays 
           integer(ilp) :: idumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = 1_ilp
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = 2_ilp
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = 3_ilp
           else
              irange = 0_ilp
           end if
           ! decode order
           if( stdlib_lsame( order, 'B' ) ) then
              iorder = 2_ilp
           else if( stdlib_lsame( order, 'E' ) ) then
              iorder = 1_ilp
           else
              iorder = 0_ilp
           end if
           ! check for errors
           if( irange<=0_ilp ) then
              info = -1_ilp
           else if( iorder<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( irange==2_ilp ) then
              if( vl>=vu ) info = -5_ilp
           else if( irange==3_ilp .and. ( il<1_ilp .or. il>max( 1_ilp, n ) ) )then
              info = -6_ilp
           else if( irange==3_ilp .and. ( iu<min( n, il ) .or. iu>n ) )then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEBZ', -info )
              return
           end if
           ! initialize error flags
           info = 0_ilp
           ncnvrg = .false.
           toofew = .false.
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! simplifications:
           if( irange==3_ilp .and. il==1_ilp .and. iu==n )irange = 1_ilp
           ! get machine constants
           ! nb is the minimum vector length for vector bisection, or 0
           ! if only scalar is to be done.
           safemn = stdlib_slamch( 'S' )
           ulp = stdlib_slamch( 'P' )
           rtoli = ulp*relfac
           nb = stdlib_ilaenv( 1_ilp, 'SSTEBZ', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp )nb = 0_ilp
           ! special case when n=1
           if( n==1_ilp ) then
              nsplit = 1_ilp
              isplit( 1_ilp ) = 1_ilp
              if( irange==2_ilp .and. ( vl>=d( 1_ilp ) .or. vu<d( 1_ilp ) ) ) then
                 m = 0_ilp
              else
                 w( 1_ilp ) = d( 1_ilp )
                 iblock( 1_ilp ) = 1_ilp
                 m = 1_ilp
              end if
              return
           end if
           ! compute splitting points
           nsplit = 1_ilp
           work( n ) = zero
           pivmin = one
           do j = 2, n
              tmp1 = e( j-1 )**2_ilp
              if( abs( d( j )*d( j-1 ) )*ulp**2_ilp+safemn>tmp1 ) then
                 isplit( nsplit ) = j - 1_ilp
                 nsplit = nsplit + 1_ilp
                 work( j-1 ) = zero
              else
                 work( j-1 ) = tmp1
                 pivmin = max( pivmin, tmp1 )
              end if
           end do
           isplit( nsplit ) = n
           pivmin = pivmin*safemn
           ! compute interval and atoli
           if( irange==3_ilp ) then
              ! range='i': compute the interval containing eigenvalues
                         ! il through iu.
              ! compute gershgorin interval for entire (split) matrix
              ! and use it as the initial interval
              gu = d( 1_ilp )
              gl = d( 1_ilp )
              tmp1 = zero
              do j = 1, n - 1
                 tmp2 = sqrt( work( j ) )
                 gu = max( gu, d( j )+tmp1+tmp2 )
                 gl = min( gl, d( j )-tmp1-tmp2 )
                 tmp1 = tmp2
              end do
              gu = max( gu, d( n )+tmp1 )
              gl = min( gl, d( n )-tmp1 )
              tnorm = max( abs( gl ), abs( gu ) )
              gl = gl - fudge*tnorm*ulp*n - fudge*two*pivmin
              gu = gu + fudge*tnorm*ulp*n + fudge*pivmin
              ! compute iteration parameters
              itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
              if( abstol<=zero ) then
                 atoli = ulp*tnorm
              else
                 atoli = abstol
              end if
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
              call stdlib_slaebz( 3_ilp, itmax, n, 2_ilp, 2_ilp, nb, atoli, rtoli, pivmin, d, e,work, iwork( &
                        5_ilp ), work( n+1 ), work( n+5 ), iout,iwork, w, iblock, iinfo )
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
              if( nwl<0_ilp .or. nwl>=n .or. nwu<1_ilp .or. nwu>n ) then
                 info = 4_ilp
                 return
              end if
           else
              ! range='a' or 'v' -- set atoli
              tnorm = max( abs( d( 1_ilp ) )+abs( e( 1_ilp ) ),abs( d( n ) )+abs( e( n-1 ) ) )
              do j = 2, n - 1
                 tnorm = max( tnorm, abs( d( j ) )+abs( e( j-1 ) )+abs( e( j ) ) )
              end do
              if( abstol<=zero ) then
                 atoli = ulp*tnorm
              else
                 atoli = abstol
              end if
              if( irange==2_ilp ) then
                 wl = vl
                 wu = vu
              else
                 wl = zero
                 wu = zero
              end if
           end if
           ! find eigenvalues -- loop over blocks and recompute nwl and nwu.
           ! nwl accumulates the number of eigenvalues .le. wl,
           ! nwu accumulates the number of eigenvalues .le. wu
           m = 0_ilp
           iend = 0_ilp
           info = 0_ilp
           nwl = 0_ilp
           nwu = 0_ilp
           loop_70: do jb = 1, nsplit
              ioff = iend
              ibegin = ioff + 1_ilp
              iend = isplit( jb )
              in = iend - ioff
              if( in==1_ilp ) then
                 ! special case -- in=1
                 if( irange==1_ilp .or. wl>=d( ibegin )-pivmin )nwl = nwl + 1_ilp
                 if( irange==1_ilp .or. wu>=d( ibegin )-pivmin )nwu = nwu + 1_ilp
                 if( irange==1_ilp .or. ( wl<d( ibegin )-pivmin .and. wu>=d( ibegin )-pivmin ) ) &
                           then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    iblock( m ) = jb
                 end if
              else
                 ! general case -- in > 1
                 ! compute gershgorin interval
                 ! and use it as the initial interval
                 gu = d( ibegin )
                 gl = d( ibegin )
                 tmp1 = zero
                 do j = ibegin, iend - 1
                    tmp2 = abs( e( j ) )
                    gu = max( gu, d( j )+tmp1+tmp2 )
                    gl = min( gl, d( j )-tmp1-tmp2 )
                    tmp1 = tmp2
                 end do
                 gu = max( gu, d( iend )+tmp1 )
                 gl = min( gl, d( iend )-tmp1 )
                 bnorm = max( abs( gl ), abs( gu ) )
                 gl = gl - fudge*bnorm*ulp*in - fudge*pivmin
                 gu = gu + fudge*bnorm*ulp*in + fudge*pivmin
                 ! compute atoli for the current submatrix
                 if( abstol<=zero ) then
                    atoli = ulp*max( abs( gl ), abs( gu ) )
                 else
                    atoli = abstol
                 end if
                 if( irange>1_ilp ) then
                    if( gu<wl ) then
                       nwl = nwl + in
                       nwu = nwu + in
                       cycle loop_70
                    end if
                    gl = max( gl, wl )
                    gu = min( gu, wu )
                    if( gl>=gu )cycle loop_70
                 end if
                 ! set up initial interval
                 work( n+1 ) = gl
                 work( n+in+1 ) = gu
                 call stdlib_slaebz( 1_ilp, 0_ilp, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e( &
                 ibegin ), work( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), im,iwork, w( m+1 &
                           ), iblock( m+1 ), iinfo )
                 nwl = nwl + iwork( 1_ilp )
                 nwu = nwu + iwork( in+1 )
                 iwoff = m - iwork( 1_ilp )
                 ! compute eigenvalues
                 itmax = int( ( log( gu-gl+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + &
                           2_ilp
                 call stdlib_slaebz( 2_ilp, itmax, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e(&
                  ibegin ), work( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), iout,iwork, w( &
                            m+1 ), iblock( m+1 ), iinfo )
                 ! copy eigenvalues into w and iblock
                 ! use -jb for block number for unconverged eigenvalues.
                 do j = 1, iout
                    tmp1 = half*( work( j+n )+work( j+in+n ) )
                    ! flag non-convergence.
                    if( j>iout-iinfo ) then
                       ncnvrg = .true.
                       ib = -jb
                    else
                       ib = jb
                    end if
                    do je = iwork( j ) + 1 + iwoff,iwork( j+in ) + iwoff
                       w( je ) = tmp1
                       iblock( je ) = ib
                    end do
                 end do
                 m = m + im
              end if
           end do loop_70
           ! if range='i', then (wl,wu) contains eigenvalues nwl+1,...,nwu
           ! if nwl+1 < il or nwu > iu, discard extra eigenvalues.
           if( irange==3_ilp ) then
              im = 0_ilp
              idiscl = il - 1_ilp - nwl
              idiscu = nwu - iu
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 do je = 1, m
                    if( w( je )<=wlu .and. idiscl>0_ilp ) then
                       idiscl = idiscl - 1_ilp
                    else if( w( je )>=wul .and. idiscu>0_ilp ) then
                       idiscu = idiscu - 1_ilp
                    else
                       im = im + 1_ilp
                       w( im ) = w( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 ! code to deal with effects of bad arithmetic:
                 ! some low eigenvalues to be discarded are not in (wl,wlu],
                 ! or high eigenvalues to be discarded are not in (wul,wu]
                 ! so just kill off the smallest idiscl/largest idiscu
                 ! eigenvalues, by simply finding the smallest/largest
                 ! eigenvalue(s).
                 ! (if n(w) is monotone non-decreasing, this should never
                     ! happen.)
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
                          if( iblock( je )/=0_ilp .and.( w( je )>wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 im = 0_ilp
                 do je = 1, m
                    if( iblock( je )/=0_ilp ) then
                       im = im + 1_ilp
                       w( im ) = w( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl<0_ilp .or. idiscu<0_ilp ) then
                 toofew = .true.
              end if
           end if
           ! if order='b', do nothing -- the eigenvalues are already sorted
              ! by block.
           ! if order='e', sort the eigenvalues from smallest to largest
           if( iorder==1_ilp .and. nsplit>1_ilp ) then
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
                    itmp1 = iblock( ie )
                    w( ie ) = w( je )
                    iblock( ie ) = iblock( je )
                    w( je ) = tmp1
                    iblock( je ) = itmp1
                 end if
              end do
           end if
           info = 0_ilp
           if( ncnvrg )info = info + 1_ilp
           if( toofew )info = info + 2_ilp
           return
     end subroutine stdlib_sstebz

     pure module subroutine stdlib_dstebz( range, order, n, vl, vu, il, iu, abstol, d, e,m, nsplit, w, &
     !! DSTEBZ computes the eigenvalues of a symmetric tridiagonal
     !! matrix T.  The user may ask for all eigenvalues, all eigenvalues
     !! in the half-open interval (VL, VU], or the IL-th through IU-th
     !! eigenvalues.
     !! To avoid overflow, the matrix must be scaled so that its
     !! largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
     !! accuracy, it should not be much smaller than that.
     !! See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
     !! Matrix", Report CS41, Computer Science Dept., Stanford
     !! University, July 21, 1966.
               iblock, isplit, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: fudge = 2.1_dp
           real(dp), parameter :: relfac = two
           
           
           ! Local Scalars 
           logical(lk) :: ncnvrg, toofew
           integer(ilp) :: ib, ibegin, idiscl, idiscu, ie, iend, iinfo, im, in, ioff, iorder, &
                     iout, irange, itmax, itmp1, iw, iwoff, j, jb, jdisc, je, nb, nwl, nwu
           real(dp) :: atoli, bnorm, gl, gu, pivmin, rtoli, safemn, tmp1, tmp2, tnorm, ulp, wkill,&
                      wl, wlu, wu, wul
           ! Local Arrays 
           integer(ilp) :: idumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! decode range
           if( stdlib_lsame( range, 'A' ) ) then
              irange = 1_ilp
           else if( stdlib_lsame( range, 'V' ) ) then
              irange = 2_ilp
           else if( stdlib_lsame( range, 'I' ) ) then
              irange = 3_ilp
           else
              irange = 0_ilp
           end if
           ! decode order
           if( stdlib_lsame( order, 'B' ) ) then
              iorder = 2_ilp
           else if( stdlib_lsame( order, 'E' ) ) then
              iorder = 1_ilp
           else
              iorder = 0_ilp
           end if
           ! check for errors
           if( irange<=0_ilp ) then
              info = -1_ilp
           else if( iorder<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( irange==2_ilp ) then
              if( vl>=vu )info = -5_ilp
           else if( irange==3_ilp .and. ( il<1_ilp .or. il>max( 1_ilp, n ) ) )then
              info = -6_ilp
           else if( irange==3_ilp .and. ( iu<min( n, il ) .or. iu>n ) )then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEBZ', -info )
              return
           end if
           ! initialize error flags
           info = 0_ilp
           ncnvrg = .false.
           toofew = .false.
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! simplifications:
           if( irange==3_ilp .and. il==1_ilp .and. iu==n )irange = 1_ilp
           ! get machine constants
           ! nb is the minimum vector length for vector bisection, or 0
           ! if only scalar is to be done.
           safemn = stdlib_dlamch( 'S' )
           ulp = stdlib_dlamch( 'P' )
           rtoli = ulp*relfac
           nb = stdlib_ilaenv( 1_ilp, 'DSTEBZ', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp )nb = 0_ilp
           ! special case when n=1
           if( n==1_ilp ) then
              nsplit = 1_ilp
              isplit( 1_ilp ) = 1_ilp
              if( irange==2_ilp .and. ( vl>=d( 1_ilp ) .or. vu<d( 1_ilp ) ) ) then
                 m = 0_ilp
              else
                 w( 1_ilp ) = d( 1_ilp )
                 iblock( 1_ilp ) = 1_ilp
                 m = 1_ilp
              end if
              return
           end if
           ! compute splitting points
           nsplit = 1_ilp
           work( n ) = zero
           pivmin = one
           do j = 2, n
              tmp1 = e( j-1 )**2_ilp
              if( abs( d( j )*d( j-1 ) )*ulp**2_ilp+safemn>tmp1 ) then
                 isplit( nsplit ) = j - 1_ilp
                 nsplit = nsplit + 1_ilp
                 work( j-1 ) = zero
              else
                 work( j-1 ) = tmp1
                 pivmin = max( pivmin, tmp1 )
              end if
           end do
           isplit( nsplit ) = n
           pivmin = pivmin*safemn
           ! compute interval and atoli
           if( irange==3_ilp ) then
              ! range='i': compute the interval containing eigenvalues
                         ! il through iu.
              ! compute gershgorin interval for entire (split) matrix
              ! and use it as the initial interval
              gu = d( 1_ilp )
              gl = d( 1_ilp )
              tmp1 = zero
              do j = 1, n - 1
                 tmp2 = sqrt( work( j ) )
                 gu = max( gu, d( j )+tmp1+tmp2 )
                 gl = min( gl, d( j )-tmp1-tmp2 )
                 tmp1 = tmp2
              end do
              gu = max( gu, d( n )+tmp1 )
              gl = min( gl, d( n )-tmp1 )
              tnorm = max( abs( gl ), abs( gu ) )
              gl = gl - fudge*tnorm*ulp*n - fudge*two*pivmin
              gu = gu + fudge*tnorm*ulp*n + fudge*pivmin
              ! compute iteration parameters
              itmax = int( ( log( tnorm+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + 2_ilp
              if( abstol<=zero ) then
                 atoli = ulp*tnorm
              else
                 atoli = abstol
              end if
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
              call stdlib_dlaebz( 3_ilp, itmax, n, 2_ilp, 2_ilp, nb, atoli, rtoli, pivmin, d, e,work, iwork( &
                        5_ilp ), work( n+1 ), work( n+5 ), iout,iwork, w, iblock, iinfo )
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
              if( nwl<0_ilp .or. nwl>=n .or. nwu<1_ilp .or. nwu>n ) then
                 info = 4_ilp
                 return
              end if
           else
              ! range='a' or 'v' -- set atoli
              tnorm = max( abs( d( 1_ilp ) )+abs( e( 1_ilp ) ),abs( d( n ) )+abs( e( n-1 ) ) )
              do j = 2, n - 1
                 tnorm = max( tnorm, abs( d( j ) )+abs( e( j-1 ) )+abs( e( j ) ) )
              end do
              if( abstol<=zero ) then
                 atoli = ulp*tnorm
              else
                 atoli = abstol
              end if
              if( irange==2_ilp ) then
                 wl = vl
                 wu = vu
              else
                 wl = zero
                 wu = zero
              end if
           end if
           ! find eigenvalues -- loop over blocks and recompute nwl and nwu.
           ! nwl accumulates the number of eigenvalues .le. wl,
           ! nwu accumulates the number of eigenvalues .le. wu
           m = 0_ilp
           iend = 0_ilp
           info = 0_ilp
           nwl = 0_ilp
           nwu = 0_ilp
           loop_70: do jb = 1, nsplit
              ioff = iend
              ibegin = ioff + 1_ilp
              iend = isplit( jb )
              in = iend - ioff
              if( in==1_ilp ) then
                 ! special case -- in=1
                 if( irange==1_ilp .or. wl>=d( ibegin )-pivmin )nwl = nwl + 1_ilp
                 if( irange==1_ilp .or. wu>=d( ibegin )-pivmin )nwu = nwu + 1_ilp
                 if( irange==1_ilp .or. ( wl<d( ibegin )-pivmin .and. wu>=d( ibegin )-pivmin ) ) &
                           then
                    m = m + 1_ilp
                    w( m ) = d( ibegin )
                    iblock( m ) = jb
                 end if
              else
                 ! general case -- in > 1
                 ! compute gershgorin interval
                 ! and use it as the initial interval
                 gu = d( ibegin )
                 gl = d( ibegin )
                 tmp1 = zero
                 do j = ibegin, iend - 1
                    tmp2 = abs( e( j ) )
                    gu = max( gu, d( j )+tmp1+tmp2 )
                    gl = min( gl, d( j )-tmp1-tmp2 )
                    tmp1 = tmp2
                 end do
                 gu = max( gu, d( iend )+tmp1 )
                 gl = min( gl, d( iend )-tmp1 )
                 bnorm = max( abs( gl ), abs( gu ) )
                 gl = gl - fudge*bnorm*ulp*in - fudge*pivmin
                 gu = gu + fudge*bnorm*ulp*in + fudge*pivmin
                 ! compute atoli for the current submatrix
                 if( abstol<=zero ) then
                    atoli = ulp*max( abs( gl ), abs( gu ) )
                 else
                    atoli = abstol
                 end if
                 if( irange>1_ilp ) then
                    if( gu<wl ) then
                       nwl = nwl + in
                       nwu = nwu + in
                       cycle loop_70
                    end if
                    gl = max( gl, wl )
                    gu = min( gu, wu )
                    if( gl>=gu )cycle loop_70
                 end if
                 ! set up initial interval
                 work( n+1 ) = gl
                 work( n+in+1 ) = gu
                 call stdlib_dlaebz( 1_ilp, 0_ilp, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e( &
                 ibegin ), work( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), im,iwork, w( m+1 &
                           ), iblock( m+1 ), iinfo )
                 nwl = nwl + iwork( 1_ilp )
                 nwu = nwu + iwork( in+1 )
                 iwoff = m - iwork( 1_ilp )
                 ! compute eigenvalues
                 itmax = int( ( log( gu-gl+pivmin )-log( pivmin ) ) /log( two ),KIND=ilp) + &
                           2_ilp
                 call stdlib_dlaebz( 2_ilp, itmax, in, in, 1_ilp, nb, atoli, rtoli, pivmin,d( ibegin ), e(&
                  ibegin ), work( ibegin ),idumma, work( n+1 ), work( n+2*in+1 ), iout,iwork, w( &
                            m+1 ), iblock( m+1 ), iinfo )
                 ! copy eigenvalues into w and iblock
                 ! use -jb for block number for unconverged eigenvalues.
                 do j = 1, iout
                    tmp1 = half*( work( j+n )+work( j+in+n ) )
                    ! flag non-convergence.
                    if( j>iout-iinfo ) then
                       ncnvrg = .true.
                       ib = -jb
                    else
                       ib = jb
                    end if
                    do je = iwork( j ) + 1 + iwoff,iwork( j+in ) + iwoff
                       w( je ) = tmp1
                       iblock( je ) = ib
                    end do
                 end do
                 m = m + im
              end if
           end do loop_70
           ! if range='i', then (wl,wu) contains eigenvalues nwl+1,...,nwu
           ! if nwl+1 < il or nwu > iu, discard extra eigenvalues.
           if( irange==3_ilp ) then
              im = 0_ilp
              idiscl = il - 1_ilp - nwl
              idiscu = nwu - iu
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 do je = 1, m
                    if( w( je )<=wlu .and. idiscl>0_ilp ) then
                       idiscl = idiscl - 1_ilp
                    else if( w( je )>=wul .and. idiscu>0_ilp ) then
                       idiscu = idiscu - 1_ilp
                    else
                       im = im + 1_ilp
                       w( im ) = w( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl>0_ilp .or. idiscu>0_ilp ) then
                 ! code to deal with effects of bad arithmetic:
                 ! some low eigenvalues to be discarded are not in (wl,wlu],
                 ! or high eigenvalues to be discarded are not in (wul,wu]
                 ! so just kill off the smallest idiscl/largest idiscu
                 ! eigenvalues, by simply finding the smallest/largest
                 ! eigenvalue(s).
                 ! (if n(w) is monotone non-decreasing, this should never
                     ! happen.)
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
                          if( iblock( je )/=0_ilp .and.( w( je )>wkill .or. iw==0_ilp ) ) then
                             iw = je
                             wkill = w( je )
                          end if
                       end do
                       iblock( iw ) = 0_ilp
                    end do
                 end if
                 im = 0_ilp
                 do je = 1, m
                    if( iblock( je )/=0_ilp ) then
                       im = im + 1_ilp
                       w( im ) = w( je )
                       iblock( im ) = iblock( je )
                    end if
                 end do
                 m = im
              end if
              if( idiscl<0_ilp .or. idiscu<0_ilp ) then
                 toofew = .true.
              end if
           end if
           ! if order='b', do nothing -- the eigenvalues are already sorted
              ! by block.
           ! if order='e', sort the eigenvalues from smallest to largest
           if( iorder==1_ilp .and. nsplit>1_ilp ) then
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
                    itmp1 = iblock( ie )
                    w( ie ) = w( je )
                    iblock( ie ) = iblock( je )
                    w( je ) = tmp1
                    iblock( je ) = itmp1
                 end if
              end do
           end if
           info = 0_ilp
           if( ncnvrg )info = info + 1_ilp
           if( toofew )info = info + 2_ilp
           return
     end subroutine stdlib_dstebz




     pure module subroutine stdlib_ssterf( n, d, e, info )
     !! SSTERF computes all eigenvalues of a symmetric tridiagonal matrix
     !! using the Pal-Walker-Kahan variant of the QL or QR algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, iscale, jtot, l, l1, lend, lendsv, lsv, m, nmaxit
           real(sp) :: alpha, anorm, bb, c, eps, eps2, gamma, oldc, oldgam, p, r, rt1, rt2, rte, &
                     s, safmax, safmin, sigma, ssfmax, ssfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! quick return if possible
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SSTERF', -info )
              return
           end if
           if( n<=1 )return
           ! determine the unit roundoff for this environment.
           eps = stdlib_slamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           ! compute the eigenvalues of the tridiagonal matrix.
           nmaxit = n*maxit
           sigma = zero
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           10 continue
           if( l1>n )go to 170
           if( l1>1_ilp )e( l1-1 ) = zero
           do m = l1, n - 1
              if( abs( e( m ) )<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) &
                        then
                 e( m ) = zero
                 go to 30
              end if
           end do
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_slanst( 'M', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( anorm>ssfmax ) then
              iscale = 1_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           do i = l, lend - 1
              e( i ) = e( i )**2_ilp
           end do
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>=l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              50 continue
              if( l/=lend ) then
                 do m = l, lend - 1
                    if( abs( e( m ) )<=eps2*abs( d( m )*d( m+1 ) ) )go to 70
                 end do
              end if
              m = lend
              70 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 90
              ! if remaining matrix is 2 by 2, use stdlib_slae2 to compute its
              ! eigenvalues.
              if( m==l+1 ) then
                 rte = sqrt( e( l ) )
                 call stdlib_slae2( d( l ), rte, d( l+1 ), rt1, rt2 )
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 50
                 go to 150
              end if
              if( jtot==nmaxit )go to 150
              jtot = jtot + 1_ilp
              ! form shift.
              rte = sqrt( e( l ) )
              sigma = ( d( l+1 )-p ) / ( two*rte )
              r = stdlib_slapy2( sigma, one )
              sigma = p - ( rte / ( sigma+sign( r, sigma ) ) )
              c = one
              s = zero
              gamma = d( m ) - sigma
              p = gamma*gamma
              ! inner loop
              do i = m - 1, l, -1
                 bb = e( i )
                 r = p + bb
                 if( i/=m-1 )e( i+1 ) = s*r
                 oldc = c
                 c = p / r
                 s = bb / r
                 oldgam = gamma
                 alpha = d( i )
                 gamma = c*( alpha-sigma ) - s*oldgam
                 d( i+1 ) = oldgam + ( alpha-gamma )
                 if( c/=zero ) then
                    p = ( gamma*gamma ) / c
                 else
                    p = oldc*bb
                 end if
              end do
              e( l ) = s*p
              d( l ) = sigma + gamma
              go to 50
              ! eigenvalue found.
              90 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 50
              go to 150
           else
              ! qr iteration
              ! look for small superdiagonal element.
              100 continue
              do m = l, lend + 1, -1
                 if( abs( e( m-1 ) )<=eps2*abs( d( m )*d( m-1 ) ) )go to 120
              end do
              m = lend
              120 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 140
              ! if remaining matrix is 2 by 2, use stdlib_slae2 to compute its
              ! eigenvalues.
              if( m==l-1 ) then
                 rte = sqrt( e( l-1 ) )
                 call stdlib_slae2( d( l ), rte, d( l-1 ), rt1, rt2 )
                 d( l ) = rt1
                 d( l-1 ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 100
                 go to 150
              end if
              if( jtot==nmaxit )go to 150
              jtot = jtot + 1_ilp
              ! form shift.
              rte = sqrt( e( l-1 ) )
              sigma = ( d( l-1 )-p ) / ( two*rte )
              r = stdlib_slapy2( sigma, one )
              sigma = p - ( rte / ( sigma+sign( r, sigma ) ) )
              c = one
              s = zero
              gamma = d( m ) - sigma
              p = gamma*gamma
              ! inner loop
              do i = m, l - 1
                 bb = e( i )
                 r = p + bb
                 if( i/=m )e( i-1 ) = s*r
                 oldc = c
                 c = p / r
                 s = bb / r
                 oldgam = gamma
                 alpha = d( i+1 )
                 gamma = c*( alpha-sigma ) - s*oldgam
                 d( i ) = oldgam + ( alpha-gamma )
                 if( c/=zero ) then
                    p = ( gamma*gamma ) / c
                 else
                    p = oldc*bb
                 end if
              end do
              e( l-1 ) = s*p
              d( l ) = sigma + gamma
              go to 100
              ! eigenvalue found.
              140 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 100
              go to 150
           end if
           ! undo scaling if necessary
           150 continue
           if( iscale==1_ilp )call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), &
                     n, info )
           if( iscale==2_ilp )call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), &
                     n, info )
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot<nmaxit )go to 10
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           go to 180
           ! sort eigenvalues in increasing order.
           170 continue
           call stdlib_slasrt( 'I', n, d, info )
           180 continue
           return
     end subroutine stdlib_ssterf

     pure module subroutine stdlib_dsterf( n, d, e, info )
     !! DSTERF computes all eigenvalues of a symmetric tridiagonal matrix
     !! using the Pal-Walker-Kahan variant of the QL or QR algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, iscale, jtot, l, l1, lend, lendsv, lsv, m, nmaxit
           real(dp) :: alpha, anorm, bb, c, eps, eps2, gamma, oldc, oldgam, p, r, rt1, rt2, rte, &
                     s, safmax, safmin, sigma, ssfmax, ssfmin, rmax
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           ! quick return if possible
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DSTERF', -info )
              return
           end if
           if( n<=1 )return
           ! determine the unit roundoff for this environment.
           eps = stdlib_dlamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           rmax = stdlib_dlamch( 'O' )
           ! compute the eigenvalues of the tridiagonal matrix.
           nmaxit = n*maxit
           sigma = zero
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           10 continue
           if( l1>n )go to 170
           if( l1>1_ilp )e( l1-1 ) = zero
           do m = l1, n - 1
              if( abs( e( m ) )<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) &
                        then
                 e( m ) = zero
                 go to 30
              end if
           end do
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_dlanst( 'M', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( (anorm>ssfmax) ) then
              iscale = 1_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           do i = l, lend - 1
              e( i ) = e( i )**2_ilp
           end do
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>=l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              50 continue
              if( l/=lend ) then
                 do m = l, lend - 1
                    if( abs( e( m ) )<=eps2*abs( d( m )*d( m+1 ) ) )go to 70
                 end do
              end if
              m = lend
              70 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 90
              ! if remaining matrix is 2 by 2, use stdlib_dlae2 to compute its
              ! eigenvalues.
              if( m==l+1 ) then
                 rte = sqrt( e( l ) )
                 call stdlib_dlae2( d( l ), rte, d( l+1 ), rt1, rt2 )
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 50
                 go to 150
              end if
              if( jtot==nmaxit )go to 150
              jtot = jtot + 1_ilp
              ! form shift.
              rte = sqrt( e( l ) )
              sigma = ( d( l+1 )-p ) / ( two*rte )
              r = stdlib_dlapy2( sigma, one )
              sigma = p - ( rte / ( sigma+sign( r, sigma ) ) )
              c = one
              s = zero
              gamma = d( m ) - sigma
              p = gamma*gamma
              ! inner loop
              do i = m - 1, l, -1
                 bb = e( i )
                 r = p + bb
                 if( i/=m-1 )e( i+1 ) = s*r
                 oldc = c
                 c = p / r
                 s = bb / r
                 oldgam = gamma
                 alpha = d( i )
                 gamma = c*( alpha-sigma ) - s*oldgam
                 d( i+1 ) = oldgam + ( alpha-gamma )
                 if( c/=zero ) then
                    p = ( gamma*gamma ) / c
                 else
                    p = oldc*bb
                 end if
              end do
              e( l ) = s*p
              d( l ) = sigma + gamma
              go to 50
              ! eigenvalue found.
              90 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 50
              go to 150
           else
              ! qr iteration
              ! look for small superdiagonal element.
              100 continue
              do m = l, lend + 1, -1
                 if( abs( e( m-1 ) )<=eps2*abs( d( m )*d( m-1 ) ) )go to 120
              end do
              m = lend
              120 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 140
              ! if remaining matrix is 2 by 2, use stdlib_dlae2 to compute its
              ! eigenvalues.
              if( m==l-1 ) then
                 rte = sqrt( e( l-1 ) )
                 call stdlib_dlae2( d( l ), rte, d( l-1 ), rt1, rt2 )
                 d( l ) = rt1
                 d( l-1 ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 100
                 go to 150
              end if
              if( jtot==nmaxit )go to 150
              jtot = jtot + 1_ilp
              ! form shift.
              rte = sqrt( e( l-1 ) )
              sigma = ( d( l-1 )-p ) / ( two*rte )
              r = stdlib_dlapy2( sigma, one )
              sigma = p - ( rte / ( sigma+sign( r, sigma ) ) )
              c = one
              s = zero
              gamma = d( m ) - sigma
              p = gamma*gamma
              ! inner loop
              do i = m, l - 1
                 bb = e( i )
                 r = p + bb
                 if( i/=m )e( i-1 ) = s*r
                 oldc = c
                 c = p / r
                 s = bb / r
                 oldgam = gamma
                 alpha = d( i+1 )
                 gamma = c*( alpha-sigma ) - s*oldgam
                 d( i ) = oldgam + ( alpha-gamma )
                 if( c/=zero ) then
                    p = ( gamma*gamma ) / c
                 else
                    p = oldc*bb
                 end if
              end do
              e( l-1 ) = s*p
              d( l ) = sigma + gamma
              go to 100
              ! eigenvalue found.
              140 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 100
              go to 150
           end if
           ! undo scaling if necessary
           150 continue
           if( iscale==1_ilp )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), &
                     n, info )
           if( iscale==2_ilp )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), &
                     n, info )
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot<nmaxit )go to 10
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           go to 180
           ! sort eigenvalues in increasing order.
           170 continue
           call stdlib_dlasrt( 'I', n, d, info )
           180 continue
           return
     end subroutine stdlib_dsterf




     pure module subroutine stdlib_sstedc( compz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
     !! SSTEDC computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
     !! The eigenvectors of a full or band real symmetric matrix can also be
     !! found if SSYTRD or SSPTRD or SSBTRD has been used to reduce this
     !! matrix to tridiagonal form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See SLAED3 for details.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: finish, i, icompz, ii, j, k, lgn, liwmin, lwmin, m, smlsiz, start, &
                     storez, strtrw
           real(sp) :: eps, orgnrm, p, tiny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or.( icompz>0_ilp .and. ldz<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              smlsiz = stdlib_ilaenv( 9_ilp, 'SSTEDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
              if( n<=1_ilp .or. icompz==0_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else if( n<=smlsiz ) then
                 liwmin = 1_ilp
                 lwmin = 2_ilp*( n - 1_ilp )
              else
                 lgn = int( log( real( n,KIND=sp) )/log( two ),KIND=ilp)
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( icompz==1_ilp ) then
                    lwmin = 1_ilp + 3_ilp*n + 2_ilp*n*lgn + 4_ilp*n**2_ilp
                    liwmin = 6_ilp + 6_ilp*n + 5_ilp*n*lgn
                 else if( icompz==2_ilp ) then
                    lwmin = 1_ilp + 4_ilp*n + n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 end if
              end if
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not. lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not. lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEDC', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz/=0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! if the following conditional clause is removed, then the routine
           ! will use the divide and conquer routine to compute only the
           ! eigenvalues, which requires (3n + 3n**2) real workspace and
           ! (2 + 5n + 2n lg(n)) integer workspace.
           ! since on many architectures stdlib_ssterf is much faster than any other
           ! algorithm for finding eigenvalues only, it is used here
           ! as the default. if the conditional clause is removed, then
           ! information on the size of workspace needs to be changed.
           ! if compz = 'n', use stdlib_ssterf to compute the eigenvalues.
           if( icompz==0_ilp ) then
              call stdlib_ssterf( n, d, e, info )
              go to 50
           end if
           ! if n is smaller than the minimum divide size (smlsiz+1), then
           ! solve the problem with another solver.
           if( n<=smlsiz ) then
              call stdlib_ssteqr( compz, n, d, e, z, ldz, work, info )
           else
              ! if compz = 'v', the z matrix must be stored elsewhere for later
              ! use.
              if( icompz==1_ilp ) then
                 storez = 1_ilp + n*n
              else
                 storez = 1_ilp
              end if
              if( icompz==2_ilp ) then
                 call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
              end if
              ! scale.
              orgnrm = stdlib_slanst( 'M', n, d, e )
              if( orgnrm==zero )go to 50
              eps = stdlib_slamch( 'EPSILON' )
              start = 1_ilp
              ! while ( start <= n )
              10 continue
              if( start<=n ) then
                 ! let finish be the position of the next subdiagonal entry
                 ! such that e( finish ) <= tiny or finish = n if no such
                 ! subdiagonal exists.  the matrix identified by the elements
                 ! between start and finish constitutes an independent
                 ! sub-problem.
                 finish = start
                 20 continue
                 if( finish<n ) then
                    tiny = eps*sqrt( abs( d( finish ) ) )*sqrt( abs( d( finish+1 ) ) )
                    if( abs( e( finish ) )>tiny ) then
                       finish = finish + 1_ilp
                       go to 20
                    end if
                 end if
                 ! (sub) problem determined.  compute its size and solve it.
                 m = finish - start + 1_ilp
                 if( m==1_ilp ) then
                    start = finish + 1_ilp
                    go to 10
                 end if
                 if( m>smlsiz ) then
                    ! scale.
                    orgnrm = stdlib_slanst( 'M', m, d( start ), e( start ) )
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m, 1_ilp, d( start ), m,info )
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m-1, 1_ilp, e( start ),m-1, info )
                              
                    if( icompz==1_ilp ) then
                       strtrw = 1_ilp
                    else
                       strtrw = start
                    end if
                    call stdlib_slaed0( icompz, n, m, d( start ), e( start ),z( strtrw, start ), &
                              ldz, work( 1_ilp ), n,work( storez ), iwork, info )
                    if( info/=0_ilp ) then
                       info = ( info / ( m+1 )+start-1 )*( n+1 ) +mod( info, ( m+1 ) ) + start - &
                                 1_ilp
                       go to 50
                    end if
                    ! scale back.
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, m, 1_ilp, d( start ), m,info )
                 else
                    if( icompz==1_ilp ) then
                       ! since qr won't update a z matrix which is larger than
                       ! the length of d, we must solve the sub-problem in a
                       ! workspace and then multiply back into z.
                       call stdlib_ssteqr( 'I', m, d( start ), e( start ), work, m,work( m*m+1 ), &
                                 info )
                       call stdlib_slacpy( 'A', n, m, z( 1_ilp, start ), ldz,work( storez ), n )
                                 
                       call stdlib_sgemm( 'N', 'N', n, m, m, one,work( storez ), n, work, m, zero,&
                                 z( 1_ilp, start ), ldz )
                    else if( icompz==2_ilp ) then
                       call stdlib_ssteqr( 'I', m, d( start ), e( start ),z( start, start ), ldz, &
                                 work, info )
                    else
                       call stdlib_ssterf( m, d( start ), e( start ), info )
                    end if
                    if( info/=0_ilp ) then
                       info = start*( n+1 ) + finish
                       go to 50
                    end if
                 end if
                 start = finish + 1_ilp
                 go to 10
              end if
              ! endwhile
              if( icompz==0_ilp ) then
                ! use quick sort
                call stdlib_slasrt( 'I', n, d, info )
              else
                ! use selection sort to minimize swaps of eigenvectors
                do ii = 2, n
                   i = ii - 1_ilp
                   k = i
                   p = d( i )
                   do j = ii, n
                      if( d( j )<p ) then
                         k = j
                         p = d( j )
                      end if
                   end do
                   if( k/=i ) then
                      d( k ) = d( i )
                      d( i ) = p
                      call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                   end if
                end do
              end if
           end if
           50 continue
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sstedc

     pure module subroutine stdlib_dstedc( compz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
     !! DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
     !! The eigenvectors of a full or band real symmetric matrix can also be
     !! found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
     !! matrix to tridiagonal form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See DLAED3 for details.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: finish, i, icompz, ii, j, k, lgn, liwmin, lwmin, m, smlsiz, start, &
                     storez, strtrw
           real(dp) :: eps, orgnrm, p, tiny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or.( icompz>0_ilp .and. ldz<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              smlsiz = stdlib_ilaenv( 9_ilp, 'DSTEDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
              if( n<=1_ilp .or. icompz==0_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else if( n<=smlsiz ) then
                 liwmin = 1_ilp
                 lwmin = 2_ilp*( n - 1_ilp )
              else
                 lgn = int( log( real( n,KIND=dp) )/log( two ),KIND=ilp)
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( icompz==1_ilp ) then
                    lwmin = 1_ilp + 3_ilp*n + 2_ilp*n*lgn + 4_ilp*n**2_ilp
                    liwmin = 6_ilp + 6_ilp*n + 5_ilp*n*lgn
                 else if( icompz==2_ilp ) then
                    lwmin = 1_ilp + 4_ilp*n + n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 end if
              end if
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not. lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not. lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEDC', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz/=0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! if the following conditional clause is removed, then the routine
           ! will use the divide and conquer routine to compute only the
           ! eigenvalues, which requires (3n + 3n**2) real workspace and
           ! (2 + 5n + 2n lg(n)) integer workspace.
           ! since on many architectures stdlib_dsterf is much faster than any other
           ! algorithm for finding eigenvalues only, it is used here
           ! as the default. if the conditional clause is removed, then
           ! information on the size of workspace needs to be changed.
           ! if compz = 'n', use stdlib_dsterf to compute the eigenvalues.
           if( icompz==0_ilp ) then
              call stdlib_dsterf( n, d, e, info )
              go to 50
           end if
           ! if n is smaller than the minimum divide size (smlsiz+1), then
           ! solve the problem with another solver.
           if( n<=smlsiz ) then
              call stdlib_dsteqr( compz, n, d, e, z, ldz, work, info )
           else
              ! if compz = 'v', the z matrix must be stored elsewhere for later
              ! use.
              if( icompz==1_ilp ) then
                 storez = 1_ilp + n*n
              else
                 storez = 1_ilp
              end if
              if( icompz==2_ilp ) then
                 call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
              end if
              ! scale.
              orgnrm = stdlib_dlanst( 'M', n, d, e )
              if( orgnrm==zero )go to 50
              eps = stdlib_dlamch( 'EPSILON' )
              start = 1_ilp
              ! while ( start <= n )
              10 continue
              if( start<=n ) then
                 ! let finish be the position of the next subdiagonal entry
                 ! such that e( finish ) <= tiny or finish = n if no such
                 ! subdiagonal exists.  the matrix identified by the elements
                 ! between start and finish constitutes an independent
                 ! sub-problem.
                 finish = start
                 20 continue
                 if( finish<n ) then
                    tiny = eps*sqrt( abs( d( finish ) ) )*sqrt( abs( d( finish+1 ) ) )
                    if( abs( e( finish ) )>tiny ) then
                       finish = finish + 1_ilp
                       go to 20
                    end if
                 end if
                 ! (sub) problem determined.  compute its size and solve it.
                 m = finish - start + 1_ilp
                 if( m==1_ilp ) then
                    start = finish + 1_ilp
                    go to 10
                 end if
                 if( m>smlsiz ) then
                    ! scale.
                    orgnrm = stdlib_dlanst( 'M', m, d( start ), e( start ) )
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m, 1_ilp, d( start ), m,info )
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m-1, 1_ilp, e( start ),m-1, info )
                              
                    if( icompz==1_ilp ) then
                       strtrw = 1_ilp
                    else
                       strtrw = start
                    end if
                    call stdlib_dlaed0( icompz, n, m, d( start ), e( start ),z( strtrw, start ), &
                              ldz, work( 1_ilp ), n,work( storez ), iwork, info )
                    if( info/=0_ilp ) then
                       info = ( info / ( m+1 )+start-1 )*( n+1 ) +mod( info, ( m+1 ) ) + start - &
                                 1_ilp
                       go to 50
                    end if
                    ! scale back.
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, m, 1_ilp, d( start ), m,info )
                 else
                    if( icompz==1_ilp ) then
                       ! since qr won't update a z matrix which is larger than
                       ! the length of d, we must solve the sub-problem in a
                       ! workspace and then multiply back into z.
                       call stdlib_dsteqr( 'I', m, d( start ), e( start ), work, m,work( m*m+1 ), &
                                 info )
                       call stdlib_dlacpy( 'A', n, m, z( 1_ilp, start ), ldz,work( storez ), n )
                                 
                       call stdlib_dgemm( 'N', 'N', n, m, m, one,work( storez ), n, work, m, zero,&
                                 z( 1_ilp, start ), ldz )
                    else if( icompz==2_ilp ) then
                       call stdlib_dsteqr( 'I', m, d( start ), e( start ),z( start, start ), ldz, &
                                 work, info )
                    else
                       call stdlib_dsterf( m, d( start ), e( start ), info )
                    end if
                    if( info/=0_ilp ) then
                       info = start*( n+1 ) + finish
                       go to 50
                    end if
                 end if
                 start = finish + 1_ilp
                 go to 10
              end if
              ! endwhile
              if( icompz==0_ilp ) then
                ! use quick sort
                call stdlib_dlasrt( 'I', n, d, info )
              else
                ! use selection sort to minimize swaps of eigenvectors
                do ii = 2, n
                   i = ii - 1_ilp
                   k = i
                   p = d( i )
                   do j = ii, n
                      if( d( j )<p ) then
                         k = j
                         p = d( j )
                      end if
                   end do
                   if( k/=i ) then
                      d( k ) = d( i )
                      d( i ) = p
                      call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                   end if
                end do
              end if
           end if
           50 continue
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dstedc


     pure module subroutine stdlib_cstedc( compz, n, d, e, z, ldz, work, lwork, rwork,lrwork, iwork, &
     !! CSTEDC computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
     !! The eigenvectors of a full or band complex Hermitian matrix can also
     !! be found if CHETRD or CHPTRD or CHBTRD has been used to reduce this
     !! matrix to tridiagonal form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See SLAED3 for details.
               liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: finish, i, icompz, ii, j, k, lgn, liwmin, ll, lrwmin, lwmin, m, smlsiz,&
                      start
           real(sp) :: eps, orgnrm, p, tiny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or.( icompz>0_ilp .and. ldz<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              smlsiz = stdlib_ilaenv( 9_ilp, 'CSTEDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
              if( n<=1_ilp .or. icompz==0_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else if( n<=smlsiz ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 2_ilp*( n - 1_ilp )
              else if( icompz==1_ilp ) then
                 lgn = int( log( real( n,KIND=sp) ) / log( two ),KIND=ilp)
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 lwmin = n*n
                 lrwmin = 1_ilp + 3_ilp*n + 2_ilp*n*lgn + 4_ilp*n**2_ilp
                 liwmin = 6_ilp + 6_ilp*n + 5_ilp*n*lgn
              else if( icompz==2_ilp ) then
                 lwmin = 1_ilp
                 lrwmin = 1_ilp + 4_ilp*n + 2_ilp*n**2_ilp
                 liwmin = 3_ilp + 5_ilp*n
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -10_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSTEDC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz/=0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! if the following conditional clause is removed, then the routine
           ! will use the divide and conquer routine to compute only the
           ! eigenvalues, which requires (3n + 3n**2) real workspace and
           ! (2 + 5n + 2n lg(n)) integer workspace.
           ! since on many architectures stdlib_ssterf is much faster than any other
           ! algorithm for finding eigenvalues only, it is used here
           ! as the default. if the conditional clause is removed, then
           ! information on the size of workspace needs to be changed.
           ! if compz = 'n', use stdlib_ssterf to compute the eigenvalues.
           if( icompz==0_ilp ) then
              call stdlib_ssterf( n, d, e, info )
              go to 70
           end if
           ! if n is smaller than the minimum divide size (smlsiz+1), then
           ! solve the problem with another solver.
           if( n<=smlsiz ) then
              call stdlib_csteqr( compz, n, d, e, z, ldz, rwork, info )
           else
              ! if compz = 'i', we simply call stdlib_sstedc instead.
              if( icompz==2_ilp ) then
                 call stdlib_slaset( 'FULL', n, n, zero, one, rwork, n )
                 ll = n*n + 1_ilp
                 call stdlib_sstedc( 'I', n, d, e, rwork, n,rwork( ll ), lrwork-ll+1, iwork, &
                           liwork, info )
                 do j = 1, n
                    do i = 1, n
                       z( i, j ) = rwork( ( j-1 )*n+i )
                    end do
                 end do
                 go to 70
              end if
              ! from now on, only option left to be handled is compz = 'v',
              ! i.e. icompz = 1.
              ! scale.
              orgnrm = stdlib_slanst( 'M', n, d, e )
              if( orgnrm==zero )go to 70
              eps = stdlib_slamch( 'EPSILON' )
              start = 1_ilp
              ! while ( start <= n )
              30 continue
              if( start<=n ) then
                 ! let finish be the position of the next subdiagonal entry
                 ! such that e( finish ) <= tiny or finish = n if no such
                 ! subdiagonal exists.  the matrix identified by the elements
                 ! between start and finish constitutes an independent
                 ! sub-problem.
                 finish = start
                 40 continue
                 if( finish<n ) then
                    tiny = eps*sqrt( abs( d( finish ) ) )*sqrt( abs( d( finish+1 ) ) )
                    if( abs( e( finish ) )>tiny ) then
                       finish = finish + 1_ilp
                       go to 40
                    end if
                 end if
                 ! (sub) problem determined.  compute its size and solve it.
                 m = finish - start + 1_ilp
                 if( m>smlsiz ) then
                    ! scale.
                    orgnrm = stdlib_slanst( 'M', m, d( start ), e( start ) )
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m, 1_ilp, d( start ), m,info )
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m-1, 1_ilp, e( start ),m-1, info )
                              
                    call stdlib_claed0( n, m, d( start ), e( start ), z( 1_ilp, start ),ldz, work, n, &
                              rwork, iwork, info )
                    if( info>0_ilp ) then
                       info = ( info / ( m+1 )+start-1 )*( n+1 ) +mod( info, ( m+1 ) ) + start - &
                                 1_ilp
                       go to 70
                    end if
                    ! scale back.
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, m, 1_ilp, d( start ), m,info )
                 else
                    call stdlib_ssteqr( 'I', m, d( start ), e( start ), rwork, m,rwork( m*m+1 ), &
                              info )
                    call stdlib_clacrm( n, m, z( 1_ilp, start ), ldz, rwork, m, work, n,rwork( m*m+1 )&
                               )
                    call stdlib_clacpy( 'A', n, m, work, n, z( 1_ilp, start ), ldz )
                    if( info>0_ilp ) then
                       info = start*( n+1 ) + finish
                       go to 70
                    end if
                 end if
                 start = finish + 1_ilp
                 go to 30
              end if
              ! endwhile
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                i = ii - 1_ilp
                k = i
                p = d( i )
                do j = ii, n
                   if( d( j )<p ) then
                      k = j
                      p = d( j )
                   end if
                end do
                if( k/=i ) then
                   d( k ) = d( i )
                   d( i ) = p
                   call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                end if
              end do
           end if
           70 continue
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_cstedc

     pure module subroutine stdlib_zstedc( compz, n, d, e, z, ldz, work, lwork, rwork,lrwork, iwork, &
     !! ZSTEDC computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the divide and conquer method.
     !! The eigenvectors of a full or band complex Hermitian matrix can also
     !! be found if ZHETRD or ZHPTRD or ZHBTRD has been used to reduce this
     !! matrix to tridiagonal form.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.  See DLAED3 for details.
               liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: finish, i, icompz, ii, j, k, lgn, liwmin, ll, lrwmin, lwmin, m, smlsiz,&
                      start
           real(dp) :: eps, orgnrm, p, tiny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or.( icompz>0_ilp .and. ldz<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              smlsiz = stdlib_ilaenv( 9_ilp, 'ZSTEDC', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
              if( n<=1_ilp .or. icompz==0_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else if( n<=smlsiz ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 2_ilp*( n - 1_ilp )
              else if( icompz==1_ilp ) then
                 lgn = int( log( real( n,KIND=dp) ) / log( two ),KIND=ilp)
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 if( 2_ilp**lgn<n )lgn = lgn + 1_ilp
                 lwmin = n*n
                 lrwmin = 1_ilp + 3_ilp*n + 2_ilp*n*lgn + 4_ilp*n**2_ilp
                 liwmin = 6_ilp + 6_ilp*n + 5_ilp*n*lgn
              else if( icompz==2_ilp ) then
                 lwmin = 1_ilp
                 lrwmin = 1_ilp + 4_ilp*n + 2_ilp*n**2_ilp
                 liwmin = 3_ilp + 5_ilp*n
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -10_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSTEDC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz/=0_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! if the following conditional clause is removed, then the routine
           ! will use the divide and conquer routine to compute only the
           ! eigenvalues, which requires (3n + 3n**2) real workspace and
           ! (2 + 5n + 2n lg(n)) integer workspace.
           ! since on many architectures stdlib_dsterf is much faster than any other
           ! algorithm for finding eigenvalues only, it is used here
           ! as the default. if the conditional clause is removed, then
           ! information on the size of workspace needs to be changed.
           ! if compz = 'n', use stdlib_dsterf to compute the eigenvalues.
           if( icompz==0_ilp ) then
              call stdlib_dsterf( n, d, e, info )
              go to 70
           end if
           ! if n is smaller than the minimum divide size (smlsiz+1), then
           ! solve the problem with another solver.
           if( n<=smlsiz ) then
              call stdlib_zsteqr( compz, n, d, e, z, ldz, rwork, info )
           else
              ! if compz = 'i', we simply call stdlib_dstedc instead.
              if( icompz==2_ilp ) then
                 call stdlib_dlaset( 'FULL', n, n, zero, one, rwork, n )
                 ll = n*n + 1_ilp
                 call stdlib_dstedc( 'I', n, d, e, rwork, n,rwork( ll ), lrwork-ll+1, iwork, &
                           liwork, info )
                 do j = 1, n
                    do i = 1, n
                       z( i, j ) = rwork( ( j-1 )*n+i )
                    end do
                 end do
                 go to 70
              end if
              ! from now on, only option left to be handled is compz = 'v',
              ! i.e. icompz = 1.
              ! scale.
              orgnrm = stdlib_dlanst( 'M', n, d, e )
              if( orgnrm==zero )go to 70
              eps = stdlib_dlamch( 'EPSILON' )
              start = 1_ilp
              ! while ( start <= n )
              30 continue
              if( start<=n ) then
                 ! let finish be the position of the next subdiagonal entry
                 ! such that e( finish ) <= tiny or finish = n if no such
                 ! subdiagonal exists.  the matrix identified by the elements
                 ! between start and finish constitutes an independent
                 ! sub-problem.
                 finish = start
                 40 continue
                 if( finish<n ) then
                    tiny = eps*sqrt( abs( d( finish ) ) )*sqrt( abs( d( finish+1 ) ) )
                    if( abs( e( finish ) )>tiny ) then
                       finish = finish + 1_ilp
                       go to 40
                    end if
                 end if
                 ! (sub) problem determined.  compute its size and solve it.
                 m = finish - start + 1_ilp
                 if( m>smlsiz ) then
                    ! scale.
                    orgnrm = stdlib_dlanst( 'M', m, d( start ), e( start ) )
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m, 1_ilp, d( start ), m,info )
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, m-1, 1_ilp, e( start ),m-1, info )
                              
                    call stdlib_zlaed0( n, m, d( start ), e( start ), z( 1_ilp, start ),ldz, work, n, &
                              rwork, iwork, info )
                    if( info>0_ilp ) then
                       info = ( info / ( m+1 )+start-1 )*( n+1 ) +mod( info, ( m+1 ) ) + start - &
                                 1_ilp
                       go to 70
                    end if
                    ! scale back.
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, m, 1_ilp, d( start ), m,info )
                 else
                    call stdlib_dsteqr( 'I', m, d( start ), e( start ), rwork, m,rwork( m*m+1 ), &
                              info )
                    call stdlib_zlacrm( n, m, z( 1_ilp, start ), ldz, rwork, m, work, n,rwork( m*m+1 )&
                               )
                    call stdlib_zlacpy( 'A', n, m, work, n, z( 1_ilp, start ), ldz )
                    if( info>0_ilp ) then
                       info = start*( n+1 ) + finish
                       go to 70
                    end if
                 end if
                 start = finish + 1_ilp
                 go to 30
              end if
              ! endwhile
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                i = ii - 1_ilp
                k = i
                p = d( i )
                do j = ii, n
                   if( d( j )<p ) then
                      k = j
                      p = d( j )
                   end if
                end do
                if( k/=i ) then
                   d( k ) = d( i )
                   d( i ) = p
                   call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                end if
              end do
           end if
           70 continue
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zstedc




     pure module subroutine stdlib_sstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! SSTEGR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! SSTEGR is a compatibility wrapper around the improved SSTEMR routine.
     !! See SSTEMR for further details.
     !! One important change is that the ABSTOL parameter no longer provides any
     !! benefit and hence is no longer used.
     !! Note : SSTEGR and SSTEMR work only on machines which follow
     !! IEEE-754 floating-point standard in their handling of infinities and
     !! NaNs.  Normal execution may create these exceptiona values and hence
     !! may abort due to a floating point exception in environments which
     !! do not conform to the IEEE-754 standard.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           real(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: tryrac
           ! Executable Statements 
           info = 0_ilp
           tryrac = .false.
           call stdlib_sstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, n, isuppz, &
                     tryrac, work, lwork,iwork, liwork, info )
     end subroutine stdlib_sstegr

     pure module subroutine stdlib_dstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! DSTEGR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! DSTEGR is a compatibility wrapper around the improved DSTEMR routine.
     !! See DSTEMR for further details.
     !! One important change is that the ABSTOL parameter no longer provides any
     !! benefit and hence is no longer used.
     !! Note : DSTEGR and DSTEMR work only on machines which follow
     !! IEEE-754 floating-point standard in their handling of infinities and
     !! NaNs.  Normal execution may create these exceptiona values and hence
     !! may abort due to a floating point exception in environments which
     !! do not conform to the IEEE-754 standard.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           real(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: tryrac
           ! Executable Statements 
           info = 0_ilp
           tryrac = .false.
           call stdlib_dstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, n, isuppz, &
                     tryrac, work, lwork,iwork, liwork, info )
     end subroutine stdlib_dstegr


     pure module subroutine stdlib_cstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! CSTEGR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! CSTEGR is a compatibility wrapper around the improved CSTEMR routine.
     !! See SSTEMR for further details.
     !! One important change is that the ABSTOL parameter no longer provides any
     !! benefit and hence is no longer used.
     !! Note : CSTEGR and CSTEMR work only on machines which follow
     !! IEEE-754 floating-point standard in their handling of infinities and
     !! NaNs.  Normal execution may create these exceptiona values and hence
     !! may abort due to a floating point exception in environments which
     !! do not conform to the IEEE-754 standard.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           complex(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: tryrac
           ! Executable Statements 
           info = 0_ilp
           tryrac = .false.
           call stdlib_cstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, n, isuppz, &
                     tryrac, work, lwork,iwork, liwork, info )
     end subroutine stdlib_cstegr

     pure module subroutine stdlib_zstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! ZSTEGR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! ZSTEGR is a compatibility wrapper around the improved ZSTEMR routine.
     !! See ZSTEMR for further details.
     !! One important change is that the ABSTOL parameter no longer provides any
     !! benefit and hence is no longer used.
     !! Note : ZSTEGR and ZSTEMR work only on machines which follow
     !! IEEE-754 floating-point standard in their handling of infinities and
     !! NaNs.  Normal execution may create these exceptiona values and hence
     !! may abort due to a floating point exception in environments which
     !! do not conform to the IEEE-754 standard.
               isuppz, work, lwork, iwork,liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           complex(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: tryrac
           ! Executable Statements 
           info = 0_ilp
           tryrac = .false.
           call stdlib_zstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, n, isuppz, &
                     tryrac, work, lwork,iwork, liwork, info )
     end subroutine stdlib_zstegr




     pure module subroutine stdlib_sstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
     !! SSTEIN computes the eigenvectors of a real symmetric tridiagonal
     !! matrix T corresponding to specified eigenvalues, using inverse
     !! iteration.
     !! The maximum number of iterations allowed for each eigenvector is
     !! specified by an internal parameter MAXITS (currently set to 5).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*), w(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: odm3 = 1.0e-3_sp
           real(sp), parameter :: odm1 = 1.0e-1_sp
           integer(ilp), parameter :: maxits = 5_ilp
           integer(ilp), parameter :: extra = 2_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: b1, blksiz, bn, gpind, i, iinfo, indrv1, indrv2, indrv3, indrv4, &
                     indrv5, its, j, j1, jblk, jmax, nblk, nrmchk
           real(sp) :: ctr, eps, eps1, nrm, onenrm, ortol, pertol, scl, sep, stpcrt, tol, xj, &
                     xjm
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           do i = 1, m
              ifail( i ) = 0_ilp
           end do
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -4_ilp
           else if( ldz<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              do j = 2, m
                 if( iblock( j )<iblock( j-1 ) ) then
                    info = -6_ilp
                    go to 30
                 end if
                 if( iblock( j )==iblock( j-1 ) .and. w( j )<w( j-1 ) )then
                    info = -5_ilp
                    go to 30
                 end if
              end do
              30 continue
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEIN', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. m==0_ilp ) then
              return
           else if( n==1_ilp ) then
              z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           eps = stdlib_slamch( 'PRECISION' )
           ! initialize seed for random number generator stdlib_slarnv.
           do i = 1, 4
              iseed( i ) = 1_ilp
           end do
           ! initialize pointers.
           indrv1 = 0_ilp
           indrv2 = indrv1 + n
           indrv3 = indrv2 + n
           indrv4 = indrv3 + n
           indrv5 = indrv4 + n
           ! compute eigenvectors of matrix blocks.
           j1 = 1_ilp
           loop_160: do nblk = 1, iblock( m )
              ! find starting and ending indices of block nblk.
              if( nblk==1_ilp ) then
                 b1 = 1_ilp
              else
                 b1 = isplit( nblk-1 ) + 1_ilp
              end if
              bn = isplit( nblk )
              blksiz = bn - b1 + 1_ilp
              if( blksiz==1 )go to 60
              gpind = j1
              ! compute reorthogonalization criterion and stopping criterion.
              onenrm = abs( d( b1 ) ) + abs( e( b1 ) )
              onenrm = max( onenrm, abs( d( bn ) )+abs( e( bn-1 ) ) )
              do i = b1 + 1, bn - 1
                 onenrm = max( onenrm, abs( d( i ) )+abs( e( i-1 ) )+abs( e( i ) ) )
              end do
              ortol = odm3*onenrm
              stpcrt = sqrt( odm1 / blksiz )
              ! loop through eigenvalues of block nblk.
              60 continue
              jblk = 0_ilp
              loop_150: do j = j1, m
                 if( iblock( j )/=nblk ) then
                    j1 = j
                    cycle loop_160
                 end if
                 jblk = jblk + 1_ilp
                 xj = w( j )
                 ! skip all the work if the block size is one.
                 if( blksiz==1_ilp ) then
                    work( indrv1+1 ) = one
                    go to 120
                 end if
                 ! if eigenvalues j and j-1 are too close, add a relatively
                 ! small perturbation.
                 if( jblk>1_ilp ) then
                    eps1 = abs( eps*xj )
                    pertol = ten*eps1
                    sep = xj - xjm
                    if( sep<pertol )xj = xjm + pertol
                 end if
                 its = 0_ilp
                 nrmchk = 0_ilp
                 ! get random starting vector.
                 call stdlib_slarnv( 2_ilp, iseed, blksiz, work( indrv1+1 ) )
                 ! copy the matrix t so it won't be destroyed in factorization.
                 call stdlib_scopy( blksiz, d( b1 ), 1_ilp, work( indrv4+1 ), 1_ilp )
                 call stdlib_scopy( blksiz-1, e( b1 ), 1_ilp, work( indrv2+2 ), 1_ilp )
                 call stdlib_scopy( blksiz-1, e( b1 ), 1_ilp, work( indrv3+1 ), 1_ilp )
                 ! compute lu factors with partial pivoting  ( pt = lu )
                 tol = zero
                 call stdlib_slagtf( blksiz, work( indrv4+1 ), xj, work( indrv2+2 ),work( indrv3+&
                           1_ilp ), tol, work( indrv5+1 ), iwork,iinfo )
                 ! update iteration count.
                 70 continue
                 its = its + 1_ilp
                 if( its>maxits )go to 100
                 ! normalize and scale the righthand side vector pb.
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 scl = blksiz*onenrm*max( eps,abs( work( indrv4+blksiz ) ) ) /abs( work( indrv1+&
                           jmax ) )
                 call stdlib_sscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 ! solve the system lu = pb.
                 call stdlib_slagts( -1_ilp, blksiz, work( indrv4+1 ), work( indrv2+2 ),work( indrv3+&
                           1_ilp ), work( indrv5+1 ), iwork,work( indrv1+1 ), tol, iinfo )
                 ! reorthogonalize by modified gram-schmidt if eigenvalues are
                 ! close enough.
                 if( jblk==1 )go to 90
                 if( abs( xj-xjm )>ortol )gpind = j
                 if( gpind/=j ) then
                    do i = gpind, j - 1
                       ctr = -stdlib_sdot( blksiz, work( indrv1+1 ), 1_ilp, z( b1, i ),1_ilp )
                       call stdlib_saxpy( blksiz, ctr, z( b1, i ), 1_ilp,work( indrv1+1 ), 1_ilp )
                    end do
                 end if
                 ! check the infinity norm of the iterate.
                 90 continue
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 nrm = abs( work( indrv1+jmax ) )
                 ! continue for additional iterations after norm reaches
                 ! stopping criterion.
                 if( nrm<stpcrt )go to 70
                 nrmchk = nrmchk + 1_ilp
                 if( nrmchk<extra+1 )go to 70
                 go to 110
                 ! if stopping criterion was not satisfied, update info and
                 ! store eigenvector number in array ifail.
                 100 continue
                 info = info + 1_ilp
                 ifail( info ) = j
                 ! accept iterate as jth eigenvector.
                 110 continue
                 scl = one / stdlib_snrm2( blksiz, work( indrv1+1 ), 1_ilp )
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 if( work( indrv1+jmax )<zero )scl = -scl
                 call stdlib_sscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 120 continue
                 do i = 1, n
                    z( i, j ) = zero
                 end do
                 do i = 1, blksiz
                    z( b1+i-1, j ) = work( indrv1+i )
                 end do
                 ! save the shift to check eigenvalue spacing at next
                 ! iteration.
                 xjm = xj
              end do loop_150
           end do loop_160
           return
     end subroutine stdlib_sstein

     pure module subroutine stdlib_dstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
     !! DSTEIN computes the eigenvectors of a real symmetric tridiagonal
     !! matrix T corresponding to specified eigenvalues, using inverse
     !! iteration.
     !! The maximum number of iterations allowed for each eigenvector is
     !! specified by an internal parameter MAXITS (currently set to 5).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*), w(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: odm3 = 1.0e-3_dp
           real(dp), parameter :: odm1 = 1.0e-1_dp
           integer(ilp), parameter :: maxits = 5_ilp
           integer(ilp), parameter :: extra = 2_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: b1, blksiz, bn, gpind, i, iinfo, indrv1, indrv2, indrv3, indrv4, &
                     indrv5, its, j, j1, jblk, jmax, nblk, nrmchk
           real(dp) :: dtpcrt, eps, eps1, nrm, onenrm, ortol, pertol, scl, sep, tol, xj, xjm, &
                     ztr
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           do i = 1, m
              ifail( i ) = 0_ilp
           end do
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -4_ilp
           else if( ldz<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              do j = 2, m
                 if( iblock( j )<iblock( j-1 ) ) then
                    info = -6_ilp
                    go to 30
                 end if
                 if( iblock( j )==iblock( j-1 ) .and. w( j )<w( j-1 ) )then
                    info = -5_ilp
                    go to 30
                 end if
              end do
              30 continue
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEIN', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. m==0_ilp ) then
              return
           else if( n==1_ilp ) then
              z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! get machine constants.
           eps = stdlib_dlamch( 'PRECISION' )
           ! initialize seed for random number generator stdlib_dlarnv.
           do i = 1, 4
              iseed( i ) = 1_ilp
           end do
           ! initialize pointers.
           indrv1 = 0_ilp
           indrv2 = indrv1 + n
           indrv3 = indrv2 + n
           indrv4 = indrv3 + n
           indrv5 = indrv4 + n
           ! compute eigenvectors of matrix blocks.
           j1 = 1_ilp
           loop_160: do nblk = 1, iblock( m )
              ! find starting and ending indices of block nblk.
              if( nblk==1_ilp ) then
                 b1 = 1_ilp
              else
                 b1 = isplit( nblk-1 ) + 1_ilp
              end if
              bn = isplit( nblk )
              blksiz = bn - b1 + 1_ilp
              if( blksiz==1 )go to 60
              gpind = j1
              ! compute reorthogonalization criterion and stopping criterion.
              onenrm = abs( d( b1 ) ) + abs( e( b1 ) )
              onenrm = max( onenrm, abs( d( bn ) )+abs( e( bn-1 ) ) )
              do i = b1 + 1, bn - 1
                 onenrm = max( onenrm, abs( d( i ) )+abs( e( i-1 ) )+abs( e( i ) ) )
              end do
              ortol = odm3*onenrm
              dtpcrt = sqrt( odm1 / blksiz )
              ! loop through eigenvalues of block nblk.
              60 continue
              jblk = 0_ilp
              loop_150: do j = j1, m
                 if( iblock( j )/=nblk ) then
                    j1 = j
                    cycle loop_160
                 end if
                 jblk = jblk + 1_ilp
                 xj = w( j )
                 ! skip all the work if the block size is one.
                 if( blksiz==1_ilp ) then
                    work( indrv1+1 ) = one
                    go to 120
                 end if
                 ! if eigenvalues j and j-1 are too close, add a relatively
                 ! small perturbation.
                 if( jblk>1_ilp ) then
                    eps1 = abs( eps*xj )
                    pertol = ten*eps1
                    sep = xj - xjm
                    if( sep<pertol )xj = xjm + pertol
                 end if
                 its = 0_ilp
                 nrmchk = 0_ilp
                 ! get random starting vector.
                 call stdlib_dlarnv( 2_ilp, iseed, blksiz, work( indrv1+1 ) )
                 ! copy the matrix t so it won't be destroyed in factorization.
                 call stdlib_dcopy( blksiz, d( b1 ), 1_ilp, work( indrv4+1 ), 1_ilp )
                 call stdlib_dcopy( blksiz-1, e( b1 ), 1_ilp, work( indrv2+2 ), 1_ilp )
                 call stdlib_dcopy( blksiz-1, e( b1 ), 1_ilp, work( indrv3+1 ), 1_ilp )
                 ! compute lu factors with partial pivoting  ( pt = lu )
                 tol = zero
                 call stdlib_dlagtf( blksiz, work( indrv4+1 ), xj, work( indrv2+2 ),work( indrv3+&
                           1_ilp ), tol, work( indrv5+1 ), iwork,iinfo )
                 ! update iteration count.
                 70 continue
                 its = its + 1_ilp
                 if( its>maxits )go to 100
                 ! normalize and scale the righthand side vector pb.
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 scl = blksiz*onenrm*max( eps,abs( work( indrv4+blksiz ) ) ) /abs( work( indrv1+&
                           jmax ) )
                 call stdlib_dscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 ! solve the system lu = pb.
                 call stdlib_dlagts( -1_ilp, blksiz, work( indrv4+1 ), work( indrv2+2 ),work( indrv3+&
                           1_ilp ), work( indrv5+1 ), iwork,work( indrv1+1 ), tol, iinfo )
                 ! reorthogonalize by modified gram-schmidt if eigenvalues are
                 ! close enough.
                 if( jblk==1 )go to 90
                 if( abs( xj-xjm )>ortol )gpind = j
                 if( gpind/=j ) then
                    do i = gpind, j - 1
                       ztr = -stdlib_ddot( blksiz, work( indrv1+1 ), 1_ilp, z( b1, i ),1_ilp )
                       call stdlib_daxpy( blksiz, ztr, z( b1, i ), 1_ilp,work( indrv1+1 ), 1_ilp )
                    end do
                 end if
                 ! check the infinity norm of the iterate.
                 90 continue
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 nrm = abs( work( indrv1+jmax ) )
                 ! continue for additional iterations after norm reaches
                 ! stopping criterion.
                 if( nrm<dtpcrt )go to 70
                 nrmchk = nrmchk + 1_ilp
                 if( nrmchk<extra+1 )go to 70
                 go to 110
                 ! if stopping criterion was not satisfied, update info and
                 ! store eigenvector number in array ifail.
                 100 continue
                 info = info + 1_ilp
                 ifail( info ) = j
                 ! accept iterate as jth eigenvector.
                 110 continue
                 scl = one / stdlib_dnrm2( blksiz, work( indrv1+1 ), 1_ilp )
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 if( work( indrv1+jmax )<zero )scl = -scl
                 call stdlib_dscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 120 continue
                 do i = 1, n
                    z( i, j ) = zero
                 end do
                 do i = 1, blksiz
                    z( b1+i-1, j ) = work( indrv1+i )
                 end do
                 ! save the shift to check eigenvalue spacing at next
                 ! iteration.
                 xjm = xj
              end do loop_150
           end do loop_160
           return
     end subroutine stdlib_dstein


     pure module subroutine stdlib_cstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
     !! CSTEIN computes the eigenvectors of a real symmetric tridiagonal
     !! matrix T corresponding to specified eigenvalues, using inverse
     !! iteration.
     !! The maximum number of iterations allowed for each eigenvector is
     !! specified by an internal parameter MAXITS (currently set to 5).
     !! Although the eigenvectors are real, they are stored in a complex
     !! array, which may be passed to CUNMTR or CUPMTR for back
     !! transformation to the eigenvectors of a complex Hermitian matrix
     !! which was reduced to tridiagonal form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*), w(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(out) :: z(ldz,*)
       ! =====================================================================
           ! Parameters 
           real(sp), parameter :: odm3 = 1.0e-3_sp
           real(sp), parameter :: odm1 = 1.0e-1_sp
           integer(ilp), parameter :: maxits = 5_ilp
           integer(ilp), parameter :: extra = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: b1, blksiz, bn, gpind, i, iinfo, indrv1, indrv2, indrv3, indrv4, &
                     indrv5, its, j, j1, jblk, jmax, jr, nblk, nrmchk
           real(sp) :: ctr, eps, eps1, nrm, onenrm, ortol, pertol, scl, sep, stpcrt, tol, xj, &
                     xjm
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           do i = 1, m
              ifail( i ) = 0_ilp
           end do
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -4_ilp
           else if( ldz<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              do j = 2, m
                 if( iblock( j )<iblock( j-1 ) ) then
                    info = -6_ilp
                    go to 30
                 end if
                 if( iblock( j )==iblock( j-1 ) .and. w( j )<w( j-1 ) )then
                    info = -5_ilp
                    go to 30
                 end if
              end do
              30 continue
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSTEIN', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. m==0_ilp ) then
              return
           else if( n==1_ilp ) then
              z( 1_ilp, 1_ilp ) = cone
              return
           end if
           ! get machine constants.
           eps = stdlib_slamch( 'PRECISION' )
           ! initialize seed for random number generator stdlib_slarnv.
           do i = 1, 4
              iseed( i ) = 1_ilp
           end do
           ! initialize pointers.
           indrv1 = 0_ilp
           indrv2 = indrv1 + n
           indrv3 = indrv2 + n
           indrv4 = indrv3 + n
           indrv5 = indrv4 + n
           ! compute eigenvectors of matrix blocks.
           j1 = 1_ilp
           loop_180: do nblk = 1, iblock( m )
              ! find starting and ending indices of block nblk.
              if( nblk==1_ilp ) then
                 b1 = 1_ilp
              else
                 b1 = isplit( nblk-1 ) + 1_ilp
              end if
              bn = isplit( nblk )
              blksiz = bn - b1 + 1_ilp
              if( blksiz==1 )go to 60
              gpind = j1
              ! compute reorthogonalization criterion and stopping criterion.
              onenrm = abs( d( b1 ) ) + abs( e( b1 ) )
              onenrm = max( onenrm, abs( d( bn ) )+abs( e( bn-1 ) ) )
              do i = b1 + 1, bn - 1
                 onenrm = max( onenrm, abs( d( i ) )+abs( e( i-1 ) )+abs( e( i ) ) )
              end do
              ortol = odm3*onenrm
              stpcrt = sqrt( odm1 / blksiz )
              ! loop through eigenvalues of block nblk.
              60 continue
              jblk = 0_ilp
              loop_170: do j = j1, m
                 if( iblock( j )/=nblk ) then
                    j1 = j
                    cycle loop_180
                 end if
                 jblk = jblk + 1_ilp
                 xj = w( j )
                 ! skip all the work if the block size is one.
                 if( blksiz==1_ilp ) then
                    work( indrv1+1 ) = one
                    go to 140
                 end if
                 ! if eigenvalues j and j-1 are too close, add a relatively
                 ! small perturbation.
                 if( jblk>1_ilp ) then
                    eps1 = abs( eps*xj )
                    pertol = ten*eps1
                    sep = xj - xjm
                    if( sep<pertol )xj = xjm + pertol
                 end if
                 its = 0_ilp
                 nrmchk = 0_ilp
                 ! get random starting vector.
                 call stdlib_slarnv( 2_ilp, iseed, blksiz, work( indrv1+1 ) )
                 ! copy the matrix t so it won't be destroyed in factorization.
                 call stdlib_scopy( blksiz, d( b1 ), 1_ilp, work( indrv4+1 ), 1_ilp )
                 call stdlib_scopy( blksiz-1, e( b1 ), 1_ilp, work( indrv2+2 ), 1_ilp )
                 call stdlib_scopy( blksiz-1, e( b1 ), 1_ilp, work( indrv3+1 ), 1_ilp )
                 ! compute lu factors with partial pivoting  ( pt = lu )
                 tol = zero
                 call stdlib_slagtf( blksiz, work( indrv4+1 ), xj, work( indrv2+2 ),work( indrv3+&
                           1_ilp ), tol, work( indrv5+1 ), iwork,iinfo )
                 ! update iteration count.
                 70 continue
                 its = its + 1_ilp
                 if( its>maxits )go to 120
                 ! normalize and scale the righthand side vector pb.
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 scl = blksiz*onenrm*max( eps,abs( work( indrv4+blksiz ) ) ) /abs( work( indrv1+&
                           jmax ) )
                 call stdlib_sscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 ! solve the system lu = pb.
                 call stdlib_slagts( -1_ilp, blksiz, work( indrv4+1 ), work( indrv2+2 ),work( indrv3+&
                           1_ilp ), work( indrv5+1 ), iwork,work( indrv1+1 ), tol, iinfo )
                 ! reorthogonalize by modified gram-schmidt if eigenvalues are
                 ! close enough.
                 if( jblk==1 )go to 110
                 if( abs( xj-xjm )>ortol )gpind = j
                 if( gpind/=j ) then
                    do i = gpind, j - 1
                       ctr = zero
                       do jr = 1, blksiz
                          ctr = ctr + work( indrv1+jr )*real( z( b1-1+jr, i ),KIND=sp)
                       end do
                       do jr = 1, blksiz
                          work( indrv1+jr ) = work( indrv1+jr ) -ctr*real( z( b1-1+jr, i ),&
                                    KIND=sp)
                       end do
                    end do
                 end if
                 ! check the infinity norm of the iterate.
                 110 continue
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 nrm = abs( work( indrv1+jmax ) )
                 ! continue for additional iterations after norm reaches
                 ! stopping criterion.
                 if( nrm<stpcrt )go to 70
                 nrmchk = nrmchk + 1_ilp
                 if( nrmchk<extra+1 )go to 70
                 go to 130
                 ! if stopping criterion was not satisfied, update info and
                 ! store eigenvector number in array ifail.
                 120 continue
                 info = info + 1_ilp
                 ifail( info ) = j
                 ! accept iterate as jth eigenvector.
                 130 continue
                 scl = one / stdlib_snrm2( blksiz, work( indrv1+1 ), 1_ilp )
                 jmax = stdlib_isamax( blksiz, work( indrv1+1 ), 1_ilp )
                 if( work( indrv1+jmax )<zero )scl = -scl
                 call stdlib_sscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 140 continue
                 do i = 1, n
                    z( i, j ) = czero
                 end do
                 do i = 1, blksiz
                    z( b1+i-1, j ) = cmplx( work( indrv1+i ), zero,KIND=sp)
                 end do
                 ! save the shift to check eigenvalue spacing at next
                 ! iteration.
                 xjm = xj
              end do loop_170
           end do loop_180
           return
     end subroutine stdlib_cstein

     pure module subroutine stdlib_zstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
     !! ZSTEIN computes the eigenvectors of a real symmetric tridiagonal
     !! matrix T corresponding to specified eigenvalues, using inverse
     !! iteration.
     !! The maximum number of iterations allowed for each eigenvector is
     !! specified by an internal parameter MAXITS (currently set to 5).
     !! Although the eigenvectors are real, they are stored in a complex
     !! array, which may be passed to ZUNMTR or ZUPMTR for back
     !! transformation to the eigenvectors of a complex Hermitian matrix
     !! which was reduced to tridiagonal form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           ! Array Arguments 
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*), w(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(out) :: z(ldz,*)
       ! =====================================================================
           ! Parameters 
           real(dp), parameter :: odm3 = 1.0e-3_dp
           real(dp), parameter :: odm1 = 1.0e-1_dp
           integer(ilp), parameter :: maxits = 5_ilp
           integer(ilp), parameter :: extra = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: b1, blksiz, bn, gpind, i, iinfo, indrv1, indrv2, indrv3, indrv4, &
                     indrv5, its, j, j1, jblk, jmax, jr, nblk, nrmchk
           real(dp) :: dtpcrt, eps, eps1, nrm, onenrm, ortol, pertol, scl, sep, tol, xj, xjm, &
                     ztr
           ! Local Arrays 
           integer(ilp) :: iseed(4_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           do i = 1, m
              ifail( i ) = 0_ilp
           end do
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -4_ilp
           else if( ldz<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              do j = 2, m
                 if( iblock( j )<iblock( j-1 ) ) then
                    info = -6_ilp
                    go to 30
                 end if
                 if( iblock( j )==iblock( j-1 ) .and. w( j )<w( j-1 ) )then
                    info = -5_ilp
                    go to 30
                 end if
              end do
              30 continue
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSTEIN', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. m==0_ilp ) then
              return
           else if( n==1_ilp ) then
              z( 1_ilp, 1_ilp ) = cone
              return
           end if
           ! get machine constants.
           eps = stdlib_dlamch( 'PRECISION' )
           ! initialize seed for random number generator stdlib_dlarnv.
           do i = 1, 4
              iseed( i ) = 1_ilp
           end do
           ! initialize pointers.
           indrv1 = 0_ilp
           indrv2 = indrv1 + n
           indrv3 = indrv2 + n
           indrv4 = indrv3 + n
           indrv5 = indrv4 + n
           ! compute eigenvectors of matrix blocks.
           j1 = 1_ilp
           loop_180: do nblk = 1, iblock( m )
              ! find starting and ending indices of block nblk.
              if( nblk==1_ilp ) then
                 b1 = 1_ilp
              else
                 b1 = isplit( nblk-1 ) + 1_ilp
              end if
              bn = isplit( nblk )
              blksiz = bn - b1 + 1_ilp
              if( blksiz==1 )go to 60
              gpind = j1
              ! compute reorthogonalization criterion and stopping criterion.
              onenrm = abs( d( b1 ) ) + abs( e( b1 ) )
              onenrm = max( onenrm, abs( d( bn ) )+abs( e( bn-1 ) ) )
              do i = b1 + 1, bn - 1
                 onenrm = max( onenrm, abs( d( i ) )+abs( e( i-1 ) )+abs( e( i ) ) )
              end do
              ortol = odm3*onenrm
              dtpcrt = sqrt( odm1 / blksiz )
              ! loop through eigenvalues of block nblk.
              60 continue
              jblk = 0_ilp
              loop_170: do j = j1, m
                 if( iblock( j )/=nblk ) then
                    j1 = j
                    cycle loop_180
                 end if
                 jblk = jblk + 1_ilp
                 xj = w( j )
                 ! skip all the work if the block size is one.
                 if( blksiz==1_ilp ) then
                    work( indrv1+1 ) = one
                    go to 140
                 end if
                 ! if eigenvalues j and j-1 are too close, add a relatively
                 ! small perturbation.
                 if( jblk>1_ilp ) then
                    eps1 = abs( eps*xj )
                    pertol = ten*eps1
                    sep = xj - xjm
                    if( sep<pertol )xj = xjm + pertol
                 end if
                 its = 0_ilp
                 nrmchk = 0_ilp
                 ! get random starting vector.
                 call stdlib_dlarnv( 2_ilp, iseed, blksiz, work( indrv1+1 ) )
                 ! copy the matrix t so it won't be destroyed in factorization.
                 call stdlib_dcopy( blksiz, d( b1 ), 1_ilp, work( indrv4+1 ), 1_ilp )
                 call stdlib_dcopy( blksiz-1, e( b1 ), 1_ilp, work( indrv2+2 ), 1_ilp )
                 call stdlib_dcopy( blksiz-1, e( b1 ), 1_ilp, work( indrv3+1 ), 1_ilp )
                 ! compute lu factors with partial pivoting  ( pt = lu )
                 tol = zero
                 call stdlib_dlagtf( blksiz, work( indrv4+1 ), xj, work( indrv2+2 ),work( indrv3+&
                           1_ilp ), tol, work( indrv5+1 ), iwork,iinfo )
                 ! update iteration count.
                 70 continue
                 its = its + 1_ilp
                 if( its>maxits )go to 120
                 ! normalize and scale the righthand side vector pb.
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 scl = blksiz*onenrm*max( eps,abs( work( indrv4+blksiz ) ) ) /abs( work( indrv1+&
                           jmax ) )
                 call stdlib_dscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 ! solve the system lu = pb.
                 call stdlib_dlagts( -1_ilp, blksiz, work( indrv4+1 ), work( indrv2+2 ),work( indrv3+&
                           1_ilp ), work( indrv5+1 ), iwork,work( indrv1+1 ), tol, iinfo )
                 ! reorthogonalize by modified gram-schmidt if eigenvalues are
                 ! close enough.
                 if( jblk==1 )go to 110
                 if( abs( xj-xjm )>ortol )gpind = j
                 if( gpind/=j ) then
                    do i = gpind, j - 1
                       ztr = zero
                       do jr = 1, blksiz
                          ztr = ztr + work( indrv1+jr )*real( z( b1-1+jr, i ),KIND=dp)
                       end do
                       do jr = 1, blksiz
                          work( indrv1+jr ) = work( indrv1+jr ) -ztr*real( z( b1-1+jr, i ),&
                                    KIND=dp)
                       end do
                    end do
                 end if
                 ! check the infinity norm of the iterate.
                 110 continue
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 nrm = abs( work( indrv1+jmax ) )
                 ! continue for additional iterations after norm reaches
                 ! stopping criterion.
                 if( nrm<dtpcrt )go to 70
                 nrmchk = nrmchk + 1_ilp
                 if( nrmchk<extra+1 )go to 70
                 go to 130
                 ! if stopping criterion was not satisfied, update info and
                 ! store eigenvector number in array ifail.
                 120 continue
                 info = info + 1_ilp
                 ifail( info ) = j
                 ! accept iterate as jth eigenvector.
                 130 continue
                 scl = one / stdlib_dnrm2( blksiz, work( indrv1+1 ), 1_ilp )
                 jmax = stdlib_idamax( blksiz, work( indrv1+1 ), 1_ilp )
                 if( work( indrv1+jmax )<zero )scl = -scl
                 call stdlib_dscal( blksiz, scl, work( indrv1+1 ), 1_ilp )
                 140 continue
                 do i = 1, n
                    z( i, j ) = czero
                 end do
                 do i = 1, blksiz
                    z( b1+i-1, j ) = cmplx( work( indrv1+i ), zero,KIND=dp)
                 end do
                 ! save the shift to check eigenvalue spacing at next
                 ! iteration.
                 xjm = xj
              end do loop_170
           end do loop_180
           return
     end subroutine stdlib_zstein




     pure module subroutine stdlib_sstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
     !! SSTEMR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! Depending on the number of desired eigenvalues, these are computed either
     !! by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
     !! computed by the use of various suitable L D L^T factorizations near clusters
     !! of close eigenvalues (referred to as RRRs, Relatively Robust
     !! Representations). An informal sketch of the algorithm follows.
     !! For each unreduced block (submatrix) of T,
     !! (a) Compute T - sigma I  = L D L^T, so that L and D
     !! define all the wanted eigenvalues to high relative accuracy.
     !! This means that small relative changes in the entries of D and L
     !! cause only small relative changes in the eigenvalues and
     !! eigenvectors. The standard (unfactored) representation of the
     !! tridiagonal matrix T does not have this property in general.
     !! (b) Compute the eigenvalues to suitable accuracy.
     !! If the eigenvectors are desired, the algorithm attains full
     !! accuracy of the computed eigenvalues only right before
     !! the corresponding vectors have to be computed, see steps c) and d).
     !! (c) For each cluster of close eigenvalues, select a new
     !! shift close to the cluster, find a new factorization, and refine
     !! the shifted eigenvalues to suitable accuracy.
     !! (d) For each eigenvalue with a large enough relative separation compute
     !! the corresponding eigenvector by forming a rank revealing twisted
     !! factorization. Go back to (c) for any clusters that remain.
     !! For more details, see:
     !! - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
     !! to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
     !! Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
     !! - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
     !! Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
     !! 2004.  Also LAPACK Working Note 154.
     !! - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem",
     !! Computer Science Division Technical Report No. UCB/CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Further Details
     !! 1.SSTEMR works only on machines which follow IEEE-754
     !! floating-point standard in their handling of infinities and NaNs.
     !! This permits the use of efficient inner loops avoiding a check for
     !! zero divisors.
               isuppz, tryrac, work, lwork,iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           real(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: minrgp = 3.0e-3_sp
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, valeig, wantz, zquery
           integer(ilp) :: i, ibegin, iend, ifirst, iil, iindbl, iindw, iindwk, iinfo, iinspl, &
           iiu, ilast, in, indd, inde2, inderr, indgp, indgrs, indwrk, itmp, itmp2, j, jblk, jj, &
                     liwmin, lwmin, nsplit, nzcmin, offset, wbegin, wend
           real(sp) :: bignum, cs, eps, pivmin, r1, r2, rmax, rmin, rtol1, rtol2, safmin, scale, &
                     smlnum, sn, thresh, tmp, tnrm, wl, wu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ).or.( liwork==-1_ilp ) )
           zquery = ( nzc==-1_ilp )
           ! stdlib_sstemr needs work of size 6*n, iwork of size 3*n.
           ! in addition, stdlib_slarre needs work of size 6*n, iwork of size 5*n.
           ! furthermore, stdlib_slarrv needs work of size 12*n, iwork of size 7*n.
           if( wantz ) then
              lwmin = 18_ilp*n
              liwmin = 10_ilp*n
           else
              ! need less workspace if only the eigenvalues are wanted
              lwmin = 12_ilp*n
              liwmin = 8_ilp*n
           endif
           wl = zero
           wu = zero
           iil = 0_ilp
           iiu = 0_ilp
           nsplit = 0_ilp
           if( valeig ) then
              ! we do not reference vl, vu in the cases range = 'i','a'
              ! the interval (wl, wu] contains all the wanted eigenvalues.
              ! it is either given by the user or computed in stdlib_slarre.
              wl = vl
              wu = vu
           elseif( indeig ) then
              ! we do not reference il, iu in the cases range = 'v','a'
              iil = il
              iiu = iu
           endif
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( valeig .and. n>0_ilp .and. wu<=wl ) then
              info = -7_ilp
           else if( indeig .and. ( iil<1_ilp .or. iil>n ) ) then
              info = -8_ilp
           else if( indeig .and. ( iiu<iil .or. iiu>n ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -13_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -17_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( wantz .and. alleig ) then
                 nzcmin = n
              else if( wantz .and. valeig ) then
                 call stdlib_slarrc( 'T', n, vl, vu, d, e, safmin,nzcmin, itmp, itmp2, info )
                           
              else if( wantz .and. indeig ) then
                 nzcmin = iiu-iil+1
              else
                 ! wantz == false.
                 nzcmin = 0_ilp
              endif
              if( zquery .and. info==0_ilp ) then
                 z( 1_ilp,1_ilp ) = nzcmin
              else if( nzc<nzcmin .and. .not.zquery ) then
                 info = -14_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEMR', -info )
              return
           else if( lquery .or. zquery ) then
              return
           end if
           ! handle n = 0, 1, and 2 cases immediately
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( wl<d( 1_ilp ) .and. wu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz.and.(.not.zquery) ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz(1_ilp) = 1_ilp
                 isuppz(2_ilp) = 1_ilp
              end if
              return
           end if
           if( n==2_ilp ) then
              if( .not.wantz ) then
                 call stdlib_slae2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2 )
              else if( wantz.and.(.not.zquery) ) then
                 call stdlib_slaev2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2, cs, sn )
              end if
              if( alleig.or.(valeig.and.(r2>wl).and.(r2<=wu)).or.(indeig.and.(iil==1_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r2
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = -sn
                    z( 2_ilp, m ) = cs
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
              if( alleig.or.(valeig.and.(r1>wl).and.(r1<=wu)).or.(indeig.and.(iiu==2_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r1
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = cs
                    z( 2_ilp, m ) = sn
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
           else
           ! continue with general n
              indgrs = 1_ilp
              inderr = 2_ilp*n + 1_ilp
              indgp = 3_ilp*n + 1_ilp
              indd = 4_ilp*n + 1_ilp
              inde2 = 5_ilp*n + 1_ilp
              indwrk = 6_ilp*n + 1_ilp
              iinspl = 1_ilp
              iindbl = n + 1_ilp
              iindw = 2_ilp*n + 1_ilp
              iindwk = 3_ilp*n + 1_ilp
              ! scale matrix to allowable range, if necessary.
              ! the allowable range is related to the pivmin parameter; see the
              ! comments in stdlib_slarrd.  the preference for scaling small values
              ! up is heuristic; we expect users' matrices not to be close to the
              ! rmax threshold.
              scale = one
              tnrm = stdlib_slanst( 'M', n, d, e )
              if( tnrm>zero .and. tnrm<rmin ) then
                 scale = rmin / tnrm
              else if( tnrm>rmax ) then
                 scale = rmax / tnrm
              end if
              if( scale/=one ) then
                 call stdlib_sscal( n, scale, d, 1_ilp )
                 call stdlib_sscal( n-1, scale, e, 1_ilp )
                 tnrm = tnrm*scale
                 if( valeig ) then
                    ! if eigenvalues in interval have to be found,
                    ! scale (wl, wu] accordingly
                    wl = wl*scale
                    wu = wu*scale
                 endif
              end if
              ! compute the desired eigenvalues of the tridiagonal after splitting
              ! into smaller subblocks if the corresponding off-diagonal elements
              ! are small
              ! thresh is the splitting parameter for stdlib_slarre
              ! a negative thresh forces the old splitting criterion based on the
              ! size of the off-diagonal. a positive thresh switches to splitting
              ! which preserves relative accuracy.
              if( tryrac ) then
                 ! test whether the matrix warrants the more expensive relative approach.
                 call stdlib_slarrr( n, d, e, iinfo )
              else
                 ! the user does not care about relative accurately eigenvalues
                 iinfo = -1_ilp
              endif
              ! set the splitting criterion
              if (iinfo==0_ilp) then
                 thresh = eps
              else
                 thresh = -eps
                 ! relative accuracy is desired but t does not guarantee it
                 tryrac = .false.
              endif
              if( tryrac ) then
                 ! copy original diagonal, needed to guarantee relative accuracy
                 call stdlib_scopy(n,d,1_ilp,work(indd),1_ilp)
              endif
              ! store the squares of the offdiagonal values of t
              do j = 1, n-1
                 work( inde2+j-1 ) = e(j)**2_ilp
              end do
              ! set the tolerance parameters for bisection
              if( .not.wantz ) then
                 ! stdlib_slarre computes the eigenvalues to full precision.
                 rtol1 = four * eps
                 rtol2 = four * eps
              else
                 ! stdlib_slarre computes the eigenvalues to less than full precision.
                 ! stdlib_slarrv will refine the eigenvalue approximations, and we can
                 ! need less accurate initial bisection in stdlib_slarre.
                 ! note: these settings do only affect the subset case and stdlib_slarre
                 rtol1 = max( sqrt(eps)*5.0e-2_sp, four * eps )
                 rtol2 = max( sqrt(eps)*5.0e-3_sp, four * eps )
              endif
              call stdlib_slarre( range, n, wl, wu, iil, iiu, d, e,work(inde2), rtol1, rtol2, &
              thresh, nsplit,iwork( iinspl ), m, w, work( inderr ),work( indgp ), iwork( iindbl ),&
              iwork( iindw ), work( indgrs ), pivmin,work( indwrk ), iwork( iindwk ), iinfo )
                        
              if( iinfo/=0_ilp ) then
                 info = 10_ilp + abs( iinfo )
                 return
              end if
              ! note that if range /= 'v', stdlib_slarre computes bounds on the desired
              ! part of the spectrum. all desired eigenvalues are contained in
              ! (wl,wu]
              if( wantz ) then
                 ! compute the desired eigenvectors corresponding to the computed
                 ! eigenvalues
                 call stdlib_slarrv( n, wl, wu, d, e,pivmin, iwork( iinspl ), m,1_ilp, m, minrgp, &
                 rtol1, rtol2,w, work( inderr ), work( indgp ), iwork( iindbl ),iwork( iindw ), &
                           work( indgrs ), z, ldz,isuppz, work( indwrk ), iwork( iindwk ), iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 20_ilp + abs( iinfo )
                    return
                 end if
              else
                 ! stdlib_slarre computes eigenvalues of the (shifted) root representation
                 ! stdlib_slarrv returns the eigenvalues of the unshifted matrix.
                 ! however, if the eigenvectors are not desired by the user, we need
                 ! to apply the corresponding shifts from stdlib_slarre to obtain the
                 ! eigenvalues of the original matrix.
                 do j = 1, m
                    itmp = iwork( iindbl+j-1 )
                    w( j ) = w( j ) + e( iwork( iinspl+itmp-1 ) )
                 end do
              end if
              if ( tryrac ) then
                 ! refine computed eigenvalues so that they are relatively accurate
                 ! with respect to the original matrix t.
                 ibegin = 1_ilp
                 wbegin = 1_ilp
                 loop_39: do jblk = 1, iwork( iindbl+m-1 )
                    iend = iwork( iinspl+jblk-1 )
                    in = iend - ibegin + 1_ilp
                    wend = wbegin - 1_ilp
                    ! check if any eigenvalues have to be refined in this block
                    36 continue
                    if( wend<m ) then
                       if( iwork( iindbl+wend )==jblk ) then
                          wend = wend + 1_ilp
                          go to 36
                       end if
                    end if
                    if( wend<wbegin ) then
                       ibegin = iend + 1_ilp
                       cycle loop_39
                    end if
                    offset = iwork(iindw+wbegin-1)-1_ilp
                    ifirst = iwork(iindw+wbegin-1)
                    ilast = iwork(iindw+wend-1)
                    rtol2 = four * eps
                    call stdlib_slarrj( in,work(indd+ibegin-1), work(inde2+ibegin-1),ifirst, &
                    ilast, rtol2, offset, w(wbegin),work( inderr+wbegin-1 ),work( indwrk ), iwork(&
                               iindwk ), pivmin,tnrm, iinfo )
                    ibegin = iend + 1_ilp
                    wbegin = wend + 1_ilp
                 end do loop_39
              endif
              ! if matrix was scaled, then rescale eigenvalues appropriately.
              if( scale/=one ) then
                 call stdlib_sscal( m, one / scale, w, 1_ilp )
              end if
           end if
           ! if eigenvalues are not in increasing order, then sort them,
           ! possibly along with eigenvectors.
           if( nsplit>1_ilp .or. n==2_ilp ) then
              if( .not. wantz ) then
                 call stdlib_slasrt( 'I', m, w, iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 3_ilp
                    return
                 end if
              else
                 do j = 1, m - 1
                    i = 0_ilp
                    tmp = w( j )
                    do jj = j + 1, m
                       if( w( jj )<tmp ) then
                          i = jj
                          tmp = w( jj )
                       end if
                    end do
                    if( i/=0_ilp ) then
                       w( i ) = w( j )
                       w( j ) = tmp
                       if( wantz ) then
                          call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                          itmp = isuppz( 2_ilp*i-1 )
                          isuppz( 2_ilp*i-1 ) = isuppz( 2_ilp*j-1 )
                          isuppz( 2_ilp*j-1 ) = itmp
                          itmp = isuppz( 2_ilp*i )
                          isuppz( 2_ilp*i ) = isuppz( 2_ilp*j )
                          isuppz( 2_ilp*j ) = itmp
                       end if
                    end if
                 end do
              end if
           endif
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sstemr

     pure module subroutine stdlib_dstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
     !! DSTEMR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! Depending on the number of desired eigenvalues, these are computed either
     !! by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
     !! computed by the use of various suitable L D L^T factorizations near clusters
     !! of close eigenvalues (referred to as RRRs, Relatively Robust
     !! Representations). An informal sketch of the algorithm follows.
     !! For each unreduced block (submatrix) of T,
     !! (a) Compute T - sigma I  = L D L^T, so that L and D
     !! define all the wanted eigenvalues to high relative accuracy.
     !! This means that small relative changes in the entries of D and L
     !! cause only small relative changes in the eigenvalues and
     !! eigenvectors. The standard (unfactored) representation of the
     !! tridiagonal matrix T does not have this property in general.
     !! (b) Compute the eigenvalues to suitable accuracy.
     !! If the eigenvectors are desired, the algorithm attains full
     !! accuracy of the computed eigenvalues only right before
     !! the corresponding vectors have to be computed, see steps c) and d).
     !! (c) For each cluster of close eigenvalues, select a new
     !! shift close to the cluster, find a new factorization, and refine
     !! the shifted eigenvalues to suitable accuracy.
     !! (d) For each eigenvalue with a large enough relative separation compute
     !! the corresponding eigenvector by forming a rank revealing twisted
     !! factorization. Go back to (c) for any clusters that remain.
     !! For more details, see:
     !! - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
     !! to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
     !! Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
     !! - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
     !! Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
     !! 2004.  Also LAPACK Working Note 154.
     !! - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem",
     !! Computer Science Division Technical Report No. UCB/CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Further Details
     !! 1.DSTEMR works only on machines which follow IEEE-754
     !! floating-point standard in their handling of infinities and NaNs.
     !! This permits the use of efficient inner loops avoiding a check for
     !! zero divisors.
               isuppz, tryrac, work, lwork,iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           real(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: minrgp = 1.0e-3_dp
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, valeig, wantz, zquery
           integer(ilp) :: i, ibegin, iend, ifirst, iil, iindbl, iindw, iindwk, iinfo, iinspl, &
           iiu, ilast, in, indd, inde2, inderr, indgp, indgrs, indwrk, itmp, itmp2, j, jblk, jj, &
                     liwmin, lwmin, nsplit, nzcmin, offset, wbegin, wend
           real(dp) :: bignum, cs, eps, pivmin, r1, r2, rmax, rmin, rtol1, rtol2, safmin, scale, &
                     smlnum, sn, thresh, tmp, tnrm, wl, wu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ).or.( liwork==-1_ilp ) )
           zquery = ( nzc==-1_ilp )
           ! stdlib_dstemr needs work of size 6*n, iwork of size 3*n.
           ! in addition, stdlib_dlarre needs work of size 6*n, iwork of size 5*n.
           ! furthermore, stdlib_dlarrv needs work of size 12*n, iwork of size 7*n.
           if( wantz ) then
              lwmin = 18_ilp*n
              liwmin = 10_ilp*n
           else
              ! need less workspace if only the eigenvalues are wanted
              lwmin = 12_ilp*n
              liwmin = 8_ilp*n
           endif
           wl = zero
           wu = zero
           iil = 0_ilp
           iiu = 0_ilp
           nsplit = 0_ilp
           if( valeig ) then
              ! we do not reference vl, vu in the cases range = 'i','a'
              ! the interval (wl, wu] contains all the wanted eigenvalues.
              ! it is either given by the user or computed in stdlib_dlarre.
              wl = vl
              wu = vu
           elseif( indeig ) then
              ! we do not reference il, iu in the cases range = 'v','a'
              iil = il
              iiu = iu
           endif
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( valeig .and. n>0_ilp .and. wu<=wl ) then
              info = -7_ilp
           else if( indeig .and. ( iil<1_ilp .or. iil>n ) ) then
              info = -8_ilp
           else if( indeig .and. ( iiu<iil .or. iiu>n ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -13_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -17_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( wantz .and. alleig ) then
                 nzcmin = n
              else if( wantz .and. valeig ) then
                 call stdlib_dlarrc( 'T', n, vl, vu, d, e, safmin,nzcmin, itmp, itmp2, info )
                           
              else if( wantz .and. indeig ) then
                 nzcmin = iiu-iil+1
              else
                 ! wantz == false.
                 nzcmin = 0_ilp
              endif
              if( zquery .and. info==0_ilp ) then
                 z( 1_ilp,1_ilp ) = nzcmin
              else if( nzc<nzcmin .and. .not.zquery ) then
                 info = -14_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEMR', -info )
              return
           else if( lquery .or. zquery ) then
              return
           end if
           ! handle n = 0, 1, and 2 cases immediately
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( wl<d( 1_ilp ) .and. wu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz.and.(.not.zquery) ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz(1_ilp) = 1_ilp
                 isuppz(2_ilp) = 1_ilp
              end if
              return
           end if
           if( n==2_ilp ) then
              if( .not.wantz ) then
                 call stdlib_dlae2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2 )
              else if( wantz.and.(.not.zquery) ) then
                 call stdlib_dlaev2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2, cs, sn )
              end if
              if( alleig.or.(valeig.and.(r2>wl).and.(r2<=wu)).or.(indeig.and.(iil==1_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r2
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = -sn
                    z( 2_ilp, m ) = cs
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
              if( alleig.or.(valeig.and.(r1>wl).and.(r1<=wu)).or.(indeig.and.(iiu==2_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r1
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = cs
                    z( 2_ilp, m ) = sn
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
           else
           ! continue with general n
              indgrs = 1_ilp
              inderr = 2_ilp*n + 1_ilp
              indgp = 3_ilp*n + 1_ilp
              indd = 4_ilp*n + 1_ilp
              inde2 = 5_ilp*n + 1_ilp
              indwrk = 6_ilp*n + 1_ilp
              iinspl = 1_ilp
              iindbl = n + 1_ilp
              iindw = 2_ilp*n + 1_ilp
              iindwk = 3_ilp*n + 1_ilp
              ! scale matrix to allowable range, if necessary.
              ! the allowable range is related to the pivmin parameter; see the
              ! comments in stdlib_dlarrd.  the preference for scaling small values
              ! up is heuristic; we expect users' matrices not to be close to the
              ! rmax threshold.
              scale = one
              tnrm = stdlib_dlanst( 'M', n, d, e )
              if( tnrm>zero .and. tnrm<rmin ) then
                 scale = rmin / tnrm
              else if( tnrm>rmax ) then
                 scale = rmax / tnrm
              end if
              if( scale/=one ) then
                 call stdlib_dscal( n, scale, d, 1_ilp )
                 call stdlib_dscal( n-1, scale, e, 1_ilp )
                 tnrm = tnrm*scale
                 if( valeig ) then
                    ! if eigenvalues in interval have to be found,
                    ! scale (wl, wu] accordingly
                    wl = wl*scale
                    wu = wu*scale
                 endif
              end if
              ! compute the desired eigenvalues of the tridiagonal after splitting
              ! into smaller subblocks if the corresponding off-diagonal elements
              ! are small
              ! thresh is the splitting parameter for stdlib_dlarre
              ! a negative thresh forces the old splitting criterion based on the
              ! size of the off-diagonal. a positive thresh switches to splitting
              ! which preserves relative accuracy.
              if( tryrac ) then
                 ! test whether the matrix warrants the more expensive relative approach.
                 call stdlib_dlarrr( n, d, e, iinfo )
              else
                 ! the user does not care about relative accurately eigenvalues
                 iinfo = -1_ilp
              endif
              ! set the splitting criterion
              if (iinfo==0_ilp) then
                 thresh = eps
              else
                 thresh = -eps
                 ! relative accuracy is desired but t does not guarantee it
                 tryrac = .false.
              endif
              if( tryrac ) then
                 ! copy original diagonal, needed to guarantee relative accuracy
                 call stdlib_dcopy(n,d,1_ilp,work(indd),1_ilp)
              endif
              ! store the squares of the offdiagonal values of t
              do j = 1, n-1
                 work( inde2+j-1 ) = e(j)**2_ilp
              end do
              ! set the tolerance parameters for bisection
              if( .not.wantz ) then
                 ! stdlib_dlarre computes the eigenvalues to full precision.
                 rtol1 = four * eps
                 rtol2 = four * eps
              else
                 ! stdlib_dlarre computes the eigenvalues to less than full precision.
                 ! stdlib_dlarrv will refine the eigenvalue approximations, and we can
                 ! need less accurate initial bisection in stdlib_dlarre.
                 ! note: these settings do only affect the subset case and stdlib_dlarre
                 rtol1 = sqrt(eps)
                 rtol2 = max( sqrt(eps)*5.0e-3_dp, four * eps )
              endif
              call stdlib_dlarre( range, n, wl, wu, iil, iiu, d, e,work(inde2), rtol1, rtol2, &
              thresh, nsplit,iwork( iinspl ), m, w, work( inderr ),work( indgp ), iwork( iindbl ),&
              iwork( iindw ), work( indgrs ), pivmin,work( indwrk ), iwork( iindwk ), iinfo )
                        
              if( iinfo/=0_ilp ) then
                 info = 10_ilp + abs( iinfo )
                 return
              end if
              ! note that if range /= 'v', stdlib_dlarre computes bounds on the desired
              ! part of the spectrum. all desired eigenvalues are contained in
              ! (wl,wu]
              if( wantz ) then
                 ! compute the desired eigenvectors corresponding to the computed
                 ! eigenvalues
                 call stdlib_dlarrv( n, wl, wu, d, e,pivmin, iwork( iinspl ), m,1_ilp, m, minrgp, &
                 rtol1, rtol2,w, work( inderr ), work( indgp ), iwork( iindbl ),iwork( iindw ), &
                           work( indgrs ), z, ldz,isuppz, work( indwrk ), iwork( iindwk ), iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 20_ilp + abs( iinfo )
                    return
                 end if
              else
                 ! stdlib_dlarre computes eigenvalues of the (shifted) root representation
                 ! stdlib_dlarrv returns the eigenvalues of the unshifted matrix.
                 ! however, if the eigenvectors are not desired by the user, we need
                 ! to apply the corresponding shifts from stdlib_dlarre to obtain the
                 ! eigenvalues of the original matrix.
                 do j = 1, m
                    itmp = iwork( iindbl+j-1 )
                    w( j ) = w( j ) + e( iwork( iinspl+itmp-1 ) )
                 end do
              end if
              if ( tryrac ) then
                 ! refine computed eigenvalues so that they are relatively accurate
                 ! with respect to the original matrix t.
                 ibegin = 1_ilp
                 wbegin = 1_ilp
                 loop_39: do jblk = 1, iwork( iindbl+m-1 )
                    iend = iwork( iinspl+jblk-1 )
                    in = iend - ibegin + 1_ilp
                    wend = wbegin - 1_ilp
                    ! check if any eigenvalues have to be refined in this block
                    36 continue
                    if( wend<m ) then
                       if( iwork( iindbl+wend )==jblk ) then
                          wend = wend + 1_ilp
                          go to 36
                       end if
                    end if
                    if( wend<wbegin ) then
                       ibegin = iend + 1_ilp
                       cycle loop_39
                    end if
                    offset = iwork(iindw+wbegin-1)-1_ilp
                    ifirst = iwork(iindw+wbegin-1)
                    ilast = iwork(iindw+wend-1)
                    rtol2 = four * eps
                    call stdlib_dlarrj( in,work(indd+ibegin-1), work(inde2+ibegin-1),ifirst, &
                    ilast, rtol2, offset, w(wbegin),work( inderr+wbegin-1 ),work( indwrk ), iwork(&
                               iindwk ), pivmin,tnrm, iinfo )
                    ibegin = iend + 1_ilp
                    wbegin = wend + 1_ilp
                 end do loop_39
              endif
              ! if matrix was scaled, then rescale eigenvalues appropriately.
              if( scale/=one ) then
                 call stdlib_dscal( m, one / scale, w, 1_ilp )
              end if
           end if
           ! if eigenvalues are not in increasing order, then sort them,
           ! possibly along with eigenvectors.
           if( nsplit>1_ilp .or. n==2_ilp ) then
              if( .not. wantz ) then
                 call stdlib_dlasrt( 'I', m, w, iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 3_ilp
                    return
                 end if
              else
                 do j = 1, m - 1
                    i = 0_ilp
                    tmp = w( j )
                    do jj = j + 1, m
                       if( w( jj )<tmp ) then
                          i = jj
                          tmp = w( jj )
                       end if
                    end do
                    if( i/=0_ilp ) then
                       w( i ) = w( j )
                       w( j ) = tmp
                       if( wantz ) then
                          call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                          itmp = isuppz( 2_ilp*i-1 )
                          isuppz( 2_ilp*i-1 ) = isuppz( 2_ilp*j-1 )
                          isuppz( 2_ilp*j-1 ) = itmp
                          itmp = isuppz( 2_ilp*i )
                          isuppz( 2_ilp*i ) = isuppz( 2_ilp*j )
                          isuppz( 2_ilp*j ) = itmp
                       end if
                    end if
                 end do
              end if
           endif
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dstemr


     pure module subroutine stdlib_cstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
     !! CSTEMR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! Depending on the number of desired eigenvalues, these are computed either
     !! by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
     !! computed by the use of various suitable L D L^T factorizations near clusters
     !! of close eigenvalues (referred to as RRRs, Relatively Robust
     !! Representations). An informal sketch of the algorithm follows.
     !! For each unreduced block (submatrix) of T,
     !! (a) Compute T - sigma I  = L D L^T, so that L and D
     !! define all the wanted eigenvalues to high relative accuracy.
     !! This means that small relative changes in the entries of D and L
     !! cause only small relative changes in the eigenvalues and
     !! eigenvectors. The standard (unfactored) representation of the
     !! tridiagonal matrix T does not have this property in general.
     !! (b) Compute the eigenvalues to suitable accuracy.
     !! If the eigenvectors are desired, the algorithm attains full
     !! accuracy of the computed eigenvalues only right before
     !! the corresponding vectors have to be computed, see steps c) and d).
     !! (c) For each cluster of close eigenvalues, select a new
     !! shift close to the cluster, find a new factorization, and refine
     !! the shifted eigenvalues to suitable accuracy.
     !! (d) For each eigenvalue with a large enough relative separation compute
     !! the corresponding eigenvector by forming a rank revealing twisted
     !! factorization. Go back to (c) for any clusters that remain.
     !! For more details, see:
     !! - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
     !! to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
     !! Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
     !! - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
     !! Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
     !! 2004.  Also LAPACK Working Note 154.
     !! - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem",
     !! Computer Science Division Technical Report No. UCB/CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Further Details
     !! 1.CSTEMR works only on machines which follow IEEE-754
     !! floating-point standard in their handling of infinities and NaNs.
     !! This permits the use of efficient inner loops avoiding a check for
     !! zero divisors.
     !! 2. LAPACK routines can be used to reduce a complex Hermitean matrix to
     !! real symmetric tridiagonal form.
     !! (Any complex Hermitean tridiagonal matrix has real values on its diagonal
     !! and potentially complex numbers on its off-diagonals. By applying a
     !! similarity transform with an appropriate diagonal matrix
     !! diag(1,e^{i \phy_1}, ... , e^{i \phy_{n-1}}), the complex Hermitean
     !! matrix can be transformed into a real symmetric matrix and complex
     !! arithmetic can be entirely avoided.)
     !! While the eigenvectors of the real symmetric tridiagonal matrix are real,
     !! the eigenvectors of original complex Hermitean matrix have complex entries
     !! in general.
     !! Since LAPACK drivers overwrite the matrix data with the eigenvectors,
     !! CSTEMR accepts complex workspace to facilitate interoperability
     !! with CUNMTR or CUPMTR.
               isuppz, tryrac, work, lwork,iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           complex(sp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: minrgp = 3.0e-3_sp
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, valeig, wantz, zquery
           integer(ilp) :: i, ibegin, iend, ifirst, iil, iindbl, iindw, iindwk, iinfo, iinspl, &
           iiu, ilast, in, indd, inde2, inderr, indgp, indgrs, indwrk, itmp, itmp2, j, jblk, jj, &
                     liwmin, lwmin, nsplit, nzcmin, offset, wbegin, wend
           real(sp) :: bignum, cs, eps, pivmin, r1, r2, rmax, rmin, rtol1, rtol2, safmin, scale, &
                     smlnum, sn, thresh, tmp, tnrm, wl, wu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ).or.( liwork==-1_ilp ) )
           zquery = ( nzc==-1_ilp )
           ! stdlib_sstemr needs work of size 6*n, iwork of size 3*n.
           ! in addition, stdlib_slarre needs work of size 6*n, iwork of size 5*n.
           ! furthermore, stdlib_clarrv needs work of size 12*n, iwork of size 7*n.
           if( wantz ) then
              lwmin = 18_ilp*n
              liwmin = 10_ilp*n
           else
              ! need less workspace if only the eigenvalues are wanted
              lwmin = 12_ilp*n
              liwmin = 8_ilp*n
           endif
           wl = zero
           wu = zero
           iil = 0_ilp
           iiu = 0_ilp
           nsplit = 0_ilp
           if( valeig ) then
              ! we do not reference vl, vu in the cases range = 'i','a'
              ! the interval (wl, wu] contains all the wanted eigenvalues.
              ! it is either given by the user or computed in stdlib_slarre.
              wl = vl
              wu = vu
           elseif( indeig ) then
              ! we do not reference il, iu in the cases range = 'v','a'
              iil = il
              iiu = iu
           endif
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( valeig .and. n>0_ilp .and. wu<=wl ) then
              info = -7_ilp
           else if( indeig .and. ( iil<1_ilp .or. iil>n ) ) then
              info = -8_ilp
           else if( indeig .and. ( iiu<iil .or. iiu>n ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -13_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -17_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! get machine constants.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           eps = stdlib_slamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( wantz .and. alleig ) then
                 nzcmin = n
              else if( wantz .and. valeig ) then
                 call stdlib_slarrc( 'T', n, vl, vu, d, e, safmin,nzcmin, itmp, itmp2, info )
                           
              else if( wantz .and. indeig ) then
                 nzcmin = iiu-iil+1
              else
                 ! wantz == false.
                 nzcmin = 0_ilp
              endif
              if( zquery .and. info==0_ilp ) then
                 z( 1_ilp,1_ilp ) = nzcmin
              else if( nzc<nzcmin .and. .not.zquery ) then
                 info = -14_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSTEMR', -info )
              return
           else if( lquery .or. zquery ) then
              return
           end if
           ! handle n = 0, 1, and 2 cases immediately
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( wl<d( 1_ilp ) .and. wu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz.and.(.not.zquery) ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz(1_ilp) = 1_ilp
                 isuppz(2_ilp) = 1_ilp
              end if
              return
           end if
           if( n==2_ilp ) then
              if( .not.wantz ) then
                 call stdlib_slae2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2 )
              else if( wantz.and.(.not.zquery) ) then
                 call stdlib_slaev2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2, cs, sn )
              end if
              if( alleig.or.(valeig.and.(r2>wl).and.(r2<=wu)).or.(indeig.and.(iil==1_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r2
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = -sn
                    z( 2_ilp, m ) = cs
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
              if( alleig.or.(valeig.and.(r1>wl).and.(r1<=wu)).or.(indeig.and.(iiu==2_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r1
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = cs
                    z( 2_ilp, m ) = sn
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
           else
              ! continue with general n
              indgrs = 1_ilp
              inderr = 2_ilp*n + 1_ilp
              indgp = 3_ilp*n + 1_ilp
              indd = 4_ilp*n + 1_ilp
              inde2 = 5_ilp*n + 1_ilp
              indwrk = 6_ilp*n + 1_ilp
              iinspl = 1_ilp
              iindbl = n + 1_ilp
              iindw = 2_ilp*n + 1_ilp
              iindwk = 3_ilp*n + 1_ilp
              ! scale matrix to allowable range, if necessary.
              ! the allowable range is related to the pivmin parameter; see the
              ! comments in stdlib_slarrd.  the preference for scaling small values
              ! up is heuristic; we expect users' matrices not to be close to the
              ! rmax threshold.
              scale = one
              tnrm = stdlib_slanst( 'M', n, d, e )
              if( tnrm>zero .and. tnrm<rmin ) then
                 scale = rmin / tnrm
              else if( tnrm>rmax ) then
                 scale = rmax / tnrm
              end if
              if( scale/=one ) then
                 call stdlib_sscal( n, scale, d, 1_ilp )
                 call stdlib_sscal( n-1, scale, e, 1_ilp )
                 tnrm = tnrm*scale
                 if( valeig ) then
                    ! if eigenvalues in interval have to be found,
                    ! scale (wl, wu] accordingly
                    wl = wl*scale
                    wu = wu*scale
                 endif
              end if
              ! compute the desired eigenvalues of the tridiagonal after splitting
              ! into smaller subblocks if the corresponding off-diagonal elements
              ! are small
              ! thresh is the splitting parameter for stdlib_slarre
              ! a negative thresh forces the old splitting criterion based on the
              ! size of the off-diagonal. a positive thresh switches to splitting
              ! which preserves relative accuracy.
              if( tryrac ) then
                 ! test whether the matrix warrants the more expensive relative approach.
                 call stdlib_slarrr( n, d, e, iinfo )
              else
                 ! the user does not care about relative accurately eigenvalues
                 iinfo = -1_ilp
              endif
              ! set the splitting criterion
              if (iinfo==0_ilp) then
                 thresh = eps
              else
                 thresh = -eps
                 ! relative accuracy is desired but t does not guarantee it
                 tryrac = .false.
              endif
              if( tryrac ) then
                 ! copy original diagonal, needed to guarantee relative accuracy
                 call stdlib_scopy(n,d,1_ilp,work(indd),1_ilp)
              endif
              ! store the squares of the offdiagonal values of t
              do j = 1, n-1
                 work( inde2+j-1 ) = e(j)**2_ilp
              end do
              ! set the tolerance parameters for bisection
              if( .not.wantz ) then
                 ! stdlib_slarre computes the eigenvalues to full precision.
                 rtol1 = four * eps
                 rtol2 = four * eps
              else
                 ! stdlib_slarre computes the eigenvalues to less than full precision.
                 ! stdlib_clarrv will refine the eigenvalue approximations, and we only
                 ! need less accurate initial bisection in stdlib_slarre.
                 ! note: these settings do only affect the subset case and stdlib_slarre
                 rtol1 = max( sqrt(eps)*5.0e-2_sp, four * eps )
                 rtol2 = max( sqrt(eps)*5.0e-3_sp, four * eps )
              endif
              call stdlib_slarre( range, n, wl, wu, iil, iiu, d, e,work(inde2), rtol1, rtol2, &
              thresh, nsplit,iwork( iinspl ), m, w, work( inderr ),work( indgp ), iwork( iindbl ),&
              iwork( iindw ), work( indgrs ), pivmin,work( indwrk ), iwork( iindwk ), iinfo )
                        
              if( iinfo/=0_ilp ) then
                 info = 10_ilp + abs( iinfo )
                 return
              end if
              ! note that if range /= 'v', stdlib_slarre computes bounds on the desired
              ! part of the spectrum. all desired eigenvalues are contained in
              ! (wl,wu]
              if( wantz ) then
                 ! compute the desired eigenvectors corresponding to the computed
                 ! eigenvalues
                 call stdlib_clarrv( n, wl, wu, d, e,pivmin, iwork( iinspl ), m,1_ilp, m, minrgp, &
                 rtol1, rtol2,w, work( inderr ), work( indgp ), iwork( iindbl ),iwork( iindw ), &
                           work( indgrs ), z, ldz,isuppz, work( indwrk ), iwork( iindwk ), iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 20_ilp + abs( iinfo )
                    return
                 end if
              else
                 ! stdlib_slarre computes eigenvalues of the (shifted) root representation
                 ! stdlib_clarrv returns the eigenvalues of the unshifted matrix.
                 ! however, if the eigenvectors are not desired by the user, we need
                 ! to apply the corresponding shifts from stdlib_slarre to obtain the
                 ! eigenvalues of the original matrix.
                 do j = 1, m
                    itmp = iwork( iindbl+j-1 )
                    w( j ) = w( j ) + e( iwork( iinspl+itmp-1 ) )
                 end do
              end if
              if ( tryrac ) then
                 ! refine computed eigenvalues so that they are relatively accurate
                 ! with respect to the original matrix t.
                 ibegin = 1_ilp
                 wbegin = 1_ilp
                 loop_39: do jblk = 1, iwork( iindbl+m-1 )
                    iend = iwork( iinspl+jblk-1 )
                    in = iend - ibegin + 1_ilp
                    wend = wbegin - 1_ilp
                    ! check if any eigenvalues have to be refined in this block
                    36 continue
                    if( wend<m ) then
                       if( iwork( iindbl+wend )==jblk ) then
                          wend = wend + 1_ilp
                          go to 36
                       end if
                    end if
                    if( wend<wbegin ) then
                       ibegin = iend + 1_ilp
                       cycle loop_39
                    end if
                    offset = iwork(iindw+wbegin-1)-1_ilp
                    ifirst = iwork(iindw+wbegin-1)
                    ilast = iwork(iindw+wend-1)
                    rtol2 = four * eps
                    call stdlib_slarrj( in,work(indd+ibegin-1), work(inde2+ibegin-1),ifirst, &
                    ilast, rtol2, offset, w(wbegin),work( inderr+wbegin-1 ),work( indwrk ), iwork(&
                               iindwk ), pivmin,tnrm, iinfo )
                    ibegin = iend + 1_ilp
                    wbegin = wend + 1_ilp
                 end do loop_39
              endif
              ! if matrix was scaled, then rescale eigenvalues appropriately.
              if( scale/=one ) then
                 call stdlib_sscal( m, one / scale, w, 1_ilp )
              end if
           end if
           ! if eigenvalues are not in increasing order, then sort them,
           ! possibly along with eigenvectors.
           if( nsplit>1_ilp .or. n==2_ilp ) then
              if( .not. wantz ) then
                 call stdlib_slasrt( 'I', m, w, iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 3_ilp
                    return
                 end if
              else
                 do j = 1, m - 1
                    i = 0_ilp
                    tmp = w( j )
                    do jj = j + 1, m
                       if( w( jj )<tmp ) then
                          i = jj
                          tmp = w( jj )
                       end if
                    end do
                    if( i/=0_ilp ) then
                       w( i ) = w( j )
                       w( j ) = tmp
                       if( wantz ) then
                          call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                          itmp = isuppz( 2_ilp*i-1 )
                          isuppz( 2_ilp*i-1 ) = isuppz( 2_ilp*j-1 )
                          isuppz( 2_ilp*j-1 ) = itmp
                          itmp = isuppz( 2_ilp*i )
                          isuppz( 2_ilp*i ) = isuppz( 2_ilp*j )
                          isuppz( 2_ilp*j ) = itmp
                       end if
                    end if
                 end do
              end if
           endif
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_cstemr

     pure module subroutine stdlib_zstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
     !! ZSTEMR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     !! a well defined set of pairwise different real eigenvalues, the corresponding
     !! real eigenvectors are pairwise orthogonal.
     !! The spectrum may be computed either completely or partially by specifying
     !! either an interval (VL,VU] or a range of indices IL:IU for the desired
     !! eigenvalues.
     !! Depending on the number of desired eigenvalues, these are computed either
     !! by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
     !! computed by the use of various suitable L D L^T factorizations near clusters
     !! of close eigenvalues (referred to as RRRs, Relatively Robust
     !! Representations). An informal sketch of the algorithm follows.
     !! For each unreduced block (submatrix) of T,
     !! (a) Compute T - sigma I  = L D L^T, so that L and D
     !! define all the wanted eigenvalues to high relative accuracy.
     !! This means that small relative changes in the entries of D and L
     !! cause only small relative changes in the eigenvalues and
     !! eigenvectors. The standard (unfactored) representation of the
     !! tridiagonal matrix T does not have this property in general.
     !! (b) Compute the eigenvalues to suitable accuracy.
     !! If the eigenvectors are desired, the algorithm attains full
     !! accuracy of the computed eigenvalues only right before
     !! the corresponding vectors have to be computed, see steps c) and d).
     !! (c) For each cluster of close eigenvalues, select a new
     !! shift close to the cluster, find a new factorization, and refine
     !! the shifted eigenvalues to suitable accuracy.
     !! (d) For each eigenvalue with a large enough relative separation compute
     !! the corresponding eigenvector by forming a rank revealing twisted
     !! factorization. Go back to (c) for any clusters that remain.
     !! For more details, see:
     !! - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
     !! to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
     !! Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
     !! - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
     !! Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
     !! 2004.  Also LAPACK Working Note 154.
     !! - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
     !! tridiagonal eigenvalue/eigenvector problem",
     !! Computer Science Division Technical Report No. UCB/CSD-97-971,
     !! UC Berkeley, May 1997.
     !! Further Details
     !! 1.ZSTEMR works only on machines which follow IEEE-754
     !! floating-point standard in their handling of infinities and NaNs.
     !! This permits the use of efficient inner loops avoiding a check for
     !! zero divisors.
     !! 2. LAPACK routines can be used to reduce a complex Hermitean matrix to
     !! real symmetric tridiagonal form.
     !! (Any complex Hermitean tridiagonal matrix has real values on its diagonal
     !! and potentially complex numbers on its off-diagonals. By applying a
     !! similarity transform with an appropriate diagonal matrix
     !! diag(1,e^{i \phy_1}, ... , e^{i \phy_{n-1}}), the complex Hermitean
     !! matrix can be transformed into a real symmetric matrix and complex
     !! arithmetic can be entirely avoided.)
     !! While the eigenvectors of the real symmetric tridiagonal matrix are real,
     !! the eigenvectors of original complex Hermitean matrix have complex entries
     !! in general.
     !! Since LAPACK drivers overwrite the matrix data with the eigenvectors,
     !! ZSTEMR accepts complex workspace to facilitate interoperability
     !! with ZUNMTR or ZUPMTR.
               isuppz, tryrac, work, lwork,iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           complex(dp), intent(out) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: minrgp = 1.0e-3_dp
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, valeig, wantz, zquery
           integer(ilp) :: i, ibegin, iend, ifirst, iil, iindbl, iindw, iindwk, iinfo, iinspl, &
           iiu, ilast, in, indd, inde2, inderr, indgp, indgrs, indwrk, itmp, itmp2, j, jblk, jj, &
                     liwmin, lwmin, nsplit, nzcmin, offset, wbegin, wend
           real(dp) :: bignum, cs, eps, pivmin, r1, r2, rmax, rmin, rtol1, rtol2, safmin, scale, &
                     smlnum, sn, thresh, tmp, tnrm, wl, wu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ).or.( liwork==-1_ilp ) )
           zquery = ( nzc==-1_ilp )
           ! stdlib_dstemr needs work of size 6*n, iwork of size 3*n.
           ! in addition, stdlib_dlarre needs work of size 6*n, iwork of size 5*n.
           ! furthermore, stdlib_zlarrv needs work of size 12*n, iwork of size 7*n.
           if( wantz ) then
              lwmin = 18_ilp*n
              liwmin = 10_ilp*n
           else
              ! need less workspace if only the eigenvalues are wanted
              lwmin = 12_ilp*n
              liwmin = 8_ilp*n
           endif
           wl = zero
           wu = zero
           iil = 0_ilp
           iiu = 0_ilp
           nsplit = 0_ilp
           if( valeig ) then
              ! we do not reference vl, vu in the cases range = 'i','a'
              ! the interval (wl, wu] contains all the wanted eigenvalues.
              ! it is either given by the user or computed in stdlib_dlarre.
              wl = vl
              wu = vu
           elseif( indeig ) then
              ! we do not reference il, iu in the cases range = 'v','a'
              iil = il
              iiu = iu
           endif
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( valeig .and. n>0_ilp .and. wu<=wl ) then
              info = -7_ilp
           else if( indeig .and. ( iil<1_ilp .or. iil>n ) ) then
              info = -8_ilp
           else if( indeig .and. ( iiu<iil .or. iiu>n ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -13_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -17_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! get machine constants.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           eps = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin / eps
           bignum = one / smlnum
           rmin = sqrt( smlnum )
           rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( wantz .and. alleig ) then
                 nzcmin = n
              else if( wantz .and. valeig ) then
                 call stdlib_dlarrc( 'T', n, vl, vu, d, e, safmin,nzcmin, itmp, itmp2, info )
                           
              else if( wantz .and. indeig ) then
                 nzcmin = iiu-iil+1
              else
                 ! wantz == false.
                 nzcmin = 0_ilp
              endif
              if( zquery .and. info==0_ilp ) then
                 z( 1_ilp,1_ilp ) = nzcmin
              else if( nzc<nzcmin .and. .not.zquery ) then
                 info = -14_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSTEMR', -info )
              return
           else if( lquery .or. zquery ) then
              return
           end if
           ! handle n = 0, 1, and 2 cases immediately
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = d( 1_ilp )
              else
                 if( wl<d( 1_ilp ) .and. wu>=d( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = d( 1_ilp )
                 end if
              end if
              if( wantz.and.(.not.zquery) ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz(1_ilp) = 1_ilp
                 isuppz(2_ilp) = 1_ilp
              end if
              return
           end if
           if( n==2_ilp ) then
              if( .not.wantz ) then
                 call stdlib_dlae2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2 )
              else if( wantz.and.(.not.zquery) ) then
                 call stdlib_dlaev2( d(1_ilp), e(1_ilp), d(2_ilp), r1, r2, cs, sn )
              end if
              if( alleig.or.(valeig.and.(r2>wl).and.(r2<=wu)).or.(indeig.and.(iil==1_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r2
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = -sn
                    z( 2_ilp, m ) = cs
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
              if( alleig.or.(valeig.and.(r1>wl).and.(r1<=wu)).or.(indeig.and.(iiu==2_ilp)) ) &
                        then
                 m = m+1
                 w( m ) = r1
                 if( wantz.and.(.not.zquery) ) then
                    z( 1_ilp, m ) = cs
                    z( 2_ilp, m ) = sn
                    ! note: at most one of sn and cs can be zero.
                    if (sn/=zero) then
                       if (cs/=zero) then
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 2_ilp
                       else
                          isuppz(2_ilp*m-1) = 1_ilp
                          isuppz(2_ilp*m) = 1_ilp
                       end if
                    else
                       isuppz(2_ilp*m-1) = 2_ilp
                       isuppz(2_ilp*m) = 2_ilp
                    end if
                 endif
              endif
           else
              ! continue with general n
              indgrs = 1_ilp
              inderr = 2_ilp*n + 1_ilp
              indgp = 3_ilp*n + 1_ilp
              indd = 4_ilp*n + 1_ilp
              inde2 = 5_ilp*n + 1_ilp
              indwrk = 6_ilp*n + 1_ilp
              iinspl = 1_ilp
              iindbl = n + 1_ilp
              iindw = 2_ilp*n + 1_ilp
              iindwk = 3_ilp*n + 1_ilp
              ! scale matrix to allowable range, if necessary.
              ! the allowable range is related to the pivmin parameter; see the
              ! comments in stdlib_dlarrd.  the preference for scaling small values
              ! up is heuristic; we expect users' matrices not to be close to the
              ! rmax threshold.
              scale = one
              tnrm = stdlib_dlanst( 'M', n, d, e )
              if( tnrm>zero .and. tnrm<rmin ) then
                 scale = rmin / tnrm
              else if( tnrm>rmax ) then
                 scale = rmax / tnrm
              end if
              if( scale/=one ) then
                 call stdlib_dscal( n, scale, d, 1_ilp )
                 call stdlib_dscal( n-1, scale, e, 1_ilp )
                 tnrm = tnrm*scale
                 if( valeig ) then
                    ! if eigenvalues in interval have to be found,
                    ! scale (wl, wu] accordingly
                    wl = wl*scale
                    wu = wu*scale
                 endif
              end if
              ! compute the desired eigenvalues of the tridiagonal after splitting
              ! into smaller subblocks if the corresponding off-diagonal elements
              ! are small
              ! thresh is the splitting parameter for stdlib_dlarre
              ! a negative thresh forces the old splitting criterion based on the
              ! size of the off-diagonal. a positive thresh switches to splitting
              ! which preserves relative accuracy.
              if( tryrac ) then
                 ! test whether the matrix warrants the more expensive relative approach.
                 call stdlib_dlarrr( n, d, e, iinfo )
              else
                 ! the user does not care about relative accurately eigenvalues
                 iinfo = -1_ilp
              endif
              ! set the splitting criterion
              if (iinfo==0_ilp) then
                 thresh = eps
              else
                 thresh = -eps
                 ! relative accuracy is desired but t does not guarantee it
                 tryrac = .false.
              endif
              if( tryrac ) then
                 ! copy original diagonal, needed to guarantee relative accuracy
                 call stdlib_dcopy(n,d,1_ilp,work(indd),1_ilp)
              endif
              ! store the squares of the offdiagonal values of t
              do j = 1, n-1
                 work( inde2+j-1 ) = e(j)**2_ilp
              end do
              ! set the tolerance parameters for bisection
              if( .not.wantz ) then
                 ! stdlib_dlarre computes the eigenvalues to full precision.
                 rtol1 = four * eps
                 rtol2 = four * eps
              else
                 ! stdlib_dlarre computes the eigenvalues to less than full precision.
                 ! stdlib_zlarrv will refine the eigenvalue approximations, and we only
                 ! need less accurate initial bisection in stdlib_dlarre.
                 ! note: these settings do only affect the subset case and stdlib_dlarre
                 rtol1 = sqrt(eps)
                 rtol2 = max( sqrt(eps)*5.0e-3_dp, four * eps )
              endif
              call stdlib_dlarre( range, n, wl, wu, iil, iiu, d, e,work(inde2), rtol1, rtol2, &
              thresh, nsplit,iwork( iinspl ), m, w, work( inderr ),work( indgp ), iwork( iindbl ),&
              iwork( iindw ), work( indgrs ), pivmin,work( indwrk ), iwork( iindwk ), iinfo )
                        
              if( iinfo/=0_ilp ) then
                 info = 10_ilp + abs( iinfo )
                 return
              end if
              ! note that if range /= 'v', stdlib_dlarre computes bounds on the desired
              ! part of the spectrum. all desired eigenvalues are contained in
              ! (wl,wu]
              if( wantz ) then
                 ! compute the desired eigenvectors corresponding to the computed
                 ! eigenvalues
                 call stdlib_zlarrv( n, wl, wu, d, e,pivmin, iwork( iinspl ), m,1_ilp, m, minrgp, &
                 rtol1, rtol2,w, work( inderr ), work( indgp ), iwork( iindbl ),iwork( iindw ), &
                           work( indgrs ), z, ldz,isuppz, work( indwrk ), iwork( iindwk ), iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 20_ilp + abs( iinfo )
                    return
                 end if
              else
                 ! stdlib_dlarre computes eigenvalues of the (shifted) root representation
                 ! stdlib_zlarrv returns the eigenvalues of the unshifted matrix.
                 ! however, if the eigenvectors are not desired by the user, we need
                 ! to apply the corresponding shifts from stdlib_dlarre to obtain the
                 ! eigenvalues of the original matrix.
                 do j = 1, m
                    itmp = iwork( iindbl+j-1 )
                    w( j ) = w( j ) + e( iwork( iinspl+itmp-1 ) )
                 end do
              end if
              if ( tryrac ) then
                 ! refine computed eigenvalues so that they are relatively accurate
                 ! with respect to the original matrix t.
                 ibegin = 1_ilp
                 wbegin = 1_ilp
                 loop_39: do jblk = 1, iwork( iindbl+m-1 )
                    iend = iwork( iinspl+jblk-1 )
                    in = iend - ibegin + 1_ilp
                    wend = wbegin - 1_ilp
                    ! check if any eigenvalues have to be refined in this block
                    36 continue
                    if( wend<m ) then
                       if( iwork( iindbl+wend )==jblk ) then
                          wend = wend + 1_ilp
                          go to 36
                       end if
                    end if
                    if( wend<wbegin ) then
                       ibegin = iend + 1_ilp
                       cycle loop_39
                    end if
                    offset = iwork(iindw+wbegin-1)-1_ilp
                    ifirst = iwork(iindw+wbegin-1)
                    ilast = iwork(iindw+wend-1)
                    rtol2 = four * eps
                    call stdlib_dlarrj( in,work(indd+ibegin-1), work(inde2+ibegin-1),ifirst, &
                    ilast, rtol2, offset, w(wbegin),work( inderr+wbegin-1 ),work( indwrk ), iwork(&
                               iindwk ), pivmin,tnrm, iinfo )
                    ibegin = iend + 1_ilp
                    wbegin = wend + 1_ilp
                 end do loop_39
              endif
              ! if matrix was scaled, then rescale eigenvalues appropriately.
              if( scale/=one ) then
                 call stdlib_dscal( m, one / scale, w, 1_ilp )
              end if
           end if
           ! if eigenvalues are not in increasing order, then sort them,
           ! possibly along with eigenvectors.
           if( nsplit>1_ilp .or. n==2_ilp ) then
              if( .not. wantz ) then
                 call stdlib_dlasrt( 'I', m, w, iinfo )
                 if( iinfo/=0_ilp ) then
                    info = 3_ilp
                    return
                 end if
              else
                 do j = 1, m - 1
                    i = 0_ilp
                    tmp = w( j )
                    do jj = j + 1, m
                       if( w( jj )<tmp ) then
                          i = jj
                          tmp = w( jj )
                       end if
                    end do
                    if( i/=0_ilp ) then
                       w( i ) = w( j )
                       w( j ) = tmp
                       if( wantz ) then
                          call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                          itmp = isuppz( 2_ilp*i-1 )
                          isuppz( 2_ilp*i-1 ) = isuppz( 2_ilp*j-1 )
                          isuppz( 2_ilp*j-1 ) = itmp
                          itmp = isuppz( 2_ilp*i )
                          isuppz( 2_ilp*i ) = isuppz( 2_ilp*j )
                          isuppz( 2_ilp*j ) = itmp
                       end if
                    end if
                 end do
              end if
           endif
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zstemr




     pure module subroutine stdlib_ssteqr( compz, n, d, e, z, ldz, work, info )
     !! SSTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the implicit QL or QR method.
     !! The eigenvectors of a full or band symmetric matrix can also be found
     !! if SSYTRD or SSPTRD or SSBTRD has been used to reduce this matrix to
     !! tridiagonal form.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, icompz, ii, iscale, j, jtot, k, l, l1, lend, lendm1, lendp1, lendsv,&
                      lm1, lsv, m, mm, mm1, nm1, nmaxit
           real(sp) :: anorm, b, c, eps, eps2, f, g, p, r, rt1, rt2, s, safmax, safmin, ssfmax, &
                     ssfmin, tst
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz==2_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! determine the unit roundoff and over/underflow thresholds.
           eps = stdlib_slamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           ! compute the eigenvalues and eigenvectors of the tridiagonal
           ! matrix.
           if( icompz==2_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
           nmaxit = n*maxit
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           nm1 = n - 1_ilp
           10 continue
           if( l1>n )go to 160
           if( l1>1_ilp )e( l1-1 ) = zero
           if( l1<=nm1 ) then
              do m = l1, nm1
                 tst = abs( e( m ) )
                 if( tst==zero )go to 30
                 if( tst<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) then
                    e( m ) = zero
                    go to 30
                 end if
              end do
           end if
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_slanst( 'M', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( anorm>ssfmax ) then
              iscale = 1_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              40 continue
              if( l/=lend ) then
                 lendm1 = lend - 1_ilp
                 do m = l, lendm1
                    tst = abs( e( m ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m+1 ) )+safmin )go to 60
                 end do
              end if
              m = lend
              60 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 80
              ! if remaining matrix is 2-by-2, use stdlib_slae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l+1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_slaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
                    work( l ) = c
                    work( n-1+l ) = s
                    call stdlib_slasr( 'R', 'V', 'B', n, 2_ilp, work( l ),work( n-1+l ), z( 1_ilp, l ), &
                              ldz )
                 else
                    call stdlib_slae2( d( l ), e( l ), d( l+1 ), rt1, rt2 )
                 end if
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 40
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l+1 )-p ) / ( two*e( l ) )
              r = stdlib_slapy2( g, one )
              g = d( m ) - p + ( e( l ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              mm1 = m - 1_ilp
              do i = mm1, l, -1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_slartg( g, f, c, s, r )
                 if( i/=m-1 )e( i+1 ) = r
                 g = d( i+1 ) - p
                 r = ( d( i )-g )*s + two*c*b
                 p = s*r
                 d( i+1 ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = -s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = m - l + 1_ilp
                 call stdlib_slasr( 'R', 'V', 'B', n, mm, work( l ), work( n-1+l ),z( 1_ilp, l ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( l ) = g
              go to 40
              ! eigenvalue found.
              80 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 40
              go to 140
           else
              ! qr iteration
              ! look for small superdiagonal element.
              90 continue
              if( l/=lend ) then
                 lendp1 = lend + 1_ilp
                 do m = l, lendp1, -1
                    tst = abs( e( m-1 ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m-1 ) )+safmin )go to 110
                 end do
              end if
              m = lend
              110 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 130
              ! if remaining matrix is 2-by-2, use stdlib_slae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l-1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_slaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s )
                    work( m ) = c
                    work( n-1+m ) = s
                    call stdlib_slasr( 'R', 'V', 'F', n, 2_ilp, work( m ),work( n-1+m ), z( 1_ilp, l-1 ), &
                              ldz )
                 else
                    call stdlib_slae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 )
                 end if
                 d( l-1 ) = rt1
                 d( l ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 90
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l-1 )-p ) / ( two*e( l-1 ) )
              r = stdlib_slapy2( g, one )
              g = d( m ) - p + ( e( l-1 ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              lm1 = l - 1_ilp
              do i = m, lm1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_slartg( g, f, c, s, r )
                 if( i/=m )e( i-1 ) = r
                 g = d( i ) - p
                 r = ( d( i+1 )-g )*s + two*c*b
                 p = s*r
                 d( i ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = l - m + 1_ilp
                 call stdlib_slasr( 'R', 'V', 'F', n, mm, work( m ), work( n-1+m ),z( 1_ilp, m ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( lm1 ) = g
              go to 90
              ! eigenvalue found.
              130 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 90
              go to 140
           end if
           ! undo scaling if necessary
           140 continue
           if( iscale==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           else if( iscale==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           end if
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot<nmaxit )go to 10
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           go to 190
           ! order eigenvalues and eigenvectors.
           160 continue
           if( icompz==0_ilp ) then
              ! use quick sort
              call stdlib_slasrt( 'I', n, d, info )
           else
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                 i = ii - 1_ilp
                 k = i
                 p = d( i )
                 do j = ii, n
                    if( d( j )<p ) then
                       k = j
                       p = d( j )
                    end if
                 end do
                 if( k/=i ) then
                    d( k ) = d( i )
                    d( i ) = p
                    call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                 end if
              end do
           end if
           190 continue
           return
     end subroutine stdlib_ssteqr

     pure module subroutine stdlib_dsteqr( compz, n, d, e, z, ldz, work, info )
     !! DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the implicit QL or QR method.
     !! The eigenvectors of a full or band symmetric matrix can also be found
     !! if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
     !! tridiagonal form.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, icompz, ii, iscale, j, jtot, k, l, l1, lend, lendm1, lendp1, lendsv,&
                      lm1, lsv, m, mm, mm1, nm1, nmaxit
           real(dp) :: anorm, b, c, eps, eps2, f, g, p, r, rt1, rt2, s, safmax, safmin, ssfmax, &
                     ssfmin, tst
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz==2_ilp )z( 1_ilp, 1_ilp ) = one
              return
           end if
           ! determine the unit roundoff and over/underflow thresholds.
           eps = stdlib_dlamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           ! compute the eigenvalues and eigenvectors of the tridiagonal
           ! matrix.
           if( icompz==2_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
           nmaxit = n*maxit
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           nm1 = n - 1_ilp
           10 continue
           if( l1>n )go to 160
           if( l1>1_ilp )e( l1-1 ) = zero
           if( l1<=nm1 ) then
              do m = l1, nm1
                 tst = abs( e( m ) )
                 if( tst==zero )go to 30
                 if( tst<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) then
                    e( m ) = zero
                    go to 30
                 end if
              end do
           end if
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_dlanst( 'M', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( anorm>ssfmax ) then
              iscale = 1_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              40 continue
              if( l/=lend ) then
                 lendm1 = lend - 1_ilp
                 do m = l, lendm1
                    tst = abs( e( m ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m+1 ) )+safmin )go to 60
                 end do
              end if
              m = lend
              60 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 80
              ! if remaining matrix is 2-by-2, use stdlib_dlae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l+1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
                    work( l ) = c
                    work( n-1+l ) = s
                    call stdlib_dlasr( 'R', 'V', 'B', n, 2_ilp, work( l ),work( n-1+l ), z( 1_ilp, l ), &
                              ldz )
                 else
                    call stdlib_dlae2( d( l ), e( l ), d( l+1 ), rt1, rt2 )
                 end if
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 40
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l+1 )-p ) / ( two*e( l ) )
              r = stdlib_dlapy2( g, one )
              g = d( m ) - p + ( e( l ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              mm1 = m - 1_ilp
              do i = mm1, l, -1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_dlartg( g, f, c, s, r )
                 if( i/=m-1 )e( i+1 ) = r
                 g = d( i+1 ) - p
                 r = ( d( i )-g )*s + two*c*b
                 p = s*r
                 d( i+1 ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = -s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = m - l + 1_ilp
                 call stdlib_dlasr( 'R', 'V', 'B', n, mm, work( l ), work( n-1+l ),z( 1_ilp, l ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( l ) = g
              go to 40
              ! eigenvalue found.
              80 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 40
              go to 140
           else
              ! qr iteration
              ! look for small superdiagonal element.
              90 continue
              if( l/=lend ) then
                 lendp1 = lend + 1_ilp
                 do m = l, lendp1, -1
                    tst = abs( e( m-1 ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m-1 ) )+safmin )go to 110
                 end do
              end if
              m = lend
              110 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 130
              ! if remaining matrix is 2-by-2, use stdlib_dlae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l-1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_dlaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s )
                    work( m ) = c
                    work( n-1+m ) = s
                    call stdlib_dlasr( 'R', 'V', 'F', n, 2_ilp, work( m ),work( n-1+m ), z( 1_ilp, l-1 ), &
                              ldz )
                 else
                    call stdlib_dlae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 )
                 end if
                 d( l-1 ) = rt1
                 d( l ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 90
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l-1 )-p ) / ( two*e( l-1 ) )
              r = stdlib_dlapy2( g, one )
              g = d( m ) - p + ( e( l-1 ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              lm1 = l - 1_ilp
              do i = m, lm1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_dlartg( g, f, c, s, r )
                 if( i/=m )e( i-1 ) = r
                 g = d( i ) - p
                 r = ( d( i+1 )-g )*s + two*c*b
                 p = s*r
                 d( i ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = l - m + 1_ilp
                 call stdlib_dlasr( 'R', 'V', 'F', n, mm, work( m ), work( n-1+m ),z( 1_ilp, m ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( lm1 ) = g
              go to 90
              ! eigenvalue found.
              130 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 90
              go to 140
           end if
           ! undo scaling if necessary
           140 continue
           if( iscale==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           else if( iscale==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           end if
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot<nmaxit )go to 10
           do i = 1, n - 1
              if( e( i )/=zero )info = info + 1_ilp
           end do
           go to 190
           ! order eigenvalues and eigenvectors.
           160 continue
           if( icompz==0_ilp ) then
              ! use quick sort
              call stdlib_dlasrt( 'I', n, d, info )
           else
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                 i = ii - 1_ilp
                 k = i
                 p = d( i )
                 do j = ii, n
                    if( d( j )<p ) then
                       k = j
                       p = d( j )
                    end if
                 end do
                 if( k/=i ) then
                    d( k ) = d( i )
                    d( i ) = p
                    call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                 end if
              end do
           end if
           190 continue
           return
     end subroutine stdlib_dsteqr


     pure module subroutine stdlib_csteqr( compz, n, d, e, z, ldz, work, info )
     !! CSTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the implicit QL or QR method.
     !! The eigenvectors of a full or band complex Hermitian matrix can also
     !! be found if CHETRD or CHPTRD or CHBTRD has been used to reduce this
     !! matrix to tridiagonal form.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, icompz, ii, iscale, j, jtot, k, l, l1, lend, lendm1, lendp1, lendsv,&
                      lm1, lsv, m, mm, mm1, nm1, nmaxit
           real(sp) :: anorm, b, c, eps, eps2, f, g, p, r, rt1, rt2, s, safmax, safmin, ssfmax, &
                     ssfmin, tst
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz==2_ilp )z( 1_ilp, 1_ilp ) = cone
              return
           end if
           ! determine the unit roundoff and over/underflow thresholds.
           eps = stdlib_slamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           ! compute the eigenvalues and eigenvectors of the tridiagonal
           ! matrix.
           if( icompz==2_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, z, ldz )
           nmaxit = n*maxit
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           nm1 = n - 1_ilp
           10 continue
           if( l1>n )go to 160
           if( l1>1_ilp )e( l1-1 ) = zero
           if( l1<=nm1 ) then
              do m = l1, nm1
                 tst = abs( e( m ) )
                 if( tst==zero )go to 30
                 if( tst<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) then
                    e( m ) = zero
                    go to 30
                 end if
              end do
           end if
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_slanst( 'I', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( anorm>ssfmax ) then
              iscale = 1_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              40 continue
              if( l/=lend ) then
                 lendm1 = lend - 1_ilp
                 do m = l, lendm1
                    tst = abs( e( m ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m+1 ) )+safmin )go to 60
                 end do
              end if
              m = lend
              60 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 80
              ! if remaining matrix is 2-by-2, use stdlib_slae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l+1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_slaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
                    work( l ) = c
                    work( n-1+l ) = s
                    call stdlib_clasr( 'R', 'V', 'B', n, 2_ilp, work( l ),work( n-1+l ), z( 1_ilp, l ), &
                              ldz )
                 else
                    call stdlib_slae2( d( l ), e( l ), d( l+1 ), rt1, rt2 )
                 end if
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 40
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l+1 )-p ) / ( two*e( l ) )
              r = stdlib_slapy2( g, one )
              g = d( m ) - p + ( e( l ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              mm1 = m - 1_ilp
              do i = mm1, l, -1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_slartg( g, f, c, s, r )
                 if( i/=m-1 )e( i+1 ) = r
                 g = d( i+1 ) - p
                 r = ( d( i )-g )*s + two*c*b
                 p = s*r
                 d( i+1 ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = -s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = m - l + 1_ilp
                 call stdlib_clasr( 'R', 'V', 'B', n, mm, work( l ), work( n-1+l ),z( 1_ilp, l ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( l ) = g
              go to 40
              ! eigenvalue found.
              80 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 40
              go to 140
           else
              ! qr iteration
              ! look for small superdiagonal element.
              90 continue
              if( l/=lend ) then
                 lendp1 = lend + 1_ilp
                 do m = l, lendp1, -1
                    tst = abs( e( m-1 ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m-1 ) )+safmin )go to 110
                 end do
              end if
              m = lend
              110 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 130
              ! if remaining matrix is 2-by-2, use stdlib_slae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l-1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_slaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s )
                    work( m ) = c
                    work( n-1+m ) = s
                    call stdlib_clasr( 'R', 'V', 'F', n, 2_ilp, work( m ),work( n-1+m ), z( 1_ilp, l-1 ), &
                              ldz )
                 else
                    call stdlib_slae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 )
                 end if
                 d( l-1 ) = rt1
                 d( l ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 90
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l-1 )-p ) / ( two*e( l-1 ) )
              r = stdlib_slapy2( g, one )
              g = d( m ) - p + ( e( l-1 ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              lm1 = l - 1_ilp
              do i = m, lm1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_slartg( g, f, c, s, r )
                 if( i/=m )e( i-1 ) = r
                 g = d( i ) - p
                 r = ( d( i+1 )-g )*s + two*c*b
                 p = s*r
                 d( i ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = l - m + 1_ilp
                 call stdlib_clasr( 'R', 'V', 'F', n, mm, work( m ), work( n-1+m ),z( 1_ilp, m ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( lm1 ) = g
              go to 90
              ! eigenvalue found.
              130 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 90
              go to 140
           end if
           ! undo scaling if necessary
           140 continue
           if( iscale==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           else if( iscale==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           end if
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot==nmaxit ) then
              do i = 1, n - 1
                 if( e( i )/=zero )info = info + 1_ilp
              end do
              return
           end if
           go to 10
           ! order eigenvalues and eigenvectors.
           160 continue
           if( icompz==0_ilp ) then
              ! use quick sort
              call stdlib_slasrt( 'I', n, d, info )
           else
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                 i = ii - 1_ilp
                 k = i
                 p = d( i )
                 do j = ii, n
                    if( d( j )<p ) then
                       k = j
                       p = d( j )
                    end if
                 end do
                 if( k/=i ) then
                    d( k ) = d( i )
                    d( i ) = p
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_csteqr

     pure module subroutine stdlib_zsteqr( compz, n, d, e, z, ldz, work, info )
     !! ZSTEQR computes all eigenvalues and, optionally, eigenvectors of a
     !! symmetric tridiagonal matrix using the implicit QL or QR method.
     !! The eigenvectors of a full or band complex Hermitian matrix can also
     !! be found if ZHETRD or ZHPTRD or ZHBTRD has been used to reduce this
     !! matrix to tridiagonal form.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 30_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, icompz, ii, iscale, j, jtot, k, l, l1, lend, lendm1, lendp1, lendsv,&
                      lm1, lsv, m, mm, mm1, nm1, nmaxit
           real(dp) :: anorm, b, c, eps, eps2, f, g, p, r, rt1, rt2, s, safmax, safmin, ssfmax, &
                     ssfmin, tst
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( stdlib_lsame( compz, 'N' ) ) then
              icompz = 0_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              icompz = 2_ilp
           else
              icompz = -1_ilp
           end if
           if( icompz<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ( ldz<1_ilp ) .or. ( icompz>0_ilp .and. ldz<max( 1_ilp,n ) ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSTEQR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( icompz==2_ilp )z( 1_ilp, 1_ilp ) = cone
              return
           end if
           ! determine the unit roundoff and over/underflow thresholds.
           eps = stdlib_dlamch( 'E' )
           eps2 = eps**2_ilp
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           ssfmax = sqrt( safmax ) / three
           ssfmin = sqrt( safmin ) / eps2
           ! compute the eigenvalues and eigenvectors of the tridiagonal
           ! matrix.
           if( icompz==2_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, z, ldz )
           nmaxit = n*maxit
           jtot = 0_ilp
           ! determine where the matrix splits and choose ql or qr iteration
           ! for each block, according to whether top or bottom diagonal
           ! element is smaller.
           l1 = 1_ilp
           nm1 = n - 1_ilp
           10 continue
           if( l1>n )go to 160
           if( l1>1_ilp )e( l1-1 ) = zero
           if( l1<=nm1 ) then
              do m = l1, nm1
                 tst = abs( e( m ) )
                 if( tst==zero )go to 30
                 if( tst<=( sqrt( abs( d( m ) ) )*sqrt( abs( d( m+1 ) ) ) )*eps ) then
                    e( m ) = zero
                    go to 30
                 end if
              end do
           end if
           m = n
           30 continue
           l = l1
           lsv = l
           lend = m
           lendsv = lend
           l1 = m + 1_ilp
           if( lend==l )go to 10
           ! scale submatrix in rows and columns l to lend
           anorm = stdlib_dlanst( 'I', lend-l+1, d( l ), e( l ) )
           iscale = 0_ilp
           if( anorm==zero )go to 10
           if( anorm>ssfmax ) then
              iscale = 1_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmax, lend-l, 1_ilp, e( l ), n,info )
           else if( anorm<ssfmin ) then
              iscale = 2_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l+1, 1_ilp, d( l ), n,info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anorm, ssfmin, lend-l, 1_ilp, e( l ), n,info )
           end if
           ! choose between ql and qr iteration
           if( abs( d( lend ) )<abs( d( l ) ) ) then
              lend = lsv
              l = lendsv
           end if
           if( lend>l ) then
              ! ql iteration
              ! look for small subdiagonal element.
              40 continue
              if( l/=lend ) then
                 lendm1 = lend - 1_ilp
                 do m = l, lendm1
                    tst = abs( e( m ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m+1 ) )+safmin )go to 60
                 end do
              end if
              m = lend
              60 continue
              if( m<lend )e( m ) = zero
              p = d( l )
              if( m==l )go to 80
              ! if remaining matrix is 2-by-2, use stdlib_dlae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l+1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
                    work( l ) = c
                    work( n-1+l ) = s
                    call stdlib_zlasr( 'R', 'V', 'B', n, 2_ilp, work( l ),work( n-1+l ), z( 1_ilp, l ), &
                              ldz )
                 else
                    call stdlib_dlae2( d( l ), e( l ), d( l+1 ), rt1, rt2 )
                 end if
                 d( l ) = rt1
                 d( l+1 ) = rt2
                 e( l ) = zero
                 l = l + 2_ilp
                 if( l<=lend )go to 40
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l+1 )-p ) / ( two*e( l ) )
              r = stdlib_dlapy2( g, one )
              g = d( m ) - p + ( e( l ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              mm1 = m - 1_ilp
              do i = mm1, l, -1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_dlartg( g, f, c, s, r )
                 if( i/=m-1 )e( i+1 ) = r
                 g = d( i+1 ) - p
                 r = ( d( i )-g )*s + two*c*b
                 p = s*r
                 d( i+1 ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = -s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = m - l + 1_ilp
                 call stdlib_zlasr( 'R', 'V', 'B', n, mm, work( l ), work( n-1+l ),z( 1_ilp, l ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( l ) = g
              go to 40
              ! eigenvalue found.
              80 continue
              d( l ) = p
              l = l + 1_ilp
              if( l<=lend )go to 40
              go to 140
           else
              ! qr iteration
              ! look for small superdiagonal element.
              90 continue
              if( l/=lend ) then
                 lendp1 = lend + 1_ilp
                 do m = l, lendp1, -1
                    tst = abs( e( m-1 ) )**2_ilp
                    if( tst<=( eps2*abs( d( m ) ) )*abs( d( m-1 ) )+safmin )go to 110
                 end do
              end if
              m = lend
              110 continue
              if( m>lend )e( m-1 ) = zero
              p = d( l )
              if( m==l )go to 130
              ! if remaining matrix is 2-by-2, use stdlib_dlae2 or stdlib_slaev2
              ! to compute its eigensystem.
              if( m==l-1 ) then
                 if( icompz>0_ilp ) then
                    call stdlib_dlaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s )
                    work( m ) = c
                    work( n-1+m ) = s
                    call stdlib_zlasr( 'R', 'V', 'F', n, 2_ilp, work( m ),work( n-1+m ), z( 1_ilp, l-1 ), &
                              ldz )
                 else
                    call stdlib_dlae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 )
                 end if
                 d( l-1 ) = rt1
                 d( l ) = rt2
                 e( l-1 ) = zero
                 l = l - 2_ilp
                 if( l>=lend )go to 90
                 go to 140
              end if
              if( jtot==nmaxit )go to 140
              jtot = jtot + 1_ilp
              ! form shift.
              g = ( d( l-1 )-p ) / ( two*e( l-1 ) )
              r = stdlib_dlapy2( g, one )
              g = d( m ) - p + ( e( l-1 ) / ( g+sign( r, g ) ) )
              s = one
              c = one
              p = zero
              ! inner loop
              lm1 = l - 1_ilp
              do i = m, lm1
                 f = s*e( i )
                 b = c*e( i )
                 call stdlib_dlartg( g, f, c, s, r )
                 if( i/=m )e( i-1 ) = r
                 g = d( i ) - p
                 r = ( d( i+1 )-g )*s + two*c*b
                 p = s*r
                 d( i ) = g + p
                 g = c*r - b
                 ! if eigenvectors are desired, then save rotations.
                 if( icompz>0_ilp ) then
                    work( i ) = c
                    work( n-1+i ) = s
                 end if
              end do
              ! if eigenvectors are desired, then apply saved rotations.
              if( icompz>0_ilp ) then
                 mm = l - m + 1_ilp
                 call stdlib_zlasr( 'R', 'V', 'F', n, mm, work( m ), work( n-1+m ),z( 1_ilp, m ), ldz &
                           )
              end if
              d( l ) = d( l ) - p
              e( lm1 ) = g
              go to 90
              ! eigenvalue found.
              130 continue
              d( l ) = p
              l = l - 1_ilp
              if( l>=lend )go to 90
              go to 140
           end if
           ! undo scaling if necessary
           140 continue
           if( iscale==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmax, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           else if( iscale==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv+1, 1_ilp,d( lsv ), n, info )
                        
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, ssfmin, anorm, lendsv-lsv, 1_ilp, e( lsv ),n, info )
                        
           end if
           ! check for no convergence to an eigenvalue after a total
           ! of n*maxit iterations.
           if( jtot==nmaxit ) then
              do i = 1, n - 1
                 if( e( i )/=zero )info = info + 1_ilp
              end do
              return
           end if
           go to 10
           ! order eigenvalues and eigenvectors.
           160 continue
           if( icompz==0_ilp ) then
              ! use quick sort
              call stdlib_dlasrt( 'I', n, d, info )
           else
              ! use selection sort to minimize swaps of eigenvectors
              do ii = 2, n
                 i = ii - 1_ilp
                 k = i
                 p = d( i )
                 do j = ii, n
                    if( d( j )<p ) then
                       k = j
                       p = d( j )
                    end if
                 end do
                 if( k/=i ) then
                    d( k ) = d( i )
                    d( i ) = p
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, k ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_zsteqr



end submodule stdlib_lapack_eigv_tridiag3
