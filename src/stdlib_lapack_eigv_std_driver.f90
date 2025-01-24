submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_std_driver
  implicit none


  contains

     module subroutine stdlib_ssyev( jobz, uplo, n, a, lda, w, work, lwork, info )
     !! SSYEV computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale, llwork, lwkopt, nb
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb+2 )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 3_ilp*n-1 ) .and. .not.lquery )info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              w( 1_ilp ) = a( 1_ilp, 1_ilp )
              work( 1_ilp ) = 2_ilp
              if( wantz )a( 1_ilp, 1_ilp ) = one
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
           anrm = stdlib_slansy( 'M', uplo, n, a, lda, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_slascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_ssytrd to reduce symmetric matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_ssytrd( uplo, n, a, lda, w, work( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_sorgtr to generate the orthogonal matrix, then call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_sorgtr( uplo, n, a, lda, work( indtau ), work( indwrk ),llwork, iinfo )
                        
              call stdlib_ssteqr( jobz, n, w, work( inde ), a, lda, work( indtau ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssyev

     module subroutine stdlib_dsyev( jobz, uplo, n, a, lda, w, work, lwork, info )
     !! DSYEV computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale, llwork, lwkopt, nb
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb+2 )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 3_ilp*n-1 ) .and. .not.lquery )info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              w( 1_ilp ) = a( 1_ilp, 1_ilp )
              work( 1_ilp ) = 2_ilp
              if( wantz )a( 1_ilp, 1_ilp ) = one
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
           anrm = stdlib_dlansy( 'M', uplo, n, a, lda, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_dlascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_dsytrd to reduce symmetric matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_dsytrd( uplo, n, a, lda, w, work( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_dorgtr to generate the orthogonal matrix, then call stdlib_dsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dorgtr( uplo, n, a, lda, work( indtau ), work( indwrk ),llwork, iinfo )
                        
              call stdlib_dsteqr( jobz, n, w, work( inde ), a, lda, work( indtau ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsyev




     module subroutine stdlib_ssyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork,liwork, info )
     !! SSYEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A. If eigenvectors are desired, it uses a
     !! divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
     !! Because of large use of BLAS of level 3, SSYEVD needs N**2 more
     !! workspace than SSYEVX.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, inde, indtau, indwk2, indwrk, iscale, liopt, liwmin, llwork, &
                     llwrk2, lopt, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
                 lopt = lwmin
                 liopt = liwmin
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n + 1_ilp
                 end if
                 lopt = max( lwmin, 2_ilp*n +stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                           
                 liopt = liwmin
              end if
              work( 1_ilp ) = lopt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = a( 1_ilp, 1_ilp )
              if( wantz )a( 1_ilp, 1_ilp ) = one
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
           anrm = stdlib_slansy( 'M', uplo, n, a, lda, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_slascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_ssytrd to reduce symmetric matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_ssytrd( uplo, n, a, lda, w, work( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_sstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_sormtr to multiply it by the
           ! householder transformations stored in a.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_sstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_sormtr( 'L', uplo, 'N', n, n, a, lda, work( indtau ),work( indwrk ), n, &
                        work( indwk2 ), llwrk2, iinfo )
              call stdlib_slacpy( 'A', n, n, work( indwrk ), n, a, lda )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_sscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lopt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_ssyevd

     module subroutine stdlib_dsyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork,liwork, info )
     !! DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A. If eigenvectors are desired, it uses a
     !! divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
     !! Because of large use of BLAS of level 3, DSYEVD needs N**2 more
     !! workspace than DSYEVX.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, inde, indtau, indwk2, indwrk, iscale, liopt, liwmin, llwork, &
                     llwrk2, lopt, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
                 lopt = lwmin
                 liopt = liwmin
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n + 1_ilp
                 end if
                 lopt = max( lwmin, 2_ilp*n +stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                           
                 liopt = liwmin
              end if
              work( 1_ilp ) = lopt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -10_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = a( 1_ilp, 1_ilp )
              if( wantz )a( 1_ilp, 1_ilp ) = one
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
           anrm = stdlib_dlansy( 'M', uplo, n, a, lda, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_dlascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_dsytrd to reduce symmetric matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_dsytrd( uplo, n, a, lda, w, work( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_dstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_dormtr to multiply it by the
           ! householder transformations stored in a.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_dormtr( 'L', uplo, 'N', n, n, a, lda, work( indtau ),work( indwrk ), n, &
                        work( indwk2 ), llwrk2, iinfo )
              call stdlib_dlacpy( 'A', n, n, work( indwrk ), n, a, lda )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_dscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lopt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_dsyevd




     module subroutine stdlib_ssyevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! SSYEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
     !! SSYEVR first reduces the matrix A to tridiagonal form T with a call
     !! to SSYTRD.  Then, whenever possible, SSYEVR calls SSTEMR to compute
     !! the eigenspectrum using Relatively Robust Representations.  SSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows.
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
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see SSTEMR's documentation and:
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
     !! Note 1 : SSYEVR calls SSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! SSYEVR calls SSTEBZ and SSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of SSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork,iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, iinfo, imax, indd, inddd, inde, indee, indibl, indifl, &
           indisp, indiwo, indtau, indwk, indwkn, iscale, j, jj, liwmin, llwork, llwrkn, lwkopt, &
                     lwmin, nb, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'SSYEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( liwork==-1_ilp ) )
           lwmin = max( 1_ilp, 26_ilp*n )
           liwmin = max( 1_ilp, 10_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              lwkopt = max( ( nb+1 )*n, lwmin )
              work( 1_ilp ) = lwkopt
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( n==1_ilp ) then
              work( 1_ilp ) = 26_ilp
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = a( 1_ilp, 1_ilp )
              else
                 if( vl<a( 1_ilp, 1_ilp ) .and. vu>=a( 1_ilp, 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = a( 1_ilp, 1_ilp )
                 end if
              end if
              if( wantz ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz( 1_ilp ) = 1_ilp
                 isuppz( 2_ilp ) = 1_ilp
              end if
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
           abstll = abstol
           if (valeig) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_slansy( 'M', uplo, n, a, lda, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_sscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_sscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: the iwork indices are
           ! used only if stdlib_ssterf or stdlib_sstemr fail.
           ! work(indtau:indtau+n-1) stores the scalar factors of the
           ! elementary reflectors used in stdlib_ssytrd.
           indtau = 1_ilp
           ! work(indd:indd+n-1) stores the tridiagonal's diagonal entries.
           indd = indtau + n
           ! work(inde:inde+n-1) stores the off-diagonal entries of the
           ! tridiagonal matrix from stdlib_ssytrd.
           inde = indd + n
           ! work(inddd:inddd+n-1) is a copy of the diagonal entries over
           ! -written by stdlib_sstemr (the stdlib_ssterf path copies the diagonal to w).
           inddd = inde + n
           ! work(indee:indee+n-1) is a copy of the off-diagonal entries over
           ! -written while computing the eigenvalues in stdlib_ssterf and stdlib_sstemr.
           indee = inddd + n
           ! indwk is the starting offset of the left-over workspace, and
           ! llwork is the remaining workspace size.
           indwk = indee + n
           llwork = lwork - indwk + 1_ilp
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
           indiwo = indifl + n
           ! call stdlib_ssytrd to reduce symmetric matrix to tridiagonal form.
           call stdlib_ssytrd( uplo, n, a, lda, work( indd ), work( inde ),work( indtau ), work( &
                     indwk ), llwork, iinfo )
           ! if all eigenvalues are desired
           ! then call stdlib_ssterf or stdlib_sstemr and stdlib_sormtr.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig.or.test ) .and. ( ieeeok==1_ilp ) ) then
              if( .not.wantz ) then
                 call stdlib_scopy( n, work( indd ), 1_ilp, w, 1_ilp )
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_scopy( n, work( indd ), 1_ilp, work( inddd ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_sstemr( jobz, 'A', n, work( inddd ), work( indee ),vl, vu, il, iu, m,&
                            w, z, ldz, n, isuppz,tryrac, work( indwk ), lwork, iwork, liwork,info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstemr.
                 if( wantz .and. info==0_ilp ) then
                    indwkn = inde
                    llwrkn = lwork - indwkn + 1_ilp
                    call stdlib_sormtr( 'L', uplo, 'N', n, m, a, lda,work( indtau ), z, ldz, work(&
                               indwkn ),llwrkn, iinfo )
                 end if
              end if
              if( info==0_ilp ) then
                 ! everything worked.  skip stdlib_sstebz/stdlib_sstein.  iwork(:) are
                 ! undefined.
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_sstein.
           ! also call stdlib_sstebz and stdlib_sstein if stdlib_sstemr fails.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwk ),iwork( indiwo ), info )
                     
           if( wantz ) then
              call stdlib_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwk ), iwork( indiwo ), iwork( indifl ),info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstein.
              indwkn = inde
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_sormtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
        ! jump here if stdlib_sstemr/stdlib_sstein succeeded.
        30 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.  note: we do not sort the ifail portion of iwork.
           ! it may not be initialized (if stdlib_sstemr/stdlib_sstein succeeded), and we do
           ! not return this detailed information to the user.
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
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_ssyevr

     module subroutine stdlib_dsyevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! DSYEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
     !! DSYEVR first reduces the matrix A to tridiagonal form T with a call
     !! to DSYTRD.  Then, whenever possible, DSYEVR calls DSTEMR to compute
     !! the eigenspectrum using Relatively Robust Representations.  DSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows.
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
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see DSTEMR's documentation and:
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
     !! Note 1 : DSYEVR calls DSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! DSYEVR calls DSTEBZ and DSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of DSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork,iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, iinfo, imax, indd, inddd, inde, indee, indibl, indifl, &
           indisp, indiwo, indtau, indwk, indwkn, iscale, j, jj, liwmin, llwork, llwrkn, lwkopt, &
                     lwmin, nb, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'DSYEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( liwork==-1_ilp ) )
           lwmin = max( 1_ilp, 26_ilp*n )
           liwmin = max( 1_ilp, 10_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              lwkopt = max( ( nb+1 )*n, lwmin )
              work( 1_ilp ) = lwkopt
              iwork( 1_ilp ) = liwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( n==1_ilp ) then
              work( 1_ilp ) = 7_ilp
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = a( 1_ilp, 1_ilp )
              else
                 if( vl<a( 1_ilp, 1_ilp ) .and. vu>=a( 1_ilp, 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = a( 1_ilp, 1_ilp )
                 end if
              end if
              if( wantz ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz( 1_ilp ) = 1_ilp
                 isuppz( 2_ilp ) = 1_ilp
              end if
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
           abstll = abstol
           if (valeig) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_dlansy( 'M', uplo, n, a, lda, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_dscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_dscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: the iwork indices are
           ! used only if stdlib_dsterf or stdlib_dstemr fail.
           ! work(indtau:indtau+n-1) stores the scalar factors of the
           ! elementary reflectors used in stdlib_dsytrd.
           indtau = 1_ilp
           ! work(indd:indd+n-1) stores the tridiagonal's diagonal entries.
           indd = indtau + n
           ! work(inde:inde+n-1) stores the off-diagonal entries of the
           ! tridiagonal matrix from stdlib_dsytrd.
           inde = indd + n
           ! work(inddd:inddd+n-1) is a copy of the diagonal entries over
           ! -written by stdlib_dstemr (the stdlib_dsterf path copies the diagonal to w).
           inddd = inde + n
           ! work(indee:indee+n-1) is a copy of the off-diagonal entries over
           ! -written while computing the eigenvalues in stdlib_dsterf and stdlib_dstemr.
           indee = inddd + n
           ! indwk is the starting offset of the left-over workspace, and
           ! llwork is the remaining workspace size.
           indwk = indee + n
           llwork = lwork - indwk + 1_ilp
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
           indiwo = indifl + n
           ! call stdlib_dsytrd to reduce symmetric matrix to tridiagonal form.
           call stdlib_dsytrd( uplo, n, a, lda, work( indd ), work( inde ),work( indtau ), work( &
                     indwk ), llwork, iinfo )
           ! if all eigenvalues are desired
           ! then call stdlib_dsterf or stdlib_dstemr and stdlib_dormtr.
           if( ( alleig .or. ( indeig .and. il==1_ilp .and. iu==n ) ) .and.ieeeok==1_ilp ) then
              if( .not.wantz ) then
                 call stdlib_dcopy( n, work( indd ), 1_ilp, w, 1_ilp )
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dcopy( n, work( indd ), 1_ilp, work( inddd ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_dstemr( jobz, 'A', n, work( inddd ), work( indee ),vl, vu, il, iu, m,&
                            w, z, ldz, n, isuppz,tryrac, work( indwk ), lwork, iwork, liwork,info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstemr.
                 if( wantz .and. info==0_ilp ) then
                    indwkn = inde
                    llwrkn = lwork - indwkn + 1_ilp
                    call stdlib_dormtr( 'L', uplo, 'N', n, m, a, lda,work( indtau ), z, ldz, work(&
                               indwkn ),llwrkn, iinfo )
                 end if
              end if
              if( info==0_ilp ) then
                 ! everything worked.  skip stdlib_dstebz/stdlib_dstein.  iwork(:) are
                 ! undefined.
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_dstein.
           ! also call stdlib_dstebz and stdlib_dstein if stdlib_dstemr fails.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwk ),iwork( indiwo ), info )
                     
           if( wantz ) then
              call stdlib_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwk ), iwork( indiwo ), iwork( indifl ),info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstein.
              indwkn = inde
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_dormtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
        ! jump here if stdlib_dstemr/stdlib_dstein succeeded.
        30 continue
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = m
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.  note: we do not sort the ifail portion of iwork.
           ! it may not be initialized (if stdlib_dstemr/stdlib_dstein succeeded), and we do
           ! not return this detailed information to the user.
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
                    call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                 end if
              end do
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dsyevr




     module subroutine stdlib_ssyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! SSYEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               work, lwork, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indtau, &
           indwkn, indwrk, iscale, itmp1, j, jj, llwork, llwrkn, lwkmin, lwkopt, nb, &
                     nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwkmin = 1_ilp
                 work( 1_ilp ) = lwkmin
              else
                 lwkmin = 8_ilp*n
                 nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lwkopt = max( lwkmin, ( nb + 3_ilp )*n )
                 work( 1_ilp ) = lwkopt
              end if
              if( lwork<lwkmin .and. .not.lquery )info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = a( 1_ilp, 1_ilp )
              else
                 if( vl<a( 1_ilp, 1_ilp ) .and. vu>=a( 1_ilp, 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = a( 1_ilp, 1_ilp )
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_slansy( 'M', uplo, n, a, lda, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_sscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_sscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_ssytrd to reduce symmetric matrix to tridiagonal form.
           indtau = 1_ilp
           inde = indtau + n
           indd = inde + n
           indwrk = indd + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_ssytrd( uplo, n, a, lda, work( indd ), work( inde ),work( indtau ), work( &
                     indwrk ), llwork, iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal to
           ! zero, then call stdlib_ssterf or stdlib_sorgtr and stdlib_ssteqr.  if this fails for
           ! some eigenvalue, then try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_scopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_slacpy( 'A', n, n, a, lda, z, ldz )
                 call stdlib_sorgtr( uplo, n, z, ldz, work( indtau ),work( indwrk ), llwork, &
                           iinfo )
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 40
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstein.
              indwkn = inde
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_sormtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           40 continue
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
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssyevx

     module subroutine stdlib_dsyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! DSYEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               work, lwork, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indtau, &
           indwkn, indwrk, iscale, itmp1, j, jj, llwork, llwrkn, lwkmin, lwkopt, nb, &
                     nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwkmin = 1_ilp
                 work( 1_ilp ) = lwkmin
              else
                 lwkmin = 8_ilp*n
                 nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lwkopt = max( lwkmin, ( nb + 3_ilp )*n )
                 work( 1_ilp ) = lwkopt
              end if
              if( lwork<lwkmin .and. .not.lquery )info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = a( 1_ilp, 1_ilp )
              else
                 if( vl<a( 1_ilp, 1_ilp ) .and. vu>=a( 1_ilp, 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = a( 1_ilp, 1_ilp )
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_dlansy( 'M', uplo, n, a, lda, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_dscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_dscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_dsytrd to reduce symmetric matrix to tridiagonal form.
           indtau = 1_ilp
           inde = indtau + n
           indd = inde + n
           indwrk = indd + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_dsytrd( uplo, n, a, lda, work( indd ), work( inde ),work( indtau ), work( &
                     indwrk ), llwork, iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal to
           ! zero, then call stdlib_dsterf or stdlib_dorgtr and stdlib_ssteqr.  if this fails for
           ! some eigenvalue, then try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_dcopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_dlacpy( 'A', n, n, a, lda, z, ldz )
                 call stdlib_dorgtr( uplo, n, z, ldz, work( indtau ),work( indwrk ), llwork, &
                           iinfo )
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 40
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstein.
              indwkn = inde
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_dormtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           40 continue
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
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsyevx




     module subroutine stdlib_sspev( jobz, uplo, n, ap, w, z, ldz, work, info )
     !! SSPEV computes all the eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A in packed storage.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ap( 1_ilp )
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
           anrm = stdlib_slansp( 'M', uplo, n, ap, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_ssptrd to reduce symmetric packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           call stdlib_ssptrd( uplo, n, ap, w, work( inde ), work( indtau ), iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_sopgtr to generate the orthogonal matrix, then call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              indwrk = indtau + n
              call stdlib_sopgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                        
              call stdlib_ssteqr( jobz, n, w, work( inde ), z, ldz, work( indtau ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_sspev

     module subroutine stdlib_dspev( jobz, uplo, n, ap, w, z, ldz, work, info )
     !! DSPEV computes all the eigenvalues and, optionally, eigenvectors of a
     !! real symmetric matrix A in packed storage.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ap( 1_ilp )
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
           anrm = stdlib_dlansp( 'M', uplo, n, ap, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_dsptrd to reduce symmetric packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           call stdlib_dsptrd( uplo, n, ap, w, work( inde ), work( indtau ), iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_dopgtr to generate the orthogonal matrix, then call stdlib_dsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              indwrk = indtau + n
              call stdlib_dopgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                        
              call stdlib_dsteqr( jobz, n, w, work( inde ), z, ldz, work( indtau ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_dspev




     module subroutine stdlib_sspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,iwork, liwork, info )
     !! SSPEVD computes all the eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A in packed storage. If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
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
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iinfo, inde, indtau, indwrk, iscale, liwmin, llwork, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n
                 end if
              end if
              iwork( 1_ilp ) = liwmin
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -9_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ap( 1_ilp )
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
           anrm = stdlib_slansp( 'M', uplo, n, ap, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_ssptrd to reduce symmetric packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           call stdlib_ssptrd( uplo, n, ap, w, work( inde ), work( indtau ), iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_sstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_sopmtr to multiply it by the
           ! householder transformations represented in ap.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              indwrk = indtau + n
              llwork = lwork - indwrk + 1_ilp
              call stdlib_sstedc( 'I', n, w, work( inde ), z, ldz, work( indwrk ),llwork, iwork, &
                        liwork, info )
              call stdlib_sopmtr( 'L', uplo, 'N', n, n, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_sscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sspevd

     module subroutine stdlib_dspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,iwork, liwork, info )
     !! DSPEVD computes all the eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A in packed storage. If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
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
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iinfo, inde, indtau, indwrk, iscale, liwmin, llwork, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n
                 end if
              end if
              iwork( 1_ilp ) = liwmin
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -9_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ap( 1_ilp )
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
           anrm = stdlib_dlansp( 'M', uplo, n, ap, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_dsptrd to reduce symmetric packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = inde + n
           call stdlib_dsptrd( uplo, n, ap, w, work( inde ), work( indtau ), iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_dstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_dopmtr to multiply it by the
           ! householder transformations represented in ap.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              indwrk = indtau + n
              llwork = lwork - indwrk + 1_ilp
              call stdlib_dstedc( 'I', n, w, work( inde ), z, ldz, work( indwrk ),llwork, iwork, &
                        liwork, info )
              call stdlib_dopmtr( 'L', uplo, 'N', n, n, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_dscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dspevd




     module subroutine stdlib_sspevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! SSPEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A in packed storage.  Eigenvalues/vectors
     !! can be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               work, iwork, ifail,info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indtau, &
                     indwrk, iscale, itmp1, j, jj, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
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
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
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
              call stdlib_xerbla( 'SSPEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = ap( 1_ilp )
              else
                 if( vl<ap( 1_ilp ) .and. vu>=ap( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = ap( 1_ilp )
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
           abstll = abstol
           if ( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           endif
           anrm = stdlib_slansp( 'M', uplo, n, ap, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_sscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_ssptrd to reduce symmetric packed matrix to tridiagonal form.
           indtau = 1_ilp
           inde = indtau + n
           indd = inde + n
           indwrk = indd + n
           call stdlib_ssptrd( uplo, n, ap, work( indd ), work( inde ),work( indtau ), iinfo )
                     
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_sopgtr and stdlib_ssteqr.  if this fails
           ! for some eigenvalue, then try stdlib_sstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_scopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_sopgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                           
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
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
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstein.
              call stdlib_sopmtr( 'L', uplo, 'N', n, m, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
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
     end subroutine stdlib_sspevx

     module subroutine stdlib_dspevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! DSPEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric matrix A in packed storage.  Eigenvalues/vectors
     !! can be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               work, iwork, ifail,info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indtau, &
                     indwrk, iscale, itmp1, j, jj, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
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
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
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
              call stdlib_xerbla( 'DSPEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = ap( 1_ilp )
              else
                 if( vl<ap( 1_ilp ) .and. vu>=ap( 1_ilp ) ) then
                    m = 1_ilp
                    w( 1_ilp ) = ap( 1_ilp )
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           end if
           anrm = stdlib_dlansp( 'M', uplo, n, ap, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_dscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_dsptrd to reduce symmetric packed matrix to tridiagonal form.
           indtau = 1_ilp
           inde = indtau + n
           indd = inde + n
           indwrk = indd + n
           call stdlib_dsptrd( uplo, n, ap, work( indd ), work( inde ),work( indtau ), iinfo )
                     
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_dopgtr and stdlib_ssteqr.  if this fails
           ! for some eigenvalue, then try stdlib_dstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_dcopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_dopgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                           
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
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
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstein.
              call stdlib_dopmtr( 'L', uplo, 'N', n, m, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
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
     end subroutine stdlib_dspevx




     module subroutine stdlib_ssbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,info )
     !! SSBEV computes all the eigenvalues and, optionally, eigenvectors of
     !! a real symmetric band matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, wantz
           integer(ilp) :: iinfo, imax, inde, indwrk, iscale
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( lower ) then
                 w( 1_ilp ) = ab( 1_ilp, 1_ilp )
              else
                 w( 1_ilp ) = ab( kd+1, 1_ilp )
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
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           anrm = stdlib_slansb( 'M', uplo, n, kd, ab, ldab, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_slascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_slascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_ssbtrd to reduce symmetric band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_ssbtrd( jobz, uplo, n, kd, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_ssteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_ssbev

     module subroutine stdlib_dsbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,info )
     !! DSBEV computes all the eigenvalues and, optionally, eigenvectors of
     !! a real symmetric band matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, wantz
           integer(ilp) :: iinfo, imax, inde, indwrk, iscale
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( lower ) then
                 w( 1_ilp ) = ab( 1_ilp, 1_ilp )
              else
                 w( 1_ilp ) = ab( kd+1, 1_ilp )
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
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           anrm = stdlib_dlansb( 'M', uplo, n, kd, ab, ldab, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_dlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_dlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_dsbtrd to reduce symmetric band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_dsbtrd( jobz, uplo, n, kd, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dsteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_dsbev




     module subroutine stdlib_ssbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, iwork, liwork, &
     !! SSBEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a real symmetric band matrix A. If eigenvectors are desired, it uses
     !! a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, inde, indwk2, indwrk, iscale, liwmin, llwrk2, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else
              if( wantz ) then
                 liwmin = 3_ilp + 5_ilp*n
                 lwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
              else
                 liwmin = 1_ilp
                 lwmin = 2_ilp*n
              end if
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ab( 1_ilp, 1_ilp )
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
           anrm = stdlib_slansb( 'M', uplo, n, kd, ab, ldab, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_slascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_slascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_ssbtrd to reduce symmetric band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_ssbtrd( jobz, uplo, n, kd, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_sstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_sstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_sgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_slacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_sscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_ssbevd

     module subroutine stdlib_dsbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, iwork, liwork, &
     !! DSBEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a real symmetric band matrix A. If eigenvectors are desired, it uses
     !! a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, inde, indwk2, indwrk, iscale, liwmin, llwrk2, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else
              if( wantz ) then
                 liwmin = 3_ilp + 5_ilp*n
                 lwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
              else
                 liwmin = 1_ilp
                 lwmin = 2_ilp*n
              end if
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = ab( 1_ilp, 1_ilp )
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
           anrm = stdlib_dlansb( 'M', uplo, n, kd, ab, ldab, work )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_dlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_dlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_dsbtrd to reduce symmetric band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_dsbtrd( jobz, uplo, n, kd, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_sstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_dgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_dlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp )call stdlib_dscal( n, one / sigma, w, 1_ilp )
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dsbevd




     module subroutine stdlib_ssbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
     !! SSBEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric band matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               m, w, z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indwrk, &
                     iscale, itmp1, j, jj, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( wantz .and. ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              m = 1_ilp
              if( lower ) then
                 tmp1 = ab( 1_ilp, 1_ilp )
              else
                 tmp1 = ab( kd+1, 1_ilp )
              end if
              if( valeig ) then
                 if( .not.( vl<tmp1 .and. vu>=tmp1 ) )m = 0_ilp
              end if
              if( m==1_ilp ) then
                 w( 1_ilp ) = tmp1
                 if( wantz )z( 1_ilp, 1_ilp ) = one
              end if
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
           abstll = abstol
           if ( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           endif
           anrm = stdlib_slansb( 'M', uplo, n, kd, ab, ldab, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_slascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_slascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_ssbtrd to reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indwrk = inde + n
           call stdlib_ssbtrd( jobz, uplo, n, kd, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_sstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_scopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_slacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_ssteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstein.
              do j = 1, m
                 call stdlib_scopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_sgemv( 'N', n, n, one, q, ldq, work, 1_ilp, zero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
     end subroutine stdlib_ssbevx

     module subroutine stdlib_dsbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
     !! DSBEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a real symmetric band matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               m, w, z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwo, indwrk, &
                     iscale, itmp1, j, jj, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( wantz .and. ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              m = 1_ilp
              if( lower ) then
                 tmp1 = ab( 1_ilp, 1_ilp )
              else
                 tmp1 = ab( kd+1, 1_ilp )
              end if
              if( valeig ) then
                 if( .not.( vl<tmp1 .and. vu>=tmp1 ) )m = 0_ilp
              end if
              if( m==1_ilp ) then
                 w( 1_ilp ) = tmp1
                 if( wantz )z( 1_ilp, 1_ilp ) = one
              end if
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           end if
           anrm = stdlib_dlansb( 'M', uplo, n, kd, ab, ldab, work )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_dlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_dlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_dsbtrd to reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indwrk = inde + n
           call stdlib_dsbtrd( jobz, uplo, n, kd, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_dstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_dcopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_dlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
                 call stdlib_dsteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,work( indd ), work( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info &
                     )
           if( wantz ) then
              call stdlib_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply orthogonal matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstein.
              do j = 1, m
                 call stdlib_dcopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_dgemv( 'N', n, n, one, q, ldq, work, 1_ilp, zero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
     end subroutine stdlib_dsbevx




     module subroutine stdlib_cheev( jobz, uplo, n, a, lda, w, work, lwork, rwork,info )
     !! CHEEV computes all eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale, llwork, lwkopt, nb
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb+1 )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n-1 ) .and. .not.lquery )info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
              work( 1_ilp ) = 1_ilp
              if( wantz )a( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_clanhe( 'M', uplo, n, a, lda, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_clascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_chetrd to reduce hermitian matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_chetrd( uplo, n, a, lda, w, rwork( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_cungtr to generate the unitary matrix, then call stdlib_csteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_cungtr( uplo, n, a, lda, work( indtau ), work( indwrk ),llwork, iinfo )
                        
              indwrk = inde + n
              call stdlib_csteqr( jobz, n, w, rwork( inde ), a, lda,rwork( indwrk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cheev

     module subroutine stdlib_zheev( jobz, uplo, n, a, lda, w, work, lwork, rwork,info )
     !! ZHEEV computes all eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indtau, indwrk, iscale, llwork, lwkopt, nb
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb+1 )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n-1 ) .and. .not.lquery )info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
              work( 1_ilp ) = 1_ilp
              if( wantz )a( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_zlanhe( 'M', uplo, n, a, lda, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_zlascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_zhetrd to reduce hermitian matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_zhetrd( uplo, n, a, lda, w, rwork( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_zungtr to generate the unitary matrix, then call stdlib_zsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zungtr( uplo, n, a, lda, work( indtau ), work( indwrk ),llwork, iinfo )
                        
              indwrk = inde + n
              call stdlib_zsteqr( jobz, n, w, rwork( inde ), a, lda,rwork( indwrk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zheev




     module subroutine stdlib_cheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork,lrwork, iwork, liwork,&
     !! CHEEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix A.  If eigenvectors are desired, it uses a
     !! divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwk2, indwrk, iscale, liopt, &
                     liwmin, llrwk, llwork, llwrk2, lopt, lropt, lrwmin, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 lrwmin = 1_ilp
                 liwmin = 1_ilp
                 lopt = lwmin
                 lropt = lrwmin
                 liopt = liwmin
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n + n*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n + 1_ilp
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
                 lopt = max( lwmin, n +stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lropt = lrwmin
                 liopt = liwmin
              end if
              work( 1_ilp ) = lopt
              rwork( 1_ilp ) = lropt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -10_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
              if( wantz )a( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_clanhe( 'M', uplo, n, a, lda, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_clascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_chetrd to reduce hermitian matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indwrk = indtau + n
           indrwk = inde + n
           indwk2 = indwrk + n*n
           llwork = lwork - indwrk + 1_ilp
           llwrk2 = lwork - indwk2 + 1_ilp
           llrwk = lrwork - indrwk + 1_ilp
           call stdlib_chetrd( uplo, n, a, lda, w, rwork( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_cstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_cunmtr to multiply it to the
           ! householder transformations represented as householder vectors in
           ! a.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_cstedc( 'I', n, w, rwork( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, rwork( indrwk ), llrwk,iwork, liwork, info )
              call stdlib_cunmtr( 'L', uplo, 'N', n, n, a, lda, work( indtau ),work( indwrk ), n, &
                        work( indwk2 ), llwrk2, iinfo )
              call stdlib_clacpy( 'A', n, n, work( indwrk ), n, a, lda )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lopt
           rwork( 1_ilp ) = lropt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_cheevd

     module subroutine stdlib_zheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork,lrwork, iwork, liwork,&
     !! ZHEEVD computes all eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix A.  If eigenvectors are desired, it uses a
     !! divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwk2, indwrk, iscale, liopt, &
                     liwmin, llrwk, llwork, llwrk2, lopt, lropt, lrwmin, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 lrwmin = 1_ilp
                 liwmin = 1_ilp
                 lopt = lwmin
                 lropt = lrwmin
                 liopt = liwmin
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n + n*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n + 1_ilp
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
                 lopt = max( lwmin, n +stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lropt = lrwmin
                 liopt = liwmin
              end if
              work( 1_ilp ) = lopt
              rwork( 1_ilp ) = lropt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -8_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -10_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
              if( wantz )a( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_zlanhe( 'M', uplo, n, a, lda, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp )call stdlib_zlascl( uplo, 0_ilp, 0_ilp, one, sigma, n, n, a, lda, info )
           ! call stdlib_zhetrd to reduce hermitian matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indwrk = indtau + n
           indrwk = inde + n
           indwk2 = indwrk + n*n
           llwork = lwork - indwrk + 1_ilp
           llwrk2 = lwork - indwk2 + 1_ilp
           llrwk = lrwork - indrwk + 1_ilp
           call stdlib_zhetrd( uplo, n, a, lda, w, rwork( inde ), work( indtau ),work( indwrk ), &
                     llwork, iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_zstedc to generate the eigenvector matrix, work(indwrk), of the
           ! tridiagonal matrix, then call stdlib_zunmtr to multiply it to the
           ! householder transformations represented as householder vectors in
           ! a.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zstedc( 'I', n, w, rwork( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, rwork( indrwk ), llrwk,iwork, liwork, info )
              call stdlib_zunmtr( 'L', uplo, 'N', n, n, a, lda, work( indtau ),work( indwrk ), n, &
                        work( indwk2 ), llwrk2, iinfo )
              call stdlib_zlacpy( 'A', n, n, work( indwrk ), n, a, lda )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lopt
           rwork( 1_ilp ) = lropt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_zheevd




     module subroutine stdlib_cheevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! CHEEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
     !! CHEEVR first reduces the matrix A to tridiagonal form T with a call
     !! to CHETRD.  Then, whenever possible, CHEEVR calls CSTEMR to compute
     !! the eigenspectrum using Relatively Robust Representations.  CSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows.
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
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see CSTEMR's documentation and:
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
     !! Note 1 : CHEEVR calls CSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! CHEEVR calls SSTEBZ and CSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of CSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork,rwork, lrwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, iinfo, imax, indibl, indifl, indisp, indiwo, indrd, indrdd, &
           indre, indree, indrwk, indtau, indwk, indwkn, iscale, itmp1, j, jj, liwmin, llwork, &
                     llrwork, llwrkn, lrwmin, lwkopt, lwmin, nb, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'CHEEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( lrwork==-1_ilp ) .or.( liwork==-1_ilp ) )
           lrwmin = max( 1_ilp, 24_ilp*n )
           liwmin = max( 1_ilp, 10_ilp*n )
           lwmin = max( 1_ilp, 2_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              lwkopt = max( ( nb+1 )*n, lwmin )
              work( 1_ilp ) = lwkopt
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -20_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -22_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( n==1_ilp ) then
              work( 1_ilp ) = 2_ilp
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
              else
                 if( vl<real( a( 1_ilp, 1_ilp ),KIND=sp) .and. vu>=real( a( 1_ilp, 1_ilp ),KIND=sp) )then
                    m = 1_ilp
                    w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
                 end if
              end if
              if( wantz ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz( 1_ilp ) = 1_ilp
                 isuppz( 2_ilp ) = 1_ilp
              end if
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
           abstll = abstol
           if (valeig) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_clansy( 'M', uplo, n, a, lda, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_csscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_csscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: the iwork indices are
           ! used only if stdlib_ssterf or stdlib_cstemr fail.
           ! work(indtau:indtau+n-1) stores the complex scalar factors of the
           ! elementary reflectors used in stdlib_chetrd.
           indtau = 1_ilp
           ! indwk is the starting offset of the remaining complex workspace,
           ! and llwork is the remaining complex workspace size.
           indwk = indtau + n
           llwork = lwork - indwk + 1_ilp
           ! rwork(indrd:indrd+n-1) stores the real tridiagonal's diagonal
           ! entries.
           indrd = 1_ilp
           ! rwork(indre:indre+n-1) stores the off-diagonal entries of the
           ! tridiagonal matrix from stdlib_chetrd.
           indre = indrd + n
           ! rwork(indrdd:indrdd+n-1) is a copy of the diagonal entries over
           ! -written by stdlib_cstemr (the stdlib_ssterf path copies the diagonal to w).
           indrdd = indre + n
           ! rwork(indree:indree+n-1) is a copy of the off-diagonal entries over
           ! -written while computing the eigenvalues in stdlib_ssterf and stdlib_cstemr.
           indree = indrdd + n
           ! indrwk is the starting offset of the left-over real workspace, and
           ! llrwork is the remaining workspace size.
           indrwk = indree + n
           llrwork = lrwork - indrwk + 1_ilp
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
           indiwo = indifl + n
           ! call stdlib_chetrd to reduce hermitian matrix to tridiagonal form.
           call stdlib_chetrd( uplo, n, a, lda, rwork( indrd ), rwork( indre ),work( indtau ), &
                     work( indwk ), llwork, iinfo )
           ! if all eigenvalues are desired
           ! then call stdlib_ssterf or stdlib_cstemr and stdlib_cunmtr.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig.or.test ) .and. ( ieeeok==1_ilp ) ) then
              if( .not.wantz ) then
                 call stdlib_scopy( n, rwork( indrd ), 1_ilp, w, 1_ilp )
                 call stdlib_scopy( n-1, rwork( indre ), 1_ilp, rwork( indree ), 1_ilp )
                 call stdlib_ssterf( n, w, rwork( indree ), info )
              else
                 call stdlib_scopy( n-1, rwork( indre ), 1_ilp, rwork( indree ), 1_ilp )
                 call stdlib_scopy( n, rwork( indrd ), 1_ilp, rwork( indrdd ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_cstemr( jobz, 'A', n, rwork( indrdd ),rwork( indree ), vl, vu, il, &
                 iu, m, w,z, ldz, n, isuppz, tryrac,rwork( indrwk ), llrwork,iwork, liwork, info )
                           
                 ! apply unitary matrix used in reduction to tridiagonal
                 ! form to eigenvectors returned by stdlib_cstemr.
                 if( wantz .and. info==0_ilp ) then
                    indwkn = indwk
                    llwrkn = lwork - indwkn + 1_ilp
                    call stdlib_cunmtr( 'L', uplo, 'N', n, m, a, lda,work( indtau ), z, ldz, work(&
                               indwkn ),llwrkn, iinfo )
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_cstein.
           ! also call stdlib_sstebz and stdlib_cstein if stdlib_cstemr fails.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indrd ), rwork( &
           indre ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwo )&
                     , info )
           if( wantz ) then
              call stdlib_cstein( n, rwork( indrd ), rwork( indre ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwo ), iwork( indifl ),info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_cstein.
              indwkn = indwk
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_cunmtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                 end if
              end do
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_cheevr

     module subroutine stdlib_zheevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! ZHEEVR computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
     !! ZHEEVR first reduces the matrix A to tridiagonal form T with a call
     !! to ZHETRD.  Then, whenever possible, ZHEEVR calls ZSTEMR to compute
     !! eigenspectrum using Relatively Robust Representations.  ZSTEMR
     !! computes eigenvalues by the dqds algorithm, while orthogonal
     !! eigenvectors are computed from various "good" L D L^T representations
     !! (also known as Relatively Robust Representations). Gram-Schmidt
     !! orthogonalization is avoided as far as possible. More specifically,
     !! the various steps of the algorithm are as follows.
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
     !! The desired accuracy of the output can be specified by the input
     !! parameter ABSTOL.
     !! For more details, see ZSTEMR's documentation and:
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
     !! Note 1 : ZHEEVR calls ZSTEMR when the full spectrum is requested
     !! on machines which conform to the ieee-754 floating point standard.
     !! ZHEEVR calls DSTEBZ and ZSTEIN on non-ieee machines and
     !! when partial spectrum requests are made.
     !! Normal execution of ZSTEMR may create NaNs and infinities and
     !! hence may abort due to a floating point exception in environments
     !! which do not handle NaNs and infinities in the ieee standard default
     !! manner.
               isuppz, work, lwork,rwork, lrwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz, tryrac
           character :: order
           integer(ilp) :: i, ieeeok, iinfo, imax, indibl, indifl, indisp, indiwo, indrd, indrdd, &
           indre, indree, indrwk, indtau, indwk, indwkn, iscale, itmp1, j, jj, liwmin, llwork, &
                     llrwork, llwrkn, lrwmin, lwkopt, lwmin, nb, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ieeeok = stdlib_ilaenv( 10_ilp, 'ZHEEVR', 'N', 1_ilp, 2_ilp, 3_ilp, 4_ilp )
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( ( lwork==-1_ilp ) .or. ( lrwork==-1_ilp ) .or.( liwork==-1_ilp ) )
           lrwmin = max( 1_ilp, 24_ilp*n )
           liwmin = max( 1_ilp, 10_ilp*n )
           lwmin = max( 1_ilp, 2_ilp*n )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              lwkopt = max( ( nb+1 )*n, lwmin )
              work( 1_ilp ) = lwkopt
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -20_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -22_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEEVR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( n==1_ilp ) then
              work( 1_ilp ) = 2_ilp
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
              else
                 if( vl<real( a( 1_ilp, 1_ilp ),KIND=dp) .and. vu>=real( a( 1_ilp, 1_ilp ),KIND=dp) )then
                    m = 1_ilp
                    w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
                 end if
              end if
              if( wantz ) then
                 z( 1_ilp, 1_ilp ) = one
                 isuppz( 1_ilp ) = 1_ilp
                 isuppz( 2_ilp ) = 1_ilp
              end if
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
           abstll = abstol
           if (valeig) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_zlansy( 'M', uplo, n, a, lda, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_zdscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_zdscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! initialize indices into workspaces.  note: the iwork indices are
           ! used only if stdlib_dsterf or stdlib_zstemr fail.
           ! work(indtau:indtau+n-1) stores the complex scalar factors of the
           ! elementary reflectors used in stdlib_zhetrd.
           indtau = 1_ilp
           ! indwk is the starting offset of the remaining complex workspace,
           ! and llwork is the remaining complex workspace size.
           indwk = indtau + n
           llwork = lwork - indwk + 1_ilp
           ! rwork(indrd:indrd+n-1) stores the real tridiagonal's diagonal
           ! entries.
           indrd = 1_ilp
           ! rwork(indre:indre+n-1) stores the off-diagonal entries of the
           ! tridiagonal matrix from stdlib_zhetrd.
           indre = indrd + n
           ! rwork(indrdd:indrdd+n-1) is a copy of the diagonal entries over
           ! -written by stdlib_zstemr (the stdlib_dsterf path copies the diagonal to w).
           indrdd = indre + n
           ! rwork(indree:indree+n-1) is a copy of the off-diagonal entries over
           ! -written while computing the eigenvalues in stdlib_dsterf and stdlib_zstemr.
           indree = indrdd + n
           ! indrwk is the starting offset of the left-over real workspace, and
           ! llrwork is the remaining workspace size.
           indrwk = indree + n
           llrwork = lrwork - indrwk + 1_ilp
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
           indiwo = indifl + n
           ! call stdlib_zhetrd to reduce hermitian matrix to tridiagonal form.
           call stdlib_zhetrd( uplo, n, a, lda, rwork( indrd ), rwork( indre ),work( indtau ), &
                     work( indwk ), llwork, iinfo )
           ! if all eigenvalues are desired
           ! then call stdlib_dsterf or stdlib_zstemr and stdlib_zunmtr.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig.or.test ) .and. ( ieeeok==1_ilp ) ) then
              if( .not.wantz ) then
                 call stdlib_dcopy( n, rwork( indrd ), 1_ilp, w, 1_ilp )
                 call stdlib_dcopy( n-1, rwork( indre ), 1_ilp, rwork( indree ), 1_ilp )
                 call stdlib_dsterf( n, w, rwork( indree ), info )
              else
                 call stdlib_dcopy( n-1, rwork( indre ), 1_ilp, rwork( indree ), 1_ilp )
                 call stdlib_dcopy( n, rwork( indrd ), 1_ilp, rwork( indrdd ), 1_ilp )
                 if (abstol <= two*n*eps) then
                    tryrac = .true.
                 else
                    tryrac = .false.
                 end if
                 call stdlib_zstemr( jobz, 'A', n, rwork( indrdd ),rwork( indree ), vl, vu, il, &
                 iu, m, w,z, ldz, n, isuppz, tryrac,rwork( indrwk ), llrwork,iwork, liwork, info )
                           
                 ! apply unitary matrix used in reduction to tridiagonal
                 ! form to eigenvectors returned by stdlib_zstemr.
                 if( wantz .and. info==0_ilp ) then
                    indwkn = indwk
                    llwrkn = lwork - indwkn + 1_ilp
                    call stdlib_zunmtr( 'L', uplo, 'N', n, m, a, lda,work( indtau ), z, ldz, work(&
                               indwkn ),llwrkn, iinfo )
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_zstein.
           ! also call stdlib_dstebz and stdlib_zstein if stdlib_zstemr fails.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indrd ), rwork( &
           indre ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwo )&
                     , info )
           if( wantz ) then
              call stdlib_zstein( n, rwork( indrd ), rwork( indre ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwo ), iwork( indifl ),info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_zstein.
              indwkn = indwk
              llwrkn = lwork - indwkn + 1_ilp
              call stdlib_zunmtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwkn ), llwrkn, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                 end if
              end do
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zheevr




     module subroutine stdlib_cheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! CHEEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               work, lwork, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indtau, indwrk, iscale, itmp1, j, jj, llwork, lwkmin, lwkopt, nb, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwkmin = 1_ilp
                 work( 1_ilp ) = lwkmin
              else
                 lwkmin = 2_ilp*n
                 nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
                 work( 1_ilp ) = lwkopt
              end if
              if( lwork<lwkmin .and. .not.lquery )info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
              else if( valeig ) then
                 if( vl<real( a( 1_ilp, 1_ilp ),KIND=sp) .and. vu>=real( a( 1_ilp, 1_ilp ),KIND=sp) )then
                    m = 1_ilp
                    w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_clanhe( 'M', uplo, n, a, lda, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_csscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_csscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_chetrd to reduce hermitian matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indtau = 1_ilp
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_chetrd( uplo, n, a, lda, rwork( indd ), rwork( inde ),work( indtau ), work(&
                      indwrk ), llwork, iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal to
           ! zero, then call stdlib_ssterf or stdlib_cungtr and stdlib_csteqr.  if this fails for
           ! some eigenvalue, then try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_scopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, rwork( indee ), info )
              else
                 call stdlib_clacpy( 'A', n, n, a, lda, z, ldz )
                 call stdlib_cungtr( uplo, n, z, ldz, work( indtau ),work( indwrk ), llwork, &
                           iinfo )
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_csteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 40
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_cstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_cstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_cstein.
              call stdlib_cunmtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwrk ), llwork, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           40 continue
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
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cheevx

     module subroutine stdlib_zheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! ZHEEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
     !! be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               work, lwork, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, lquery, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indtau, indwrk, iscale, itmp1, j, jj, llwork, lwkmin, lwkopt, nb, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           lower = stdlib_lsame( uplo, 'L' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -8_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -9_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -10_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -15_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwkmin = 1_ilp
                 work( 1_ilp ) = lwkmin
              else
                 lwkmin = 2_ilp*n
                 nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMTR', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
                 lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
                 work( 1_ilp ) = lwkopt
              end if
              if( lwork<lwkmin .and. .not.lquery )info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
              else if( valeig ) then
                 if( vl<real( a( 1_ilp, 1_ilp ),KIND=dp) .and. vu>=real( a( 1_ilp, 1_ilp ),KIND=dp) )then
                    m = 1_ilp
                    w( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           end if
           anrm = stdlib_zlanhe( 'M', uplo, n, a, lda, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 do j = 1, n
                    call stdlib_zdscal( n-j+1, sigma, a( j, j ), 1_ilp )
                 end do
              else
                 do j = 1, n
                    call stdlib_zdscal( j, sigma, a( 1_ilp, j ), 1_ilp )
                 end do
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_zhetrd to reduce hermitian matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indtau = 1_ilp
           indwrk = indtau + n
           llwork = lwork - indwrk + 1_ilp
           call stdlib_zhetrd( uplo, n, a, lda, rwork( indd ), rwork( inde ),work( indtau ), work(&
                      indwrk ), llwork, iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal to
           ! zero, then call stdlib_dsterf or stdlib_zungtr and stdlib_zsteqr.  if this fails for
           ! some eigenvalue, then try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_dcopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, rwork( indee ), info )
              else
                 call stdlib_zlacpy( 'A', n, n, a, lda, z, ldz )
                 call stdlib_zungtr( uplo, n, z, ldz, work( indtau ),work( indwrk ), llwork, &
                           iinfo )
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_zsteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 40
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_zstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_zstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_zstein.
              call stdlib_zunmtr( 'L', uplo, 'N', n, m, a, lda, work( indtau ), z,ldz, work( &
                        indwrk ), llwork, iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           40 continue
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
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zheevx




     module subroutine stdlib_chpev( jobz, uplo, n, ap, w, z, ldz, work, rwork,info )
     !! CHPEV computes all the eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix in packed storage.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwrk, iscale
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
              rwork( 1_ilp ) = 1_ilp
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
           anrm = stdlib_clanhp( 'M', uplo, n, ap, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_csscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_chptrd to reduce hermitian packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           call stdlib_chptrd( uplo, n, ap, w, rwork( inde ), work( indtau ),iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_cupgtr to generate the orthogonal matrix, then call stdlib_csteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              indwrk = indtau + n
              call stdlib_cupgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                        
              indrwk = inde + n
              call stdlib_csteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indrwk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_chpev

     module subroutine stdlib_zhpev( jobz, uplo, n, ap, w, z, ldz, work, rwork,info )
     !! ZHPEV computes all the eigenvalues and, optionally, eigenvectors of a
     !! complex Hermitian matrix in packed storage.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwrk, iscale
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
              rwork( 1_ilp ) = 1_ilp
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
           anrm = stdlib_zlanhp( 'M', uplo, n, ap, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_zdscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_zhptrd to reduce hermitian packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           call stdlib_zhptrd( uplo, n, ap, w, rwork( inde ), work( indtau ),iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_zupgtr to generate the orthogonal matrix, then call stdlib_zsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              indwrk = indtau + n
              call stdlib_zupgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                        
              indrwk = inde + n
              call stdlib_zsteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indrwk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_zhpev




     module subroutine stdlib_chpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,rwork, lrwork, iwork, &
     !! CHPEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian matrix A in packed storage.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwrk, iscale, liwmin, llrwk, &
                     llwrk, lrwmin, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -9_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_clanhp( 'M', uplo, n, ap, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_csscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_chptrd to reduce hermitian packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indrwk = inde + n
           indwrk = indtau + n
           llwrk = lwork - indwrk + 1_ilp
           llrwk = lrwork - indrwk + 1_ilp
           call stdlib_chptrd( uplo, n, ap, w, rwork( inde ), work( indtau ),iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, first call
           ! stdlib_cupgtr to generate the orthogonal matrix, then call stdlib_cstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_cstedc( 'I', n, w, rwork( inde ), z, ldz, work( indwrk ),llwrk, rwork( &
                        indrwk ), llrwk, iwork, liwork,info )
              call stdlib_cupmtr( 'L', uplo, 'N', n, n, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_chpevd

     module subroutine stdlib_zhpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,rwork, lrwork, iwork, &
     !! ZHPEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian matrix A in packed storage.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, indtau, indwrk, iscale, liwmin, llrwk, &
                     llwrk, lrwmin, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -9_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_zlanhp( 'M', uplo, n, ap, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_zdscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
           end if
           ! call stdlib_zhptrd to reduce hermitian packed matrix to tridiagonal form.
           inde = 1_ilp
           indtau = 1_ilp
           indrwk = inde + n
           indwrk = indtau + n
           llwrk = lwork - indwrk + 1_ilp
           llrwk = lrwork - indrwk + 1_ilp
           call stdlib_zhptrd( uplo, n, ap, w, rwork( inde ), work( indtau ),iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, first call
           ! stdlib_zupgtr to generate the orthogonal matrix, then call stdlib_zstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zstedc( 'I', n, w, rwork( inde ), z, ldz, work( indwrk ),llwrk, rwork( &
                        indrwk ), llrwk, iwork, liwork,info )
              call stdlib_zupmtr( 'L', uplo, 'N', n, n, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zhpevd




     module subroutine stdlib_chpevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! CHPEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A in packed storage.
     !! Eigenvalues/vectors can be selected by specifying either a range of
     !! values or a range of indices for the desired eigenvalues.
               work, rwork, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indtau, indwrk, iscale, itmp1, j, jj, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
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
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
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
              call stdlib_xerbla( 'CHPEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
              else
                 if( vl<real( ap( 1_ilp ),KIND=sp) .and. vu>=real( ap( 1_ilp ),KIND=sp) ) then
                    m = 1_ilp
                    w( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           abstll = abstol
           if ( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           endif
           anrm = stdlib_clanhp( 'M', uplo, n, ap, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_csscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_chptrd to reduce hermitian packed matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indtau = 1_ilp
           indwrk = indtau + n
           call stdlib_chptrd( uplo, n, ap, rwork( indd ), rwork( inde ),work( indtau ), iinfo )
                     
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_cupgtr and stdlib_csteqr.  if this fails
           ! for some eigenvalue, then try stdlib_sstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_scopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, rwork( indee ), info )
              else
                 call stdlib_cupgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                           
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_csteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
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
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_cstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_cstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_cstein.
              indwrk = indtau + n
              call stdlib_cupmtr( 'L', uplo, 'N', n, m, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
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
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_chpevx

     module subroutine stdlib_zhpevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
     !! ZHPEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian matrix A in packed storage.
     !! Eigenvalues/vectors can be selected by specifying either a range of
     !! values or a range of indices for the desired eigenvalues.
               work, rwork, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indtau, indwrk, iscale, itmp1, j, jj, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
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
           else if( .not.( stdlib_lsame( uplo, 'L' ) .or. stdlib_lsame( uplo, 'U' ) ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
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
              call stdlib_xerbla( 'ZHPEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              if( alleig .or. indeig ) then
                 m = 1_ilp
                 w( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
              else
                 if( vl<real( ap( 1_ilp ),KIND=dp) .and. vu>=real( ap( 1_ilp ),KIND=dp) ) then
                    m = 1_ilp
                    w( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
                 end if
              end if
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           end if
           anrm = stdlib_zlanhp( 'M', uplo, n, ap, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              call stdlib_zdscal( ( n*( n+1 ) ) / 2_ilp, sigma, ap, 1_ilp )
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_zhptrd to reduce hermitian packed matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indtau = 1_ilp
           indwrk = indtau + n
           call stdlib_zhptrd( uplo, n, ap, rwork( indd ), rwork( inde ),work( indtau ), iinfo )
                     
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_zupgtr and stdlib_zsteqr.  if this fails
           ! for some eigenvalue, then try stdlib_dstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_dcopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, rwork( indee ), info )
              else
                 call stdlib_zupgtr( uplo, n, ap, work( indtau ), z, ldz,work( indwrk ), iinfo )
                           
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_zsteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
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
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_zstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_zstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_zstein.
              indwrk = indtau + n
              call stdlib_zupmtr( 'L', uplo, 'N', n, m, ap, work( indtau ), z, ldz,work( indwrk ),&
                         iinfo )
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
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_zhpevx




     module subroutine stdlib_chbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,rwork, info )
     !! CHBEV computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian band matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, iscale
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( lower ) then
                 w( 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=sp)
              else
                 w( 1_ilp ) = real( ab( kd+1, 1_ilp ),KIND=sp)
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
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           anrm = stdlib_clanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_clascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_clascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_chbtrd to reduce hermitian band matrix to tridiagonal form.
           inde = 1_ilp
           call stdlib_chbtrd( jobz, uplo, n, kd, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_csteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              indrwk = inde + n
              call stdlib_csteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indrwk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_chbev

     module subroutine stdlib_zhbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,rwork, info )
     !! ZHBEV computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian band matrix A.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, wantz
           integer(ilp) :: iinfo, imax, inde, indrwk, iscale
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBEV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( lower ) then
                 w( 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=dp)
              else
                 w( 1_ilp ) = real( ab( kd+1, 1_ilp ),KIND=dp)
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
           rmax = sqrt( bignum )
           ! scale matrix to allowable range, if necessary.
           anrm = stdlib_zlanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_zlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_zlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_zhbtrd to reduce hermitian band matrix to tridiagonal form.
           inde = 1_ilp
           call stdlib_zhbtrd( jobz, uplo, n, kd, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_zsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              indrwk = inde + n
              call stdlib_zsteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indrwk ), info )
                        
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           return
     end subroutine stdlib_zhbev




     module subroutine stdlib_chbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, rwork, lrwork, &
     !! CHBEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian band matrix A.  If eigenvectors are desired, it
     !! uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indwk2, indwrk, iscale, liwmin, llrwk, llwk2, &
                     lrwmin, lwmin
           real(sp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp .or. lrwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp
              lrwmin = 1_ilp
              liwmin = 1_ilp
           else
              if( wantz ) then
                 lwmin = 2_ilp*n**2_ilp
                 lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                 liwmin = 3_ilp + 5_ilp*n
              else
                 lwmin = n
                 lrwmin = n
                 liwmin = 1_ilp
              end if
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=sp)
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_clanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_clascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_clascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_chbtrd to reduce hermitian band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = 1_ilp + n*n
           llwk2 = lwork - indwk2 + 1_ilp
           llrwk = lrwork - indwrk + 1_ilp
           call stdlib_chbtrd( jobz, uplo, n, kd, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_cstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_cstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_cgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_clacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_sscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_chbevd

     module subroutine stdlib_zhbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, rwork, lrwork, &
     !! ZHBEVD computes all the eigenvalues and, optionally, eigenvectors of
     !! a complex Hermitian band matrix A.  If eigenvectors are desired, it
     !! uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, lquery, wantz
           integer(ilp) :: iinfo, imax, inde, indwk2, indwrk, iscale, liwmin, llrwk, llwk2, &
                     lrwmin, lwmin
           real(dp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           lower = stdlib_lsame( uplo, 'L' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp .or. lrwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp
              lrwmin = 1_ilp
              liwmin = 1_ilp
           else
              if( wantz ) then
                 lwmin = 2_ilp*n**2_ilp
                 lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                 liwmin = 3_ilp + 5_ilp*n
              else
                 lwmin = n
                 lrwmin = n
                 liwmin = 1_ilp
              end if
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBEVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              w( 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=dp)
              if( wantz )z( 1_ilp, 1_ilp ) = cone
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
           anrm = stdlib_zlanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           iscale = 0_ilp
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_zlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_zlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
           end if
           ! call stdlib_zhbtrd to reduce hermitian band matrix to tridiagonal form.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = 1_ilp + n*n
           llwk2 = lwork - indwk2 + 1_ilp
           llrwk = lrwork - indwrk + 1_ilp
           call stdlib_zhbtrd( jobz, uplo, n, kd, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_zstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_zgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_zlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           if( iscale==1_ilp ) then
              if( info==0_ilp ) then
                 imax = n
              else
                 imax = info - 1_ilp
              end if
              call stdlib_dscal( imax, one / sigma, w, 1_ilp )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zhbevd




     module subroutine stdlib_chbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
     !! CHBEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian band matrix A.  Eigenvalues and eigenvectors
     !! can be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               m, w, z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indwrk, iscale, itmp1, j, jj, nsplit
           real(sp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           complex(sp) :: ctmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( wantz .and. ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              m = 1_ilp
              if( lower ) then
                 ctmp1 = ab( 1_ilp, 1_ilp )
              else
                 ctmp1 = ab( kd+1, 1_ilp )
              end if
              tmp1 = real( ctmp1,KIND=sp)
              if( valeig ) then
                 if( .not.( vl<tmp1 .and. vu>=tmp1 ) )m = 0_ilp
              end if
              if( m==1_ilp ) then
                 w( 1_ilp ) = real( ctmp1,KIND=sp)
                 if( wantz )z( 1_ilp, 1_ilp ) = cone
              end if
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
           abstll = abstol
           if ( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           endif
           anrm = stdlib_clanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_clascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_clascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_chbtrd to reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp
           call stdlib_chbtrd( jobz, uplo, n, kd, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_csteqr.  if this fails for some
           ! eigenvalue, then try stdlib_sstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_scopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_ssterf( n, w, rwork( indee ), info )
              else
                 call stdlib_clacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_csteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired, stdlib_cstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_sstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_cstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_cstein.
              do j = 1, m
                 call stdlib_ccopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_cgemv( 'N', n, n, cone, q, ldq, work, 1_ilp, czero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_chbevx

     module subroutine stdlib_zhbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
     !! ZHBEVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex Hermitian band matrix A.  Eigenvalues and eigenvectors
     !! can be selected by specifying either a range of values or a range of
     !! indices for the desired eigenvalues.
               m, w, z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lower, test, valeig, wantz
           character :: order
           integer(ilp) :: i, iinfo, imax, indd, inde, indee, indibl, indisp, indiwk, indrwk, &
                     indwrk, iscale, itmp1, j, jj, nsplit
           real(dp) :: abstll, anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum, tmp1, vll, &
                     vuu
           complex(dp) :: ctmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lower = stdlib_lsame( uplo, 'L' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( lower .or. stdlib_lsame( uplo, 'U' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( wantz .and. ldq<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBEVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           if( n==1_ilp ) then
              m = 1_ilp
              if( lower ) then
                 ctmp1 = ab( 1_ilp, 1_ilp )
              else
                 ctmp1 = ab( kd+1, 1_ilp )
              end if
              tmp1 = real( ctmp1,KIND=dp)
              if( valeig ) then
                 if( .not.( vl<tmp1 .and. vu>=tmp1 ) )m = 0_ilp
              end if
              if( m==1_ilp ) then
                 w( 1_ilp ) = real( ctmp1,KIND=dp)
                 if( wantz )z( 1_ilp, 1_ilp ) = cone
              end if
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
           abstll = abstol
           if( valeig ) then
              vll = vl
              vuu = vu
           else
              vll = zero
              vuu = zero
           end if
           anrm = stdlib_zlanhb( 'M', uplo, n, kd, ab, ldab, rwork )
           if( anrm>zero .and. anrm<rmin ) then
              iscale = 1_ilp
              sigma = rmin / anrm
           else if( anrm>rmax ) then
              iscale = 1_ilp
              sigma = rmax / anrm
           end if
           if( iscale==1_ilp ) then
              if( lower ) then
                 call stdlib_zlascl( 'B', kd, kd, one, sigma, n, n, ab, ldab, info )
              else
                 call stdlib_zlascl( 'Q', kd, kd, one, sigma, n, n, ab, ldab, info )
              end if
              if( abstol>0_ilp )abstll = abstol*sigma
              if( valeig ) then
                 vll = vl*sigma
                 vuu = vu*sigma
              end if
           end if
           ! call stdlib_zhbtrd to reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp
           call stdlib_zhbtrd( jobz, uplo, n, kd, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_zsteqr.  if this fails for some
           ! eigenvalue, then try stdlib_dstebz.
           test = .false.
           if (indeig) then
              if (il==1_ilp .and. iu==n) then
                 test = .true.
              end if
           end if
           if ((alleig .or. test) .and. (abstol<=zero)) then
              call stdlib_dcopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              if( .not.wantz ) then
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_dsterf( n, w, rwork( indee ), info )
              else
                 call stdlib_zlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
                 call stdlib_zsteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired, stdlib_zstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_dstebz( range, order, n, vll, vuu, il, iu, abstll,rwork( indd ), rwork( &
           inde ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ),&
                      info )
           if( wantz ) then
              call stdlib_zstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_zstein.
              do j = 1, m
                 call stdlib_zcopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_zgemv( 'N', n, n, cone, q, ldq, work, 1_ilp, czero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           ! if matrix was scaled, then rescale eigenvalues appropriately.
           30 continue
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
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_zhbevx



end submodule stdlib_lapack_eigv_std_driver
