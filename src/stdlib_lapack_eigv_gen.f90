submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_gen
  implicit none


  contains

     module subroutine stdlib_sgeev( jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr,ldvr, work, lwork, &
     !! SGEEV computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate-transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), wi(*), work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr
           character :: side
           integer(ilp) :: hswork, i, ibal, ierr, ihi, ilo, itau, iwrk, k, lwork_trevc, maxwrk, &
                     minwrk, nout
           real(sp) :: anrm, bignum, cs, cscale, eps, r, scl, smlnum, sn
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -9_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_shseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    minwrk = 4_ilp*n
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    call stdlib_shseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vl, ldvl,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                    call stdlib_strevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr, n, nout,&
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    maxwrk = max( maxwrk, 4_ilp*n )
                 else if( wantvr ) then
                    minwrk = 4_ilp*n
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    call stdlib_shseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                    call stdlib_strevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr, n, nout,&
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    maxwrk = max( maxwrk, 4_ilp*n )
                 else
                    minwrk = 3_ilp*n
                    call stdlib_shseqr( 'E', 'N', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix
           ! (workspace: need n)
           ibal = 1_ilp
           call stdlib_sgebal( 'B', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (workspace: need 3*n, prefer 2*n+n*nb)
           itau = ibal + n
           iwrk = itau + n
           call stdlib_sgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_slacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate orthogonal matrix in vl
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vl, ldvl,work( iwrk ), &
                        lwork-iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_slacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_slacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate orthogonal matrix in vr
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           else
              ! compute eigenvalues only
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( 'E', 'N', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_shseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (workspace: need 4*n, prefer n + n + 2*n*nb)
              call stdlib_strevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1, ierr )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              ! (workspace: need n)
              call stdlib_sgebak( 'B', 'L', n, ilo, ihi, work( ibal ), n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_snrm2( n, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_slapy2( stdlib_snrm2( n, vl( 1_ilp, i ), 1_ilp ),stdlib_snrm2( n, &
                              vl( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( iwrk+k-1 ) = vl( k, i )**2_ilp + vl( k, i+1 )**2_ilp
                    end do
                    k = stdlib_isamax( n, work( iwrk ), 1_ilp )
                    call stdlib_slartg( vl( k, i ), vl( k, i+1 ), cs, sn, r )
                    call stdlib_srot( n, vl( 1_ilp, i ), 1_ilp, vl( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vl( k, i+1 ) = zero
                 end if
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              ! (workspace: need n)
              call stdlib_sgebak( 'B', 'R', n, ilo, ihi, work( ibal ), n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_snrm2( n, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_slapy2( stdlib_snrm2( n, vr( 1_ilp, i ), 1_ilp ),stdlib_snrm2( n, &
                              vr( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( iwrk+k-1 ) = vr( k, i )**2_ilp + vr( k, i+1 )**2_ilp
                    end do
                    k = stdlib_isamax( n, work( iwrk ), 1_ilp )
                    call stdlib_slartg( vr( k, i ), vr( k, i+1 ), cs, sn, r )
                    call stdlib_srot( n, vr( 1_ilp, i ), 1_ilp, vr( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vr( k, i+1 ) = zero
                 end if
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wr( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wi( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              if( info>0_ilp ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wr, n,ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgeev

     module subroutine stdlib_dgeev( jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr,ldvr, work, lwork, &
     !! DGEEV computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate-transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), wi(*), work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr
           character :: side
           integer(ilp) :: hswork, i, ibal, ierr, ihi, ilo, itau, iwrk, k, lwork_trevc, maxwrk, &
                     minwrk, nout
           real(dp) :: anrm, bignum, cs, cscale, eps, r, scl, smlnum, sn
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -9_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_dhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    minwrk = 4_ilp*n
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'DORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    call stdlib_dhseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vl, ldvl,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                    call stdlib_dtrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr, n, nout,&
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    maxwrk = max( maxwrk, 4_ilp*n )
                 else if( wantvr ) then
                    minwrk = 4_ilp*n
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'DORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    call stdlib_dhseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                    call stdlib_dtrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr, n, nout,&
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    maxwrk = max( maxwrk, 4_ilp*n )
                 else
                    minwrk = 3_ilp*n
                    call stdlib_dhseqr( 'E', 'N', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                    hswork = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + 1_ilp, n + hswork )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix
           ! (workspace: need n)
           ibal = 1_ilp
           call stdlib_dgebal( 'B', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (workspace: need 3*n, prefer 2*n+n*nb)
           itau = ibal + n
           iwrk = itau + n
           call stdlib_dgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_dlacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate orthogonal matrix in vl
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vl, ldvl,work( iwrk ), &
                        lwork-iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_dlacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_dlacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate orthogonal matrix in vr
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           else
              ! compute eigenvalues only
              ! (workspace: need n+1, prefer n+hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( 'E', 'N', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_dhseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (workspace: need 4*n, prefer n + n + 2*n*nb)
              call stdlib_dtrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1, ierr )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              ! (workspace: need n)
              call stdlib_dgebak( 'B', 'L', n, ilo, ihi, work( ibal ), n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_dnrm2( n, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_dlapy2( stdlib_dnrm2( n, vl( 1_ilp, i ), 1_ilp ),stdlib_dnrm2( n, &
                              vl( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( iwrk+k-1 ) = vl( k, i )**2_ilp + vl( k, i+1 )**2_ilp
                    end do
                    k = stdlib_idamax( n, work( iwrk ), 1_ilp )
                    call stdlib_dlartg( vl( k, i ), vl( k, i+1 ), cs, sn, r )
                    call stdlib_drot( n, vl( 1_ilp, i ), 1_ilp, vl( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vl( k, i+1 ) = zero
                 end if
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              ! (workspace: need n)
              call stdlib_dgebak( 'B', 'R', n, ilo, ihi, work( ibal ), n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_dnrm2( n, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_dlapy2( stdlib_dnrm2( n, vr( 1_ilp, i ), 1_ilp ),stdlib_dnrm2( n, &
                              vr( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( iwrk+k-1 ) = vr( k, i )**2_ilp + vr( k, i+1 )**2_ilp
                    end do
                    k = stdlib_idamax( n, work( iwrk ), 1_ilp )
                    call stdlib_dlartg( vr( k, i ), vr( k, i+1 ), cs, sn, r )
                    call stdlib_drot( n, vr( 1_ilp, i ), 1_ilp, vr( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vr( k, i+1 ) = zero
                 end if
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wr( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wi( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              if( info>0_ilp ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wr, n,ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgeev


     module subroutine stdlib_cgeev( jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr,work, lwork, rwork, &
     !! CGEEV computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr
           character :: side
           integer(ilp) :: hswork, i, ibal, ierr, ihi, ilo, irwork, itau, iwrk, k, lwork_trevc, &
                     maxwrk, minwrk, nout
           real(sp) :: anrm, bignum, cscale, eps, scl, smlnum
           complex(sp) :: tmp
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -10_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_chseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 if( wantvl ) then
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'CUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    call stdlib_ctrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_chseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vl, ldvl,work, -1_ilp, info )
                              
                 else if( wantvr ) then
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'CUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    call stdlib_ctrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_chseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 else
                    call stdlib_chseqr( 'E', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 maxwrk = max( maxwrk, hswork, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_cgebal( 'B', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_cgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_clacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate unitary matrix in vl
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vl, ldvl,work( iwrk ), lwork-&
                        iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_clacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_clacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate unitary matrix in vr
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           else
              ! compute eigenvalues only
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( 'E', 'N', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_chseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (cworkspace: need 2*n, prefer n + 2*n*nb)
              ! (rworkspace: need 2*n)
              irwork = ibal + n
              call stdlib_ctrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1,rwork( irwork ), n, ierr )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_cgebak( 'B', 'L', n, ilo, ihi, rwork( ibal ), n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_scnrm2( n, vl( 1_ilp, i ), 1_ilp )
                 call stdlib_csscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( irwork+k-1 ) = real( vl( k, i ),KIND=sp)**2_ilp +aimag( vl( k, i ) )&
                              **2_ilp
                 end do
                 k = stdlib_isamax( n, rwork( irwork ), 1_ilp )
                 tmp = conjg( vl( k, i ) ) / sqrt( rwork( irwork+k-1 ) )
                 call stdlib_cscal( n, tmp, vl( 1_ilp, i ), 1_ilp )
                 vl( k, i ) = cmplx( real( vl( k, i ),KIND=sp), zero,KIND=sp)
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_cgebak( 'B', 'R', n, ilo, ihi, rwork( ibal ), n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_scnrm2( n, vr( 1_ilp, i ), 1_ilp )
                 call stdlib_csscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( irwork+k-1 ) = real( vr( k, i ),KIND=sp)**2_ilp +aimag( vr( k, i ) )&
                              **2_ilp
                 end do
                 k = stdlib_isamax( n, rwork( irwork ), 1_ilp )
                 tmp = conjg( vr( k, i ) ) / sqrt( rwork( irwork+k-1 ) )
                 call stdlib_cscal( n, tmp, vr( 1_ilp, i ), 1_ilp )
                 vr( k, i ) = cmplx( real( vr( k, i ),KIND=sp), zero,KIND=sp)
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, w( info+1 ),max( n-info, 1_ilp )&
                        , ierr )
              if( info>0_ilp ) then
                 call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, w, n, ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgeev

     module subroutine stdlib_zgeev( jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr,work, lwork, rwork, &
     !! ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr
           character :: side
           integer(ilp) :: hswork, i, ibal, ierr, ihi, ilo, irwork, itau, iwrk, k, lwork_trevc, &
                     maxwrk, minwrk, nout
           real(dp) :: anrm, bignum, cscale, eps, scl, smlnum
           complex(dp) :: tmp
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -10_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_zhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 if( wantvl ) then
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'ZUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    call stdlib_ztrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_zhseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vl, ldvl,work, -1_ilp, info )
                              
                 else if( wantvr ) then
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'ZUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    call stdlib_ztrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_zhseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 else
                    call stdlib_zhseqr( 'E', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 maxwrk = max( maxwrk, hswork, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_zgebal( 'B', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_zgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_zlacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate unitary matrix in vl
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vl, ldvl,work( iwrk ), lwork-&
                        iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_zlacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_zlacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate unitary matrix in vr
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           else
              ! compute eigenvalues only
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( 'E', 'N', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_zhseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (cworkspace: need 2*n, prefer n + 2*n*nb)
              ! (rworkspace: need 2*n)
              irwork = ibal + n
              call stdlib_ztrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1,rwork( irwork ), n, ierr )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_zgebak( 'B', 'L', n, ilo, ihi, rwork( ibal ), n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_dznrm2( n, vl( 1_ilp, i ), 1_ilp )
                 call stdlib_zdscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( irwork+k-1 ) = real( vl( k, i ),KIND=dp)**2_ilp +aimag( vl( k, i ) )&
                              **2_ilp
                 end do
                 k = stdlib_idamax( n, rwork( irwork ), 1_ilp )
                 tmp = conjg( vl( k, i ) ) / sqrt( rwork( irwork+k-1 ) )
                 call stdlib_zscal( n, tmp, vl( 1_ilp, i ), 1_ilp )
                 vl( k, i ) = cmplx( real( vl( k, i ),KIND=dp), zero,KIND=dp)
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_zgebak( 'B', 'R', n, ilo, ihi, rwork( ibal ), n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_dznrm2( n, vr( 1_ilp, i ), 1_ilp )
                 call stdlib_zdscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( irwork+k-1 ) = real( vr( k, i ),KIND=dp)**2_ilp +aimag( vr( k, i ) )&
                              **2_ilp
                 end do
                 k = stdlib_idamax( n, rwork( irwork ), 1_ilp )
                 tmp = conjg( vr( k, i ) ) / sqrt( rwork( irwork+k-1 ) )
                 call stdlib_zscal( n, tmp, vr( 1_ilp, i ), 1_ilp )
                 vr( k, i ) = cmplx( real( vr( k, i ),KIND=dp), zero,KIND=dp)
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, w( info+1 ),max( n-info, 1_ilp )&
                        , ierr )
              if( info>0_ilp ) then
                 call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, w, n, ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgeev




     module subroutine stdlib_sgeevx( balanc, jobvl, jobvr, sense, n, a, lda, wr, wi,vl, ldvl, vr, ldvr, &
     !! SGEEVX computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
     !! (RCONDE), and reciprocal condition numbers for the right
     !! eigenvectors (RCONDV).
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate-transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
     !! Balancing a matrix means permuting the rows and columns to make it
     !! more nearly upper triangular, and applying a diagonal similarity
     !! transformation D * A * D**(-1), where D is a diagonal matrix, to
     !! make its rows and columns closer in norm and the condition numbers
     !! of its eigenvalues and eigenvectors smaller.  The computed
     !! reciprocal condition numbers correspond to the balanced matrix.
     !! Permuting rows and columns will not change the condition numbers
     !! (in exact arithmetic) but diagonal scaling will.  For further
     !! explanation of balancing, see section 4.10.2_sp of the LAPACK
     !! Users' Guide.
               ilo, ihi, scale, abnrm,rconde, rcondv, work, lwork, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: rconde(*), rcondv(*), scale(*), vl(ldvl,*), vr(ldvr,*), wi(*),&
                      work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr, wntsnb, wntsne, wntsnn, wntsnv
           character :: job, side
           integer(ilp) :: hswork, i, icond, ierr, itau, iwrk, k, lwork_trevc, maxwrk, minwrk, &
                     nout
           real(sp) :: anrm, bignum, cs, cscale, eps, r, scl, smlnum, sn
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           wntsnn = stdlib_lsame( sense, 'N' )
           wntsne = stdlib_lsame( sense, 'E' )
           wntsnv = stdlib_lsame( sense, 'V' )
           wntsnb = stdlib_lsame( sense, 'B' )
           if( .not.( stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'S' ).or. &
                     stdlib_lsame( balanc, 'P' ) .or. stdlib_lsame( balanc, 'B' ) ) )then
              info = -1_ilp
           else if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -2_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wntsnn .or. wntsne .or. wntsnb .or. wntsnv ) .or.( ( wntsne .or. &
                     wntsnb ) .and. .not.( wantvl .and.wantvr ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -13_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_shseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    call stdlib_strevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_shseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vl, ldvl,work, -1_ilp, &
                              info )
                 else if( wantvr ) then
                    call stdlib_strevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_shseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                 else
                    if( wntsnn ) then
                       call stdlib_shseqr( 'E', 'N', n, 1_ilp, n, a, lda, wr, wi, vr,ldvr, work, -1_ilp, &
                                 info )
                    else
                       call stdlib_shseqr( 'S', 'N', n, 1_ilp, n, a, lda, wr, wi, vr,ldvr, work, -1_ilp, &
                                 info )
                    end if
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 if( ( .not.wantvl ) .and. ( .not.wantvr ) ) then
                    minwrk = 2_ilp*n
                    if( .not.wntsnn )minwrk = max( minwrk, n*n+6*n )
                    maxwrk = max( maxwrk, hswork )
                    if( .not.wntsnn )maxwrk = max( maxwrk, n*n + 6_ilp*n )
                 else
                    minwrk = 3_ilp*n
                    if( ( .not.wntsnn ) .and. ( .not.wntsne ) )minwrk = max( minwrk, n*n + 6_ilp*n )
                              
                    maxwrk = max( maxwrk, hswork )
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'SORGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    if( ( .not.wntsnn ) .and. ( .not.wntsne ) )maxwrk = max( maxwrk, n*n + 6_ilp*n )
                              
                    maxwrk = max( maxwrk, 3_ilp*n )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           icond = 0_ilp
           anrm = stdlib_slange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix and compute abnrm
           call stdlib_sgebal( balanc, n, a, lda, ilo, ihi, scale, ierr )
           abnrm = stdlib_slange( '1', n, n, a, lda, dum )
           if( scalea ) then
              dum( 1_ilp ) = abnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
              abnrm = dum( 1_ilp )
           end if
           ! reduce to upper hessenberg form
           ! (workspace: need 2*n, prefer n+n*nb)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_sgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_slacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate orthogonal matrix in vl
              ! (workspace: need 2*n-1, prefer n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vl, ldvl,work( iwrk ), &
                        lwork-iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_slacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_slacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate orthogonal matrix in vr
              ! (workspace: need 2*n-1, prefer n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           else
              ! compute eigenvalues only
              ! if condition numbers desired, compute schur form
              if( wntsnn ) then
                 job = 'E'
              else
                 job = 'S'
              end if
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_shseqr( job, 'N', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_shseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (workspace: need 3*n, prefer n + 2*n*nb)
              call stdlib_strevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1, ierr )
           end if
           ! compute condition numbers if desired
           ! (workspace: need n*n+6*n unless sense = 'e')
           if( .not.wntsnn ) then
              call stdlib_strsna( sense, 'A', select, n, a, lda, vl, ldvl, vr, ldvr,rconde, &
                        rcondv, n, nout, work( iwrk ), n, iwork,icond )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              call stdlib_sgebak( balanc, 'L', n, ilo, ihi, scale, n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_snrm2( n, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_slapy2( stdlib_snrm2( n, vl( 1_ilp, i ), 1_ilp ),stdlib_snrm2( n, &
                              vl( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vl( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( k ) = vl( k, i )**2_ilp + vl( k, i+1 )**2_ilp
                    end do
                    k = stdlib_isamax( n, work, 1_ilp )
                    call stdlib_slartg( vl( k, i ), vl( k, i+1 ), cs, sn, r )
                    call stdlib_srot( n, vl( 1_ilp, i ), 1_ilp, vl( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vl( k, i+1 ) = zero
                 end if
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              call stdlib_sgebak( balanc, 'R', n, ilo, ihi, scale, n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_snrm2( n, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_slapy2( stdlib_snrm2( n, vr( 1_ilp, i ), 1_ilp ),stdlib_snrm2( n, &
                              vr( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_sscal( n, scl, vr( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( k ) = vr( k, i )**2_ilp + vr( k, i+1 )**2_ilp
                    end do
                    k = stdlib_isamax( n, work, 1_ilp )
                    call stdlib_slartg( vr( k, i ), vr( k, i+1 ), cs, sn, r )
                    call stdlib_srot( n, vr( 1_ilp, i ), 1_ilp, vr( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vr( k, i+1 ) = zero
                 end if
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wr( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wi( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              if( info==0_ilp ) then
                 if( ( wntsnv .or. wntsnb ) .and. icond==0_ilp )call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale,&
                            anrm, n, 1_ilp, rcondv, n,ierr )
              else
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wr, n,ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgeevx

     module subroutine stdlib_dgeevx( balanc, jobvl, jobvr, sense, n, a, lda, wr, wi,vl, ldvl, vr, ldvr, &
     !! DGEEVX computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
     !! (RCONDE), and reciprocal condition numbers for the right
     !! eigenvectors (RCONDV).
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate-transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
     !! Balancing a matrix means permuting the rows and columns to make it
     !! more nearly upper triangular, and applying a diagonal similarity
     !! transformation D * A * D**(-1), where D is a diagonal matrix, to
     !! make its rows and columns closer in norm and the condition numbers
     !! of its eigenvalues and eigenvectors smaller.  The computed
     !! reciprocal condition numbers correspond to the balanced matrix.
     !! Permuting rows and columns will not change the condition numbers
     !! (in exact arithmetic) but diagonal scaling will.  For further
     !! explanation of balancing, see section 4.10.2_dp of the LAPACK
     !! Users' Guide.
               ilo, ihi, scale, abnrm,rconde, rcondv, work, lwork, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: rconde(*), rcondv(*), scale(*), vl(ldvl,*), vr(ldvr,*), wi(*),&
                      work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr, wntsnb, wntsne, wntsnn, wntsnv
           character :: job, side
           integer(ilp) :: hswork, i, icond, ierr, itau, iwrk, k, lwork_trevc, maxwrk, minwrk, &
                     nout
           real(dp) :: anrm, bignum, cs, cscale, eps, r, scl, smlnum, sn
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           wntsnn = stdlib_lsame( sense, 'N' )
           wntsne = stdlib_lsame( sense, 'E' )
           wntsnv = stdlib_lsame( sense, 'V' )
           wntsnb = stdlib_lsame( sense, 'B' )
           if( .not.( stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'S' ).or. &
                     stdlib_lsame( balanc, 'P' ) .or. stdlib_lsame( balanc, 'B' ) ) )then
              info = -1_ilp
           else if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -2_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wntsnn .or. wntsne .or. wntsnb .or. wntsnv ) .or.( ( wntsne .or. &
                     wntsnb ) .and. .not.( wantvl .and.wantvr ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -13_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_dhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    call stdlib_dtrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_dhseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vl, ldvl,work, -1_ilp, &
                              info )
                 else if( wantvr ) then
                    call stdlib_dtrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, n + lwork_trevc )
                    call stdlib_dhseqr( 'S', 'V', n, 1_ilp, n, a, lda, wr, wi, vr, ldvr,work, -1_ilp, &
                              info )
                 else
                    if( wntsnn ) then
                       call stdlib_dhseqr( 'E', 'N', n, 1_ilp, n, a, lda, wr, wi, vr,ldvr, work, -1_ilp, &
                                 info )
                    else
                       call stdlib_dhseqr( 'S', 'N', n, 1_ilp, n, a, lda, wr, wi, vr,ldvr, work, -1_ilp, &
                                 info )
                    end if
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 if( ( .not.wantvl ) .and. ( .not.wantvr ) ) then
                    minwrk = 2_ilp*n
                    if( .not.wntsnn )minwrk = max( minwrk, n*n+6*n )
                    maxwrk = max( maxwrk, hswork )
                    if( .not.wntsnn )maxwrk = max( maxwrk, n*n + 6_ilp*n )
                 else
                    minwrk = 3_ilp*n
                    if( ( .not.wntsnn ) .and. ( .not.wntsne ) )minwrk = max( minwrk, n*n + 6_ilp*n )
                              
                    maxwrk = max( maxwrk, hswork )
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'DORGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    if( ( .not.wntsnn ) .and. ( .not.wntsne ) )maxwrk = max( maxwrk, n*n + 6_ilp*n )
                              
                    maxwrk = max( maxwrk, 3_ilp*n )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           icond = 0_ilp
           anrm = stdlib_dlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix and compute abnrm
           call stdlib_dgebal( balanc, n, a, lda, ilo, ihi, scale, ierr )
           abnrm = stdlib_dlange( '1', n, n, a, lda, dum )
           if( scalea ) then
              dum( 1_ilp ) = abnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
              abnrm = dum( 1_ilp )
           end if
           ! reduce to upper hessenberg form
           ! (workspace: need 2*n, prefer n+n*nb)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_dgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_dlacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate orthogonal matrix in vl
              ! (workspace: need 2*n-1, prefer n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vl, ldvl,work( iwrk ), &
                        lwork-iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_dlacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_dlacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate orthogonal matrix in vr
              ! (workspace: need 2*n-1, prefer n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( 'S', 'V', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           else
              ! compute eigenvalues only
              ! if condition numbers desired, compute schur form
              if( wntsnn ) then
                 job = 'E'
              else
                 job = 'S'
              end if
              ! (workspace: need 1, prefer hswork (see comments) )
              iwrk = itau
              call stdlib_dhseqr( job, 'N', n, ilo, ihi, a, lda, wr, wi, vr, ldvr,work( iwrk ), &
                        lwork-iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_dhseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (workspace: need 3*n, prefer n + 2*n*nb)
              call stdlib_dtrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1, ierr )
           end if
           ! compute condition numbers if desired
           ! (workspace: need n*n+6*n unless sense = 'e')
           if( .not.wntsnn ) then
              call stdlib_dtrsna( sense, 'A', select, n, a, lda, vl, ldvl, vr, ldvr,rconde, &
                        rcondv, n, nout, work( iwrk ), n, iwork,icond )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              call stdlib_dgebak( balanc, 'L', n, ilo, ihi, scale, n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_dnrm2( n, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_dlapy2( stdlib_dnrm2( n, vl( 1_ilp, i ), 1_ilp ),stdlib_dnrm2( n, &
                              vl( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vl( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( k ) = vl( k, i )**2_ilp + vl( k, i+1 )**2_ilp
                    end do
                    k = stdlib_idamax( n, work, 1_ilp )
                    call stdlib_dlartg( vl( k, i ), vl( k, i+1 ), cs, sn, r )
                    call stdlib_drot( n, vl( 1_ilp, i ), 1_ilp, vl( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vl( k, i+1 ) = zero
                 end if
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              call stdlib_dgebak( balanc, 'R', n, ilo, ihi, scale, n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 if( wi( i )==zero ) then
                    scl = one / stdlib_dnrm2( n, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 else if( wi( i )>zero ) then
                    scl = one / stdlib_dlapy2( stdlib_dnrm2( n, vr( 1_ilp, i ), 1_ilp ),stdlib_dnrm2( n, &
                              vr( 1_ilp, i+1 ), 1_ilp ) )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                    call stdlib_dscal( n, scl, vr( 1_ilp, i+1 ), 1_ilp )
                    do k = 1, n
                       work( k ) = vr( k, i )**2_ilp + vr( k, i+1 )**2_ilp
                    end do
                    k = stdlib_idamax( n, work, 1_ilp )
                    call stdlib_dlartg( vr( k, i ), vr( k, i+1 ), cs, sn, r )
                    call stdlib_drot( n, vr( 1_ilp, i ), 1_ilp, vr( 1_ilp, i+1 ), 1_ilp, cs, sn )
                    vr( k, i+1 ) = zero
                 end if
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wr( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, wi( info+1 ),max( n-info, 1_ilp &
                        ), ierr )
              if( info==0_ilp ) then
                 if( ( wntsnv .or. wntsnb ) .and. icond==0_ilp )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale,&
                            anrm, n, 1_ilp, rcondv, n,ierr )
              else
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wr, n,ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgeevx


     module subroutine stdlib_cgeevx( balanc, jobvl, jobvr, sense, n, a, lda, w, vl,ldvl, vr, ldvr, ilo, &
     !! CGEEVX computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
     !! (RCONDE), and reciprocal condition numbers for the right
     !! eigenvectors (RCONDV).
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
     !! Balancing a matrix means permuting the rows and columns to make it
     !! more nearly upper triangular, and applying a diagonal similarity
     !! transformation D * A * D**(-1), where D is a diagonal matrix, to
     !! make its rows and columns closer in norm and the condition numbers
     !! of its eigenvalues and eigenvectors smaller.  The computed
     !! reciprocal condition numbers correspond to the balanced matrix.
     !! Permuting rows and columns will not change the condition numbers
     !! (in exact arithmetic) but diagonal scaling will.  For further
     !! explanation of balancing, see section 4.10.2_sp of the LAPACK
     !! Users' Guide.
               ihi, scale, abnrm, rconde,rcondv, work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm
           ! Array Arguments 
           real(sp), intent(out) :: rconde(*), rcondv(*), rwork(*), scale(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr, wntsnb, wntsne, wntsnn, wntsnv
           character :: job, side
           integer(ilp) :: hswork, i, icond, ierr, itau, iwrk, k, lwork_trevc, maxwrk, minwrk, &
                     nout
           real(sp) :: anrm, bignum, cscale, eps, scl, smlnum
           complex(sp) :: tmp
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           wntsnn = stdlib_lsame( sense, 'N' )
           wntsne = stdlib_lsame( sense, 'E' )
           wntsnv = stdlib_lsame( sense, 'V' )
           wntsnb = stdlib_lsame( sense, 'B' )
           if( .not.( stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'S' ) &
                     .or.stdlib_lsame( balanc, 'P' ) .or. stdlib_lsame( balanc, 'B' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -2_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wntsnn .or. wntsne .or. wntsnb .or. wntsnv ) .or.( ( wntsne .or. &
                     wntsnb ) .and. .not.( wantvl .and.wantvr ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -10_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -12_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_chseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    call stdlib_ctrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, lwork_trevc )
                    call stdlib_chseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vl, ldvl,work, -1_ilp, info )
                              
                 else if( wantvr ) then
                    call stdlib_ctrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, lwork_trevc )
                    call stdlib_chseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 else
                    if( wntsnn ) then
                       call stdlib_chseqr( 'E', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                                 
                    else
                       call stdlib_chseqr( 'S', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                                 
                    end if
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 if( ( .not.wantvl ) .and. ( .not.wantvr ) ) then
                    minwrk = 2_ilp*n
                    if( .not.( wntsnn .or. wntsne ) )minwrk = max( minwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, hswork )
                    if( .not.( wntsnn .or. wntsne ) )maxwrk = max( maxwrk, n*n + 2_ilp*n )
                 else
                    minwrk = 2_ilp*n
                    if( .not.( wntsnn .or. wntsne ) )minwrk = max( minwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, hswork )
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'CUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    if( .not.( wntsnn .or. wntsne ) )maxwrk = max( maxwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, 2_ilp*n )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           icond = 0_ilp
           anrm = stdlib_clange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix and compute abnrm
           call stdlib_cgebal( balanc, n, a, lda, ilo, ihi, scale, ierr )
           abnrm = stdlib_clange( '1', n, n, a, lda, dum )
           if( scalea ) then
              dum( 1_ilp ) = abnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
              abnrm = dum( 1_ilp )
           end if
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_cgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_clacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate unitary matrix in vl
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vl, ldvl,work( iwrk ), lwork-&
                        iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_clacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_clacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate unitary matrix in vr
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           else
              ! compute eigenvalues only
              ! if condition numbers desired, compute schur form
              if( wntsnn ) then
                 job = 'E'
              else
                 job = 'S'
              end if
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_chseqr( job, 'N', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_chseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (cworkspace: need 2*n, prefer n + 2*n*nb)
              ! (rworkspace: need n)
              call stdlib_ctrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1,rwork, n, ierr )
           end if
           ! compute condition numbers if desired
           ! (cworkspace: need n*n+2*n unless sense = 'e')
           ! (rworkspace: need 2*n unless sense = 'e')
           if( .not.wntsnn ) then
              call stdlib_ctrsna( sense, 'A', select, n, a, lda, vl, ldvl, vr, ldvr,rconde, &
                        rcondv, n, nout, work( iwrk ), n, rwork,icond )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              call stdlib_cgebak( balanc, 'L', n, ilo, ihi, scale, n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_scnrm2( n, vl( 1_ilp, i ), 1_ilp )
                 call stdlib_csscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( k ) = real( vl( k, i ),KIND=sp)**2_ilp +aimag( vl( k, i ) )**2_ilp
                 end do
                 k = stdlib_isamax( n, rwork, 1_ilp )
                 tmp = conjg( vl( k, i ) ) / sqrt( rwork( k ) )
                 call stdlib_cscal( n, tmp, vl( 1_ilp, i ), 1_ilp )
                 vl( k, i ) = cmplx( real( vl( k, i ),KIND=sp), zero,KIND=sp)
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              call stdlib_cgebak( balanc, 'R', n, ilo, ihi, scale, n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_scnrm2( n, vr( 1_ilp, i ), 1_ilp )
                 call stdlib_csscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( k ) = real( vr( k, i ),KIND=sp)**2_ilp +aimag( vr( k, i ) )**2_ilp
                 end do
                 k = stdlib_isamax( n, rwork, 1_ilp )
                 tmp = conjg( vr( k, i ) ) / sqrt( rwork( k ) )
                 call stdlib_cscal( n, tmp, vr( 1_ilp, i ), 1_ilp )
                 vr( k, i ) = cmplx( real( vr( k, i ),KIND=sp), zero,KIND=sp)
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, w( info+1 ),max( n-info, 1_ilp )&
                        , ierr )
              if( info==0_ilp ) then
                 if( ( wntsnv .or. wntsnb ) .and. icond==0_ilp )call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale,&
                            anrm, n, 1_ilp, rcondv, n,ierr )
              else
                 call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, w, n, ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgeevx

     module subroutine stdlib_zgeevx( balanc, jobvl, jobvr, sense, n, a, lda, w, vl,ldvl, vr, ldvr, ilo, &
     !! ZGEEVX computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues and, optionally, the left and/or right eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
     !! (RCONDE), and reciprocal condition numbers for the right
     !! eigenvectors (RCONDV).
     !! The right eigenvector v(j) of A satisfies
     !! A * v(j) = lambda(j) * v(j)
     !! where lambda(j) is its eigenvalue.
     !! The left eigenvector u(j) of A satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H
     !! where u(j)**H denotes the conjugate transpose of u(j).
     !! The computed eigenvectors are normalized to have Euclidean norm
     !! equal to 1 and largest component real.
     !! Balancing a matrix means permuting the rows and columns to make it
     !! more nearly upper triangular, and applying a diagonal similarity
     !! transformation D * A * D**(-1), where D is a diagonal matrix, to
     !! make its rows and columns closer in norm and the condition numbers
     !! of its eigenvalues and eigenvectors smaller.  The computed
     !! reciprocal condition numbers correspond to the balanced matrix.
     !! Permuting rows and columns will not change the condition numbers
     !! (in exact arithmetic) but diagonal scaling will.  For further
     !! explanation of balancing, see section 4.10.2_dp of the LAPACK
     !! Users' Guide.
               ihi, scale, abnrm, rconde,rcondv, work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm
           ! Array Arguments 
           real(dp), intent(out) :: rconde(*), rcondv(*), rwork(*), scale(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantvl, wantvr, wntsnb, wntsne, wntsnn, wntsnv
           character :: job, side
           integer(ilp) :: hswork, i, icond, ierr, itau, iwrk, k, lwork_trevc, maxwrk, minwrk, &
                     nout
           real(dp) :: anrm, bignum, cscale, eps, scl, smlnum
           complex(dp) :: tmp
           ! Local Arrays 
           logical(lk) :: select(1_ilp)
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvl = stdlib_lsame( jobvl, 'V' )
           wantvr = stdlib_lsame( jobvr, 'V' )
           wntsnn = stdlib_lsame( sense, 'N' )
           wntsne = stdlib_lsame( sense, 'E' )
           wntsnv = stdlib_lsame( sense, 'V' )
           wntsnb = stdlib_lsame( sense, 'B' )
           if( .not.( stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'S' ) &
                     .or.stdlib_lsame( balanc, 'P' ) .or. stdlib_lsame( balanc, 'B' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantvl ) .and. ( .not.stdlib_lsame( jobvl, 'N' ) ) ) then
              info = -2_ilp
           else if( ( .not.wantvr ) .and. ( .not.stdlib_lsame( jobvr, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wntsnn .or. wntsne .or. wntsnb .or. wntsnv ) .or.( ( wntsne .or. &
                     wntsnb ) .and. .not.( wantvl .and.wantvr ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( wantvl .and. ldvl<n ) ) then
              info = -10_ilp
           else if( ldvr<1_ilp .or. ( wantvr .and. ldvr<n ) ) then
              info = -12_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_zhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 if( wantvl ) then
                    call stdlib_ztrevc3( 'L', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, lwork_trevc )
                    call stdlib_zhseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vl, ldvl,work, -1_ilp, info )
                              
                 else if( wantvr ) then
                    call stdlib_ztrevc3( 'R', 'B', select, n, a, lda,vl, ldvl, vr, ldvr,n, nout, &
                              work, -1_ilp, rwork, -1_ilp, ierr )
                    lwork_trevc = int( work(1_ilp),KIND=ilp)
                    maxwrk = max( maxwrk, lwork_trevc )
                    call stdlib_zhseqr( 'S', 'V', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                              
                 else
                    if( wntsnn ) then
                       call stdlib_zhseqr( 'E', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                                 
                    else
                       call stdlib_zhseqr( 'S', 'N', n, 1_ilp, n, a, lda, w, vr, ldvr,work, -1_ilp, info )
                                 
                    end if
                 end if
                 hswork = int( work(1_ilp),KIND=ilp)
                 if( ( .not.wantvl ) .and. ( .not.wantvr ) ) then
                    minwrk = 2_ilp*n
                    if( .not.( wntsnn .or. wntsne ) )minwrk = max( minwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, hswork )
                    if( .not.( wntsnn .or. wntsne ) )maxwrk = max( maxwrk, n*n + 2_ilp*n )
                 else
                    minwrk = 2_ilp*n
                    if( .not.( wntsnn .or. wntsne ) )minwrk = max( minwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, hswork )
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'ZUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    if( .not.( wntsnn .or. wntsne ) )maxwrk = max( maxwrk, n*n + 2_ilp*n )
                    maxwrk = max( maxwrk, 2_ilp*n )
                 end if
                 maxwrk = max( maxwrk, minwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           icond = 0_ilp
           anrm = stdlib_zlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! balance the matrix and compute abnrm
           call stdlib_zgebal( balanc, n, a, lda, ilo, ihi, scale, ierr )
           abnrm = stdlib_zlange( '1', n, n, a, lda, dum )
           if( scalea ) then
              dum( 1_ilp ) = abnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
              abnrm = dum( 1_ilp )
           end if
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = itau + n
           call stdlib_zgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvl ) then
              ! want left eigenvectors
              ! copy householder vectors to vl
              side = 'L'
              call stdlib_zlacpy( 'L', n, n, a, lda, vl, ldvl )
              ! generate unitary matrix in vl
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vl, ldvl, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vl
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vl, ldvl,work( iwrk ), lwork-&
                        iwrk+1, info )
              if( wantvr ) then
                 ! want left and right eigenvectors
                 ! copy schur vectors to vr
                 side = 'B'
                 call stdlib_zlacpy( 'F', n, n, vl, ldvl, vr, ldvr )
              end if
           else if( wantvr ) then
              ! want right eigenvectors
              ! copy householder vectors to vr
              side = 'R'
              call stdlib_zlacpy( 'L', n, n, a, lda, vr, ldvr )
              ! generate unitary matrix in vr
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vr, ldvr, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
              ! perform qr iteration, accumulating schur vectors in vr
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( 'S', 'V', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           else
              ! compute eigenvalues only
              ! if condition numbers desired, compute schur form
              if( wntsnn ) then
                 job = 'E'
              else
                 job = 'S'
              end if
              ! (cworkspace: need 1, prefer hswork (see comments) )
              ! (rworkspace: none)
              iwrk = itau
              call stdlib_zhseqr( job, 'N', n, ilo, ihi, a, lda, w, vr, ldvr,work( iwrk ), lwork-&
                        iwrk+1, info )
           end if
           ! if info /= 0 from stdlib_zhseqr, then quit
           if( info/=0 )go to 50
           if( wantvl .or. wantvr ) then
              ! compute left and/or right eigenvectors
              ! (cworkspace: need 2*n, prefer n + 2*n*nb)
              ! (rworkspace: need n)
              call stdlib_ztrevc3( side, 'B', select, n, a, lda, vl, ldvl, vr, ldvr,n, nout, work(&
                         iwrk ), lwork-iwrk+1,rwork, n, ierr )
           end if
           ! compute condition numbers if desired
           ! (cworkspace: need n*n+2*n unless sense = 'e')
           ! (rworkspace: need 2*n unless sense = 'e')
           if( .not.wntsnn ) then
              call stdlib_ztrsna( sense, 'A', select, n, a, lda, vl, ldvl, vr, ldvr,rconde, &
                        rcondv, n, nout, work( iwrk ), n, rwork,icond )
           end if
           if( wantvl ) then
              ! undo balancing of left eigenvectors
              call stdlib_zgebak( balanc, 'L', n, ilo, ihi, scale, n, vl, ldvl,ierr )
              ! normalize left eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_dznrm2( n, vl( 1_ilp, i ), 1_ilp )
                 call stdlib_zdscal( n, scl, vl( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( k ) = real( vl( k, i ),KIND=dp)**2_ilp +aimag( vl( k, i ) )**2_ilp
                 end do
                 k = stdlib_idamax( n, rwork, 1_ilp )
                 tmp = conjg( vl( k, i ) ) / sqrt( rwork( k ) )
                 call stdlib_zscal( n, tmp, vl( 1_ilp, i ), 1_ilp )
                 vl( k, i ) = cmplx( real( vl( k, i ),KIND=dp), zero,KIND=dp)
              end do
           end if
           if( wantvr ) then
              ! undo balancing of right eigenvectors
              call stdlib_zgebak( balanc, 'R', n, ilo, ihi, scale, n, vr, ldvr,ierr )
              ! normalize right eigenvectors and make largest component real
              do i = 1, n
                 scl = one / stdlib_dznrm2( n, vr( 1_ilp, i ), 1_ilp )
                 call stdlib_zdscal( n, scl, vr( 1_ilp, i ), 1_ilp )
                 do k = 1, n
                    rwork( k ) = real( vr( k, i ),KIND=dp)**2_ilp +aimag( vr( k, i ) )**2_ilp
                 end do
                 k = stdlib_idamax( n, rwork, 1_ilp )
                 tmp = conjg( vr( k, i ) ) / sqrt( rwork( k ) )
                 call stdlib_zscal( n, tmp, vr( 1_ilp, i ), 1_ilp )
                 vr( k, i ) = cmplx( real( vr( k, i ),KIND=dp), zero,KIND=dp)
              end do
           end if
           ! undo scaling if necessary
           50 continue
           if( scalea ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-info, 1_ilp, w( info+1 ),max( n-info, 1_ilp )&
                        , ierr )
              if( info==0_ilp ) then
                 if( ( wntsnv .or. wntsnb ) .and. icond==0_ilp )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale,&
                            anrm, n, 1_ilp, rcondv, n,ierr )
              else
                 call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, w, n, ierr )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgeevx




     module subroutine stdlib_sgees( jobvs, sort, select, n, a, lda, sdim, wr, wi,vs, ldvs, work, lwork, &
     !! SGEES computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues, the real Schur form T, and, optionally, the matrix of
     !! Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! real Schur form so that selected eigenvalues are at the top left.
     !! The leading columns of Z then form an orthonormal basis for the
     !! invariant subspace corresponding to the selected eigenvalues.
     !! A matrix is in real Schur form if it is upper quasi-triangular with
     !! 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
     !! form
     !! [  a  b  ]
     !! [  c  a  ]
     !! where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
               bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           ! Function Arguments 
           procedure(stdlib_select_s) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, lastsl, lquery, lst2sl, scalea, wantst, wantvs
           integer(ilp) :: hswork, i, i1, i2, ibal, icond, ierr, ieval, ihi, ilo, inxt, ip, itau, &
                     iwrk, maxwrk, minwrk
           real(sp) :: anrm, bignum, cscale, eps, s, sep, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_shseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 3_ilp*n
                 call stdlib_shseqr( 'S', jobvs, n, 1_ilp, n, a, lda, wr, wi, vs, ldvs,work, -1_ilp, &
                           ieval )
                 hswork = work( 1_ilp )
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, n + hswork )
                 else
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    maxwrk = max( maxwrk, n + hswork )
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need n)
           ibal = 1_ilp
           call stdlib_sgebal( 'P', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (workspace: need 3*n, prefer 2*n+n*nb)
           itau = n + ibal
           iwrk = n + itau
           call stdlib_sgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_slacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate orthogonal matrix in vs
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (workspace: need n+1, prefer n+hswork (see comments) )
           iwrk = itau
           call stdlib_shseqr( 'S', jobvs, n, ilo, ihi, a, lda, wr, wi, vs, ldvs,work( iwrk ), &
                     lwork-iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wr, n, ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wi, n, ierr )
              end if
              do i = 1, n
                 bwork( i ) = select( wr( i ), wi( i ) )
              end do
              ! reorder eigenvalues and transform schur vectors
              ! (workspace: none needed)
              call stdlib_strsen( 'N', jobvs, bwork, n, a, lda, vs, ldvs, wr, wi,sdim, s, sep, &
                        work( iwrk ), lwork-iwrk+1, idum, 1_ilp,icond )
              if( icond>0_ilp )info = n + icond
           end if
           if( wantvs ) then
              ! undo balancing
              ! (workspace: need n)
              call stdlib_sgebak( 'P', 'R', n, ilo, ihi, work( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_slascl( 'H', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_scopy( n, a, lda+1, wr, 1_ilp )
              if( cscale==smlnum ) then
                 ! if scaling back towards underflow, adjust wi if an
                 ! offdiagonal element of a 2-by-2 block in the schur form
                 ! underflows.
                 if( ieval>0_ilp ) then
                    i1 = ieval + 1_ilp
                    i2 = ihi - 1_ilp
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi,max( ilo-1, 1_ilp ), &
                              ierr )
                 else if( wantst ) then
                    i1 = 1_ilp
                    i2 = n - 1_ilp
                 else
                    i1 = ilo
                    i2 = ihi - 1_ilp
                 end if
                 inxt = i1 - 1_ilp
                 loop_20: do i = i1, i2
                    if( i<inxt )cycle loop_20
                    if( wi( i )==zero ) then
                       inxt = i + 1_ilp
                    else
                       if( a( i+1, i )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                       else if( a( i+1, i )/=zero .and. a( i, i+1 )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                          if( i>1_ilp )call stdlib_sswap( i-1, a( 1_ilp, i ), 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                          if( n>i+1 )call stdlib_sswap( n-i-1, a( i, i+2 ), lda,a( i+1, i+2 ), &
                                    lda )
                          if( wantvs ) then
                             call stdlib_sswap( n, vs( 1_ilp, i ), 1_ilp, vs( 1_ilp, i+1 ), 1_ilp )
                          end if
                          a( i, i+1 ) = a( i+1, i )
                          a( i+1, i ) = zero
                       end if
                       inxt = i + 2_ilp
                    end if
                 end do loop_20
              end if
              ! undo scaling for the imaginary part of the eigenvalues
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-ieval, 1_ilp,wi( ieval+1 ), max( n-ieval,&
                         1_ilp ), ierr )
           end if
           if( wantst .and. info==0_ilp ) then
              ! check if reordering successful
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = select( wr( i ), wi( i ) )
                 if( wi( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgees

     module subroutine stdlib_dgees( jobvs, sort, select, n, a, lda, sdim, wr, wi,vs, ldvs, work, lwork, &
     !! DGEES computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues, the real Schur form T, and, optionally, the matrix of
     !! Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! real Schur form so that selected eigenvalues are at the top left.
     !! The leading columns of Z then form an orthonormal basis for the
     !! invariant subspace corresponding to the selected eigenvalues.
     !! A matrix is in real Schur form if it is upper quasi-triangular with
     !! 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
     !! form
     !! [  a  b  ]
     !! [  c  a  ]
     !! where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
               bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           ! Function Arguments 
           procedure(stdlib_select_d) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, lastsl, lquery, lst2sl, scalea, wantst, wantvs
           integer(ilp) :: hswork, i, i1, i2, ibal, icond, ierr, ieval, ihi, ilo, inxt, ip, itau, &
                     iwrk, maxwrk, minwrk
           real(dp) :: anrm, bignum, cscale, eps, s, sep, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_dhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 3_ilp*n
                 call stdlib_dhseqr( 'S', jobvs, n, 1_ilp, n, a, lda, wr, wi, vs, ldvs,work, -1_ilp, &
                           ieval )
                 hswork = work( 1_ilp )
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, n + hswork )
                 else
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'DORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    maxwrk = max( maxwrk, n + hswork )
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need n)
           ibal = 1_ilp
           call stdlib_dgebal( 'P', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (workspace: need 3*n, prefer 2*n+n*nb)
           itau = n + ibal
           iwrk = n + itau
           call stdlib_dgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_dlacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate orthogonal matrix in vs
              ! (workspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (workspace: need n+1, prefer n+hswork (see comments) )
           iwrk = itau
           call stdlib_dhseqr( 'S', jobvs, n, ilo, ihi, a, lda, wr, wi, vs, ldvs,work( iwrk ), &
                     lwork-iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wr, n, ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wi, n, ierr )
              end if
              do i = 1, n
                 bwork( i ) = select( wr( i ), wi( i ) )
              end do
              ! reorder eigenvalues and transform schur vectors
              ! (workspace: none needed)
              call stdlib_dtrsen( 'N', jobvs, bwork, n, a, lda, vs, ldvs, wr, wi,sdim, s, sep, &
                        work( iwrk ), lwork-iwrk+1, idum, 1_ilp,icond )
              if( icond>0_ilp )info = n + icond
           end if
           if( wantvs ) then
              ! undo balancing
              ! (workspace: need n)
              call stdlib_dgebak( 'P', 'R', n, ilo, ihi, work( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_dlascl( 'H', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_dcopy( n, a, lda+1, wr, 1_ilp )
              if( cscale==smlnum ) then
                 ! if scaling back towards underflow, adjust wi if an
                 ! offdiagonal element of a 2-by-2 block in the schur form
                 ! underflows.
                 if( ieval>0_ilp ) then
                    i1 = ieval + 1_ilp
                    i2 = ihi - 1_ilp
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi,max( ilo-1, 1_ilp ), &
                              ierr )
                 else if( wantst ) then
                    i1 = 1_ilp
                    i2 = n - 1_ilp
                 else
                    i1 = ilo
                    i2 = ihi - 1_ilp
                 end if
                 inxt = i1 - 1_ilp
                 loop_20: do i = i1, i2
                    if( i<inxt )cycle loop_20
                    if( wi( i )==zero ) then
                       inxt = i + 1_ilp
                    else
                       if( a( i+1, i )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                       else if( a( i+1, i )/=zero .and. a( i, i+1 )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                          if( i>1_ilp )call stdlib_dswap( i-1, a( 1_ilp, i ), 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                          if( n>i+1 )call stdlib_dswap( n-i-1, a( i, i+2 ), lda,a( i+1, i+2 ), &
                                    lda )
                          if( wantvs ) then
                             call stdlib_dswap( n, vs( 1_ilp, i ), 1_ilp, vs( 1_ilp, i+1 ), 1_ilp )
                          end if
                          a( i, i+1 ) = a( i+1, i )
                          a( i+1, i ) = zero
                       end if
                       inxt = i + 2_ilp
                    end if
                 end do loop_20
              end if
              ! undo scaling for the imaginary part of the eigenvalues
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-ieval, 1_ilp,wi( ieval+1 ), max( n-ieval,&
                         1_ilp ), ierr )
           end if
           if( wantst .and. info==0_ilp ) then
              ! check if reordering successful
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = select( wr( i ), wi( i ) )
                 if( wi( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgees


     module subroutine stdlib_cgees( jobvs, sort, select, n, a, lda, sdim, w, vs,ldvs, work, lwork, &
     !! CGEES computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues, the Schur form T, and, optionally, the matrix of Schur
     !! vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! Schur form so that selected eigenvalues are at the top left.
     !! The leading columns of Z then form an orthonormal basis for the
     !! invariant subspace corresponding to the selected eigenvalues.
     !! A complex matrix is in Schur form if it is upper triangular.
               rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vs(ldvs,*), w(*), work(*)
           ! Function Arguments 
           procedure(stdlib_select_c) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantst, wantvs
           integer(ilp) :: hswork, i, ibal, icond, ierr, ieval, ihi, ilo, itau, iwrk, maxwrk, &
                     minwrk
           real(sp) :: anrm, bignum, cscale, eps, s, sep, smlnum
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -10_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_chseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 call stdlib_chseqr( 'S', jobvs, n, 1_ilp, n, a, lda, w, vs, ldvs,work, -1_ilp, ieval )
                           
                 hswork = real( work( 1_ilp ),KIND=sp)
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, hswork )
                 else
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'CUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    maxwrk = max( maxwrk, hswork )
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_cgebal( 'P', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = n + itau
           call stdlib_cgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_clacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate unitary matrix in vs
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (cworkspace: need 1, prefer hswork (see comments) )
           ! (rworkspace: none)
           iwrk = itau
           call stdlib_chseqr( 'S', jobvs, n, ilo, ihi, a, lda, w, vs, ldvs,work( iwrk ), lwork-&
                     iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, w, n, ierr )
              do i = 1, n
                 bwork( i ) = select( w( i ) )
              end do
              ! reorder eigenvalues and transform schur vectors
              ! (cworkspace: none)
              ! (rworkspace: none)
              call stdlib_ctrsen( 'N', jobvs, bwork, n, a, lda, vs, ldvs, w, sdim,s, sep, work( &
                        iwrk ), lwork-iwrk+1, icond )
           end if
           if( wantvs ) then
              ! undo balancing
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_cgebak( 'P', 'R', n, ilo, ihi, rwork( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_ccopy( n, a, lda+1, w, 1_ilp )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgees

     module subroutine stdlib_zgees( jobvs, sort, select, n, a, lda, sdim, w, vs,ldvs, work, lwork, &
     !! ZGEES computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues, the Schur form T, and, optionally, the matrix of Schur
     !! vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! Schur form so that selected eigenvalues are at the top left.
     !! The leading columns of Z then form an orthonormal basis for the
     !! invariant subspace corresponding to the selected eigenvalues.
     !! A complex matrix is in Schur form if it is upper triangular.
               rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vs(ldvs,*), w(*), work(*)
           ! Function Arguments 
           procedure(stdlib_select_z) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantst, wantvs
           integer(ilp) :: hswork, i, ibal, icond, ierr, ieval, ihi, ilo, itau, iwrk, maxwrk, &
                     minwrk
           real(dp) :: anrm, bignum, cscale, eps, s, sep, smlnum
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -10_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_zhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 call stdlib_zhseqr( 'S', jobvs, n, 1_ilp, n, a, lda, w, vs, ldvs,work, -1_ilp, ieval )
                           
                 hswork = real( work( 1_ilp ),KIND=dp)
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, hswork )
                 else
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'ZUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    maxwrk = max( maxwrk, hswork )
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_zgebal( 'P', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = n + itau
           call stdlib_zgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_zlacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate unitary matrix in vs
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (cworkspace: need 1, prefer hswork (see comments) )
           ! (rworkspace: none)
           iwrk = itau
           call stdlib_zhseqr( 'S', jobvs, n, ilo, ihi, a, lda, w, vs, ldvs,work( iwrk ), lwork-&
                     iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, w, n, ierr )
              do i = 1, n
                 bwork( i ) = select( w( i ) )
              end do
              ! reorder eigenvalues and transform schur vectors
              ! (cworkspace: none)
              ! (rworkspace: none)
              call stdlib_ztrsen( 'N', jobvs, bwork, n, a, lda, vs, ldvs, w, sdim,s, sep, work( &
                        iwrk ), lwork-iwrk+1, icond )
           end if
           if( wantvs ) then
              ! undo balancing
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_zgebak( 'P', 'R', n, ilo, ihi, rwork( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_zcopy( n, a, lda+1, w, 1_ilp )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgees




     module subroutine stdlib_sgeesx( jobvs, sort, select, sense, n, a, lda, sdim,wr, wi, vs, ldvs, &
     !! SGEESX computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues, the real Schur form T, and, optionally, the matrix of
     !! Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! real Schur form so that selected eigenvalues are at the top left;
     !! computes a reciprocal condition number for the average of the
     !! selected eigenvalues (RCONDE); and computes a reciprocal condition
     !! number for the right invariant subspace corresponding to the
     !! selected eigenvalues (RCONDV).  The leading columns of Z form an
     !! orthonormal basis for this invariant subspace.
     !! For further explanation of the reciprocal condition numbers RCONDE
     !! and RCONDV, see Section 4.10_sp of the LAPACK Users' Guide (where
     !! these quantities are called s and sep respectively).
     !! A real matrix is in real Schur form if it is upper quasi-triangular
     !! with 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in
     !! the form
     !! [  a  b  ]
     !! [  c  a  ]
     !! where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
               rconde, rcondv, work, lwork,iwork, liwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, liwork, lwork, n
           real(sp), intent(out) :: rconde, rcondv
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           ! Function Arguments 
           procedure(stdlib_select_s) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, lastsl, lquery, lst2sl, scalea, wantsb, wantse, wantsn, wantst, &
                     wantsv, wantvs
           integer(ilp) :: hswork, i, i1, i2, ibal, icond, ierr, ieval, ihi, ilo, inxt, ip, itau, &
                     iwrk, lwrk, liwrk, maxwrk, minwrk
           real(sp) :: anrm, bignum, cscale, eps, smlnum
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -12_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "rworkspace:" describe the
             ! minimal amount of real workspace needed at that point in the
             ! code, as well as the preferred amount for good performance.
             ! iworkspace refers to integer workspace.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_shseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.
             ! if sense = 'e', 'v' or 'b', then the amount of workspace needed
             ! depends on sdim, which is computed by the routine stdlib_strsen later
             ! in the code.)
           if( info==0_ilp ) then
              liwrk = 1_ilp
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 lwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 3_ilp*n
                 call stdlib_shseqr( 'S', jobvs, n, 1_ilp, n, a, lda, wr, wi, vs, ldvs,work, -1_ilp, &
                           ieval )
                 hswork = work( 1_ilp )
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, n + hswork )
                 else
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    maxwrk = max( maxwrk, n + hswork )
                 end if
                 lwrk = maxwrk
                 if( .not.wantsn )lwrk = max( lwrk, n + ( n*n )/2_ilp )
                 if( wantsv .or. wantsb )liwrk = ( n*n )/4_ilp
              end if
              iwork( 1_ilp ) = liwrk
              work( 1_ilp ) = lwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -16_ilp
              else if( liwork<1_ilp .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEESX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_sgebal( 'P', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (rworkspace: need 3*n, prefer 2*n+n*nb)
           itau = n + ibal
           iwrk = n + itau
           call stdlib_sgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_slacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate orthogonal matrix in vs
              ! (rworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_sorghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (rworkspace: need n+1, prefer n+hswork (see comments) )
           iwrk = itau
           call stdlib_shseqr( 'S', jobvs, n, ilo, ihi, a, lda, wr, wi, vs, ldvs,work( iwrk ), &
                     lwork-iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wr, n, ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wi, n, ierr )
              end if
              do i = 1, n
                 bwork( i ) = select( wr( i ), wi( i ) )
              end do
              ! reorder eigenvalues, transform schur vectors, and compute
              ! reciprocal condition numbers
              ! (rworkspace: if sense is not 'n', need n+2*sdim*(n-sdim)
                           ! otherwise, need n )
              ! (iworkspace: if sense is 'v' or 'b', need sdim*(n-sdim)
                           ! otherwise, need 0 )
              call stdlib_strsen( sense, jobvs, bwork, n, a, lda, vs, ldvs, wr, wi,sdim, rconde, &
                        rcondv, work( iwrk ), lwork-iwrk+1,iwork, liwork, icond )
              if( .not.wantsn )maxwrk = max( maxwrk, n+2*sdim*( n-sdim ) )
              if( icond==-15_ilp ) then
                 ! not enough real workspace
                 info = -16_ilp
              else if( icond==-17_ilp ) then
                 ! not enough integer workspace
                 info = -18_ilp
              else if( icond>0_ilp ) then
                 ! stdlib_strsen failed to reorder or to restore standard schur form
                 info = icond + n
              end if
           end if
           if( wantvs ) then
              ! undo balancing
              ! (rworkspace: need n)
              call stdlib_sgebak( 'P', 'R', n, ilo, ihi, work( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_slascl( 'H', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_scopy( n, a, lda+1, wr, 1_ilp )
              if( ( wantsv .or. wantsb ) .and. info==0_ilp ) then
                 dum( 1_ilp ) = rcondv
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
                 rcondv = dum( 1_ilp )
              end if
              if( cscale==smlnum ) then
                 ! if scaling back towards underflow, adjust wi if an
                 ! offdiagonal element of a 2-by-2 block in the schur form
                 ! underflows.
                 if( ieval>0_ilp ) then
                    i1 = ieval + 1_ilp
                    i2 = ihi - 1_ilp
                    call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
                 else if( wantst ) then
                    i1 = 1_ilp
                    i2 = n - 1_ilp
                 else
                    i1 = ilo
                    i2 = ihi - 1_ilp
                 end if
                 inxt = i1 - 1_ilp
                 loop_20: do i = i1, i2
                    if( i<inxt )cycle loop_20
                    if( wi( i )==zero ) then
                       inxt = i + 1_ilp
                    else
                       if( a( i+1, i )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                       else if( a( i+1, i )/=zero .and. a( i, i+1 )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                          if( i>1_ilp )call stdlib_sswap( i-1, a( 1_ilp, i ), 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                          if( n>i+1 )call stdlib_sswap( n-i-1, a( i, i+2 ), lda,a( i+1, i+2 ), &
                                    lda )
                          if( wantvs ) then
                            call stdlib_sswap( n, vs( 1_ilp, i ), 1_ilp, vs( 1_ilp, i+1 ), 1_ilp )
                          end if
                          a( i, i+1 ) = a( i+1, i )
                          a( i+1, i ) = zero
                       end if
                       inxt = i + 2_ilp
                    end if
                 end do loop_20
              end if
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-ieval, 1_ilp,wi( ieval+1 ), max( n-ieval,&
                         1_ilp ), ierr )
           end if
           if( wantst .and. info==0_ilp ) then
              ! check if reordering successful
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = select( wr( i ), wi( i ) )
                 if( wi( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           work( 1_ilp ) = maxwrk
           if( wantsv .or. wantsb ) then
              iwork( 1_ilp ) = sdim*(n-sdim)
           else
              iwork( 1_ilp ) = 1_ilp
           end if
           return
     end subroutine stdlib_sgeesx

     module subroutine stdlib_dgeesx( jobvs, sort, select, sense, n, a, lda, sdim,wr, wi, vs, ldvs, &
     !! DGEESX computes for an N-by-N real nonsymmetric matrix A, the
     !! eigenvalues, the real Schur form T, and, optionally, the matrix of
     !! Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! real Schur form so that selected eigenvalues are at the top left;
     !! computes a reciprocal condition number for the average of the
     !! selected eigenvalues (RCONDE); and computes a reciprocal condition
     !! number for the right invariant subspace corresponding to the
     !! selected eigenvalues (RCONDV).  The leading columns of Z form an
     !! orthonormal basis for this invariant subspace.
     !! For further explanation of the reciprocal condition numbers RCONDE
     !! and RCONDV, see Section 4.10_dp of the LAPACK Users' Guide (where
     !! these quantities are called s and sep respectively).
     !! A real matrix is in real Schur form if it is upper quasi-triangular
     !! with 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in
     !! the form
     !! [  a  b  ]
     !! [  c  a  ]
     !! where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
               rconde, rcondv, work, lwork,iwork, liwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, liwork, lwork, n
           real(dp), intent(out) :: rconde, rcondv
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           ! Function Arguments 
           procedure(stdlib_select_d) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, lastsl, lquery, lst2sl, scalea, wantsb, wantse, wantsn, wantst, &
                     wantsv, wantvs
           integer(ilp) :: hswork, i, i1, i2, ibal, icond, ierr, ieval, ihi, ilo, inxt, ip, itau, &
                     iwrk, liwrk, lwrk, maxwrk, minwrk
           real(dp) :: anrm, bignum, cscale, eps, smlnum
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -12_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "rworkspace:" describe the
             ! minimal amount of real workspace needed at that point in the
             ! code, as well as the preferred amount for good performance.
             ! iworkspace refers to integer workspace.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_dhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.
             ! if sense = 'e', 'v' or 'b', then the amount of workspace needed
             ! depends on sdim, which is computed by the routine stdlib_dtrsen later
             ! in the code.)
           if( info==0_ilp ) then
              liwrk = 1_ilp
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 lwrk = 1_ilp
              else
                 maxwrk = 2_ilp*n + n*stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 3_ilp*n
                 call stdlib_dhseqr( 'S', jobvs, n, 1_ilp, n, a, lda, wr, wi, vs, ldvs,work, -1_ilp, &
                           ieval )
                 hswork = work( 1_ilp )
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, n + hswork )
                 else
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'DORGHR', ' ', n, 1_ilp, n,&
                               -1_ilp ) )
                    maxwrk = max( maxwrk, n + hswork )
                 end if
                 lwrk = maxwrk
                 if( .not.wantsn )lwrk = max( lwrk, n + ( n*n )/2_ilp )
                 if( wantsv .or. wantsb )liwrk = ( n*n )/4_ilp
              end if
              iwork( 1_ilp ) = liwrk
              work( 1_ilp ) = lwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -16_ilp
              else if( liwork<1_ilp .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEESX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_dgebal( 'P', n, a, lda, ilo, ihi, work( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (rworkspace: need 3*n, prefer 2*n+n*nb)
           itau = n + ibal
           iwrk = n + itau
           call stdlib_dgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_dlacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate orthogonal matrix in vs
              ! (rworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              call stdlib_dorghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (rworkspace: need n+1, prefer n+hswork (see comments) )
           iwrk = itau
           call stdlib_dhseqr( 'S', jobvs, n, ilo, ihi, a, lda, wr, wi, vs, ldvs,work( iwrk ), &
                     lwork-iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wr, n, ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, wi, n, ierr )
              end if
              do i = 1, n
                 bwork( i ) = select( wr( i ), wi( i ) )
              end do
              ! reorder eigenvalues, transform schur vectors, and compute
              ! reciprocal condition numbers
              ! (rworkspace: if sense is not 'n', need n+2*sdim*(n-sdim)
                           ! otherwise, need n )
              ! (iworkspace: if sense is 'v' or 'b', need sdim*(n-sdim)
                           ! otherwise, need 0 )
              call stdlib_dtrsen( sense, jobvs, bwork, n, a, lda, vs, ldvs, wr, wi,sdim, rconde, &
                        rcondv, work( iwrk ), lwork-iwrk+1,iwork, liwork, icond )
              if( .not.wantsn )maxwrk = max( maxwrk, n+2*sdim*( n-sdim ) )
              if( icond==-15_ilp ) then
                 ! not enough real workspace
                 info = -16_ilp
              else if( icond==-17_ilp ) then
                 ! not enough integer workspace
                 info = -18_ilp
              else if( icond>0_ilp ) then
                 ! stdlib_dtrsen failed to reorder or to restore standard schur form
                 info = icond + n
              end if
           end if
           if( wantvs ) then
              ! undo balancing
              ! (rworkspace: need n)
              call stdlib_dgebak( 'P', 'R', n, ilo, ihi, work( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_dlascl( 'H', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_dcopy( n, a, lda+1, wr, 1_ilp )
              if( ( wantsv .or. wantsb ) .and. info==0_ilp ) then
                 dum( 1_ilp ) = rcondv
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
                 rcondv = dum( 1_ilp )
              end if
              if( cscale==smlnum ) then
                 ! if scaling back towards underflow, adjust wi if an
                 ! offdiagonal element of a 2-by-2 block in the schur form
                 ! underflows.
                 if( ieval>0_ilp ) then
                    i1 = ieval + 1_ilp
                    i2 = ihi - 1_ilp
                    call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, ilo-1, 1_ilp, wi, n,ierr )
                 else if( wantst ) then
                    i1 = 1_ilp
                    i2 = n - 1_ilp
                 else
                    i1 = ilo
                    i2 = ihi - 1_ilp
                 end if
                 inxt = i1 - 1_ilp
                 loop_20: do i = i1, i2
                    if( i<inxt )cycle loop_20
                    if( wi( i )==zero ) then
                       inxt = i + 1_ilp
                    else
                       if( a( i+1, i )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                       else if( a( i+1, i )/=zero .and. a( i, i+1 )==zero ) then
                          wi( i ) = zero
                          wi( i+1 ) = zero
                          if( i>1_ilp )call stdlib_dswap( i-1, a( 1_ilp, i ), 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                          if( n>i+1 )call stdlib_dswap( n-i-1, a( i, i+2 ), lda,a( i+1, i+2 ), &
                                    lda )
                          if( wantvs ) then
                            call stdlib_dswap( n, vs( 1_ilp, i ), 1_ilp, vs( 1_ilp, i+1 ), 1_ilp )
                          end if
                          a( i, i+1 ) = a( i+1, i )
                          a( i+1, i ) = zero
                       end if
                       inxt = i + 2_ilp
                    end if
                 end do loop_20
              end if
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n-ieval, 1_ilp,wi( ieval+1 ), max( n-ieval,&
                         1_ilp ), ierr )
           end if
           if( wantst .and. info==0_ilp ) then
              ! check if reordering successful
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = select( wr( i ), wi( i ) )
                 if( wi( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           work( 1_ilp ) = maxwrk
           if( wantsv .or. wantsb ) then
              iwork( 1_ilp ) = max( 1_ilp, sdim*( n-sdim ) )
           else
              iwork( 1_ilp ) = 1_ilp
           end if
           return
     end subroutine stdlib_dgeesx


     module subroutine stdlib_cgeesx( jobvs, sort, select, sense, n, a, lda, sdim, w,vs, ldvs, rconde, &
     !! CGEESX computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues, the Schur form T, and, optionally, the matrix of Schur
     !! vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! Schur form so that selected eigenvalues are at the top left;
     !! computes a reciprocal condition number for the average of the
     !! selected eigenvalues (RCONDE); and computes a reciprocal condition
     !! number for the right invariant subspace corresponding to the
     !! selected eigenvalues (RCONDV).  The leading columns of Z form an
     !! orthonormal basis for this invariant subspace.
     !! For further explanation of the reciprocal condition numbers RCONDE
     !! and RCONDV, see Section 4.10_sp of the LAPACK Users' Guide (where
     !! these quantities are called s and sep respectively).
     !! A complex matrix is in Schur form if it is upper triangular.
               rcondv, work, lwork, rwork,bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           real(sp), intent(out) :: rconde, rcondv
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vs(ldvs,*), w(*), work(*)
           ! Function Arguments 
           procedure(stdlib_select_c) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantsb, wantse, wantsn, wantst, wantsv, wantvs
           integer(ilp) :: hswork, i, ibal, icond, ierr, ieval, ihi, ilo, itau, iwrk, lwrk, &
                     maxwrk, minwrk
           real(sp) :: anrm, bignum, cscale, eps, smlnum
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of real workspace needed at that point in the
             ! code, as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_chseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.
             ! if sense = 'e', 'v' or 'b', then the amount of workspace needed
             ! depends on sdim, which is computed by the routine stdlib_ctrsen later
             ! in the code.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 lwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 call stdlib_chseqr( 'S', jobvs, n, 1_ilp, n, a, lda, w, vs, ldvs,work, -1_ilp, ieval )
                           
                 hswork = real( work( 1_ilp ),KIND=sp)
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, hswork )
                 else
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'CUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    maxwrk = max( maxwrk, hswork )
                 end if
                 lwrk = maxwrk
                 if( .not.wantsn )lwrk = max( lwrk, ( n*n )/2_ilp )
              end if
              work( 1_ilp ) = lwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEESX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_cgebal( 'P', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = n + itau
           call stdlib_cgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_clacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate unitary matrix in vs
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cunghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (cworkspace: need 1, prefer hswork (see comments) )
           ! (rworkspace: none)
           iwrk = itau
           call stdlib_chseqr( 'S', jobvs, n, ilo, ihi, a, lda, w, vs, ldvs,work( iwrk ), lwork-&
                     iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea )call stdlib_clascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, w, n, ierr )
              do i = 1, n
                 bwork( i ) = select( w( i ) )
              end do
              ! reorder eigenvalues, transform schur vectors, and compute
              ! reciprocal condition numbers
              ! (cworkspace: if sense is not 'n', need 2*sdim*(n-sdim)
                           ! otherwise, need none )
              ! (rworkspace: none)
              call stdlib_ctrsen( sense, jobvs, bwork, n, a, lda, vs, ldvs, w, sdim,rconde, &
                        rcondv, work( iwrk ), lwork-iwrk+1,icond )
              if( .not.wantsn )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( icond==-14_ilp ) then
                 ! not enough complex workspace
                 info = -15_ilp
              end if
           end if
           if( wantvs ) then
              ! undo balancing
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_cgebak( 'P', 'R', n, ilo, ihi, rwork( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_ccopy( n, a, lda+1, w, 1_ilp )
              if( ( wantsv .or. wantsb ) .and. info==0_ilp ) then
                 dum( 1_ilp ) = rcondv
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
                 rcondv = dum( 1_ilp )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgeesx

     module subroutine stdlib_zgeesx( jobvs, sort, select, sense, n, a, lda, sdim, w,vs, ldvs, rconde, &
     !! ZGEESX computes for an N-by-N complex nonsymmetric matrix A, the
     !! eigenvalues, the Schur form T, and, optionally, the matrix of Schur
     !! vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
     !! Optionally, it also orders the eigenvalues on the diagonal of the
     !! Schur form so that selected eigenvalues are at the top left;
     !! computes a reciprocal condition number for the average of the
     !! selected eigenvalues (RCONDE); and computes a reciprocal condition
     !! number for the right invariant subspace corresponding to the
     !! selected eigenvalues (RCONDV).  The leading columns of Z form an
     !! orthonormal basis for this invariant subspace.
     !! For further explanation of the reciprocal condition numbers RCONDE
     !! and RCONDV, see Section 4.10_dp of the LAPACK Users' Guide (where
     !! these quantities are called s and sep respectively).
     !! A complex matrix is in Schur form if it is upper triangular.
               rcondv, work, lwork, rwork,bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           real(dp), intent(out) :: rconde, rcondv
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vs(ldvs,*), w(*), work(*)
           ! Function Arguments 
           procedure(stdlib_select_z) :: select
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, scalea, wantsb, wantse, wantsn, wantst, wantsv, wantvs
           integer(ilp) :: hswork, i, ibal, icond, ierr, ieval, ihi, ilo, itau, iwrk, lwrk, &
                     maxwrk, minwrk
           real(dp) :: anrm, bignum, cscale, eps, smlnum
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantvs = stdlib_lsame( jobvs, 'V' )
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp )
           if( ( .not.wantvs ) .and. ( .not.stdlib_lsame( jobvs, 'N' ) ) ) then
              info = -1_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvs<1_ilp .or. ( wantvs .and. ldvs<n ) ) then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of real workspace needed at that point in the
             ! code, as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to real
             ! workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.
             ! hswork refers to the workspace preferred by stdlib_zhseqr, as
             ! calculated below. hswork is computed assuming ilo=1 and ihi=n,
             ! the worst case.
             ! if sense = 'e', 'v' or 'b', then the amount of workspace needed
             ! depends on sdim, which is computed by the routine stdlib_ztrsen later
             ! in the code.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 lwrk = 1_ilp
              else
                 maxwrk = n + n*stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, 1_ilp, n, 0_ilp )
                 minwrk = 2_ilp*n
                 call stdlib_zhseqr( 'S', jobvs, n, 1_ilp, n, a, lda, w, vs, ldvs,work, -1_ilp, ieval )
                           
                 hswork = real( work( 1_ilp ),KIND=dp)
                 if( .not.wantvs ) then
                    maxwrk = max( maxwrk, hswork )
                 else
                    maxwrk = max( maxwrk, n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp, 'ZUNGHR',' ', n, 1_ilp, n, -&
                              1_ilp ) )
                    maxwrk = max( maxwrk, hswork )
                 end if
                 lwrk = maxwrk
                 if( .not.wantsn )lwrk = max( lwrk, ( n*n )/2_ilp )
              end if
              work( 1_ilp ) = lwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEESX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, dum )
           scalea = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              scalea = .true.
              cscale = smlnum
           else if( anrm>bignum ) then
              scalea = .true.
              cscale = bignum
           end if
           if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, cscale, n, n, a, lda, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (cworkspace: none)
           ! (rworkspace: need n)
           ibal = 1_ilp
           call stdlib_zgebal( 'P', n, a, lda, ilo, ihi, rwork( ibal ), ierr )
           ! reduce to upper hessenberg form
           ! (cworkspace: need 2*n, prefer n+n*nb)
           ! (rworkspace: none)
           itau = 1_ilp
           iwrk = n + itau
           call stdlib_zgehrd( n, ilo, ihi, a, lda, work( itau ), work( iwrk ),lwork-iwrk+1, ierr &
                     )
           if( wantvs ) then
              ! copy householder vectors to vs
              call stdlib_zlacpy( 'L', n, n, a, lda, vs, ldvs )
              ! generate unitary matrix in vs
              ! (cworkspace: need 2*n-1, prefer n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zunghr( n, ilo, ihi, vs, ldvs, work( itau ), work( iwrk ),lwork-iwrk+1, &
                        ierr )
           end if
           sdim = 0_ilp
           ! perform qr iteration, accumulating schur vectors in vs if desired
           ! (cworkspace: need 1, prefer hswork (see comments) )
           ! (rworkspace: none)
           iwrk = itau
           call stdlib_zhseqr( 'S', jobvs, n, ilo, ihi, a, lda, w, vs, ldvs,work( iwrk ), lwork-&
                     iwrk+1, ieval )
           if( ieval>0_ilp )info = ieval
           ! sort eigenvalues if desired
           if( wantst .and. info==0_ilp ) then
              if( scalea )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, n, 1_ilp, w, n, ierr )
              do i = 1, n
                 bwork( i ) = select( w( i ) )
              end do
              ! reorder eigenvalues, transform schur vectors, and compute
              ! reciprocal condition numbers
              ! (cworkspace: if sense is not 'n', need 2*sdim*(n-sdim)
                           ! otherwise, need none )
              ! (rworkspace: none)
              call stdlib_ztrsen( sense, jobvs, bwork, n, a, lda, vs, ldvs, w, sdim,rconde, &
                        rcondv, work( iwrk ), lwork-iwrk+1,icond )
              if( .not.wantsn )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( icond==-14_ilp ) then
                 ! not enough complex workspace
                 info = -15_ilp
              end if
           end if
           if( wantvs ) then
              ! undo balancing
              ! (cworkspace: none)
              ! (rworkspace: need n)
              call stdlib_zgebak( 'P', 'R', n, ilo, ihi, rwork( ibal ), n, vs, ldvs,ierr )
           end if
           if( scalea ) then
              ! undo scaling for the schur form of a
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, cscale, anrm, n, n, a, lda, ierr )
              call stdlib_zcopy( n, a, lda+1, w, 1_ilp )
              if( ( wantsv .or. wantsb ) .and. info==0_ilp ) then
                 dum( 1_ilp ) = rcondv
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, cscale, anrm, 1_ilp, 1_ilp, dum, 1_ilp, ierr )
                 rcondv = dum( 1_ilp )
              end if
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgeesx




     module subroutine stdlib_sggev3( jobvl, jobvr, n, a, lda, b, ldb, alphar,alphai, beta, vl, ldvl, vr,&
     !! SGGEV3 computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B .
     !! where u(j)**H is the conjugate-transpose of u(j).
                ldvr, work, lwork,info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, itau, &
                     iwrk, jc, jr, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -12_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -14_ilp
           else if( lwork<max( 1_ilp, 8_ilp*n ) .and. .not.lquery ) then
              info = -16_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_sgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 1_ilp, 8_ilp*n, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_sormqr( 'L', 'T', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_sgghd3( jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, work, &
                        -1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              if( ilvl ) then
                 call stdlib_sorgqr( n, n, n, vl, ldvl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
                 call stdlib_slaqz0( 'S', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                           beta, vl, ldvl, vr, ldvr,work, -1_ilp, 0_ilp, ierr )
                 lwkopt = max( lwkopt, 2_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              else
                 call stdlib_slaqz0( 'E', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                           beta, vl, ldvl, vr, ldvr,work, -1_ilp, 0_ilp, ierr )
                 lwkopt = max( lwkopt, 2_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = real( lwkopt,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGEV3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_sggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = iwrk
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           if( ilvl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_slaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_sgghd3( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        work( iwrk ), lwork+1-iwrk, ierr )
           else
              call stdlib_sgghd3( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_slaqz0( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 110
           end if
           ! compute eigenvectors
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_stgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 110
              end if
              ! undo balancing on vl and vr and normalization
              if( ilvl ) then
                 call stdlib_sggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, vl, &
                           ldvl, ierr )
                 loop_50: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_50
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_50
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                          vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_50
              end if
              if( ilvr ) then
                 call stdlib_sggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, vr, &
                           ldvr, ierr )
                 loop_100: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_100
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_100
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                          vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_100
              end if
              ! end of eigenvector calculation
           end if
           ! undo scaling if necessary
           110 continue
           if( ilascl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           return
     end subroutine stdlib_sggev3

     module subroutine stdlib_dggev3( jobvl, jobvr, n, a, lda, b, ldb, alphar,alphai, beta, vl, ldvl, vr,&
     !! DGGEV3 computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B .
     !! where u(j)**H is the conjugate-transpose of u(j).
                ldvr, work, lwork,info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, itau, &
                     iwrk, jc, jr, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -12_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -14_ilp
           else if( lwork<max( 1_ilp, 8_ilp*n ) .and. .not.lquery ) then
              info = -16_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_dgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max(1_ilp, 8_ilp*n, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_dormqr( 'L', 'T', n, n, n, b, ldb, work, a, lda, work, -1_ilp,ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              if( ilvl ) then
                 call stdlib_dorgqr( n, n, n, vl, ldvl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              end if
              if( ilv ) then
                 call stdlib_dgghd3( jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                           work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
                 call stdlib_dlaqz0( 'S', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                           beta, vl, ldvl, vr, ldvr,work, -1_ilp, 0_ilp, ierr )
                 lwkopt = max( lwkopt, 2_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              else
                 call stdlib_dgghd3( 'N', 'N', n, 1_ilp, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, work, -&
                           1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
                 call stdlib_dlaqz0( 'E', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                           beta, vl, ldvl, vr, ldvr,work, -1_ilp, 0_ilp, ierr )
                 lwkopt = max( lwkopt, 2_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGEV3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_dggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = iwrk
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           if( ilvl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_dlaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_dgghd3( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        work( iwrk ), lwork+1-iwrk, ierr )
           else
              call stdlib_dgghd3( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_dlaqz0( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 110
           end if
           ! compute eigenvectors
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_dtgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 110
              end if
              ! undo balancing on vl and vr and normalization
              if( ilvl ) then
                 call stdlib_dggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, vl, &
                           ldvl, ierr )
                 loop_50: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_50
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_50
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                          vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_50
              end if
              if( ilvr ) then
                 call stdlib_dggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, vr, &
                           ldvr, ierr )
                 loop_100: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_100
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_100
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                          vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_100
              end if
              ! end of eigenvector calculation
           end if
           ! undo scaling if necessary
           110 continue
           if( ilascl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dggev3


     module subroutine stdlib_cggev3( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
     !! CGGEV3 computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right generalized eigenvector v(j) corresponding to the
     !! generalized eigenvalue lambda(j) of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left generalized eigenvector u(j) corresponding to the
     !! generalized eigenvalues lambda(j) of (A,B) satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H * B
     !! where u(j)**H is the conjugate-transpose of u(j).
               work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, irwrk,&
                      itau, iwrk, jc, jr, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(sp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -15_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_cgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( n,  n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_cunmqr( 'L', 'C', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              if( ilvl ) then
                 call stdlib_cungqr( n, n, n, vl, ldvl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              end if
              if( ilv ) then
                 call stdlib_cgghd3( jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                           work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
                 call stdlib_claqz0( 'S', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vl, &
                           ldvl, vr, ldvr, work, -1_ilp,rwork, 0_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              else
                 call stdlib_cgghd3( 'N', 'N', n, 1_ilp, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, work, -&
                           1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
                 call stdlib_claqz0( 'E', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vl, &
                           ldvl, vr, ldvr, work, -1_ilp,rwork, 0_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGEV3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'E' )*stdlib_slamch( 'B' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_cggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           if( ilvl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_cungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_claset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_cgghd3( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        work( iwrk ), lwork+1-iwrk,ierr )
           else
              call stdlib_cgghd3( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur form and schur vectors)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_claqz0( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 70
           end if
           ! compute eigenvectors
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_ctgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), rwork( irwrk ),ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 70
              end if
              ! undo balancing on vl and vr and normalization
              if( ilvl ) then
                 call stdlib_cggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vl,&
                            ldvl, ierr )
                 loop_30: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vl( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_30
                    temp = one / temp
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 end do loop_30
              end if
              if( ilvr ) then
                 call stdlib_cggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vr,&
                            ldvr, ierr )
                 loop_60: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vr( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_60
                    temp = one / temp
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 end do loop_60
              end if
           end if
           ! undo scaling if necessary
           70 continue
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cggev3

     module subroutine stdlib_zggev3( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
     !! ZGGEV3 computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right generalized eigenvector v(j) corresponding to the
     !! generalized eigenvalue lambda(j) of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left generalized eigenvector u(j) corresponding to the
     !! generalized eigenvalues lambda(j) of (A,B) satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H * B
     !! where u(j)**H is the conjugate-transpose of u(j).
               work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, irwrk,&
                      itau, iwrk, jc, jr, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(dp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -15_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_zgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 1_ilp,  n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_zunmqr( 'L', 'C', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              if( ilvl ) then
                 call stdlib_zungqr( n, n, n, vl, ldvl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              end if
              if( ilv ) then
                 call stdlib_zgghd3( jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                           work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
                 call stdlib_zlaqz0( 'S', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vl, &
                           ldvl, vr, ldvr, work, -1_ilp,rwork, 0_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              else
                 call stdlib_zgghd3( jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                           work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
                 call stdlib_zlaqz0( 'E', jobvl, jobvr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vl, &
                           ldvl, vr, ldvr, work, -1_ilp,rwork, 0_ilp, ierr )
                 lwkopt = max( lwkopt, n+int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGEV3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'E' )*stdlib_dlamch( 'B' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_zggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           if( ilvl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_zungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_zgghd3( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        work( iwrk ), lwork+1-iwrk, ierr )
           else
              call stdlib_zgghd3( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur form and schur vectors)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_zlaqz0( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 70
           end if
           ! compute eigenvectors
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_ztgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), rwork( irwrk ),ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 70
              end if
              ! undo balancing on vl and vr and normalization
              if( ilvl ) then
                 call stdlib_zggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vl,&
                            ldvl, ierr )
                 loop_30: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vl( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_30
                    temp = one / temp
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 end do loop_30
              end if
              if( ilvr ) then
                 call stdlib_zggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vr,&
                            ldvr, ierr )
                 loop_60: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vr( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_60
                    temp = one / temp
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 end do loop_60
              end if
           end if
           ! undo scaling if necessary
           70 continue
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zggev3




     module subroutine stdlib_sggev( jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai,beta, vl, ldvl, vr, &
     !! SGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B .
     !! where u(j)**H is the conjugate-transpose of u(j).
               ldvr, work, lwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, itau, &
                     iwrk, jc, jr, maxwrk, minwrk
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -12_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -14_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              minwrk = max( 1_ilp, 8_ilp*n )
              maxwrk = max( 1_ilp, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, 1_ilp, n, 0_ilp ) ) )
              maxwrk = max( maxwrk, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, 1_ilp, n, 0_ilp ) ) )
                        
              if( ilvl ) then
                 maxwrk = max( maxwrk, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', n, 1_ilp, n, -1_ilp ) ) )
                           
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ! (workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_sggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = iwrk
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_slaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_sgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_sgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (workspace: need n)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_shgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 110
           end if
           ! compute eigenvectors
           ! (workspace: need 6*n)
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_stgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 110
              end if
              ! undo balancing on vl and vr and normalization
              ! (workspace: none needed)
              if( ilvl ) then
                 call stdlib_sggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, vl, &
                           ldvl, ierr )
                 loop_50: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_50
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_50
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                          vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_50
              end if
              if( ilvr ) then
                 call stdlib_sggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, vr, &
                           ldvr, ierr )
                 loop_100: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_100
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_100
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                          vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_100
              end if
              ! end of eigenvector calculation
           end if
           ! undo scaling if necessary
           110 continue
           if( ilascl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sggev

     module subroutine stdlib_dggev( jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai,beta, vl, ldvl, vr, &
     !! DGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B .
     !! where u(j)**H is the conjugate-transpose of u(j).
               ldvr, work, lwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, itau, &
                     iwrk, jc, jr, maxwrk, minwrk
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -12_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -14_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              minwrk = max( 1_ilp, 8_ilp*n )
              maxwrk = max( 1_ilp, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, 1_ilp, n, 0_ilp ) ) )
              maxwrk = max( maxwrk, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, 1_ilp, n, 0_ilp ) ) )
                        
              if( ilvl ) then
                 maxwrk = max( maxwrk, n*( 7_ilp +stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', n, 1_ilp, n, -1_ilp ) ) )
                           
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ! (workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_dggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = iwrk
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_dlaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_dgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_dgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (workspace: need n)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_dhgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 110
           end if
           ! compute eigenvectors
           ! (workspace: need 6*n)
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_dtgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 110
              end if
              ! undo balancing on vl and vr and normalization
              ! (workspace: none needed)
              if( ilvl ) then
                 call stdlib_dggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, vl, &
                           ldvl, ierr )
                 loop_50: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_50
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_50
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vl( jr, jc ) = vl( jr, jc )*temp
                          vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_50
              end if
              if( ilvr ) then
                 call stdlib_dggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, vr, &
                           ldvr, ierr )
                 loop_100: do jc = 1, n
                    if( alphai( jc )<zero )cycle loop_100
                    temp = zero
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) ) )
                       end do
                    else
                       do jr = 1, n
                          temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                       end do
                    end if
                    if( temp<smlnum )cycle loop_100
                    temp = one / temp
                    if( alphai( jc )==zero ) then
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                       end do
                    else
                       do jr = 1, n
                          vr( jr, jc ) = vr( jr, jc )*temp
                          vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                       end do
                    end if
                 end do loop_100
              end if
              ! end of eigenvector calculation
           end if
           ! undo scaling if necessary
           110 continue
           if( ilascl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dggev


     module subroutine stdlib_cggev( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
     !! CGGEV computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right generalized eigenvector v(j) corresponding to the
     !! generalized eigenvalue lambda(j) of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left generalized eigenvector u(j) corresponding to the
     !! generalized eigenvalues lambda(j) of (A,B) satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H * B
     !! where u(j)**H is the conjugate-transpose of u(j).
               work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, irwrk,&
                      itau, iwrk, jc, jr, lwkmin, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(sp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -13_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 2_ilp*n )
              lwkopt = max( 1_ilp, n + n*stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
              lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, 1_ilp, n, 0_ilp ) )
              if( ilvl ) then
                 lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', n, 1_ilp, n, -1_ilp ) )
                           
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery )info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'E' )*stdlib_slamch( 'B' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_cggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_cungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_claset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_cgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_cgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur form and schur vectors)
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_chgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 70
           end if
           ! compute eigenvectors
           ! (real workspace: need 2*n)
           ! (complex workspace: need 2*n)
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_ctgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), rwork( irwrk ),ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 70
              end if
              ! undo balancing on vl and vr and normalization
              ! (workspace: none needed)
              if( ilvl ) then
                 call stdlib_cggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vl,&
                            ldvl, ierr )
                 loop_30: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vl( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_30
                    temp = one / temp
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 end do loop_30
              end if
              if( ilvr ) then
                 call stdlib_cggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vr,&
                            ldvr, ierr )
                 loop_60: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vr( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_60
                    temp = one / temp
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 end do loop_60
              end if
           end if
           ! undo scaling if necessary
           70 continue
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cggev

     module subroutine stdlib_zggev( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
     !! ZGGEV computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right generalized eigenvector v(j) corresponding to the
     !! generalized eigenvalue lambda(j) of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j).
     !! The left generalized eigenvector u(j) corresponding to the
     !! generalized eigenvalues lambda(j) of (A,B) satisfies
     !! u(j)**H * A = lambda(j) * u(j)**H * B
     !! where u(j)**H is the conjugate-transpose of u(j).
               work, lwork, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery
           character :: chtemp
           integer(ilp) :: icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, in, iright, irows, irwrk,&
                      itau, iwrk, jc, jr, lwkmin, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(dp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -13_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 2_ilp*n )
              lwkopt = max( 1_ilp, n + n*stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
              lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, 1_ilp, n, 0_ilp ) )
              if( ilvl ) then
                 lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', n, 1_ilp, n, -1_ilp ) )
                           
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery )info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGEV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'E' )*stdlib_dlamch( 'B' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrices a, b to isolate eigenvalues if possible
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_zggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           if( ilv ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_zungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vr
           if( ilvr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           if( ilv ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_zgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_zgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur form and schur vectors)
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           if( ilv ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_zhgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 70
           end if
           ! compute eigenvectors
           ! (real workspace: need 2*n)
           ! (complex workspace: need 2*n)
           if( ilv ) then
              if( ilvl ) then
                 if( ilvr ) then
                    chtemp = 'B'
                 else
                    chtemp = 'L'
                 end if
              else
                 chtemp = 'R'
              end if
              call stdlib_ztgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl, ldvl,vr, ldvr, n, &
                        in, work( iwrk ), rwork( irwrk ),ierr )
              if( ierr/=0_ilp ) then
                 info = n + 2_ilp
                 go to 70
              end if
              ! undo balancing on vl and vr and normalization
              ! (workspace: none needed)
              if( ilvl ) then
                 call stdlib_zggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vl,&
                            ldvl, ierr )
                 loop_30: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vl( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_30
                    temp = one / temp
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 end do loop_30
              end if
              if( ilvr ) then
                 call stdlib_zggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), n, vr,&
                            ldvr, ierr )
                 loop_60: do jc = 1, n
                    temp = zero
                    do jr = 1, n
                       temp = max( temp, abs1( vr( jr, jc ) ) )
                    end do
                    if( temp<smlnum )cycle loop_60
                    temp = one / temp
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 end do loop_60
              end if
           end if
           ! undo scaling if necessary
           70 continue
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zggev




     module subroutine stdlib_sggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alphar, alphai, &
     !! SGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
     !! the eigenvalues (RCONDE), and reciprocal condition numbers for the
     !! right eigenvectors (RCONDV).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j) .
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B.
     !! where u(j)**H is the conjugate-transpose of u(j).
     beta, vl, ldvl, vr, ldvr, ilo,ihi, lscale, rscale, abnrm, bbnrm, rconde,rcondv, work, lwork, &
               iwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm, bbnrm
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), lscale(*), rconde(*), rcondv(*)&
                     , rscale(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery, noscl, pair, wantsb, wantse, &
                     wantsn, wantsv
           character :: chtemp
           integer(ilp) :: i, icols, ierr, ijobvl, ijobvr, in, irows, itau, iwrk, iwrk1, j, jc, &
                     jr, m, maxwrk, minwrk, mm
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           noscl  = stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'P' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.( noscl .or. stdlib_lsame( balanc, 'S' ) .or.stdlib_lsame( balanc, 'B' ) ) ) &
                     then
              info = -1_ilp
           else if( ijobvl<=0_ilp ) then
              info = -2_ilp
           else if( ijobvr<=0_ilp ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsb .or. wantsv ) )then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -14_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -16_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 if( noscl .and. .not.ilv ) then
                    minwrk = 2_ilp*n
                 else
                    minwrk = 6_ilp*n
                 end if
                 if( wantse ) then
                    minwrk = 10_ilp*n
                 else if( wantsv .or. wantsb ) then
                    minwrk = 2_ilp*n*( n + 4_ilp ) + 16_ilp
                 end if
                 maxwrk = minwrk
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 if( ilvl ) then
                    maxwrk = max( maxwrk, n +n*stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', n, 1_ilp, n, 0_ilp ) )
                              
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -26_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute and/or balance the matrix pair (a,b)
           ! (workspace: need 6*n if balanc = 's' or 'b', 1 otherwise)
           call stdlib_sggbal( balanc, n, a, lda, b, ldb, ilo, ihi, lscale, rscale,work, ierr )
                     
           ! compute abnrm and bbnrm
           abnrm = stdlib_slange( '1', n, n, a, lda, work( 1_ilp ) )
           if( ilascl ) then
              work( 1_ilp ) = abnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, 1_ilp, 1_ilp, work( 1_ilp ), 1_ilp,ierr )
              abnrm = work( 1_ilp )
           end if
           bbnrm = stdlib_slange( '1', n, n, b, ldb, work( 1_ilp ) )
           if( ilbscl ) then
              work( 1_ilp ) = bbnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, 1_ilp, 1_ilp, work( 1_ilp ), 1_ilp,ierr )
              bbnrm = work( 1_ilp )
           end if
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb )
           irows = ihi + 1_ilp - ilo
           if( ilv .or. .not.wantsn ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to a
           ! (workspace: need n, prefer n*nb)
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl and/or vr
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           if( ilvr )call stdlib_slaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv .or. .not.wantsn ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_sgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_sgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (workspace: need n)
           if( ilv .or. .not.wantsn ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_shgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work,lwork, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 130
           end if
           ! compute eigenvectors and estimate condition numbers if desired
           ! (workspace: stdlib_stgevc: need 6*n
                       ! stdlib_stgsna: need 2*n*(n+2)+16 if sense = 'v' or 'b',
                               ! need n otherwise )
           if( ilv .or. .not.wantsn ) then
              if( ilv ) then
                 if( ilvl ) then
                    if( ilvr ) then
                       chtemp = 'B'
                    else
                       chtemp = 'L'
                    end if
                 else
                    chtemp = 'R'
                 end if
                 call stdlib_stgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, n,&
                            in, work, ierr )
                 if( ierr/=0_ilp ) then
                    info = n + 2_ilp
                    go to 130
                 end if
              end if
              if( .not.wantsn ) then
                 ! compute eigenvectors (stdlib_stgevc) and estimate condition
                 ! numbers (stdlib_stgsna). note that the definition of the condition
                 ! number is not invariant under transformation (u,v) to
                 ! (q*u, z*v), where (u,v) are eigenvectors of the generalized
                 ! schur form (s,t), q and z are orthogonal matrices. in order
                 ! to avoid using extra 2*n*n workspace, we have to recalculate
                 ! eigenvectors and estimate one condition numbers at a time.
                 pair = .false.
                 loop_20: do i = 1, n
                    if( pair ) then
                       pair = .false.
                       cycle loop_20
                    end if
                    mm = 1_ilp
                    if( i<n ) then
                       if( a( i+1, i )/=zero ) then
                          pair = .true.
                          mm = 2_ilp
                       end if
                    end if
                    do j = 1, n
                       bwork( j ) = .false.
                    end do
                    if( mm==1_ilp ) then
                       bwork( i ) = .true.
                    else if( mm==2_ilp ) then
                       bwork( i ) = .true.
                       bwork( i+1 ) = .true.
                    end if
                    iwrk = mm*n + 1_ilp
                    iwrk1 = iwrk + mm*n
                    ! compute a pair of left and right eigenvectors.
                    ! (compute workspace: need up to 4*n + 6*n)
                    if( wantse .or. wantsb ) then
                       call stdlib_stgevc( 'B', 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                                 iwrk ), n, mm, m,work( iwrk1 ), ierr )
                       if( ierr/=0_ilp ) then
                          info = n + 2_ilp
                          go to 130
                       end if
                    end if
                    call stdlib_stgsna( sense, 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                    iwrk ), n, rconde( i ),rcondv( i ), mm, m, work( iwrk1 ),lwork-iwrk1+1, iwork,&
                               ierr )
                 end do loop_20
              end if
           end if
           ! undo balancing on vl and vr and normalization
           ! (workspace: none needed)
           if( ilvl ) then
              call stdlib_sggbak( balanc, 'L', n, ilo, ihi, lscale, rscale, n, vl,ldvl, ierr )
                        
              loop_70: do jc = 1, n
                 if( alphai( jc )<zero )cycle loop_70
                 temp = zero
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       temp = max( temp, abs( vl( jr, jc ) ) )
                    end do
                 else
                    do jr = 1, n
                       temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                    end do
                 end if
                 if( temp<smlnum )cycle loop_70
                 temp = one / temp
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 else
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                       vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                    end do
                 end if
              end do loop_70
           end if
           if( ilvr ) then
              call stdlib_sggbak( balanc, 'R', n, ilo, ihi, lscale, rscale, n, vr,ldvr, ierr )
                        
              loop_120: do jc = 1, n
                 if( alphai( jc )<zero )cycle loop_120
                 temp = zero
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       temp = max( temp, abs( vr( jr, jc ) ) )
                    end do
                 else
                    do jr = 1, n
                       temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                    end do
                 end if
                 if( temp<smlnum )cycle loop_120
                 temp = one / temp
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 else
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                       vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                    end do
                 end if
              end do loop_120
           end if
           ! undo scaling if necessary
           130 continue
           if( ilascl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sggevx

     module subroutine stdlib_dggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alphar, alphai, &
     !! DGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
     !! the generalized eigenvalues, and optionally, the left and/or right
     !! generalized eigenvectors.
     !! Optionally also, it computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
     !! the eigenvalues (RCONDE), and reciprocal condition numbers for the
     !! right eigenvectors (RCONDV).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j) .
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B.
     !! where u(j)**H is the conjugate-transpose of u(j).
     beta, vl, ldvl, vr, ldvr, ilo,ihi, lscale, rscale, abnrm, bbnrm, rconde,rcondv, work, lwork, &
               iwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm, bbnrm
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), lscale(*), rconde(*), rcondv(*)&
                     , rscale(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery, noscl, pair, wantsb, wantse, &
                     wantsn, wantsv
           character :: chtemp
           integer(ilp) :: i, icols, ierr, ijobvl, ijobvr, in, irows, itau, iwrk, iwrk1, j, jc, &
                     jr, m, maxwrk, minwrk, mm
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           noscl  = stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'P' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.( stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc,'S' ) .or. &
                     stdlib_lsame( balanc, 'P' ) .or. stdlib_lsame( balanc, 'B' ) ) )then
              info = -1_ilp
           else if( ijobvl<=0_ilp ) then
              info = -2_ilp
           else if( ijobvr<=0_ilp ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsb .or. wantsv ) )then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -14_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -16_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 if( noscl .and. .not.ilv ) then
                    minwrk = 2_ilp*n
                 else
                    minwrk = 6_ilp*n
                 end if
                 if( wantse .or. wantsb ) then
                    minwrk = 10_ilp*n
                 end if
                 if( wantsv .or. wantsb ) then
                    minwrk = max( minwrk, 2_ilp*n*( n + 4_ilp ) + 16_ilp )
                 end if
                 maxwrk = minwrk
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 if( ilvl ) then
                    maxwrk = max( maxwrk, n +n*stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', n, 1_ilp, n, 0_ilp ) )
                              
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -26_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute and/or balance the matrix pair (a,b)
           ! (workspace: need 6*n if balanc = 's' or 'b', 1 otherwise)
           call stdlib_dggbal( balanc, n, a, lda, b, ldb, ilo, ihi, lscale, rscale,work, ierr )
                     
           ! compute abnrm and bbnrm
           abnrm = stdlib_dlange( '1', n, n, a, lda, work( 1_ilp ) )
           if( ilascl ) then
              work( 1_ilp ) = abnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, 1_ilp, 1_ilp, work( 1_ilp ), 1_ilp,ierr )
              abnrm = work( 1_ilp )
           end if
           bbnrm = stdlib_dlange( '1', n, n, b, ldb, work( 1_ilp ) )
           if( ilbscl ) then
              work( 1_ilp ) = bbnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, 1_ilp, 1_ilp, work( 1_ilp ), 1_ilp,ierr )
              bbnrm = work( 1_ilp )
           end if
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb )
           irows = ihi + 1_ilp - ilo
           if( ilv .or. .not.wantsn ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to a
           ! (workspace: need n, prefer n*nb)
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl and/or vr
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           if( ilvr )call stdlib_dlaset( 'FULL', n, n, zero, one, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv .or. .not.wantsn ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_dgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_dgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (workspace: need n)
           if( ilv .or. .not.wantsn ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_dhgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work,lwork, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 130
           end if
           ! compute eigenvectors and estimate condition numbers if desired
           ! (workspace: stdlib_dtgevc: need 6*n
                       ! stdlib_dtgsna: need 2*n*(n+2)+16 if sense = 'v' or 'b',
                               ! need n otherwise )
           if( ilv .or. .not.wantsn ) then
              if( ilv ) then
                 if( ilvl ) then
                    if( ilvr ) then
                       chtemp = 'B'
                    else
                       chtemp = 'L'
                    end if
                 else
                    chtemp = 'R'
                 end if
                 call stdlib_dtgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, n,&
                            in, work, ierr )
                 if( ierr/=0_ilp ) then
                    info = n + 2_ilp
                    go to 130
                 end if
              end if
              if( .not.wantsn ) then
                 ! compute eigenvectors (stdlib_dtgevc) and estimate condition
                 ! numbers (stdlib_dtgsna). note that the definition of the condition
                 ! number is not invariant under transformation (u,v) to
                 ! (q*u, z*v), where (u,v) are eigenvectors of the generalized
                 ! schur form (s,t), q and z are orthogonal matrices. in order
                 ! to avoid using extra 2*n*n workspace, we have to recalculate
                 ! eigenvectors and estimate one condition numbers at a time.
                 pair = .false.
                 loop_20: do i = 1, n
                    if( pair ) then
                       pair = .false.
                       cycle loop_20
                    end if
                    mm = 1_ilp
                    if( i<n ) then
                       if( a( i+1, i )/=zero ) then
                          pair = .true.
                          mm = 2_ilp
                       end if
                    end if
                    do j = 1, n
                       bwork( j ) = .false.
                    end do
                    if( mm==1_ilp ) then
                       bwork( i ) = .true.
                    else if( mm==2_ilp ) then
                       bwork( i ) = .true.
                       bwork( i+1 ) = .true.
                    end if
                    iwrk = mm*n + 1_ilp
                    iwrk1 = iwrk + mm*n
                    ! compute a pair of left and right eigenvectors.
                    ! (compute workspace: need up to 4*n + 6*n)
                    if( wantse .or. wantsb ) then
                       call stdlib_dtgevc( 'B', 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                                 iwrk ), n, mm, m,work( iwrk1 ), ierr )
                       if( ierr/=0_ilp ) then
                          info = n + 2_ilp
                          go to 130
                       end if
                    end if
                    call stdlib_dtgsna( sense, 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                    iwrk ), n, rconde( i ),rcondv( i ), mm, m, work( iwrk1 ),lwork-iwrk1+1, iwork,&
                               ierr )
                 end do loop_20
              end if
           end if
           ! undo balancing on vl and vr and normalization
           ! (workspace: none needed)
           if( ilvl ) then
              call stdlib_dggbak( balanc, 'L', n, ilo, ihi, lscale, rscale, n, vl,ldvl, ierr )
                        
              loop_70: do jc = 1, n
                 if( alphai( jc )<zero )cycle loop_70
                 temp = zero
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       temp = max( temp, abs( vl( jr, jc ) ) )
                    end do
                 else
                    do jr = 1, n
                       temp = max( temp, abs( vl( jr, jc ) )+abs( vl( jr, jc+1 ) ) )
                    end do
                 end if
                 if( temp<smlnum )cycle loop_70
                 temp = one / temp
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                    end do
                 else
                    do jr = 1, n
                       vl( jr, jc ) = vl( jr, jc )*temp
                       vl( jr, jc+1 ) = vl( jr, jc+1 )*temp
                    end do
                 end if
              end do loop_70
           end if
           if( ilvr ) then
              call stdlib_dggbak( balanc, 'R', n, ilo, ihi, lscale, rscale, n, vr,ldvr, ierr )
                        
              loop_120: do jc = 1, n
                 if( alphai( jc )<zero )cycle loop_120
                 temp = zero
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       temp = max( temp, abs( vr( jr, jc ) ) )
                    end do
                 else
                    do jr = 1, n
                       temp = max( temp, abs( vr( jr, jc ) )+abs( vr( jr, jc+1 ) ) )
                    end do
                 end if
                 if( temp<smlnum )cycle loop_120
                 temp = one / temp
                 if( alphai( jc )==zero ) then
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                    end do
                 else
                    do jr = 1, n
                       vr( jr, jc ) = vr( jr, jc )*temp
                       vr( jr, jc+1 ) = vr( jr, jc+1 )*temp
                    end do
                 end if
              end do loop_120
           end if
           ! undo scaling if necessary
           130 continue
           if( ilascl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dggevx


     module subroutine stdlib_cggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alpha, beta, vl, &
     !! CGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B) the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! Optionally, it also computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
     !! the eigenvalues (RCONDE), and reciprocal condition numbers for the
     !! right eigenvectors (RCONDV).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j) .
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B.
     !! where u(j)**H is the conjugate-transpose of u(j).
     ldvl, vr, ldvr, ilo, ihi,lscale, rscale, abnrm, bbnrm, rconde, rcondv,work, lwork, rwork, &
               iwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm, bbnrm
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: lscale(*), rconde(*), rcondv(*), rscale(*), rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery, noscl, wantsb, wantse, wantsn, &
                     wantsv
           character :: chtemp
           integer(ilp) :: i, icols, ierr, ijobvl, ijobvr, in, irows, itau, iwrk, iwrk1, j, jc, &
                     jr, m, maxwrk, minwrk
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(sp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           noscl  = stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'P' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.( noscl .or. stdlib_lsame( balanc,'S' ) .or.stdlib_lsame( balanc, 'B' ) ) ) &
                     then
              info = -1_ilp
           else if( ijobvl<=0_ilp ) then
              info = -2_ilp
           else if( ijobvr<=0_ilp ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsb .or. wantsv ) )then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -13_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -15_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 minwrk = 2_ilp*n
                 if( wantse ) then
                    minwrk = 4_ilp*n
                 else if( wantsv .or. wantsb ) then
                    minwrk = 2_ilp*n*( n + 1_ilp)
                 end if
                 maxwrk = minwrk
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 if( ilvl ) then
                    maxwrk = max( maxwrk, n +n*stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', n, 1_ilp, n, 0_ilp ) )
                              
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -25_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute and/or balance the matrix pair (a,b)
           ! (real workspace: need 6*n if balanc = 's' or 'b', 1 otherwise)
           call stdlib_cggbal( balanc, n, a, lda, b, ldb, ilo, ihi, lscale, rscale,rwork, ierr )
                     
           ! compute abnrm and bbnrm
           abnrm = stdlib_clange( '1', n, n, a, lda, rwork( 1_ilp ) )
           if( ilascl ) then
              rwork( 1_ilp ) = abnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, 1_ilp, 1_ilp, rwork( 1_ilp ), 1_ilp,ierr )
              abnrm = rwork( 1_ilp )
           end if
           bbnrm = stdlib_clange( '1', n, n, b, ldb, rwork( 1_ilp ) )
           if( ilbscl ) then
              rwork( 1_ilp ) = bbnrm
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, 1_ilp, 1_ilp, rwork( 1_ilp ), 1_ilp,ierr )
              bbnrm = rwork( 1_ilp )
           end if
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb )
           irows = ihi + 1_ilp - ilo
           if( ilv .or. .not.wantsn ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the unitary transformation to a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl and/or vr
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_cungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           if( ilvr )call stdlib_claset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv .or. .not.wantsn ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_cgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_cgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           if( ilv .or. .not.wantsn ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_chgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 90
           end if
           ! compute eigenvectors and estimate condition numbers if desired
           ! stdlib_ctgevc: (complex workspace: need 2*n )
                   ! (real workspace:    need 2*n )
           ! stdlib_ctgsna: (complex workspace: need 2*n*n if sense='v' or 'b')
                   ! (integer workspace: need n+2 )
           if( ilv .or. .not.wantsn ) then
              if( ilv ) then
                 if( ilvl ) then
                    if( ilvr ) then
                       chtemp = 'B'
                    else
                       chtemp = 'L'
                    end if
                 else
                    chtemp = 'R'
                 end if
                 call stdlib_ctgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, n,&
                            in, work( iwrk ), rwork,ierr )
                 if( ierr/=0_ilp ) then
                    info = n + 2_ilp
                    go to 90
                 end if
              end if
              if( .not.wantsn ) then
                 ! compute eigenvectors (stdlib_ctgevc) and estimate condition
                 ! numbers (stdlib_ctgsna). note that the definition of the condition
                 ! number is not invariant under transformation (u,v) to
                 ! (q*u, z*v), where (u,v) are eigenvectors of the generalized
                 ! schur form (s,t), q and z are orthogonal matrices. in order
                 ! to avoid using extra 2*n*n workspace, we have to
                 ! re-calculate eigenvectors and estimate the condition numbers
                 ! one at a time.
                 do i = 1, n
                    do j = 1, n
                       bwork( j ) = .false.
                    end do
                    bwork( i ) = .true.
                    iwrk = n + 1_ilp
                    iwrk1 = iwrk + n
                    if( wantse .or. wantsb ) then
                       call stdlib_ctgevc( 'B', 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                                 iwrk ), n, 1_ilp, m,work( iwrk1 ), rwork, ierr )
                       if( ierr/=0_ilp ) then
                          info = n + 2_ilp
                          go to 90
                       end if
                    end if
                    call stdlib_ctgsna( sense, 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                    iwrk ), n, rconde( i ),rcondv( i ), 1_ilp, m, work( iwrk1 ),lwork-iwrk1+1, iwork, &
                              ierr )
                 end do
              end if
           end if
           ! undo balancing on vl and vr and normalization
           ! (workspace: none needed)
           if( ilvl ) then
              call stdlib_cggbak( balanc, 'L', n, ilo, ihi, lscale, rscale, n, vl,ldvl, ierr )
                        
              loop_50: do jc = 1, n
                 temp = zero
                 do jr = 1, n
                    temp = max( temp, abs1( vl( jr, jc ) ) )
                 end do
                 if( temp<smlnum )cycle loop_50
                 temp = one / temp
                 do jr = 1, n
                    vl( jr, jc ) = vl( jr, jc )*temp
                 end do
              end do loop_50
           end if
           if( ilvr ) then
              call stdlib_cggbak( balanc, 'R', n, ilo, ihi, lscale, rscale, n, vr,ldvr, ierr )
                        
              loop_80: do jc = 1, n
                 temp = zero
                 do jr = 1, n
                    temp = max( temp, abs1( vr( jr, jc ) ) )
                 end do
                 if( temp<smlnum )cycle loop_80
                 temp = one / temp
                 do jr = 1, n
                    vr( jr, jc ) = vr( jr, jc )*temp
                 end do
              end do loop_80
           end if
           ! undo scaling if necessary
           90 continue
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cggevx

     module subroutine stdlib_zggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alpha, beta, vl, &
     !! ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B) the generalized eigenvalues, and optionally, the left and/or
     !! right generalized eigenvectors.
     !! Optionally, it also computes a balancing transformation to improve
     !! the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
     !! LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
     !! the eigenvalues (RCONDE), and reciprocal condition numbers for the
     !! right eigenvectors (RCONDV).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar
     !! lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
     !! singular. It is usually represented as the pair (alpha,beta), as
     !! there is a reasonable interpretation for beta=0, and even for both
     !! being zero.
     !! The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! A * v(j) = lambda(j) * B * v(j) .
     !! The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
     !! of (A,B) satisfies
     !! u(j)**H * A  = lambda(j) * u(j)**H * B.
     !! where u(j)**H is the conjugate-transpose of u(j).
     ldvl, vr, ldvr, ilo, ihi,lscale, rscale, abnrm, bbnrm, rconde, rcondv,work, lwork, rwork, &
               iwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm, bbnrm
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: lscale(*), rconde(*), rcondv(*), rscale(*), rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: ilascl, ilbscl, ilv, ilvl, ilvr, lquery, noscl, wantsb, wantse, wantsn, &
                     wantsv
           character :: chtemp
           integer(ilp) :: i, icols, ierr, ijobvl, ijobvr, in, irows, itau, iwrk, iwrk1, j, jc, &
                     jr, m, maxwrk, minwrk
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, smlnum, temp
           complex(dp) :: x
           ! Local Arrays 
           logical(lk) :: ldumma(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvl = .false.
           else if( stdlib_lsame( jobvl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvl = .true.
           else
              ijobvl = -1_ilp
              ilvl = .false.
           end if
           if( stdlib_lsame( jobvr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvr = .false.
           else if( stdlib_lsame( jobvr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvr = .true.
           else
              ijobvr = -1_ilp
              ilvr = .false.
           end if
           ilv = ilvl .or. ilvr
           noscl  = stdlib_lsame( balanc, 'N' ) .or. stdlib_lsame( balanc, 'P' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.( noscl .or. stdlib_lsame( balanc,'S' ) .or.stdlib_lsame( balanc, 'B' ) ) ) &
                     then
              info = -1_ilp
           else if( ijobvl<=0_ilp ) then
              info = -2_ilp
           else if( ijobvr<=0_ilp ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsb .or. wantsv ) )then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvl<1_ilp .or. ( ilvl .and. ldvl<n ) ) then
              info = -13_ilp
           else if( ldvr<1_ilp .or. ( ilvr .and. ldvr<n ) ) then
              info = -15_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv. the workspace is
             ! computed assuming ilo = 1 and ihi = n, the worst case.)
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              else
                 minwrk = 2_ilp*n
                 if( wantse ) then
                    minwrk = 4_ilp*n
                 else if( wantsv .or. wantsb ) then
                    minwrk = 2_ilp*n*( n + 1_ilp)
                 end if
                 maxwrk = minwrk
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 maxwrk = max( maxwrk,n + n*stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, 1_ilp, n, 0_ilp ) )
                           
                 if( ilvl ) then
                    maxwrk = max( maxwrk, n +n*stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', n, 1_ilp, n, 0_ilp ) )
                              
                 end if
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -25_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGEVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute and/or balance the matrix pair (a,b)
           ! (real workspace: need 6*n if balanc = 's' or 'b', 1 otherwise)
           call stdlib_zggbal( balanc, n, a, lda, b, ldb, ilo, ihi, lscale, rscale,rwork, ierr )
                     
           ! compute abnrm and bbnrm
           abnrm = stdlib_zlange( '1', n, n, a, lda, rwork( 1_ilp ) )
           if( ilascl ) then
              rwork( 1_ilp ) = abnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, 1_ilp, 1_ilp, rwork( 1_ilp ), 1_ilp,ierr )
              abnrm = rwork( 1_ilp )
           end if
           bbnrm = stdlib_zlange( '1', n, n, b, ldb, rwork( 1_ilp ) )
           if( ilbscl ) then
              rwork( 1_ilp ) = bbnrm
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, 1_ilp, 1_ilp, rwork( 1_ilp ), 1_ilp,ierr )
              bbnrm = rwork( 1_ilp )
           end if
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb )
           irows = ihi + 1_ilp - ilo
           if( ilv .or. .not.wantsn ) then
              icols = n + 1_ilp - ilo
           else
              icols = irows
           end if
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the unitary transformation to a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vl and/or vr
           ! (workspace: need n, prefer n*nb)
           if( ilvl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vl, ldvl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vl( ilo+1, ilo ),&
                            ldvl )
              end if
              call stdlib_zungqr( irows, irows, irows, vl( ilo, ilo ), ldvl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           if( ilvr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vr, ldvr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           if( ilv .or. .not.wantsn ) then
              ! eigenvectors requested -- work on whole matrix.
              call stdlib_zgghrd( jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb, vl,ldvl, vr, ldvr, &
                        ierr )
           else
              call stdlib_zgghrd( 'N', 'N', irows, 1_ilp, irows, a( ilo, ilo ), lda,b( ilo, ilo ), &
                        ldb, vl, ldvl, vr, ldvr, ierr )
           end if
           ! perform qz algorithm (compute eigenvalues, and optionally, the
           ! schur forms and schur vectors)
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           if( ilv .or. .not.wantsn ) then
              chtemp = 'S'
           else
              chtemp = 'E'
           end if
           call stdlib_zhgeqz( chtemp, jobvl, jobvr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vl, &
                     ldvl, vr, ldvr, work( iwrk ),lwork+1-iwrk, rwork, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 90
           end if
           ! compute eigenvectors and estimate condition numbers if desired
           ! stdlib_ztgevc: (complex workspace: need 2*n )
                   ! (real workspace:    need 2*n )
           ! stdlib_ztgsna: (complex workspace: need 2*n*n if sense='v' or 'b')
                   ! (integer workspace: need n+2 )
           if( ilv .or. .not.wantsn ) then
              if( ilv ) then
                 if( ilvl ) then
                    if( ilvr ) then
                       chtemp = 'B'
                    else
                       chtemp = 'L'
                    end if
                 else
                    chtemp = 'R'
                 end if
                 call stdlib_ztgevc( chtemp, 'B', ldumma, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, n,&
                            in, work( iwrk ), rwork,ierr )
                 if( ierr/=0_ilp ) then
                    info = n + 2_ilp
                    go to 90
                 end if
              end if
              if( .not.wantsn ) then
                 ! compute eigenvectors (stdlib_ztgevc) and estimate condition
                 ! numbers (stdlib_ztgsna). note that the definition of the condition
                 ! number is not invariant under transformation (u,v) to
                 ! (q*u, z*v), where (u,v) are eigenvectors of the generalized
                 ! schur form (s,t), q and z are orthogonal matrices. in order
                 ! to avoid using extra 2*n*n workspace, we have to
                 ! re-calculate eigenvectors and estimate the condition numbers
                 ! one at a time.
                 do i = 1, n
                    do j = 1, n
                       bwork( j ) = .false.
                    end do
                    bwork( i ) = .true.
                    iwrk = n + 1_ilp
                    iwrk1 = iwrk + n
                    if( wantse .or. wantsb ) then
                       call stdlib_ztgevc( 'B', 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                                 iwrk ), n, 1_ilp, m,work( iwrk1 ), rwork, ierr )
                       if( ierr/=0_ilp ) then
                          info = n + 2_ilp
                          go to 90
                       end if
                    end if
                    call stdlib_ztgsna( sense, 'S', bwork, n, a, lda, b, ldb,work( 1_ilp ), n, work( &
                    iwrk ), n, rconde( i ),rcondv( i ), 1_ilp, m, work( iwrk1 ),lwork-iwrk1+1, iwork, &
                              ierr )
                 end do
              end if
           end if
           ! undo balancing on vl and vr and normalization
           ! (workspace: none needed)
           if( ilvl ) then
              call stdlib_zggbak( balanc, 'L', n, ilo, ihi, lscale, rscale, n, vl,ldvl, ierr )
                        
              loop_50: do jc = 1, n
                 temp = zero
                 do jr = 1, n
                    temp = max( temp, abs1( vl( jr, jc ) ) )
                 end do
                 if( temp<smlnum )cycle loop_50
                 temp = one / temp
                 do jr = 1, n
                    vl( jr, jc ) = vl( jr, jc )*temp
                 end do
              end do loop_50
           end if
           if( ilvr ) then
              call stdlib_zggbak( balanc, 'R', n, ilo, ihi, lscale, rscale, n, vr,ldvr, ierr )
                        
              loop_80: do jc = 1, n
                 temp = zero
                 do jr = 1, n
                    temp = max( temp, abs1( vr( jr, jc ) ) )
                 end do
                 if( temp<smlnum )cycle loop_80
                 temp = one / temp
                 do jr = 1, n
                    vr( jr, jc ) = vr( jr, jc )*temp
                 end do
              end do loop_80
           end if
           ! undo scaling if necessary
           90 continue
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zggevx




     module subroutine stdlib_sgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alphar, &
     !! SGGES3 computes for a pair of N-by-N real nonsymmetric matrices (A,B),
     !! the generalized eigenvalues, the generalized real Schur form (S,T),
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T.The
     !! leading columns of VSL and VSR then form an orthonormal basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! SGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
               alphai, beta, vsl, ldvsl,vsr, ldvsr, work, lwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_s) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, &
                     wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, ip, iright, irows, &
                     itau, iwrk, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           else if( lwork<6_ilp*n+16 .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_sgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 6_ilp*n+16, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_sormqr( 'L', 'T', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              if( ilvsl ) then
                 call stdlib_sorgqr( n, n, n, vsl, ldvsl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              end if
              call stdlib_sgghd3( jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr, &
                        work, -1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              call stdlib_slaqz0( 'S', jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                        beta, vsl, ldvsl, vsr, ldvsr,work, -1_ilp, 0_ilp, ierr )
              lwkopt = max( lwkopt, 2_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              if( wantst ) then
                 call stdlib_stgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alphar, alphai, &
                 beta, vsl, ldvsl, vsr, ldvsr,sdim, pvsl, pvsr, dif, work, -1_ilp, idum, 1_ilp,ierr )
                           
                 lwkopt = max( lwkopt, 2_ilp*n+int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGES3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_sggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           if( ilvsl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_slaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           call stdlib_sgghd3( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      work( iwrk ), lwork+1-iwrk, ierr )
           ! perform qz algorithm, computing schur vectors if desired
           iwrk = itau
           call stdlib_slaqz0( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 40
           end if
           ! sort eigenvalues alpha/beta if desired
           sdim = 0_ilp
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              call stdlib_stgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alphar,alphai, beta, &
              vsl, ldvsl, vsr, ldvsr, sdim, pvsl,pvsr, dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp,&
                        ierr )
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           if( ilvsl )call stdlib_sggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_sggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl )then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i )/safmax )>( anrmto/anrm ) .or.( safmin/alphar( i ) )>( &
                              anrm/anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i )/alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i )/safmax )>( anrmto/anrm ) .or.( safmin/alphai( i ) )>( &
                              anrm/anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i+1 )/alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl )then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                     if( ( beta( i )/safmax )>( bnrmto/bnrm ) .or.( safmin/beta( i ) )>( &
                               bnrm/bnrmto ) ) then
                        work( 1_ilp ) = abs(b( i, i )/beta( i ))
                        beta( i ) = beta( i )*work( 1_ilp )
                        alphar( i ) = alphar( i )*work( 1_ilp )
                        alphai( i ) = alphai( i )*work( 1_ilp )
                     end if
                  end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_slascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sgges3

     module subroutine stdlib_dgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alphar, &
     !! DGGES3 computes for a pair of N-by-N real nonsymmetric matrices (A,B),
     !! the generalized eigenvalues, the generalized real Schur form (S,T),
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T.The
     !! leading columns of VSL and VSR then form an orthonormal basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! DGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
               alphai, beta, vsl, ldvsl,vsr, ldvsr, work, lwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_d) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, &
                     wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, ip, iright, irows, &
                     itau, iwrk, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           else if( lwork<6_ilp*n+16 .and. .not.lquery ) then
              info = -19_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_dgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 6_ilp*n+16, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              call stdlib_dormqr( 'L', 'T', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              if( ilvsl ) then
                 call stdlib_dorgqr( n, n, n, vsl, ldvsl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              end if
              call stdlib_dgghd3( jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr, &
                        work, -1_ilp, ierr )
              lwkopt = max( lwkopt, 3_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              call stdlib_dlaqz0( 'S', jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb,alphar, alphai, &
                        beta, vsl, ldvsl, vsr, ldvsr,work, -1_ilp, 0_ilp, ierr )
              lwkopt = max( lwkopt, 2_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              if( wantst ) then
                 call stdlib_dtgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alphar, alphai, &
                 beta, vsl, ldvsl, vsr, ldvsr,sdim, pvsl, pvsr, dif, work, -1_ilp, idum, 1_ilp,ierr )
                           
                 lwkopt = max( lwkopt, 2_ilp*n+int( work ( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGES3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_dggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           if( ilvsl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_dlaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           call stdlib_dgghd3( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      work( iwrk ), lwork+1-iwrk,ierr )
           ! perform qz algorithm, computing schur vectors if desired
           iwrk = itau
           call stdlib_dlaqz0( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 50
           end if
           ! sort eigenvalues alpha/beta if desired
           sdim = 0_ilp
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              call stdlib_dtgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alphar,alphai, beta, &
              vsl, ldvsl, vsr, ldvsr, sdim, pvsl,pvsr, dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp,&
                        ierr )
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           if( ilvsl )call stdlib_dggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_dggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphar( i ) )>( &
                              anrm / anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i ) / alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphai( i )&
                               )>( anrm / anrmto ) )then
                       work( 1_ilp ) = abs( a( i, i+1 ) / alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( beta( i ) / safmax )>( bnrmto / bnrm ) .or.( safmin / beta( i ) )>( &
                              bnrm / bnrmto ) ) then
                       work( 1_ilp ) = abs( b( i, i ) / beta( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_dlascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           50 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dgges3


     module subroutine stdlib_cgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alpha, beta, &
     !! CGGES3 computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the generalized complex Schur
     !! form (S, T), and optionally left and/or right Schur vectors (VSL
     !! and VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T. The leading
     !! columns of VSL and VSR then form an unitary basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! CGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0, and even for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if S
     !! and T are upper triangular and, in addition, the diagonal elements
     !! of T are non-negative real numbers.
               vsl, ldvsl, vsr, ldvsr,work, lwork, rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_c) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, iright, irows, irwrk, &
                     itau, iwrk, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -14_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -16_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_cgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 1_ilp,  n + int( work( 1_ilp ),KIND=ilp) )
              call stdlib_cunmqr( 'L', 'C', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              if( ilvsl ) then
                 call stdlib_cungqr( n, n, n, vsl, ldvsl, work, work, -1_ilp,ierr )
                 lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              end if
              call stdlib_cgghd3( jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr, &
                        work, -1_ilp, ierr )
              lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              call stdlib_claqz0( 'S', jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vsl, &
                        ldvsl, vsr, ldvsr, work, -1_ilp,rwork, 0_ilp, ierr )
              lwkopt = max( lwkopt, int( work( 1_ilp ),KIND=ilp) )
              if( wantst ) then
                 call stdlib_ctgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alpha, beta, vsl, &
                           ldvsl, vsr, ldvsr, sdim,pvsl, pvsr, dif, work, -1_ilp, idum, 1_ilp, ierr )
                 lwkopt = max( lwkopt, int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGES3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_cggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           if( ilvsl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_cungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_claset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           call stdlib_cgghd3( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      work( iwrk ), lwork+1-iwrk, ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           iwrk = itau
           call stdlib_claqz0( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 30
           end if
           ! sort eigenvalues alpha/beta if desired
           if( wantst ) then
              ! undo scaling on eigenvalues before selecting
              if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              call stdlib_ctgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alpha,beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pvsl, pvsr,dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp, ierr )
                        
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           if( ilvsl )call stdlib_cggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_cggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           30 continue
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cgges3

     module subroutine stdlib_zgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alpha, beta, &
     !! ZGGES3 computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the generalized complex Schur
     !! form (S, T), and optionally left and/or right Schur vectors (VSL
     !! and VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T. The leading
     !! columns of VSL and VSR then form an unitary basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! ZGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0, and even for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if S
     !! and T are upper triangular and, in addition, the diagonal elements
     !! of T are non-negative real numbers.
               vsl, ldvsl, vsr, ldvsr,work, lwork, rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_z) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, iright, irows, irwrk, &
                     itau, iwrk, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -14_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -16_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           ! compute workspace
           if( info==0_ilp ) then
              call stdlib_zgeqrf( n, n, b, ldb, work, work, -1_ilp, ierr )
              lwkopt = max( 1_ilp,  n + int( work( 1_ilp ),KIND=ilp) )
              call stdlib_zunmqr( 'L', 'C', n, n, n, b, ldb, work, a, lda, work,-1_ilp, ierr )
              lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              if( ilvsl ) then
                 call stdlib_zungqr( n, n, n, vsl, ldvsl, work, work, -1_ilp, ierr )
                 lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              end if
              call stdlib_zgghd3( jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr, &
                        work, -1_ilp, ierr )
              lwkopt = max( lwkopt, n + int( work( 1_ilp ),KIND=ilp) )
              call stdlib_zlaqz0( 'S', jobvsl, jobvsr, n, 1_ilp, n, a, lda, b, ldb,alpha, beta, vsl, &
                        ldvsl, vsr, ldvsr, work, -1_ilp,rwork, 0_ilp, ierr )
              lwkopt = max( lwkopt, int( work( 1_ilp ),KIND=ilp) )
              if( wantst ) then
                 call stdlib_ztgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alpha, beta, vsl, &
                           ldvsl, vsr, ldvsr, sdim,pvsl, pvsr, dif, work, -1_ilp, idum, 1_ilp, ierr )
                 lwkopt = max( lwkopt, int( work( 1_ilp ),KIND=ilp) )
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGES3 ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_zggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           if( ilvsl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_zungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           call stdlib_zgghd3( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      work( iwrk ), lwork+1-iwrk, ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           iwrk = itau
           call stdlib_zlaqz0( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), 0_ilp, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 30
           end if
           ! sort eigenvalues alpha/beta if desired
           if( wantst ) then
              ! undo scaling on eigenvalues before selecting
              if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              call stdlib_ztgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alpha,beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pvsl, pvsr,dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp, ierr )
                        
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           if( ilvsl )call stdlib_zggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_zggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           30 continue
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zgges3




     module subroutine stdlib_sgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alphar, &
     !! SGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B),
     !! the generalized eigenvalues, the generalized real Schur form (S,T),
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T.The
     !! leading columns of VSL and VSR then form an orthonormal basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! SGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
               alphai, beta, vsl, ldvsl, vsr,ldvsr, work, lwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_s) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, &
                     wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, ip, iright, irows, &
                     itau, iwrk, maxwrk, minwrk
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp )then
                 minwrk = max( 8_ilp*n, 6_ilp*n + 16_ilp )
                 maxwrk = minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, 1_ilp, n, 0_ilp )
                 maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, 1_ilp, n, -1_ilp &
                           ) )
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', n, 1_ilp, n, &
                              -1_ilp ) )
                 end if
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need 6*n + 2*n space for storing balancing factors)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_sggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_slaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_sgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           ! perform qz algorithm, computing schur vectors if desired
           ! (workspace: need n)
           iwrk = itau
           call stdlib_shgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 40
           end if
           ! sort eigenvalues alpha/beta if desired
           ! (workspace: need 4*n+16 )
           sdim = 0_ilp
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              call stdlib_stgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alphar,alphai, beta, &
              vsl, ldvsl, vsr, ldvsr, sdim, pvsl,pvsr, dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp,&
                        ierr )
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_sggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_sggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl )then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i )/safmax )>( anrmto/anrm ) .or.( safmin/alphar( i ) )>( &
                              anrm/anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i )/alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i )/safmax )>( anrmto/anrm ) .or.( safmin/alphai( i ) )>( &
                              anrm/anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i+1 )/alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl )then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                     if( ( beta( i )/safmax )>( bnrmto/bnrm ) .or.( safmin/beta( i ) )>( &
                               bnrm/bnrmto ) ) then
                        work( 1_ilp ) = abs(b( i, i )/beta( i ))
                        beta( i ) = beta( i )*work( 1_ilp )
                        alphar( i ) = alphar( i )*work( 1_ilp )
                        alphai( i ) = alphai( i )*work( 1_ilp )
                     end if
                  end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_slascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           40 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgges

     module subroutine stdlib_dgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alphar, &
     !! DGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B),
     !! the generalized eigenvalues, the generalized real Schur form (S,T),
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T.The
     !! leading columns of VSL and VSR then form an orthonormal basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! DGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
               alphai, beta, vsl, ldvsl, vsr,ldvsr, work, lwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_d) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, &
                     wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, ip, iright, irows, &
                     itau, iwrk, maxwrk, minwrk
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp )then
                 minwrk = max( 8_ilp*n, 6_ilp*n + 16_ilp )
                 maxwrk = minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, 1_ilp, n, 0_ilp )
                 maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, 1_ilp, n, -1_ilp &
                           ) )
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', n, 1_ilp, n, &
                              -1_ilp ) )
                 end if
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need 6*n + 2*n space for storing balancing factors)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_dggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_dlaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_dgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           ! perform qz algorithm, computing schur vectors if desired
           ! (workspace: need n)
           iwrk = itau
           call stdlib_dhgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 50
           end if
           ! sort eigenvalues alpha/beta if desired
           ! (workspace: need 4*n+16 )
           sdim = 0_ilp
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              call stdlib_dtgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alphar,alphai, beta, &
              vsl, ldvsl, vsr, ldvsr, sdim, pvsl,pvsr, dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp,&
                        ierr )
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_dggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_dggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphar( i ) )>( &
                              anrm / anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i ) / alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphai( i )&
                               )>( anrm / anrmto ) )then
                       work( 1_ilp ) = abs( a( i, i+1 ) / alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( beta( i ) / safmax )>( bnrmto / bnrm ) .or.( safmin / beta( i ) )>( &
                              bnrm / bnrmto ) ) then
                       work( 1_ilp ) = abs( b( i, i ) / beta( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_dlascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           50 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgges


     module subroutine stdlib_cgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alpha, beta, &
     !! CGGES computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the generalized complex Schur
     !! form (S, T), and optionally left and/or right Schur vectors (VSL
     !! and VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T. The leading
     !! columns of VSL and VSR then form an unitary basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! CGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0, and even for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if S
     !! and T are upper triangular and, in addition, the diagonal elements
     !! of T are non-negative real numbers.
               vsl, ldvsl, vsr, ldvsr, work,lwork, rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_c) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, iright, irows, irwrk, &
                     itau, iwrk, lwkmin, lwkopt
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -14_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -16_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 2_ilp*n )
              lwkopt = max( 1_ilp, n + n*stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
              lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, 1_ilp, n, -1_ilp ) )
              if( ilvsl ) then
                 lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', n, 1_ilp, n, -1_ilp ) )
                           
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_cggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_cungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_claset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_cgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           call stdlib_chgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 30
           end if
           ! sort eigenvalues alpha/beta if desired
           ! (workspace: none needed)
           if( wantst ) then
              ! undo scaling on eigenvalues before selecting
              if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              call stdlib_ctgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alpha,beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pvsl, pvsr,dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp, ierr )
                        
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_cggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_cggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           30 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cgges

     module subroutine stdlib_zgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alpha, beta, &
     !! ZGGES computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the generalized complex Schur
     !! form (S, T), and optionally left and/or right Schur vectors (VSL
     !! and VSR). This gives the generalized Schur factorization
     !! (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T. The leading
     !! columns of VSL and VSR then form an unitary basis for the
     !! corresponding left and right eigenspaces (deflating subspaces).
     !! (If only the generalized eigenvalues are needed, use the driver
     !! ZGGEV instead, which is faster.)
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0, and even for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if S
     !! and T are upper triangular and, in addition, the diagonal elements
     !! of T are non-negative real numbers.
               vsl, ldvsl, vsr, ldvsr, work,lwork, rwork, bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_z) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantst
           integer(ilp) :: i, icols, ierr, ihi, ijobvl, ijobvr, ileft, ilo, iright, irows, irwrk, &
                     itau, iwrk, lwkmin, lwkopt
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pvsl, pvsr, smlnum
           ! Local Arrays 
           integer(ilp) :: idum(1_ilp)
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -14_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -16_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 2_ilp*n )
              lwkopt = max( 1_ilp, n + n*stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
              lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, 1_ilp, n, -1_ilp ) )
              if( ilvsl ) then
                 lwkopt = max( lwkopt, n +n*stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', n, 1_ilp, n, -1_ilp ) )
                           
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery )info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGES ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_zggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_zungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_zgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (complex workspace: need n)
           ! (real workspace: need n)
           iwrk = itau
           call stdlib_zhgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 30
           end if
           ! sort eigenvalues alpha/beta if desired
           ! (workspace: none needed)
           if( wantst ) then
              ! undo scaling on eigenvalues before selecting
              if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              call stdlib_ztgsen( 0_ilp, ilvsl, ilvsr, bwork, n, a, lda, b, ldb, alpha,beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pvsl, pvsr,dif, work( iwrk ), lwork-iwrk+1, idum, 1_ilp, ierr )
                        
              if( ierr==1_ilp )info = n + 3_ilp
           end if
           ! apply back-permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_zggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_zggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           30 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zgges




     module subroutine stdlib_sggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, &
     !! SGGESX computes for a pair of N-by-N real nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the real Schur form (S,T), and,
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR).  This gives the generalized Schur factorization
     !! (A,B) = ( (VSL) S (VSR)**T, (VSL) T (VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T; computes
     !! a reciprocal condition number for the average of the selected
     !! eigenvalues (RCONDE); and computes a reciprocal condition number for
     !! the right and left deflating subspaces corresponding to the selected
     !! eigenvalues (RCONDV). The leading columns of VSL and VSR then form
     !! an orthonormal basis for the corresponding left and right eigenspaces
     !! (deflating subspaces).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or for both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
     alphar, alphai, beta, vsl, ldvsl,vsr, ldvsr, rconde, rcondv, work, lwork, iwork,liwork, &
               bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), rconde(2_ilp), rcondv(2_ilp), vsl(&
                     ldvsl,*), vsr(ldvsr,*), work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_s) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, wantsb, &
                     wantse, wantsn, wantst, wantsv
           integer(ilp) :: i, icols, ierr, ihi, ijob, ijobvl, ijobvr, ileft, ilo, ip, iright, &
                     irows, itau, iwrk, liwmin, lwrk, maxwrk, minwrk
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pl, pr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( wantsn ) then
              ijob = 0_ilp
           else if( wantse ) then
              ijob = 1_ilp
           else if( wantsv ) then
              ijob = 2_ilp
           else if( wantsb ) then
              ijob = 4_ilp
           end if
           ! test the input arguments
           info = 0_ilp
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -16_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -18_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp) then
                 minwrk = max( 8_ilp*n, 6_ilp*n + 16_ilp )
                 maxwrk = minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, 1_ilp, n, 0_ilp )
                 maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, 1_ilp, n, -1_ilp &
                           ) )
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', n, 1_ilp, n, &
                              -1_ilp ) )
                 end if
                 lwrk = maxwrk
                 if( ijob>=1_ilp )lwrk = max( lwrk, n*n/2_ilp )
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
                 lwrk   = 1_ilp
              end if
              work( 1_ilp ) = lwrk
              if( wantsn .or. n==0_ilp ) then
                 liwmin = 1_ilp
              else
                 liwmin = n + 6_ilp
              end if
              iwork( 1_ilp ) = liwmin
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -22_ilp
              else if( liwork<liwmin  .and. .not.lquery ) then
                 info = -24_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGESX', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need 6*n + 2*n for permutation parameters)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_sggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_sgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_sormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_slaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_slacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_sorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_slaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_sgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (workspace: need n)
           iwrk = itau
           call stdlib_shgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 50
           end if
           ! sort eigenvalues alpha/beta and compute the reciprocal of
           ! condition number(s)
           ! (workspace: if ijob >= 1, need max( 8*(n+1), 2*sdim*(n-sdim) )
                       ! otherwise, need 8*(n+1) )
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              ! reorder eigenvalues, transform generalized schur vectors, and
              ! compute reciprocal condition numbers
              call stdlib_stgsen( ijob, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alphar, alphai, &
              beta, vsl, ldvsl, vsr, ldvsr,sdim, pl, pr, dif, work( iwrk ), lwork-iwrk+1,iwork, &
                        liwork, ierr )
              if( ijob>=1_ilp )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( ierr==-22_ilp ) then
                  ! not enough real workspace
                 info = -22_ilp
              else
                 if( ijob==1_ilp .or. ijob==4_ilp ) then
                    rconde( 1_ilp ) = pl
                    rconde( 2_ilp ) = pr
                 end if
                 if( ijob==2_ilp .or. ijob==4_ilp ) then
                    rcondv( 1_ilp ) = dif( 1_ilp )
                    rcondv( 2_ilp ) = dif( 2_ilp )
                 end if
                 if( ierr==1_ilp )info = n + 3_ilp
              end if
           end if
           ! apply permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_sggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_sggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphar( i ) )>( &
                              anrm / anrmto ) )then
                       work( 1_ilp ) = abs( a( i, i ) / alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i ) / safmax )>( anrmto / anrm ).or. ( safmin / alphai( i )&
                               )>( anrm / anrmto ) )then
                       work( 1_ilp ) = abs( a( i, i+1 ) / alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( beta( i ) / safmax )>( bnrmto / bnrm ) .or.( safmin / beta( i ) )>( &
                              bnrm / bnrmto ) ) then
                       work( 1_ilp ) = abs( b( i, i ) / beta( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_slascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_slascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           50 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sggesx

     module subroutine stdlib_dggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, &
     !! DGGESX computes for a pair of N-by-N real nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the real Schur form (S,T), and,
     !! optionally, the left and/or right matrices of Schur vectors (VSL and
     !! VSR).  This gives the generalized Schur factorization
     !! (A,B) = ( (VSL) S (VSR)**T, (VSL) T (VSR)**T )
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! quasi-triangular matrix S and the upper triangular matrix T; computes
     !! a reciprocal condition number for the average of the selected
     !! eigenvalues (RCONDE); and computes a reciprocal condition number for
     !! the right and left deflating subspaces corresponding to the selected
     !! eigenvalues (RCONDV). The leading columns of VSL and VSR then form
     !! an orthonormal basis for the corresponding left and right eigenspaces
     !! (deflating subspaces).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or for both being zero.
     !! A pair of matrices (S,T) is in generalized real Schur form if T is
     !! upper triangular with non-negative diagonal and S is block upper
     !! triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
     !! to real generalized eigenvalues, while 2-by-2 blocks of S will be
     !! "standardized" by making the corresponding elements of T have the
     !! form:
     !! [  a  0  ]
     !! [  0  b  ]
     !! and the pair of corresponding 2-by-2 blocks in S and T will have a
     !! complex conjugate pair of generalized eigenvalues.
     alphar, alphai, beta, vsl, ldvsl,vsr, ldvsr, rconde, rcondv, work, lwork, iwork,liwork, &
               bwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), rconde(2_ilp), rcondv(2_ilp), vsl(&
                     ldvsl,*), vsr(ldvsr,*), work(*)
           ! Function Arguments 
           procedure(stdlib_selctg_d) :: selctg
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, lst2sl, wantsb, &
                     wantse, wantsn, wantst, wantsv
           integer(ilp) :: i, icols, ierr, ihi, ijob, ijobvl, ijobvr, ileft, ilo, ip, iright, &
                     irows, itau, iwrk, liwmin, lwrk, maxwrk, minwrk
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pl, pr, safmax, safmin, &
                     smlnum
           ! Local Arrays 
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( wantsn ) then
              ijob = 0_ilp
           else if( wantse ) then
              ijob = 1_ilp
           else if( wantsv ) then
              ijob = 2_ilp
           else if( wantsb ) then
              ijob = 4_ilp
           end if
           ! test the input arguments
           info = 0_ilp
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -16_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -18_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp) then
                 minwrk = max( 8_ilp*n, 6_ilp*n + 16_ilp )
                 maxwrk = minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, 1_ilp, n, 0_ilp )
                 maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, 1_ilp, n, -1_ilp &
                           ) )
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, minwrk - n +n*stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', n, 1_ilp, n, &
                              -1_ilp ) )
                 end if
                 lwrk = maxwrk
                 if( ijob>=1_ilp )lwrk = max( lwrk, n*n/2_ilp )
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
                 lwrk   = 1_ilp
              end if
              work( 1_ilp ) = lwrk
              if( wantsn .or. n==0_ilp ) then
                 liwmin = 1_ilp
              else
                 liwmin = n + 6_ilp
              end if
              iwork( 1_ilp ) = liwmin
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -22_ilp
              else if( liwork<liwmin  .and. .not.lquery ) then
                 info = -24_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGESX', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           smlnum = sqrt( safmin ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', n, n, a, lda, work )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', n, n, b, ldb, work )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (workspace: need 6*n + 2*n for permutation parameters)
           ileft = 1_ilp
           iright = n + 1_ilp
           iwrk = iright + n
           call stdlib_dggbal( 'P', n, a, lda, b, ldb, ilo, ihi, work( ileft ),work( iright ), &
                     work( iwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = iwrk
           iwrk = itau + irows
           call stdlib_dgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the orthogonal transformation to matrix a
           ! (workspace: need n, prefer n*nb)
           call stdlib_dormqr( 'L', 'T', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_dlaset( 'FULL', n, n, zero, one, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_dlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_dorgqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_dlaset( 'FULL', n, n, zero, one, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_dgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (workspace: need n)
           iwrk = itau
           call stdlib_dhgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                     beta, vsl, ldvsl, vsr, ldvsr,work( iwrk ), lwork+1-iwrk, ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 60
           end if
           ! sort eigenvalues alpha/beta and compute the reciprocal of
           ! condition number(s)
           ! (workspace: if ijob >= 1, need max( 8*(n+1), 2*sdim*(n-sdim) )
                       ! otherwise, need 8*(n+1) )
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl ) then
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n,ierr )
                 call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n,ierr )
              end if
              if( ilbscl )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alphar( i ), alphai( i ), beta( i ) )
              end do
              ! reorder eigenvalues, transform generalized schur vectors, and
              ! compute reciprocal condition numbers
              call stdlib_dtgsen( ijob, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alphar, alphai, &
              beta, vsl, ldvsl, vsr, ldvsr,sdim, pl, pr, dif, work( iwrk ), lwork-iwrk+1,iwork, &
                        liwork, ierr )
              if( ijob>=1_ilp )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( ierr==-22_ilp ) then
                  ! not enough real workspace
                 info = -22_ilp
              else
                 if( ijob==1_ilp .or. ijob==4_ilp ) then
                    rconde( 1_ilp ) = pl
                    rconde( 2_ilp ) = pr
                 end if
                 if( ijob==2_ilp .or. ijob==4_ilp ) then
                    rcondv( 1_ilp ) = dif( 1_ilp )
                    rcondv( 2_ilp ) = dif( 2_ilp )
                 end if
                 if( ierr==1_ilp )info = n + 3_ilp
              end if
           end if
           ! apply permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_dggbak( 'P', 'L', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_dggbak( 'P', 'R', n, ilo, ihi, work( ileft ),work( iright ), n, &
                     vsr, ldvsr, ierr )
           ! check if unscaling would cause over/underflow, if so, rescale
           ! (alphar(i),alphai(i),beta(i)) so beta(i) is on the order of
           ! b(i,i) and alphar(i) and alphai(i) are on the order of a(i,i)
           if( ilascl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( alphar( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphar( i ) )>( &
                              anrm / anrmto ) ) then
                       work( 1_ilp ) = abs( a( i, i ) / alphar( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    else if( ( alphai( i ) / safmax )>( anrmto / anrm ) .or.( safmin / alphai( i )&
                               )>( anrm / anrmto ) )then
                       work( 1_ilp ) = abs( a( i, i+1 ) / alphai( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           if( ilbscl ) then
              do i = 1, n
                 if( alphai( i )/=zero ) then
                    if( ( beta( i ) / safmax )>( bnrmto / bnrm ) .or.( safmin / beta( i ) )>( &
                              bnrm / bnrmto ) ) then
                       work( 1_ilp ) = abs( b( i, i ) / beta( i ) )
                       beta( i ) = beta( i )*work( 1_ilp )
                       alphar( i ) = alphar( i )*work( 1_ilp )
                       alphai( i ) = alphai( i )*work( 1_ilp )
                    end if
                 end if
              end do
           end if
           ! undo scaling
           if( ilascl ) then
              call stdlib_dlascl( 'H', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphar, n, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alphai, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_dlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              lst2sl = .true.
              sdim = 0_ilp
              ip = 0_ilp
              do i = 1, n
                 cursl = selctg( alphar( i ), alphai( i ), beta( i ) )
                 if( alphai( i )==zero ) then
                    if( cursl )sdim = sdim + 1_ilp
                    ip = 0_ilp
                    if( cursl .and. .not.lastsl )info = n + 2_ilp
                 else
                    if( ip==1_ilp ) then
                       ! last eigenvalue of conjugate pair
                       cursl = cursl .or. lastsl
                       lastsl = cursl
                       if( cursl )sdim = sdim + 2_ilp
                       ip = -1_ilp
                       if( cursl .and. .not.lst2sl )info = n + 2_ilp
                    else
                       ! first eigenvalue of conjugate pair
                       ip = 1_ilp
                    end if
                 end if
                 lst2sl = lastsl
                 lastsl = cursl
              end do
           end if
           60 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dggesx


     module subroutine stdlib_cggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, alpha,&
     !! CGGESX computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the complex Schur form (S,T),
     !! and, optionally, the left and/or right matrices of Schur vectors (VSL
     !! and VSR).  This gives the generalized Schur factorization
     !! (A,B) = ( (VSL) S (VSR)**H, (VSL) T (VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T; computes
     !! a reciprocal condition number for the average of the selected
     !! eigenvalues (RCONDE); and computes a reciprocal condition number for
     !! the right and left deflating subspaces corresponding to the selected
     !! eigenvalues (RCONDV). The leading columns of VSL and VSR then form
     !! an orthonormal basis for the corresponding left and right eigenspaces
     !! (deflating subspaces).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if T is
     !! upper triangular with non-negative diagonal and S is upper
     !! triangular.
      beta, vsl, ldvsl, vsr,ldvsr, rconde, rcondv, work, lwork, rwork,iwork, liwork, bwork, info )
                
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rconde(2_ilp), rcondv(2_ilp), rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_c) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantsb, wantse, &
                     wantsn, wantst, wantsv
           integer(ilp) :: i, icols, ierr, ihi, ijob, ijobvl, ijobvr, ileft, ilo, iright, irows, &
                     irwrk, itau, iwrk, liwmin, lwrk, maxwrk, minwrk
           real(sp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pl, pr, smlnum
           ! Local Arrays 
           real(sp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( wantsn ) then
              ijob = 0_ilp
           else if( wantse ) then
              ijob = 1_ilp
           else if( wantsv ) then
              ijob = 2_ilp
           else if( wantsb ) then
              ijob = 4_ilp
           end if
           ! test the input arguments
           info = 0_ilp
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp) then
                 minwrk = 2_ilp*n
                 maxwrk = n*(1_ilp + stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                 maxwrk = max( maxwrk, n*( 1_ilp +stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, 1_ilp, n, -1_ilp ) ) )
                           
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, n*( 1_ilp +stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', n, 1_ilp, n, -1_ilp ) ) &
                              )
                 end if
                 lwrk = maxwrk
                 if( ijob>=1_ilp )lwrk = max( lwrk, n*n/2_ilp )
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
                 lwrk   = 1_ilp
              end if
              work( 1_ilp ) = lwrk
              if( wantsn .or. n==0_ilp ) then
                 liwmin = 1_ilp
              else
                 liwmin = n + 2_ilp
              end if
              iwork( 1_ilp ) = liwmin
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -21_ilp
              else if( liwork<liwmin  .and. .not.lquery) then
                 info = -24_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGESX', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_cggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_cgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the unitary transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_cunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_claset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_clacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_cungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_claset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_cgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (complex workspace: need n)
           ! (real workspace:    need n)
           iwrk = itau
           call stdlib_chgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 40
           end if
           ! sort eigenvalues alpha/beta and compute the reciprocal of
           ! condition number(s)
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              ! reorder eigenvalues, transform generalized schur vectors, and
              ! compute reciprocal condition numbers
              ! (complex workspace: if ijob >= 1, need max(1, 2*sdim*(n-sdim))
                                  ! otherwise, need 1 )
              call stdlib_ctgsen( ijob, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alpha, beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pl, pr,dif, work( iwrk ), lwork-iwrk+1, iwork, liwork,ierr &
                        )
              if( ijob>=1_ilp )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( ierr==-21_ilp ) then
                  ! not enough complex workspace
                 info = -21_ilp
              else
                 if( ijob==1_ilp .or. ijob==4_ilp ) then
                    rconde( 1_ilp ) = pl
                    rconde( 2_ilp ) = pr
                 end if
                 if( ijob==2_ilp .or. ijob==4_ilp ) then
                    rcondv( 1_ilp ) = dif( 1_ilp )
                    rcondv( 2_ilp ) = dif( 2_ilp )
                 end if
                 if( ierr==1_ilp )info = n + 3_ilp
              end if
           end if
           ! apply permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_cggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_cggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           40 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_cggesx

     module subroutine stdlib_zggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, alpha,&
     !! ZGGESX computes for a pair of N-by-N complex nonsymmetric matrices
     !! (A,B), the generalized eigenvalues, the complex Schur form (S,T),
     !! and, optionally, the left and/or right matrices of Schur vectors (VSL
     !! and VSR).  This gives the generalized Schur factorization
     !! (A,B) = ( (VSL) S (VSR)**H, (VSL) T (VSR)**H )
     !! where (VSR)**H is the conjugate-transpose of VSR.
     !! Optionally, it also orders the eigenvalues so that a selected cluster
     !! of eigenvalues appears in the leading diagonal blocks of the upper
     !! triangular matrix S and the upper triangular matrix T; computes
     !! a reciprocal condition number for the average of the selected
     !! eigenvalues (RCONDE); and computes a reciprocal condition number for
     !! the right and left deflating subspaces corresponding to the selected
     !! eigenvalues (RCONDV). The leading columns of VSL and VSR then form
     !! an orthonormal basis for the corresponding left and right eigenspaces
     !! (deflating subspaces).
     !! A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
     !! or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
     !! usually represented as the pair (alpha,beta), as there is a
     !! reasonable interpretation for beta=0 or for both being zero.
     !! A pair of matrices (S,T) is in generalized complex Schur form if T is
     !! upper triangular with non-negative diagonal and S is upper
     !! triangular.
      beta, vsl, ldvsl, vsr,ldvsr, rconde, rcondv, work, lwork, rwork,iwork, liwork, bwork, info )
                
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           ! Array Arguments 
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rconde(2_ilp), rcondv(2_ilp), rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           ! Function Arguments 
           procedure(stdlib_selctg_z) :: selctg
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: cursl, ilascl, ilbscl, ilvsl, ilvsr, lastsl, lquery, wantsb, wantse, &
                     wantsn, wantst, wantsv
           integer(ilp) :: i, icols, ierr, ihi, ijob, ijobvl, ijobvr, ileft, ilo, iright, irows, &
                     irwrk, itau, iwrk, liwmin, lwrk, maxwrk, minwrk
           real(dp) :: anrm, anrmto, bignum, bnrm, bnrmto, eps, pl, pr, smlnum
           ! Local Arrays 
           real(dp) :: dif(2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode the input arguments
           if( stdlib_lsame( jobvsl, 'N' ) ) then
              ijobvl = 1_ilp
              ilvsl = .false.
           else if( stdlib_lsame( jobvsl, 'V' ) ) then
              ijobvl = 2_ilp
              ilvsl = .true.
           else
              ijobvl = -1_ilp
              ilvsl = .false.
           end if
           if( stdlib_lsame( jobvsr, 'N' ) ) then
              ijobvr = 1_ilp
              ilvsr = .false.
           else if( stdlib_lsame( jobvsr, 'V' ) ) then
              ijobvr = 2_ilp
              ilvsr = .true.
           else
              ijobvr = -1_ilp
              ilvsr = .false.
           end if
           wantst = stdlib_lsame( sort, 'S' )
           wantsn = stdlib_lsame( sense, 'N' )
           wantse = stdlib_lsame( sense, 'E' )
           wantsv = stdlib_lsame( sense, 'V' )
           wantsb = stdlib_lsame( sense, 'B' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( wantsn ) then
              ijob = 0_ilp
           else if( wantse ) then
              ijob = 1_ilp
           else if( wantsv ) then
              ijob = 2_ilp
           else if( wantsb ) then
              ijob = 4_ilp
           end if
           ! test the input arguments
           info = 0_ilp
           if( ijobvl<=0_ilp ) then
              info = -1_ilp
           else if( ijobvr<=0_ilp ) then
              info = -2_ilp
           else if( ( .not.wantst ) .and. ( .not.stdlib_lsame( sort, 'N' ) ) ) then
              info = -3_ilp
           else if( .not.( wantsn .or. wantse .or. wantsv .or. wantsb ) .or.( .not.wantst .and. &
                     .not.wantsn ) ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldvsl<1_ilp .or. ( ilvsl .and. ldvsl<n ) ) then
              info = -15_ilp
           else if( ldvsr<1_ilp .or. ( ilvsr .and. ldvsr<n ) ) then
              info = -17_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              if( n>0_ilp) then
                 minwrk = 2_ilp*n
                 maxwrk = n*(1_ilp + stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, 1_ilp, n, 0_ilp ) )
                 maxwrk = max( maxwrk, n*( 1_ilp +stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, 1_ilp, n, -1_ilp ) ) )
                           
                 if( ilvsl ) then
                    maxwrk = max( maxwrk, n*( 1_ilp +stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', n, 1_ilp, n, -1_ilp ) ) &
                              )
                 end if
                 lwrk = maxwrk
                 if( ijob>=1_ilp )lwrk = max( lwrk, n*n/2_ilp )
              else
                 minwrk = 1_ilp
                 maxwrk = 1_ilp
                 lwrk   = 1_ilp
              end if
              work( 1_ilp ) = lwrk
              if( wantsn .or. n==0_ilp ) then
                 liwmin = 1_ilp
              else
                 liwmin = n + 2_ilp
              end if
              iwork( 1_ilp ) = liwmin
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -21_ilp
              else if( liwork<liwmin  .and. .not.lquery) then
                 info = -24_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGESX', -info )
              return
           else if (lquery) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              sdim = 0_ilp
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = sqrt( smlnum ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', n, n, a, lda, rwork )
           ilascl = .false.
           if( anrm>zero .and. anrm<smlnum ) then
              anrmto = smlnum
              ilascl = .true.
           else if( anrm>bignum ) then
              anrmto = bignum
              ilascl = .true.
           end if
           if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, anrmto, n, n, a, lda, ierr )
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', n, n, b, ldb, rwork )
           ilbscl = .false.
           if( bnrm>zero .and. bnrm<smlnum ) then
              bnrmto = smlnum
              ilbscl = .true.
           else if( bnrm>bignum ) then
              bnrmto = bignum
              ilbscl = .true.
           end if
           if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bnrmto, n, n, b, ldb, ierr )
           ! permute the matrix to make it more nearly triangular
           ! (real workspace: need 6*n)
           ileft = 1_ilp
           iright = n + 1_ilp
           irwrk = iright + n
           call stdlib_zggbal( 'P', n, a, lda, b, ldb, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     rwork( irwrk ), ierr )
           ! reduce b to triangular form (qr decomposition of b)
           ! (complex workspace: need n, prefer n*nb)
           irows = ihi + 1_ilp - ilo
           icols = n + 1_ilp - ilo
           itau = 1_ilp
           iwrk = itau + irows
           call stdlib_zgeqrf( irows, icols, b( ilo, ilo ), ldb, work( itau ),work( iwrk ), lwork+&
                     1_ilp-iwrk, ierr )
           ! apply the unitary transformation to matrix a
           ! (complex workspace: need n, prefer n*nb)
           call stdlib_zunmqr( 'L', 'C', irows, icols, irows, b( ilo, ilo ), ldb,work( itau ), a( &
                     ilo, ilo ), lda, work( iwrk ),lwork+1-iwrk, ierr )
           ! initialize vsl
           ! (complex workspace: need n, prefer n*nb)
           if( ilvsl ) then
              call stdlib_zlaset( 'FULL', n, n, czero, cone, vsl, ldvsl )
              if( irows>1_ilp ) then
                 call stdlib_zlacpy( 'L', irows-1, irows-1, b( ilo+1, ilo ), ldb,vsl( ilo+1, ilo )&
                           , ldvsl )
              end if
              call stdlib_zungqr( irows, irows, irows, vsl( ilo, ilo ), ldvsl,work( itau ), work( &
                        iwrk ), lwork+1-iwrk, ierr )
           end if
           ! initialize vsr
           if( ilvsr )call stdlib_zlaset( 'FULL', n, n, czero, cone, vsr, ldvsr )
           ! reduce to generalized hessenberg form
           ! (workspace: none needed)
           call stdlib_zgghrd( jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb, vsl,ldvsl, vsr, ldvsr,&
                      ierr )
           sdim = 0_ilp
           ! perform qz algorithm, computing schur vectors if desired
           ! (complex workspace: need n)
           ! (real workspace:    need n)
           iwrk = itau
           call stdlib_zhgeqz( 'S', jobvsl, jobvsr, n, ilo, ihi, a, lda, b, ldb,alpha, beta, vsl, &
                     ldvsl, vsr, ldvsr, work( iwrk ),lwork+1-iwrk, rwork( irwrk ), ierr )
           if( ierr/=0_ilp ) then
              if( ierr>0_ilp .and. ierr<=n ) then
                 info = ierr
              else if( ierr>n .and. ierr<=2_ilp*n ) then
                 info = ierr - n
              else
                 info = n + 1_ilp
              end if
              go to 40
           end if
           ! sort eigenvalues alpha/beta and compute the reciprocal of
           ! condition number(s)
           if( wantst ) then
              ! undo scaling on eigenvalues before selctging
              if( ilascl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
                        
              if( ilbscl )call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
                        
              ! select eigenvalues
              do i = 1, n
                 bwork( i ) = selctg( alpha( i ), beta( i ) )
              end do
              ! reorder eigenvalues, transform generalized schur vectors, and
              ! compute reciprocal condition numbers
              ! (complex workspace: if ijob >= 1, need max(1, 2*sdim*(n-sdim))
                                  ! otherwise, need 1 )
              call stdlib_ztgsen( ijob, ilvsl, ilvsr, bwork, n, a, lda, b, ldb,alpha, beta, vsl, &
              ldvsl, vsr, ldvsr, sdim, pl, pr,dif, work( iwrk ), lwork-iwrk+1, iwork, liwork,ierr &
                        )
              if( ijob>=1_ilp )maxwrk = max( maxwrk, 2_ilp*sdim*( n-sdim ) )
              if( ierr==-21_ilp ) then
                  ! not enough complex workspace
                 info = -21_ilp
              else
                 if( ijob==1_ilp .or. ijob==4_ilp ) then
                    rconde( 1_ilp ) = pl
                    rconde( 2_ilp ) = pr
                 end if
                 if( ijob==2_ilp .or. ijob==4_ilp ) then
                    rcondv( 1_ilp ) = dif( 1_ilp )
                    rcondv( 2_ilp ) = dif( 2_ilp )
                 end if
                 if( ierr==1_ilp )info = n + 3_ilp
              end if
           end if
           ! apply permutation to vsl and vsr
           ! (workspace: none needed)
           if( ilvsl )call stdlib_zggbak( 'P', 'L', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsl, ldvsl, ierr )
           if( ilvsr )call stdlib_zggbak( 'P', 'R', n, ilo, ihi, rwork( ileft ),rwork( iright ), &
                     n, vsr, ldvsr, ierr )
           ! undo scaling
           if( ilascl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, anrmto, anrm, n, n, a, lda, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrmto, anrm, n, 1_ilp, alpha, n, ierr )
           end if
           if( ilbscl ) then
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, bnrmto, bnrm, n, n, b, ldb, ierr )
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrmto, bnrm, n, 1_ilp, beta, n, ierr )
           end if
           if( wantst ) then
              ! check if reordering is correct
              lastsl = .true.
              sdim = 0_ilp
              do i = 1, n
                 cursl = selctg( alpha( i ), beta( i ) )
                 if( cursl )sdim = sdim + 1_ilp
                 if( cursl .and. .not.lastsl )info = n + 2_ilp
                 lastsl = cursl
              end do
           end if
           40 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zggesx




     pure module subroutine stdlib_sgebal( job, n, a, lda, ilo, ihi, scale, info )
     !! SGEBAL balances a general real matrix A.  This involves, first,
     !! permuting A by a similarity transformation to isolate eigenvalues
     !! in the first 1 to ILO-1 and last IHI+1 to N elements on the
     !! diagonal; and second, applying a diagonal similarity transformation
     !! to rows and columns ILO to IHI to make the rows and columns as
     !! close in norm as possible.  Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrix, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: scale(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sclfac = 2.0e+0_sp
           real(sp), parameter :: factor = 0.95e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: noconv
           integer(ilp) :: i, ica, iexc, ira, j, k, l, m
           real(sp) :: c, ca, f, g, r, ra, s, sfmax1, sfmax2, sfmin1, sfmin2
           ! Intrinsic Functions 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEBAL', -info )
              return
           end if
           k = 1_ilp
           l = n
           if( n==0 )go to 210
           if( stdlib_lsame( job, 'N' ) ) then
              do i = 1, n
                 scale( i ) = one
              end do
              go to 210
           end if
           if( stdlib_lsame( job, 'S' ) )go to 120
           ! permutation to isolate eigenvalues if possible
           go to 50
           ! row and column exchange.
           20 continue
           scale( m ) = j
           if( j==m )go to 30
           call stdlib_sswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_sswap( n-k+1, a( j, k ), lda, a( m, k ), lda )
           30 continue
           go to ( 40, 80 )iexc
           ! search for rows isolating an eigenvalue and push them down.
           40 continue
           if( l==1 )go to 210
           l = l - 1_ilp
           50 continue
           loop_70: do j = l, 1, -1
              loop_60: do i = 1, l
                 if( i==j )cycle loop_60
                 if( a( j, i )/=zero )cycle loop_70
              end do loop_60
              m = l
              iexc = 1_ilp
              go to 20
           end do loop_70
           go to 90
           ! search for columns isolating an eigenvalue and push them left.
           80 continue
           k = k + 1_ilp
           90 continue
           loop_110: do j = k, l
              loop_100: do i = k, l
                 if( i==j )cycle loop_100
                 if( a( i, j )/=zero )cycle loop_110
              end do loop_100
              m = k
              iexc = 2_ilp
              go to 20
           end do loop_110
           120 continue
           do i = k, l
              scale( i ) = one
           end do
           if( stdlib_lsame( job, 'P' ) )go to 210
           ! balance the submatrix in rows k to l.
           ! iterative loop for norm reduction
           sfmin1 = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           sfmax1 = one / sfmin1
           sfmin2 = sfmin1*sclfac
           sfmax2 = one / sfmin2
           140 continue
           noconv = .false.
           loop_200: do i = k, l
              c = stdlib_snrm2( l-k+1, a( k, i ), 1_ilp )
              r = stdlib_snrm2( l-k+1, a( i, k ), lda )
              ica = stdlib_isamax( l, a( 1_ilp, i ), 1_ilp )
              ca = abs( a( ica, i ) )
              ira = stdlib_isamax( n-k+1, a( i, k ), lda )
              ra = abs( a( i, ira+k-1 ) )
              ! guard against zero c or r due to underflow.
              if( c==zero .or. r==zero )cycle loop_200
              g = r / sclfac
              f = one
              s = c + r
              160 continue
              if( c>=g .or. max( f, c, ca )>=sfmax2 .or.min( r, g, ra )<=sfmin2 )go to 170
              f = f*sclfac
              c = c*sclfac
              ca = ca*sclfac
              r = r / sclfac
              g = g / sclfac
              ra = ra / sclfac
              go to 160
              170 continue
              g = c / sclfac
              180 continue
              if( g<r .or. max( r, ra )>=sfmax2 .or.min( f, c, g, ca )<=sfmin2 )go to 190
                 if( stdlib_sisnan( c+f+ca+r+g+ra ) ) then
                 ! exit if nan to avoid infinite loop
                 info = -3_ilp
                 call stdlib_xerbla( 'SGEBAL', -info )
                 return
              end if
              f = f / sclfac
              c = c / sclfac
              g = g / sclfac
              ca = ca / sclfac
              r = r*sclfac
              ra = ra*sclfac
              go to 180
              ! now balance.
              190 continue
              if( ( c+r )>=factor*s )cycle loop_200
              if( f<one .and. scale( i )<one ) then
                 if( f*scale( i )<=sfmin1 )cycle loop_200
              end if
              if( f>one .and. scale( i )>one ) then
                 if( scale( i )>=sfmax1 / f )cycle loop_200
              end if
              g = one / f
              scale( i ) = scale( i )*f
              noconv = .true.
              call stdlib_sscal( n-k+1, g, a( i, k ), lda )
              call stdlib_sscal( l, f, a( 1_ilp, i ), 1_ilp )
           end do loop_200
           if( noconv )go to 140
           210 continue
           ilo = k
           ihi = l
           return
     end subroutine stdlib_sgebal

     pure module subroutine stdlib_dgebal( job, n, a, lda, ilo, ihi, scale, info )
     !! DGEBAL balances a general real matrix A.  This involves, first,
     !! permuting A by a similarity transformation to isolate eigenvalues
     !! in the first 1 to ILO-1 and last IHI+1 to N elements on the
     !! diagonal; and second, applying a diagonal similarity transformation
     !! to rows and columns ILO to IHI to make the rows and columns as
     !! close in norm as possible.  Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrix, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: scale(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sclfac = 2.0e+0_dp
           real(dp), parameter :: factor = 0.95e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: noconv
           integer(ilp) :: i, ica, iexc, ira, j, k, l, m
           real(dp) :: c, ca, f, g, r, ra, s, sfmax1, sfmax2, sfmin1, sfmin2
           ! Intrinsic Functions 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEBAL', -info )
              return
           end if
           k = 1_ilp
           l = n
           if( n==0 )go to 210
           if( stdlib_lsame( job, 'N' ) ) then
              do i = 1, n
                 scale( i ) = one
              end do
              go to 210
           end if
           if( stdlib_lsame( job, 'S' ) )go to 120
           ! permutation to isolate eigenvalues if possible
           go to 50
           ! row and column exchange.
           20 continue
           scale( m ) = j
           if( j==m )go to 30
           call stdlib_dswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_dswap( n-k+1, a( j, k ), lda, a( m, k ), lda )
           30 continue
           go to ( 40, 80 )iexc
           ! search for rows isolating an eigenvalue and push them down.
           40 continue
           if( l==1 )go to 210
           l = l - 1_ilp
           50 continue
           loop_70: do j = l, 1, -1
              loop_60: do i = 1, l
                 if( i==j )cycle loop_60
                 if( a( j, i )/=zero )cycle loop_70
              end do loop_60
              m = l
              iexc = 1_ilp
              go to 20
           end do loop_70
           go to 90
           ! search for columns isolating an eigenvalue and push them left.
           80 continue
           k = k + 1_ilp
           90 continue
           loop_110: do j = k, l
              loop_100: do i = k, l
                 if( i==j )cycle loop_100
                 if( a( i, j )/=zero )cycle loop_110
              end do loop_100
              m = k
              iexc = 2_ilp
              go to 20
           end do loop_110
           120 continue
           do i = k, l
              scale( i ) = one
           end do
           if( stdlib_lsame( job, 'P' ) )go to 210
           ! balance the submatrix in rows k to l.
           ! iterative loop for norm reduction
           sfmin1 = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           sfmax1 = one / sfmin1
           sfmin2 = sfmin1*sclfac
           sfmax2 = one / sfmin2
           140 continue
           noconv = .false.
           loop_200: do i = k, l
              c = stdlib_dnrm2( l-k+1, a( k, i ), 1_ilp )
              r = stdlib_dnrm2( l-k+1, a( i, k ), lda )
              ica = stdlib_idamax( l, a( 1_ilp, i ), 1_ilp )
              ca = abs( a( ica, i ) )
              ira = stdlib_idamax( n-k+1, a( i, k ), lda )
              ra = abs( a( i, ira+k-1 ) )
              ! guard against zero c or r due to underflow.
              if( c==zero .or. r==zero )cycle loop_200
              g = r / sclfac
              f = one
              s = c + r
              160 continue
              if( c>=g .or. max( f, c, ca )>=sfmax2 .or.min( r, g, ra )<=sfmin2 )go to 170
                 if( stdlib_disnan( c+f+ca+r+g+ra ) ) then
                 ! exit if nan to avoid infinite loop
                 info = -3_ilp
                 call stdlib_xerbla( 'DGEBAL', -info )
                 return
              end if
              f = f*sclfac
              c = c*sclfac
              ca = ca*sclfac
              r = r / sclfac
              g = g / sclfac
              ra = ra / sclfac
              go to 160
              170 continue
              g = c / sclfac
              180 continue
              if( g<r .or. max( r, ra )>=sfmax2 .or.min( f, c, g, ca )<=sfmin2 )go to 190
              f = f / sclfac
              c = c / sclfac
              g = g / sclfac
              ca = ca / sclfac
              r = r*sclfac
              ra = ra*sclfac
              go to 180
              ! now balance.
              190 continue
              if( ( c+r )>=factor*s )cycle loop_200
              if( f<one .and. scale( i )<one ) then
                 if( f*scale( i )<=sfmin1 )cycle loop_200
              end if
              if( f>one .and. scale( i )>one ) then
                 if( scale( i )>=sfmax1 / f )cycle loop_200
              end if
              g = one / f
              scale( i ) = scale( i )*f
              noconv = .true.
              call stdlib_dscal( n-k+1, g, a( i, k ), lda )
              call stdlib_dscal( l, f, a( 1_ilp, i ), 1_ilp )
           end do loop_200
           if( noconv )go to 140
           210 continue
           ilo = k
           ihi = l
           return
     end subroutine stdlib_dgebal


     pure module subroutine stdlib_cgebal( job, n, a, lda, ilo, ihi, scale, info )
     !! CGEBAL balances a general complex matrix A.  This involves, first,
     !! permuting A by a similarity transformation to isolate eigenvalues
     !! in the first 1 to ILO-1 and last IHI+1 to N elements on the
     !! diagonal; and second, applying a diagonal similarity transformation
     !! to rows and columns ILO to IHI to make the rows and columns as
     !! close in norm as possible.  Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrix, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: scale(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sclfac = 2.0e+0_sp
           real(sp), parameter :: factor = 0.95e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: noconv
           integer(ilp) :: i, ica, iexc, ira, j, k, l, m
           real(sp) :: c, ca, f, g, r, ra, s, sfmax1, sfmax2, sfmin1, sfmin2
           ! Intrinsic Functions 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEBAL', -info )
              return
           end if
           k = 1_ilp
           l = n
           if( n==0 )go to 210
           if( stdlib_lsame( job, 'N' ) ) then
              do i = 1, n
                 scale( i ) = one
              end do
              go to 210
           end if
           if( stdlib_lsame( job, 'S' ) )go to 120
           ! permutation to isolate eigenvalues if possible
           go to 50
           ! row and column exchange.
           20 continue
           scale( m ) = j
           if( j==m )go to 30
           call stdlib_cswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_cswap( n-k+1, a( j, k ), lda, a( m, k ), lda )
           30 continue
           go to ( 40, 80 )iexc
           ! search for rows isolating an eigenvalue and push them down.
           40 continue
           if( l==1 )go to 210
           l = l - 1_ilp
           50 continue
           loop_70: do j = l, 1, -1
              loop_60: do i = 1, l
                 if( i==j )cycle loop_60
                 if( real( a( j, i ),KIND=sp)/=zero .or. aimag( a( j, i ) )/=zero )cycle &
                           loop_70
              end do loop_60
              m = l
              iexc = 1_ilp
              go to 20
           end do loop_70
           go to 90
           ! search for columns isolating an eigenvalue and push them left.
           80 continue
           k = k + 1_ilp
           90 continue
           loop_110: do j = k, l
              loop_100: do i = k, l
                 if( i==j )cycle loop_100
                 if( real( a( i, j ),KIND=sp)/=zero .or. aimag( a( i, j ) )/=zero )cycle &
                           loop_110
              end do loop_100
              m = k
              iexc = 2_ilp
              go to 20
           end do loop_110
           120 continue
           do i = k, l
              scale( i ) = one
           end do
           if( stdlib_lsame( job, 'P' ) )go to 210
           ! balance the submatrix in rows k to l.
           ! iterative loop for norm reduction
           sfmin1 = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           sfmax1 = one / sfmin1
           sfmin2 = sfmin1*sclfac
           sfmax2 = one / sfmin2
           140 continue
           noconv = .false.
           loop_200: do i = k, l
              c = stdlib_scnrm2( l-k+1, a( k, i ), 1_ilp )
              r = stdlib_scnrm2( l-k+1, a( i , k ), lda )
              ica = stdlib_icamax( l, a( 1_ilp, i ), 1_ilp )
              ca = abs( a( ica, i ) )
              ira = stdlib_icamax( n-k+1, a( i, k ), lda )
              ra = abs( a( i, ira+k-1 ) )
              ! guard against zero c or r due to underflow.
              if( c==zero .or. r==zero )cycle loop_200
              g = r / sclfac
              f = one
              s = c + r
              160 continue
              if( c>=g .or. max( f, c, ca )>=sfmax2 .or.min( r, g, ra )<=sfmin2 )go to 170
                 if( stdlib_sisnan( c+f+ca+r+g+ra ) ) then
                 ! exit if nan to avoid infinite loop
                 info = -3_ilp
                 call stdlib_xerbla( 'CGEBAL', -info )
                 return
              end if
              f = f*sclfac
              c = c*sclfac
              ca = ca*sclfac
              r = r / sclfac
              g = g / sclfac
              ra = ra / sclfac
              go to 160
              170 continue
              g = c / sclfac
              180 continue
              if( g<r .or. max( r, ra )>=sfmax2 .or.min( f, c, g, ca )<=sfmin2 )go to 190
              f = f / sclfac
              c = c / sclfac
              g = g / sclfac
              ca = ca / sclfac
              r = r*sclfac
              ra = ra*sclfac
              go to 180
              ! now balance.
              190 continue
              if( ( c+r )>=factor*s )cycle loop_200
              if( f<one .and. scale( i )<one ) then
                 if( f*scale( i )<=sfmin1 )cycle loop_200
              end if
              if( f>one .and. scale( i )>one ) then
                 if( scale( i )>=sfmax1 / f )cycle loop_200
              end if
              g = one / f
              scale( i ) = scale( i )*f
              noconv = .true.
              call stdlib_csscal( n-k+1, g, a( i, k ), lda )
              call stdlib_csscal( l, f, a( 1_ilp, i ), 1_ilp )
           end do loop_200
           if( noconv )go to 140
           210 continue
           ilo = k
           ihi = l
           return
     end subroutine stdlib_cgebal

     pure module subroutine stdlib_zgebal( job, n, a, lda, ilo, ihi, scale, info )
     !! ZGEBAL balances a general complex matrix A.  This involves, first,
     !! permuting A by a similarity transformation to isolate eigenvalues
     !! in the first 1 to ILO-1 and last IHI+1 to N elements on the
     !! diagonal; and second, applying a diagonal similarity transformation
     !! to rows and columns ILO to IHI to make the rows and columns as
     !! close in norm as possible.  Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrix, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: scale(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sclfac = 2.0e+0_dp
           real(dp), parameter :: factor = 0.95e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: noconv
           integer(ilp) :: i, ica, iexc, ira, j, k, l, m
           real(dp) :: c, ca, f, g, r, ra, s, sfmax1, sfmax2, sfmin1, sfmin2
           ! Intrinsic Functions 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEBAL', -info )
              return
           end if
           k = 1_ilp
           l = n
           if( n==0 )go to 210
           if( stdlib_lsame( job, 'N' ) ) then
              do i = 1, n
                 scale( i ) = one
              end do
              go to 210
           end if
           if( stdlib_lsame( job, 'S' ) )go to 120
           ! permutation to isolate eigenvalues if possible
           go to 50
           ! row and column exchange.
           20 continue
           scale( m ) = j
           if( j==m )go to 30
           call stdlib_zswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_zswap( n-k+1, a( j, k ), lda, a( m, k ), lda )
           30 continue
           go to ( 40, 80 )iexc
           ! search for rows isolating an eigenvalue and push them down.
           40 continue
           if( l==1 )go to 210
           l = l - 1_ilp
           50 continue
           loop_70: do j = l, 1, -1
              loop_60: do i = 1, l
                 if( i==j )cycle loop_60
                 if( real( a( j, i ),KIND=dp)/=zero .or. aimag( a( j, i ) )/=zero )cycle &
                           loop_70
              end do loop_60
              m = l
              iexc = 1_ilp
              go to 20
           end do loop_70
           go to 90
           ! search for columns isolating an eigenvalue and push them left.
           80 continue
           k = k + 1_ilp
           90 continue
           loop_110: do j = k, l
              loop_100: do i = k, l
                 if( i==j )cycle loop_100
                 if( real( a( i, j ),KIND=dp)/=zero .or. aimag( a( i, j ) )/=zero )cycle &
                           loop_110
              end do loop_100
              m = k
              iexc = 2_ilp
              go to 20
           end do loop_110
           120 continue
           do i = k, l
              scale( i ) = one
           end do
           if( stdlib_lsame( job, 'P' ) )go to 210
           ! balance the submatrix in rows k to l.
           ! iterative loop for norm reduction
           sfmin1 = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           sfmax1 = one / sfmin1
           sfmin2 = sfmin1*sclfac
           sfmax2 = one / sfmin2
           140 continue
           noconv = .false.
           loop_200: do i = k, l
              c = stdlib_dznrm2( l-k+1, a( k, i ), 1_ilp )
              r = stdlib_dznrm2( l-k+1, a( i, k ), lda )
              ica = stdlib_izamax( l, a( 1_ilp, i ), 1_ilp )
              ca = abs( a( ica, i ) )
              ira = stdlib_izamax( n-k+1, a( i, k ), lda )
              ra = abs( a( i, ira+k-1 ) )
              ! guard against zero c or r due to underflow.
              if( c==zero .or. r==zero )cycle loop_200
              g = r / sclfac
              f = one
              s = c + r
              160 continue
              if( c>=g .or. max( f, c, ca )>=sfmax2 .or.min( r, g, ra )<=sfmin2 )go to 170
                 if( stdlib_disnan( c+f+ca+r+g+ra ) ) then
                 ! exit if nan to avoid infinite loop
                 info = -3_ilp
                 call stdlib_xerbla( 'ZGEBAL', -info )
                 return
              end if
              f = f*sclfac
              c = c*sclfac
              ca = ca*sclfac
              r = r / sclfac
              g = g / sclfac
              ra = ra / sclfac
              go to 160
              170 continue
              g = c / sclfac
              180 continue
              if( g<r .or. max( r, ra )>=sfmax2 .or.min( f, c, g, ca )<=sfmin2 )go to 190
              f = f / sclfac
              c = c / sclfac
              g = g / sclfac
              ca = ca / sclfac
              r = r*sclfac
              ra = ra*sclfac
              go to 180
              ! now balance.
              190 continue
              if( ( c+r )>=factor*s )cycle loop_200
              if( f<one .and. scale( i )<one ) then
                 if( f*scale( i )<=sfmin1 )cycle loop_200
              end if
              if( f>one .and. scale( i )>one ) then
                 if( scale( i )>=sfmax1 / f )cycle loop_200
              end if
              g = one / f
              scale( i ) = scale( i )*f
              noconv = .true.
              call stdlib_zdscal( n-k+1, g, a( i, k ), lda )
              call stdlib_zdscal( l, f, a( 1_ilp, i ), 1_ilp )
           end do loop_200
           if( noconv )go to 140
           210 continue
           ilo = k
           ihi = l
           return
     end subroutine stdlib_zgebal




     pure module subroutine stdlib_sgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! SGEHRD reduces a real general matrix A to upper Hessenberg form H by
     !! an orthogonal similarity transformation:  Q**T * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iwt, j, ldwork, lwkopt, nb, nbmin, nh, nx
           real(sp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
             ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              lwkopt = n*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEHRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! set elements 1:ilo-1 and ihi:n-1 of tau to zero
           do i = 1, ilo - 1
              tau( i ) = zero
           end do
           do i = max( 1, ihi ), n - 1
              tau( i ) = zero
           end do
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! determine the block size
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
           nbmin = 2_ilp
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code)
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'SGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code
                 if( lwork<n*nb+tsize ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGEHRD', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=(n*nbmin + tsize) ) then
                       nb = (lwork-tsize) / n
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           ldwork = n
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              i = ilo
           else
              ! use blocked code
              iwt = 1_ilp + n*nb
              do i = ilo, ihi - 1 - nx, nb
                 ib = min( nb, ihi-i )
                 ! reduce columns i:i+ib-1 to hessenberg form, returning the
                 ! matrices v and t of the block reflector h = i - v*t*v**t
                 ! which performs the reduction, and also the matrix y = a*v*t
                 call stdlib_slahr2( ihi, i, ib, a( 1_ilp, i ), lda, tau( i ),work( iwt ), ldt, work, &
                           ldwork )
                 ! apply the block reflector h to a(1:ihi,i+ib:ihi) from the
                 ! right, computing  a := a - y * v**t. v(i+ib,ib-1) must be set
                 ! to 1
                 ei = a( i+ib, i+ib-1 )
                 a( i+ib, i+ib-1 ) = one
                 call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE',ihi, ihi-i-ib+1,ib, -one, work, &
                           ldwork, a( i+ib, i ), lda, one,a( 1_ilp, i+ib ), lda )
                 a( i+ib, i+ib-1 ) = ei
                 ! apply the block reflector h to a(1:i,i+1:i+ib-1) from the
                 ! right
                 call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE','UNIT', i, ib-1,one, a( i+1, i )&
                           , lda, work, ldwork )
                 do j = 0, ib-2
                    call stdlib_saxpy( i, -one, work( ldwork*j+1 ), 1_ilp,a( 1_ilp, i+j+1 ), 1_ilp )
                 end do
                 ! apply the block reflector h to a(i+1:ihi,i+ib:n) from the
                 ! left
                 call stdlib_slarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE',ihi-i, n-i-ib+1, &
                           ib, a( i+1, i ), lda,work( iwt ), ldt, a( i+1, i+ib ), lda,work, ldwork )
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           call stdlib_sgehd2( n, i, ihi, a, lda, tau, work, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sgehrd

     pure module subroutine stdlib_dgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! DGEHRD reduces a real general matrix A to upper Hessenberg form H by
     !! an orthogonal similarity transformation:  Q**T * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iwt, j, ldwork, lwkopt, nb, nbmin, nh, nx
           real(dp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              lwkopt = n*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEHRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! set elements 1:ilo-1 and ihi:n-1 of tau to zero
           do i = 1, ilo - 1
              tau( i ) = zero
           end do
           do i = max( 1, ihi ), n - 1
              tau( i ) = zero
           end do
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! determine the block size
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
           nbmin = 2_ilp
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code)
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'DGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code
                 if( lwork<n*nb+tsize ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGEHRD', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=(n*nbmin + tsize) ) then
                       nb = (lwork-tsize) / n
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           ldwork = n
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              i = ilo
           else
              ! use blocked code
              iwt = 1_ilp + n*nb
              do i = ilo, ihi - 1 - nx, nb
                 ib = min( nb, ihi-i )
                 ! reduce columns i:i+ib-1 to hessenberg form, returning the
                 ! matrices v and t of the block reflector h = i - v*t*v**t
                 ! which performs the reduction, and also the matrix y = a*v*t
                 call stdlib_dlahr2( ihi, i, ib, a( 1_ilp, i ), lda, tau( i ),work( iwt ), ldt, work, &
                           ldwork )
                 ! apply the block reflector h to a(1:ihi,i+ib:ihi) from the
                 ! right, computing  a := a - y * v**t. v(i+ib,ib-1) must be set
                 ! to 1
                 ei = a( i+ib, i+ib-1 )
                 a( i+ib, i+ib-1 ) = one
                 call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE',ihi, ihi-i-ib+1,ib, -one, work, &
                           ldwork, a( i+ib, i ), lda, one,a( 1_ilp, i+ib ), lda )
                 a( i+ib, i+ib-1 ) = ei
                 ! apply the block reflector h to a(1:i,i+1:i+ib-1) from the
                 ! right
                 call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE','UNIT', i, ib-1,one, a( i+1, i )&
                           , lda, work, ldwork )
                 do j = 0, ib-2
                    call stdlib_daxpy( i, -one, work( ldwork*j+1 ), 1_ilp,a( 1_ilp, i+j+1 ), 1_ilp )
                 end do
                 ! apply the block reflector h to a(i+1:ihi,i+ib:n) from the
                 ! left
                 call stdlib_dlarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE',ihi-i, n-i-ib+1, &
                           ib, a( i+1, i ), lda,work( iwt ), ldt, a( i+1, i+ib ), lda,work, ldwork )
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           call stdlib_dgehd2( n, i, ihi, a, lda, tau, work, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dgehrd


     pure module subroutine stdlib_cgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! CGEHRD reduces a complex general matrix A to upper Hessenberg form H by
     !! an unitary similarity transformation:  Q**H * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iwt, j, ldwork, lwkopt, nb, nbmin, nh, nx
           complex(sp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              lwkopt = n*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEHRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! set elements 1:ilo-1 and ihi:n-1 of tau to czero
           do i = 1, ilo - 1
              tau( i ) = czero
           end do
           do i = max( 1, ihi ), n - 1
              tau( i ) = czero
           end do
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! determine the block size
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
           nbmin = 2_ilp
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code)
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'CGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code
                 if( lwork<n*nb+tsize ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGEHRD', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=(n*nbmin+tsize) ) then
                       nb = (lwork-tsize) / n
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           ldwork = n
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              i = ilo
           else
              ! use blocked code
              iwt = 1_ilp + n*nb
              do i = ilo, ihi - 1 - nx, nb
                 ib = min( nb, ihi-i )
                 ! reduce columns i:i+ib-1 to hessenberg form, returning the
                 ! matrices v and t of the block reflector h = i - v*t*v**h
                 ! which performs the reduction, and also the matrix y = a*v*t
                 call stdlib_clahr2( ihi, i, ib, a( 1_ilp, i ), lda, tau( i ),work( iwt ), ldt, work, &
                           ldwork )
                 ! apply the block reflector h to a(1:ihi,i+ib:ihi) from the
                 ! right, computing  a := a - y * v**h. v(i+ib,ib-1) must be set
                 ! to 1
                 ei = a( i+ib, i+ib-1 )
                 a( i+ib, i+ib-1 ) = cone
                 call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',ihi, ihi-i-ib+1,ib, -&
                           cone, work, ldwork, a( i+ib, i ), lda, cone,a( 1_ilp, i+ib ), lda )
                 a( i+ib, i+ib-1 ) = ei
                 ! apply the block reflector h to a(1:i,i+1:i+ib-1) from the
                 ! right
                 call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', i, ib-1,cone, &
                           a( i+1, i ), lda, work, ldwork )
                 do j = 0, ib-2
                    call stdlib_caxpy( i, -cone, work( ldwork*j+1 ), 1_ilp,a( 1_ilp, i+j+1 ), 1_ilp )
                 end do
                 ! apply the block reflector h to a(i+1:ihi,i+ib:n) from the
                 ! left
                 call stdlib_clarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE',ihi-i, &
                 n-i-ib+1, ib, a( i+1, i ), lda,work( iwt ), ldt, a( i+1, i+ib ), lda,work, &
                           ldwork )
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           call stdlib_cgehd2( n, i, ihi, a, lda, tau, work, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cgehrd

     pure module subroutine stdlib_zgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! ZGEHRD reduces a complex general matrix A to upper Hessenberg form H by
     !! an unitary similarity transformation:  Q**H * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iwt, j, ldwork, lwkopt, nb, nbmin, nh, nx
           complex(dp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              lwkopt = n*nb + tsize
              work( 1_ilp ) = lwkopt
           endif
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEHRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! set elements 1:ilo-1 and ihi:n-1 of tau to czero
           do i = 1, ilo - 1
              tau( i ) = czero
           end do
           do i = max( 1, ihi ), n - 1
              tau( i ) = czero
           end do
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! determine the block size
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
           nbmin = 2_ilp
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code)
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'ZGEHRD', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code
                 if( lwork<n*nb+tsize ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGEHRD', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=(n*nbmin + tsize) ) then
                       nb = (lwork-tsize) / n
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           ldwork = n
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              i = ilo
           else
              ! use blocked code
              iwt = 1_ilp + n*nb
              do i = ilo, ihi - 1 - nx, nb
                 ib = min( nb, ihi-i )
                 ! reduce columns i:i+ib-1 to hessenberg form, returning the
                 ! matrices v and t of the block reflector h = i - v*t*v**h
                 ! which performs the reduction, and also the matrix y = a*v*t
                 call stdlib_zlahr2( ihi, i, ib, a( 1_ilp, i ), lda, tau( i ),work( iwt ), ldt, work, &
                           ldwork )
                 ! apply the block reflector h to a(1:ihi,i+ib:ihi) from the
                 ! right, computing  a := a - y * v**h. v(i+ib,ib-1) must be set
                 ! to 1
                 ei = a( i+ib, i+ib-1 )
                 a( i+ib, i+ib-1 ) = cone
                 call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',ihi, ihi-i-ib+1,ib, -&
                           cone, work, ldwork, a( i+ib, i ), lda, cone,a( 1_ilp, i+ib ), lda )
                 a( i+ib, i+ib-1 ) = ei
                 ! apply the block reflector h to a(1:i,i+1:i+ib-1) from the
                 ! right
                 call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', i, ib-1,cone, &
                           a( i+1, i ), lda, work, ldwork )
                 do j = 0, ib-2
                    call stdlib_zaxpy( i, -cone, work( ldwork*j+1 ), 1_ilp,a( 1_ilp, i+j+1 ), 1_ilp )
                 end do
                 ! apply the block reflector h to a(i+1:ihi,i+ib:n) from the
                 ! left
                 call stdlib_zlarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE',ihi-i, &
                 n-i-ib+1, ib, a( i+1, i ), lda,work( iwt ), ldt, a( i+1, i+ib ), lda,work, &
                           ldwork )
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           call stdlib_zgehd2( n, i, ihi, a, lda, tau, work, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zgehrd




     pure module subroutine stdlib_sgehd2( n, ilo, ihi, a, lda, tau, work, info )
     !! SGEHD2 reduces a real general matrix A to upper Hessenberg form H by
     !! an orthogonal similarity transformation:  Q**T * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEHD2', -info )
              return
           end if
           do i = ilo, ihi - 1
              ! compute elementary reflector h(i) to annihilate a(i+2:ihi,i)
              call stdlib_slarfg( ihi-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,tau( i ) )
              aii = a( i+1, i )
              a( i+1, i ) = one
              ! apply h(i) to a(1:ihi,i+1:ihi) from the right
              call stdlib_slarf( 'RIGHT', ihi, ihi-i, a( i+1, i ), 1_ilp, tau( i ),a( 1_ilp, i+1 ), lda, &
                        work )
              ! apply h(i) to a(i+1:ihi,i+1:n) from the left
              call stdlib_slarf( 'LEFT', ihi-i, n-i, a( i+1, i ), 1_ilp, tau( i ),a( i+1, i+1 ), lda, &
                        work )
              a( i+1, i ) = aii
           end do
           return
     end subroutine stdlib_sgehd2

     pure module subroutine stdlib_dgehd2( n, ilo, ihi, a, lda, tau, work, info )
     !! DGEHD2 reduces a real general matrix A to upper Hessenberg form H by
     !! an orthogonal similarity transformation:  Q**T * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEHD2', -info )
              return
           end if
           do i = ilo, ihi - 1
              ! compute elementary reflector h(i) to annihilate a(i+2:ihi,i)
              call stdlib_dlarfg( ihi-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,tau( i ) )
              aii = a( i+1, i )
              a( i+1, i ) = one
              ! apply h(i) to a(1:ihi,i+1:ihi) from the right
              call stdlib_dlarf( 'RIGHT', ihi, ihi-i, a( i+1, i ), 1_ilp, tau( i ),a( 1_ilp, i+1 ), lda, &
                        work )
              ! apply h(i) to a(i+1:ihi,i+1:n) from the left
              call stdlib_dlarf( 'LEFT', ihi-i, n-i, a( i+1, i ), 1_ilp, tau( i ),a( i+1, i+1 ), lda, &
                        work )
              a( i+1, i ) = aii
           end do
           return
     end subroutine stdlib_dgehd2


     pure module subroutine stdlib_cgehd2( n, ilo, ihi, a, lda, tau, work, info )
     !! CGEHD2 reduces a complex general matrix A to upper Hessenberg form H
     !! by a unitary similarity transformation:  Q**H * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEHD2', -info )
              return
           end if
           do i = ilo, ihi - 1
              ! compute elementary reflector h(i) to annihilate a(i+2:ihi,i)
              alpha = a( i+1, i )
              call stdlib_clarfg( ihi-i, alpha, a( min( i+2, n ), i ), 1_ilp, tau( i ) )
              a( i+1, i ) = cone
              ! apply h(i) to a(1:ihi,i+1:ihi) from the right
              call stdlib_clarf( 'RIGHT', ihi, ihi-i, a( i+1, i ), 1_ilp, tau( i ),a( 1_ilp, i+1 ), lda, &
                        work )
              ! apply h(i)**h to a(i+1:ihi,i+1:n) from the left
              call stdlib_clarf( 'LEFT', ihi-i, n-i, a( i+1, i ), 1_ilp,conjg( tau( i ) ), a( i+1, i+&
                        1_ilp ), lda, work )
              a( i+1, i ) = alpha
           end do
           return
     end subroutine stdlib_cgehd2

     pure module subroutine stdlib_zgehd2( n, ilo, ihi, a, lda, tau, work, info )
     !! ZGEHD2 reduces a complex general matrix A to upper Hessenberg form H
     !! by a unitary similarity transformation:  Q**H * A * Q = H .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEHD2', -info )
              return
           end if
           do i = ilo, ihi - 1
              ! compute elementary reflector h(i) to annihilate a(i+2:ihi,i)
              alpha = a( i+1, i )
              call stdlib_zlarfg( ihi-i, alpha, a( min( i+2, n ), i ), 1_ilp, tau( i ) )
              a( i+1, i ) = cone
              ! apply h(i) to a(1:ihi,i+1:ihi) from the right
              call stdlib_zlarf( 'RIGHT', ihi, ihi-i, a( i+1, i ), 1_ilp, tau( i ),a( 1_ilp, i+1 ), lda, &
                        work )
              ! apply h(i)**h to a(i+1:ihi,i+1:n) from the left
              call stdlib_zlarf( 'LEFT', ihi-i, n-i, a( i+1, i ), 1_ilp,conjg( tau( i ) ), a( i+1, i+&
                        1_ilp ), lda, work )
              a( i+1, i ) = alpha
           end do
           return
     end subroutine stdlib_zgehd2




     pure module subroutine stdlib_sgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
     !! SGEBAK forms the right or left eigenvectors of a real general matrix
     !! by backward transformation on the computed eigenvectors of the
     !! balanced matrix output by SGEBAL.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: v(ldv,*)
           real(sp), intent(in) :: scale(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, ii, k
           real(sp) :: s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -7_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 do i = ilo, ihi
                    s = scale( i )
                    call stdlib_sscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
              if( leftv ) then
                 do i = ilo, ihi
                    s = one / scale( i )
                    call stdlib_sscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           ! for  i = ilo-1 step -1 until 1,
                    ! ihi+1 step 1 until n do --
                    30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 loop_40: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_40
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_40
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
              end if
              if( leftv ) then
                 loop_50: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_50
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_50
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_50
              end if
           end if
           return
     end subroutine stdlib_sgebak

     pure module subroutine stdlib_dgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
     !! DGEBAK forms the right or left eigenvectors of a real general matrix
     !! by backward transformation on the computed eigenvectors of the
     !! balanced matrix output by DGEBAL.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: scale(*)
           real(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, ii, k
           real(dp) :: s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -7_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 do i = ilo, ihi
                    s = scale( i )
                    call stdlib_dscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
              if( leftv ) then
                 do i = ilo, ihi
                    s = one / scale( i )
                    call stdlib_dscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           ! for  i = ilo-1 step -1 until 1,
                    ! ihi+1 step 1 until n do --
                    30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 loop_40: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_40
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_40
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
              end if
              if( leftv ) then
                 loop_50: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_50
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_50
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_50
              end if
           end if
           return
     end subroutine stdlib_dgebak


     pure module subroutine stdlib_cgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
     !! CGEBAK forms the right or left eigenvectors of a complex general
     !! matrix by backward transformation on the computed eigenvectors of the
     !! balanced matrix output by CGEBAL.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: scale(*)
           complex(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, ii, k
           real(sp) :: s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -7_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 do i = ilo, ihi
                    s = scale( i )
                    call stdlib_csscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
              if( leftv ) then
                 do i = ilo, ihi
                    s = one / scale( i )
                    call stdlib_csscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           ! for  i = ilo-1 step -1 until 1,
                    ! ihi+1 step 1 until n do --
                    30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 loop_40: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_40
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_40
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
              end if
              if( leftv ) then
                 loop_50: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_50
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_50
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_50
              end if
           end if
           return
     end subroutine stdlib_cgebak

     pure module subroutine stdlib_zgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
     !! ZGEBAK forms the right or left eigenvectors of a complex general
     !! matrix by backward transformation on the computed eigenvectors of the
     !! balanced matrix output by ZGEBAL.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: scale(*)
           complex(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, ii, k
           real(dp) :: s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -7_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 do i = ilo, ihi
                    s = scale( i )
                    call stdlib_zdscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
              if( leftv ) then
                 do i = ilo, ihi
                    s = one / scale( i )
                    call stdlib_zdscal( m, s, v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           ! for  i = ilo-1 step -1 until 1,
                    ! ihi+1 step 1 until n do --
                    30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              if( rightv ) then
                 loop_40: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_40
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_40
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
              end if
              if( leftv ) then
                 loop_50: do ii = 1, n
                    i = ii
                    if( i>=ilo .and. i<=ihi )cycle loop_50
                    if( i<ilo )i = ilo - ii
                    k = scale( i )
                    if( k==i )cycle loop_50
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_50
              end if
           end if
           return
     end subroutine stdlib_zgebak




     pure module subroutine stdlib_slahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
     !! SLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
     !! matrix A so that elements below the k-th subdiagonal are zero. The
     !! reduction is performed by an orthogonal similarity transformation
     !! Q**T * A * Q. The routine returns the matrices V and T which determine
     !! Q as a block reflector I - V*T*V**T, and also the matrix Y = A * V * T.
     !! This is an auxiliary routine called by SGEHRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1 )return
           loop_10: do i = 1, nb
              if( i>1_ilp ) then
                 ! update a(k+1:n,i)
                 ! update i-th column of a - y * v**t
                 call stdlib_sgemv( 'NO TRANSPOSE', n-k, i-1, -one, y(k+1,1_ilp), ldy,a( k+i-1, 1_ilp ), &
                           lda, one, a( k+1, i ), 1_ilp )
                 ! apply i - v * t**t * v**t to this column (call it b) from the
                 ! left, using the last column of t as workspace
                 ! let  v = ( v1 )   and   b = ( b1 )   (first i-1 rows)
                          ! ( v2 )             ( b2 )
                 ! where v1 is unit lower triangular
                 ! w := v1**t * b1
                 call stdlib_scopy( i-1, a( k+1, i ), 1_ilp, t( 1_ilp, nb ), 1_ilp )
                 call stdlib_strmv( 'LOWER', 'TRANSPOSE', 'UNIT',i-1, a( k+1, 1_ilp ),lda, t( 1_ilp, nb ),&
                            1_ilp )
                 ! w := w + v2**t * b2
                 call stdlib_sgemv( 'TRANSPOSE', n-k-i+1, i-1,one, a( k+i, 1_ilp ),lda, a( k+i, i ), &
                           1_ilp, one, t( 1_ilp, nb ), 1_ilp )
                 ! w := t**t * w
                 call stdlib_strmv( 'UPPER', 'TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, nb ), 1_ilp )
                           
                 ! b2 := b2 - v2*w
                 call stdlib_sgemv( 'NO TRANSPOSE', n-k-i+1, i-1, -one,a( k+i, 1_ilp ),lda, t( 1_ilp, nb )&
                           , 1_ilp, one, a( k+i, i ), 1_ilp )
                 ! b1 := b1 - v1*w
                 call stdlib_strmv( 'LOWER', 'NO TRANSPOSE','UNIT', i-1,a( k+1, 1_ilp ), lda, t( 1_ilp, &
                           nb ), 1_ilp )
                 call stdlib_saxpy( i-1, -one, t( 1_ilp, nb ), 1_ilp, a( k+1, i ), 1_ilp )
                 a( k+i-1, i-1 ) = ei
              end if
              ! generate the elementary reflector h(i) to annihilate
              ! a(k+i+1:n,i)
              call stdlib_slarfg( n-k-i+1, a( k+i, i ), a( min( k+i+1, n ), i ), 1_ilp,tau( i ) )
                        
              ei = a( k+i, i )
              a( k+i, i ) = one
              ! compute  y(k+1:n,i)
              call stdlib_sgemv( 'NO TRANSPOSE', n-k, n-k-i+1,one, a( k+1, i+1 ),lda, a( k+i, i ),&
                         1_ilp, zero, y( k+1, i ), 1_ilp )
              call stdlib_sgemv( 'TRANSPOSE', n-k-i+1, i-1,one, a( k+i, 1_ilp ), lda,a( k+i, i ), 1_ilp, &
                        zero, t( 1_ilp, i ), 1_ilp )
              call stdlib_sgemv( 'NO TRANSPOSE', n-k, i-1, -one,y( k+1, 1_ilp ), ldy,t( 1_ilp, i ), 1_ilp, &
                        one, y( k+1, i ), 1_ilp )
              call stdlib_sscal( n-k, tau( i ), y( k+1, i ), 1_ilp )
              ! compute t(1:i,i)
              call stdlib_sscal( i-1, -tau( i ), t( 1_ilp, i ), 1_ilp )
              call stdlib_strmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, i ), 1_ilp )
                        
              t( i, i ) = tau( i )
           end do loop_10
           a( k+nb, nb ) = ei
           ! compute y(1:k,1:nb)
           call stdlib_slacpy( 'ALL', k, nb, a( 1_ilp, 2_ilp ), lda, y, ldy )
           call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE','UNIT', k, nb,one, a( k+1, 1_ilp ), &
                     lda, y, ldy )
           if( n>k+nb )call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', k,nb, n-k-nb, one,a( 1_ilp, &
                     2_ilp+nb ), lda, a( k+1+nb, 1_ilp ), lda, one, y,ldy )
           call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE','NON-UNIT', k, nb,one, t, ldt, y, &
                     ldy )
           return
     end subroutine stdlib_slahr2

     pure module subroutine stdlib_dlahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
     !! DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
     !! matrix A so that elements below the k-th subdiagonal are zero. The
     !! reduction is performed by an orthogonal similarity transformation
     !! Q**T * A * Q. The routine returns the matrices V and T which determine
     !! Q as a block reflector I - V*T*V**T, and also the matrix Y = A * V * T.
     !! This is an auxiliary routine called by DGEHRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1 )return
           loop_10: do i = 1, nb
              if( i>1_ilp ) then
                 ! update a(k+1:n,i)
                 ! update i-th column of a - y * v**t
                 call stdlib_dgemv( 'NO TRANSPOSE', n-k, i-1, -one, y(k+1,1_ilp), ldy,a( k+i-1, 1_ilp ), &
                           lda, one, a( k+1, i ), 1_ilp )
                 ! apply i - v * t**t * v**t to this column (call it b) from the
                 ! left, using the last column of t as workspace
                 ! let  v = ( v1 )   and   b = ( b1 )   (first i-1 rows)
                          ! ( v2 )             ( b2 )
                 ! where v1 is unit lower triangular
                 ! w := v1**t * b1
                 call stdlib_dcopy( i-1, a( k+1, i ), 1_ilp, t( 1_ilp, nb ), 1_ilp )
                 call stdlib_dtrmv( 'LOWER', 'TRANSPOSE', 'UNIT',i-1, a( k+1, 1_ilp ),lda, t( 1_ilp, nb ),&
                            1_ilp )
                 ! w := w + v2**t * b2
                 call stdlib_dgemv( 'TRANSPOSE', n-k-i+1, i-1,one, a( k+i, 1_ilp ),lda, a( k+i, i ), &
                           1_ilp, one, t( 1_ilp, nb ), 1_ilp )
                 ! w := t**t * w
                 call stdlib_dtrmv( 'UPPER', 'TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, nb ), 1_ilp )
                           
                 ! b2 := b2 - v2*w
                 call stdlib_dgemv( 'NO TRANSPOSE', n-k-i+1, i-1, -one,a( k+i, 1_ilp ),lda, t( 1_ilp, nb )&
                           , 1_ilp, one, a( k+i, i ), 1_ilp )
                 ! b1 := b1 - v1*w
                 call stdlib_dtrmv( 'LOWER', 'NO TRANSPOSE','UNIT', i-1,a( k+1, 1_ilp ), lda, t( 1_ilp, &
                           nb ), 1_ilp )
                 call stdlib_daxpy( i-1, -one, t( 1_ilp, nb ), 1_ilp, a( k+1, i ), 1_ilp )
                 a( k+i-1, i-1 ) = ei
              end if
              ! generate the elementary reflector h(i) to annihilate
              ! a(k+i+1:n,i)
              call stdlib_dlarfg( n-k-i+1, a( k+i, i ), a( min( k+i+1, n ), i ), 1_ilp,tau( i ) )
                        
              ei = a( k+i, i )
              a( k+i, i ) = one
              ! compute  y(k+1:n,i)
              call stdlib_dgemv( 'NO TRANSPOSE', n-k, n-k-i+1,one, a( k+1, i+1 ),lda, a( k+i, i ),&
                         1_ilp, zero, y( k+1, i ), 1_ilp )
              call stdlib_dgemv( 'TRANSPOSE', n-k-i+1, i-1,one, a( k+i, 1_ilp ), lda,a( k+i, i ), 1_ilp, &
                        zero, t( 1_ilp, i ), 1_ilp )
              call stdlib_dgemv( 'NO TRANSPOSE', n-k, i-1, -one,y( k+1, 1_ilp ), ldy,t( 1_ilp, i ), 1_ilp, &
                        one, y( k+1, i ), 1_ilp )
              call stdlib_dscal( n-k, tau( i ), y( k+1, i ), 1_ilp )
              ! compute t(1:i,i)
              call stdlib_dscal( i-1, -tau( i ), t( 1_ilp, i ), 1_ilp )
              call stdlib_dtrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, i ), 1_ilp )
                        
              t( i, i ) = tau( i )
           end do loop_10
           a( k+nb, nb ) = ei
           ! compute y(1:k,1:nb)
           call stdlib_dlacpy( 'ALL', k, nb, a( 1_ilp, 2_ilp ), lda, y, ldy )
           call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE','UNIT', k, nb,one, a( k+1, 1_ilp ), &
                     lda, y, ldy )
           if( n>k+nb )call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', k,nb, n-k-nb, one,a( 1_ilp, &
                     2_ilp+nb ), lda, a( k+1+nb, 1_ilp ), lda, one, y,ldy )
           call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE','NON-UNIT', k, nb,one, t, ldt, y, &
                     ldy )
           return
     end subroutine stdlib_dlahr2


     pure module subroutine stdlib_clahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
     !! CLAHR2 reduces the first NB columns of A complex general n-BY-(n-k+1)
     !! matrix A so that elements below the k-th subdiagonal are zero. The
     !! reduction is performed by an unitary similarity transformation
     !! Q**H * A * Q. The routine returns the matrices V and T which determine
     !! Q as a block reflector I - V*T*v**H, and also the matrix Y = A * V * T.
     !! This is an auxiliary routine called by CGEHRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1 )return
           loop_10: do i = 1, nb
              if( i>1_ilp ) then
                 ! update a(k+1:n,i)
                 ! update i-th column of a - y * v**h
                 call stdlib_clacgv( i-1, a( k+i-1, 1_ilp ), lda )
                 call stdlib_cgemv( 'NO TRANSPOSE', n-k, i-1, -cone, y(k+1,1_ilp), ldy,a( k+i-1, 1_ilp ), &
                           lda, cone, a( k+1, i ), 1_ilp )
                 call stdlib_clacgv( i-1, a( k+i-1, 1_ilp ), lda )
                 ! apply i - v * t**h * v**h to this column (call it b) from the
                 ! left, using the last column of t as workspace
                 ! let  v = ( v1 )   and   b = ( b1 )   (first i-1 rows)
                          ! ( v2 )             ( b2 )
                 ! where v1 is unit lower triangular
                 ! w := v1**h * b1
                 call stdlib_ccopy( i-1, a( k+1, i ), 1_ilp, t( 1_ilp, nb ), 1_ilp )
                 call stdlib_ctrmv( 'LOWER', 'CONJUGATE TRANSPOSE', 'UNIT',i-1, a( k+1, 1_ilp ),lda, &
                           t( 1_ilp, nb ), 1_ilp )
                 ! w := w + v2**h * b2
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k-i+1, i-1,cone, a( k+i, 1_ilp ),lda, a( &
                           k+i, i ), 1_ilp, cone, t( 1_ilp, nb ), 1_ilp )
                 ! w := t**h * w
                 call stdlib_ctrmv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, &
                           nb ), 1_ilp )
                 ! b2 := b2 - v2*w
                 call stdlib_cgemv( 'NO TRANSPOSE', n-k-i+1, i-1, -cone,a( k+i, 1_ilp ),lda, t( 1_ilp, nb &
                           ), 1_ilp, cone, a( k+i, i ), 1_ilp )
                 ! b1 := b1 - v1*w
                 call stdlib_ctrmv( 'LOWER', 'NO TRANSPOSE','UNIT', i-1,a( k+1, 1_ilp ), lda, t( 1_ilp, &
                           nb ), 1_ilp )
                 call stdlib_caxpy( i-1, -cone, t( 1_ilp, nb ), 1_ilp, a( k+1, i ), 1_ilp )
                 a( k+i-1, i-1 ) = ei
              end if
              ! generate the elementary reflector h(i) to annihilate
              ! a(k+i+1:n,i)
              call stdlib_clarfg( n-k-i+1, a( k+i, i ), a( min( k+i+1, n ), i ), 1_ilp,tau( i ) )
                        
              ei = a( k+i, i )
              a( k+i, i ) = cone
              ! compute  y(k+1:n,i)
              call stdlib_cgemv( 'NO TRANSPOSE', n-k, n-k-i+1,cone, a( k+1, i+1 ),lda, a( k+i, i )&
                        , 1_ilp, czero, y( k+1, i ), 1_ilp )
              call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k-i+1, i-1,cone, a( k+i, 1_ilp ), lda,a( k+&
                        i, i ), 1_ilp, czero, t( 1_ilp, i ), 1_ilp )
              call stdlib_cgemv( 'NO TRANSPOSE', n-k, i-1, -cone,y( k+1, 1_ilp ), ldy,t( 1_ilp, i ), 1_ilp, &
                        cone, y( k+1, i ), 1_ilp )
              call stdlib_cscal( n-k, tau( i ), y( k+1, i ), 1_ilp )
              ! compute t(1:i,i)
              call stdlib_cscal( i-1, -tau( i ), t( 1_ilp, i ), 1_ilp )
              call stdlib_ctrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, i ), 1_ilp )
                        
              t( i, i ) = tau( i )
           end do loop_10
           a( k+nb, nb ) = ei
           ! compute y(1:k,1:nb)
           call stdlib_clacpy( 'ALL', k, nb, a( 1_ilp, 2_ilp ), lda, y, ldy )
           call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE','UNIT', k, nb,cone, a( k+1, 1_ilp ), &
                     lda, y, ldy )
           if( n>k+nb )call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', k,nb, n-k-nb, cone,a( 1_ilp,&
                      2_ilp+nb ), lda, a( k+1+nb, 1_ilp ), lda, cone, y,ldy )
           call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE','NON-UNIT', k, nb,cone, t, ldt, y, &
                     ldy )
           return
     end subroutine stdlib_clahr2

     pure module subroutine stdlib_zlahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
     !! ZLAHR2 reduces the first NB columns of A complex general n-BY-(n-k+1)
     !! matrix A so that elements below the k-th subdiagonal are zero. The
     !! reduction is performed by an unitary similarity transformation
     !! Q**H * A * Q. The routine returns the matrices V and T which determine
     !! Q as a block reflector I - V*T*V**H, and also the matrix Y = A * V * T.
     !! This is an auxiliary routine called by ZGEHRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1 )return
           loop_10: do i = 1, nb
              if( i>1_ilp ) then
                 ! update a(k+1:n,i)
                 ! update i-th column of a - y * v**h
                 call stdlib_zlacgv( i-1, a( k+i-1, 1_ilp ), lda )
                 call stdlib_zgemv( 'NO TRANSPOSE', n-k, i-1, -cone, y(k+1,1_ilp), ldy,a( k+i-1, 1_ilp ), &
                           lda, cone, a( k+1, i ), 1_ilp )
                 call stdlib_zlacgv( i-1, a( k+i-1, 1_ilp ), lda )
                 ! apply i - v * t**h * v**h to this column (call it b) from the
                 ! left, using the last column of t as workspace
                 ! let  v = ( v1 )   and   b = ( b1 )   (first i-1 rows)
                          ! ( v2 )             ( b2 )
                 ! where v1 is unit lower triangular
                 ! w := v1**h * b1
                 call stdlib_zcopy( i-1, a( k+1, i ), 1_ilp, t( 1_ilp, nb ), 1_ilp )
                 call stdlib_ztrmv( 'LOWER', 'CONJUGATE TRANSPOSE', 'UNIT',i-1, a( k+1, 1_ilp ),lda, &
                           t( 1_ilp, nb ), 1_ilp )
                 ! w := w + v2**h * b2
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k-i+1, i-1,cone, a( k+i, 1_ilp ),lda, a( &
                           k+i, i ), 1_ilp, cone, t( 1_ilp, nb ), 1_ilp )
                 ! w := t**h * w
                 call stdlib_ztrmv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, &
                           nb ), 1_ilp )
                 ! b2 := b2 - v2*w
                 call stdlib_zgemv( 'NO TRANSPOSE', n-k-i+1, i-1, -cone,a( k+i, 1_ilp ),lda, t( 1_ilp, nb &
                           ), 1_ilp, cone, a( k+i, i ), 1_ilp )
                 ! b1 := b1 - v1*w
                 call stdlib_ztrmv( 'LOWER', 'NO TRANSPOSE','UNIT', i-1,a( k+1, 1_ilp ), lda, t( 1_ilp, &
                           nb ), 1_ilp )
                 call stdlib_zaxpy( i-1, -cone, t( 1_ilp, nb ), 1_ilp, a( k+1, i ), 1_ilp )
                 a( k+i-1, i-1 ) = ei
              end if
              ! generate the elementary reflector h(i) to annihilate
              ! a(k+i+1:n,i)
              call stdlib_zlarfg( n-k-i+1, a( k+i, i ), a( min( k+i+1, n ), i ), 1_ilp,tau( i ) )
                        
              ei = a( k+i, i )
              a( k+i, i ) = cone
              ! compute  y(k+1:n,i)
              call stdlib_zgemv( 'NO TRANSPOSE', n-k, n-k-i+1,cone, a( k+1, i+1 ),lda, a( k+i, i )&
                        , 1_ilp, czero, y( k+1, i ), 1_ilp )
              call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k-i+1, i-1,cone, a( k+i, 1_ilp ), lda,a( k+&
                        i, i ), 1_ilp, czero, t( 1_ilp, i ), 1_ilp )
              call stdlib_zgemv( 'NO TRANSPOSE', n-k, i-1, -cone,y( k+1, 1_ilp ), ldy,t( 1_ilp, i ), 1_ilp, &
                        cone, y( k+1, i ), 1_ilp )
              call stdlib_zscal( n-k, tau( i ), y( k+1, i ), 1_ilp )
              ! compute t(1:i,i)
              call stdlib_zscal( i-1, -tau( i ), t( 1_ilp, i ), 1_ilp )
              call stdlib_ztrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',i-1, t, ldt,t( 1_ilp, i ), 1_ilp )
                        
              t( i, i ) = tau( i )
           end do loop_10
           a( k+nb, nb ) = ei
           ! compute y(1:k,1:nb)
           call stdlib_zlacpy( 'ALL', k, nb, a( 1_ilp, 2_ilp ), lda, y, ldy )
           call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE','UNIT', k, nb,cone, a( k+1, 1_ilp ), &
                     lda, y, ldy )
           if( n>k+nb )call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', k,nb, n-k-nb, cone,a( 1_ilp,&
                      2_ilp+nb ), lda, a( k+1+nb, 1_ilp ), lda, cone, y,ldy )
           call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE','NON-UNIT', k, nb,cone, t, ldt, y, &
                     ldy )
           return
     end subroutine stdlib_zlahr2




     pure module subroutine stdlib_cunghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! CUNGHR generates a complex unitary matrix Q which is defined as the
     !! product of IHI-ILO elementary reflectors of order N, as returned by
     !! CGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lwkopt, nb, nh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, nh ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', nh, nh, nh, -1_ilp )
              lwkopt = max( 1_ilp, nh )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! shift the vectors which define the elementary reflectors cone
           ! column to the right, and set the first ilo and the last n-ihi
           ! rows and columns to those of the unit matrix
           do j = ihi, ilo + 1, -1
              do i = 1, j - 1
                 a( i, j ) = czero
              end do
              do i = j + 1, ihi
                 a( i, j ) = a( i, j-1 )
              end do
              do i = ihi + 1, n
                 a( i, j ) = czero
              end do
           end do
           do j = 1, ilo
              do i = 1, n
                 a( i, j ) = czero
              end do
              a( j, j ) = cone
           end do
           do j = ihi + 1, n
              do i = 1, n
                 a( i, j ) = czero
              end do
              a( j, j ) = cone
           end do
           if( nh>0_ilp ) then
              ! generate q(ilo+1:ihi,ilo+1:ihi)
              call stdlib_cungqr( nh, nh, nh, a( ilo+1, ilo+1 ), lda, tau( ilo ),work, lwork, &
                        iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunghr

     pure module subroutine stdlib_zunghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! ZUNGHR generates a complex unitary matrix Q which is defined as the
     !! product of IHI-ILO elementary reflectors of order N, as returned by
     !! ZGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lwkopt, nb, nh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, nh ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', nh, nh, nh, -1_ilp )
              lwkopt = max( 1_ilp, nh )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! shift the vectors which define the elementary reflectors cone
           ! column to the right, and set the first ilo and the last n-ihi
           ! rows and columns to those of the unit matrix
           do j = ihi, ilo + 1, -1
              do i = 1, j - 1
                 a( i, j ) = czero
              end do
              do i = j + 1, ihi
                 a( i, j ) = a( i, j-1 )
              end do
              do i = ihi + 1, n
                 a( i, j ) = czero
              end do
           end do
           do j = 1, ilo
              do i = 1, n
                 a( i, j ) = czero
              end do
              a( j, j ) = cone
           end do
           do j = ihi + 1, n
              do i = 1, n
                 a( i, j ) = czero
              end do
              a( j, j ) = cone
           end do
           if( nh>0_ilp ) then
              ! generate q(ilo+1:ihi,ilo+1:ihi)
              call stdlib_zungqr( nh, nh, nh, a( ilo+1, ilo+1 ), lda, tau( ilo ),work, lwork, &
                        iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunghr




     pure module subroutine stdlib_cunmhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
     !! CUNMHR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! IHI-ILO elementary reflectors, as returned by CGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, nh, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           left = stdlib_lsame( side, 'L' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, nq ) ) then
              info = -5_ilp
           else if( ihi<min( ilo, nq ) .or. ihi>nq ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( left ) then
                 nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, nh, n, nh, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m, nh, nh, -1_ilp )
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nh==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = nh
              ni = n
              i1 = ilo + 1_ilp
              i2 = 1_ilp
           else
              mi = m
              ni = nh
              i1 = 1_ilp
              i2 = ilo + 1_ilp
           end if
           call stdlib_cunmqr( side, trans, mi, ni, nh, a( ilo+1, ilo ), lda,tau( ilo ), c( i1, &
                     i2 ), ldc, work, lwork, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmhr

     pure module subroutine stdlib_zunmhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
     !! ZUNMHR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! IHI-ILO elementary reflectors, as returned by ZGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, nh, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           left = stdlib_lsame( side, 'L' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, nq ) ) then
              info = -5_ilp
           else if( ihi<min( ilo, nq ) .or. ihi>nq ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( left ) then
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, nh, n, nh, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m, nh, nh, -1_ilp )
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nh==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = nh
              ni = n
              i1 = ilo + 1_ilp
              i2 = 1_ilp
           else
              mi = m
              ni = nh
              i1 = 1_ilp
              i2 = ilo + 1_ilp
           end if
           call stdlib_zunmqr( side, trans, mi, ni, nh, a( ilo+1, ilo ), lda,tau( ilo ), c( i1, &
                     i2 ), ldc, work, lwork, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmhr




     pure module subroutine stdlib_sorghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! SORGHR generates a real orthogonal matrix Q which is defined as the
     !! product of IHI-ILO elementary reflectors of order N, as returned by
     !! SGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lwkopt, nb, nh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, nh ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', nh, nh, nh, -1_ilp )
              lwkopt = max( 1_ilp, nh )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! shift the vectors which define the elementary reflectors one
           ! column to the right, and set the first ilo and the last n-ihi
           ! rows and columns to those of the unit matrix
           do j = ihi, ilo + 1, -1
              do i = 1, j - 1
                 a( i, j ) = zero
              end do
              do i = j + 1, ihi
                 a( i, j ) = a( i, j-1 )
              end do
              do i = ihi + 1, n
                 a( i, j ) = zero
              end do
           end do
           do j = 1, ilo
              do i = 1, n
                 a( i, j ) = zero
              end do
              a( j, j ) = one
           end do
           do j = ihi + 1, n
              do i = 1, n
                 a( i, j ) = zero
              end do
              a( j, j ) = one
           end do
           if( nh>0_ilp ) then
              ! generate q(ilo+1:ihi,ilo+1:ihi)
              call stdlib_sorgqr( nh, nh, nh, a( ilo+1, ilo+1 ), lda, tau( ilo ),work, lwork, &
                        iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sorghr

     pure module subroutine stdlib_dorghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
     !! DORGHR generates a real orthogonal matrix Q which is defined as the
     !! product of IHI-ILO elementary reflectors of order N, as returned by
     !! DGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lwkopt, nb, nh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -2_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, nh ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', nh, nh, nh, -1_ilp )
              lwkopt = max( 1_ilp, nh )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! shift the vectors which define the elementary reflectors one
           ! column to the right, and set the first ilo and the last n-ihi
           ! rows and columns to those of the unit matrix
           do j = ihi, ilo + 1, -1
              do i = 1, j - 1
                 a( i, j ) = zero
              end do
              do i = j + 1, ihi
                 a( i, j ) = a( i, j-1 )
              end do
              do i = ihi + 1, n
                 a( i, j ) = zero
              end do
           end do
           do j = 1, ilo
              do i = 1, n
                 a( i, j ) = zero
              end do
              a( j, j ) = one
           end do
           do j = ihi + 1, n
              do i = 1, n
                 a( i, j ) = zero
              end do
              a( j, j ) = one
           end do
           if( nh>0_ilp ) then
              ! generate q(ilo+1:ihi,ilo+1:ihi)
              call stdlib_dorgqr( nh, nh, nh, a( ilo+1, ilo+1 ), lda, tau( ilo ),work, lwork, &
                        iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dorghr




     pure module subroutine stdlib_sormhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
     !! SORMHR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! IHI-ILO elementary reflectors, as returned by SGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, nh, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           left = stdlib_lsame( side, 'L' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, nq ) ) then
              info = -5_ilp
           else if( ihi<min( ilo, nq ) .or. ihi>nq ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( left ) then
                 nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, nh, n, nh, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m, nh, nh, -1_ilp )
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nh==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = nh
              ni = n
              i1 = ilo + 1_ilp
              i2 = 1_ilp
           else
              mi = m
              ni = nh
              i1 = 1_ilp
              i2 = ilo + 1_ilp
           end if
           call stdlib_sormqr( side, trans, mi, ni, nh, a( ilo+1, ilo ), lda,tau( ilo ), c( i1, &
                     i2 ), ldc, work, lwork, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormhr

     pure module subroutine stdlib_dormhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
     !! DORMHR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! IHI-ILO elementary reflectors, as returned by DGEHRD:
     !! Q = H(ilo) H(ilo+1) . . . H(ihi-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, nh, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nh = ihi - ilo
           left = stdlib_lsame( side, 'L' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, nq ) ) then
              info = -5_ilp
           else if( ihi<min( ilo, nq ) .or. ihi>nq ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( left ) then
                 nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, nh, n, nh, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m, nh, nh, -1_ilp )
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMHR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nh==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = nh
              ni = n
              i1 = ilo + 1_ilp
              i2 = 1_ilp
           else
              mi = m
              ni = nh
              i1 = 1_ilp
              i2 = ilo + 1_ilp
           end if
           call stdlib_dormqr( side, trans, mi, ni, nh, a( ilo+1, ilo ), lda,tau( ilo ), c( i1, &
                     i2 ), ldc, work, lwork, iinfo )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormhr



end submodule stdlib_lapack_eigv_gen
