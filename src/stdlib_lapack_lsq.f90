submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_lsq
  implicit none


  contains

     module subroutine stdlib_sgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, info )
     !! SGELSS computes the minimum norm solution to a real linear least
     !! squares problem:
     !! Minimize 2-norm(| b - A*x |).
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
     !! X.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: s(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: bdspac, bl, chunk, i, iascl, ibscl, ie, il, itau, itaup, itauq, iwork, &
                     ldwork, maxmn, maxwrk, minmn, minwrk, mm, mnthr
           integer(ilp) :: lwork_sgeqrf, lwork_sormqr, lwork_sgebrd, lwork_sormbr, lwork_sorgbr, &
                     lwork_sormlq
           real(sp) :: anrm, bignum, bnrm, eps, sfmin, smlnum, thr
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( minmn>0_ilp ) then
                 mm = m
                 mnthr = stdlib_ilaenv( 6_ilp, 'SGELSS', ' ', m, n, nrhs, -1_ilp )
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns
                    ! compute space needed for stdlib_sgeqrf
                    call stdlib_sgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, info )
                    lwork_sgeqrf=dum(1_ilp)
                    ! compute space needed for stdlib_sormqr
                    call stdlib_sormqr( 'L', 'T', m, nrhs, n, a, lda, dum(1_ilp), b,ldb, dum(1_ilp), -1_ilp, &
                              info )
                    lwork_sormqr=dum(1_ilp)
                    mm = n
                    maxwrk = max( maxwrk, n + lwork_sgeqrf )
                    maxwrk = max( maxwrk, n + lwork_sormqr )
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined
                    ! compute workspace needed for stdlib_sbdsqr
                    bdspac = max( 1_ilp, 5_ilp*n )
                    ! compute space needed for stdlib_sgebrd
                    call stdlib_sgebrd( mm, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, info &
                              )
                    lwork_sgebrd=dum(1_ilp)
                    ! compute space needed for stdlib_sormbr
                    call stdlib_sormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, dum(1_ilp),b, ldb, dum(1_ilp),&
                               -1_ilp, info )
                    lwork_sormbr=dum(1_ilp)
                    ! compute space needed for stdlib_sorgbr
                    call stdlib_sorgbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                    lwork_sorgbr=dum(1_ilp)
                    ! compute total workspace needed
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_sgebrd )
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_sormbr )
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_sorgbr )
                    maxwrk = max( maxwrk, bdspac )
                    maxwrk = max( maxwrk, n*nrhs )
                    minwrk = max( 3_ilp*n + mm, 3_ilp*n + nrhs, bdspac )
                    maxwrk = max( minwrk, maxwrk )
                 end if
                 if( n>m ) then
                    ! compute workspace needed for stdlib_sbdsqr
                    bdspac = max( 1_ilp, 5_ilp*m )
                    minwrk = max( 3_ilp*m+nrhs, 3_ilp*m+n, bdspac )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                       ! than rows
                       ! compute space needed for stdlib_sgebrd
                       call stdlib_sgebrd( m, m, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, &
                                 info )
                       lwork_sgebrd=dum(1_ilp)
                       ! compute space needed for stdlib_sormbr
                       call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_sormbr=dum(1_ilp)
                       ! compute space needed for stdlib_sorgbr
                       call stdlib_sorgbr( 'P', m, m, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_sorgbr=dum(1_ilp)
                       ! compute space needed for stdlib_sormlq
                       call stdlib_sormlq( 'L', 'T', n, nrhs, m, a, lda, dum(1_ilp),b, ldb, dum(1_ilp), -&
                                 1_ilp, info )
                       lwork_sormlq=dum(1_ilp)
                       ! compute total workspace needed
                       maxwrk = m + m*stdlib_ilaenv( 1_ilp, 'SGELQF', ' ', m, n, -1_ilp,-1_ilp )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_sgebrd )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_sormbr )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_sorgbr )
                       maxwrk = max( maxwrk, m*m + m + bdspac )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m + lwork_sormlq )
                    else
                       ! path 2 - underdetermined
                       ! compute space needed for stdlib_sgebrd
                       call stdlib_sgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, &
                                 info )
                       lwork_sgebrd=dum(1_ilp)
                       ! compute space needed for stdlib_sormbr
                       call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, m, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_sormbr=dum(1_ilp)
                       ! compute space needed for stdlib_sorgbr
                       call stdlib_sorgbr( 'P', m, n, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_sorgbr=dum(1_ilp)
                       maxwrk = 3_ilp*m + lwork_sgebrd
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_sormbr )
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_sorgbr )
                       maxwrk = max( maxwrk, bdspac )
                       maxwrk = max( maxwrk, n*nrhs )
                    end if
                 end if
                 maxwrk = max( minwrk, maxwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELSS', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           eps = stdlib_slamch( 'P' )
           sfmin = stdlib_slamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_slaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              call stdlib_slaset( 'F', minmn, 1_ilp, zero, zero, s, minmn )
              rank = 0_ilp
              go to 70
           end if
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_slange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! overdetermined case
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 iwork = itau + n
                 ! compute a=q*r
                 ! (workspace: need 2*n, prefer n+n*nb)
                 call stdlib_sgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                           info )
                 ! multiply b by transpose(q)
                 ! (workspace: need n+nrhs, prefer n+nrhs*nb)
                 call stdlib_sormqr( 'L', 'T', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           iwork ), lwork-iwork+1, info )
                 ! zero out below r
                 if( n>1_ilp )call stdlib_slaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ), lda )
              end if
              ie = 1_ilp
              itauq = ie + n
              itaup = itauq + n
              iwork = itaup + n
              ! bidiagonalize r in a
              ! (workspace: need 3*n+mm, prefer 3*n+(mm+n)*nb)
              call stdlib_sgebrd( mm, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work(&
                         iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r
              ! (workspace: need 3*n+nrhs, prefer 3*n+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in a
              ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
              call stdlib_sorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              iwork = ie + n
              ! perform bidiagonal qr iteration
                ! multiply b by transpose of left singular vectors
                ! compute right singular vectors in a
              ! (workspace: need bdspac)
              call stdlib_sbdsqr( 'U', n, n, 0_ilp, nrhs, s, work( ie ), a, lda, dum,1_ilp, b, ldb, work( &
                        iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, n
                 if( s( i )>thr ) then
                    call stdlib_srscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_slaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors
              ! (workspace: need n, prefer n*nrhs)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_sgemm( 'T', 'N', n, nrhs, n, one, a, lda, b, ldb, zero,work, ldb )
                           
                 call stdlib_slacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_sgemm( 'T', 'N', n, bl, n, one, a, lda, b( 1_ilp, i ),ldb, zero, work,&
                               n )
                    call stdlib_slacpy( 'G', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_sgemv( 'T', n, n, one, a, lda, b, 1_ilp, zero, work, 1_ilp )
                 call stdlib_scopy( n, work, 1_ilp, b, 1_ilp )
              end if
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) ) then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs ) )ldwork = &
                        lda
              itau = 1_ilp
              iwork = m + 1_ilp
              ! compute a=l*q
              ! (workspace: need 2*m, prefer m+m*nb)
              call stdlib_sgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, info )
                        
              il = iwork
              ! copy l to work(il), zeroing out above it
              call stdlib_slacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_slaset( 'U', m-1, m-1, zero, zero, work( il+ldwork ),ldwork )
              ie = il + ldwork*m
              itauq = ie + m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize l in work(il)
              ! (workspace: need m*m+5*m, prefer m*m+4*m+2*m*nb)
              call stdlib_sgebrd( m, m, work( il ), ldwork, s, work( ie ),work( itauq ), work( &
                        itaup ), work( iwork ),lwork-iwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l
              ! (workspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( iwork ),lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in work(il)
              ! (workspace: need m*m+5*m-1, prefer m*m+4*m+(m-1)*nb)
              call stdlib_sorgbr( 'P', m, m, m, work( il ), ldwork, work( itaup ),work( iwork ), &
                        lwork-iwork+1, info )
              iwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of l in work(il) and
                 ! multiplying b by transpose of left singular vectors
              ! (workspace: need m*m+m+bdspac)
              call stdlib_sbdsqr( 'U', m, m, 0_ilp, nrhs, s, work( ie ), work( il ),ldwork, a, lda, b,&
                         ldb, work( iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_srscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_slaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              iwork = ie
              ! multiply b by right singular vectors of l in work(il)
              ! (workspace: need m*m+2*m, prefer m*m+m+m*nrhs)
              if( lwork>=ldb*nrhs+iwork-1 .and. nrhs>1_ilp ) then
                 call stdlib_sgemm( 'T', 'N', m, nrhs, m, one, work( il ), ldwork,b, ldb, zero, &
                           work( iwork ), ldb )
                 call stdlib_slacpy( 'G', m, nrhs, work( iwork ), ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = ( lwork-iwork+1 ) / m
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_sgemm( 'T', 'N', m, bl, m, one, work( il ), ldwork,b( 1_ilp, i ), ldb,&
                               zero, work( iwork ), m )
                    call stdlib_slacpy( 'G', m, bl, work( iwork ), m, b( 1_ilp, i ),ldb )
                 end do
              else
                 call stdlib_sgemv( 'T', m, m, one, work( il ), ldwork, b( 1_ilp, 1_ilp ),1_ilp, zero, work( &
                           iwork ), 1_ilp )
                 call stdlib_scopy( m, work( iwork ), 1_ilp, b( 1_ilp, 1_ilp ), 1_ilp )
              end if
              ! zero out below first m rows of b
              call stdlib_slaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
              iwork = itau + m
              ! multiply transpose(q) by b
              ! (workspace: need m+nrhs, prefer m+nrhs*nb)
              call stdlib_sormlq( 'L', 'T', n, nrhs, m, a, lda, work( itau ), b,ldb, work( iwork )&
                        , lwork-iwork+1, info )
           else
              ! path 2 - remaining underdetermined cases
              ie = 1_ilp
              itauq = ie + m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize a
              ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
              call stdlib_sgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work( &
                        iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors
              ! (workspace: need 3*m+nrhs, prefer 3*m+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors in a
              ! (workspace: need 4*m, prefer 3*m+m*nb)
              call stdlib_sorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              iwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of a in a and
                 ! multiplying b by transpose of left singular vectors
              ! (workspace: need bdspac)
              call stdlib_sbdsqr( 'L', m, n, 0_ilp, nrhs, s, work( ie ), a, lda, dum,1_ilp, b, ldb, work( &
                        iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_srscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_slaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors of a
              ! (workspace: need n, prefer n*nrhs)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_sgemm( 'T', 'N', n, nrhs, m, one, a, lda, b, ldb, zero,work, ldb )
                           
                 call stdlib_slacpy( 'F', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_sgemm( 'T', 'N', n, bl, m, one, a, lda, b( 1_ilp, i ),ldb, zero, work,&
                               n )
                    call stdlib_slacpy( 'F', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_sgemv( 'T', m, n, one, a, lda, b, 1_ilp, zero, work, 1_ilp )
                 call stdlib_scopy( n, work, 1_ilp, b, 1_ilp )
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgelss

     module subroutine stdlib_dgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, info )
     !! DGELSS computes the minimum norm solution to a real linear least
     !! squares problem:
     !! Minimize 2-norm(| b - A*x |).
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
     !! X.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: s(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: bdspac, bl, chunk, i, iascl, ibscl, ie, il, itau, itaup, itauq, iwork, &
                     ldwork, maxmn, maxwrk, minmn, minwrk, mm, mnthr
           integer(ilp) :: lwork_dgeqrf, lwork_dormqr, lwork_dgebrd, lwork_dormbr, lwork_dorgbr, &
                     lwork_dormlq, lwork_dgelqf
           real(dp) :: anrm, bignum, bnrm, eps, sfmin, smlnum, thr
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( minmn>0_ilp ) then
                 mm = m
                 mnthr = stdlib_ilaenv( 6_ilp, 'DGELSS', ' ', m, n, nrhs, -1_ilp )
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns
                    ! compute space needed for stdlib_dgeqrf
                    call stdlib_dgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, info )
                    lwork_dgeqrf=dum(1_ilp)
                    ! compute space needed for stdlib_dormqr
                    call stdlib_dormqr( 'L', 'T', m, nrhs, n, a, lda, dum(1_ilp), b,ldb, dum(1_ilp), -1_ilp, &
                              info )
                    lwork_dormqr=dum(1_ilp)
                    mm = n
                    maxwrk = max( maxwrk, n + lwork_dgeqrf )
                    maxwrk = max( maxwrk, n + lwork_dormqr )
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined
                    ! compute workspace needed for stdlib_dbdsqr
                    bdspac = max( 1_ilp, 5_ilp*n )
                    ! compute space needed for stdlib_dgebrd
                    call stdlib_dgebrd( mm, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, info &
                              )
                    lwork_dgebrd=dum(1_ilp)
                    ! compute space needed for stdlib_dormbr
                    call stdlib_dormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, dum(1_ilp),b, ldb, dum(1_ilp),&
                               -1_ilp, info )
                    lwork_dormbr=dum(1_ilp)
                    ! compute space needed for stdlib_dorgbr
                    call stdlib_dorgbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                    lwork_dorgbr=dum(1_ilp)
                    ! compute total workspace needed
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_dgebrd )
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_dormbr )
                    maxwrk = max( maxwrk, 3_ilp*n + lwork_dorgbr )
                    maxwrk = max( maxwrk, bdspac )
                    maxwrk = max( maxwrk, n*nrhs )
                    minwrk = max( 3_ilp*n + mm, 3_ilp*n + nrhs, bdspac )
                    maxwrk = max( minwrk, maxwrk )
                 end if
                 if( n>m ) then
                    ! compute workspace needed for stdlib_dbdsqr
                    bdspac = max( 1_ilp, 5_ilp*m )
                    minwrk = max( 3_ilp*m+nrhs, 3_ilp*m+n, bdspac )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                       ! than rows
                       ! compute space needed for stdlib_dgelqf
                       call stdlib_dgelqf( m, n, a, lda, dum(1_ilp), dum(1_ilp),-1_ilp, info )
                       lwork_dgelqf=dum(1_ilp)
                       ! compute space needed for stdlib_dgebrd
                       call stdlib_dgebrd( m, m, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, &
                                 info )
                       lwork_dgebrd=dum(1_ilp)
                       ! compute space needed for stdlib_dormbr
                       call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_dormbr=dum(1_ilp)
                       ! compute space needed for stdlib_dorgbr
                       call stdlib_dorgbr( 'P', m, m, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_dorgbr=dum(1_ilp)
                       ! compute space needed for stdlib_dormlq
                       call stdlib_dormlq( 'L', 'T', n, nrhs, m, a, lda, dum(1_ilp),b, ldb, dum(1_ilp), -&
                                 1_ilp, info )
                       lwork_dormlq=dum(1_ilp)
                       ! compute total workspace needed
                       maxwrk = m + lwork_dgelqf
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_dgebrd )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_dormbr )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + lwork_dorgbr )
                       maxwrk = max( maxwrk, m*m + m + bdspac )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m + lwork_dormlq )
                    else
                       ! path 2 - underdetermined
                       ! compute space needed for stdlib_dgebrd
                       call stdlib_dgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, &
                                 info )
                       lwork_dgebrd=dum(1_ilp)
                       ! compute space needed for stdlib_dormbr
                       call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, m, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_dormbr=dum(1_ilp)
                       ! compute space needed for stdlib_dorgbr
                       call stdlib_dorgbr( 'P', m, n, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_dorgbr=dum(1_ilp)
                       maxwrk = 3_ilp*m + lwork_dgebrd
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_dormbr )
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_dorgbr )
                       maxwrk = max( maxwrk, bdspac )
                       maxwrk = max( maxwrk, n*nrhs )
                    end if
                 end if
                 maxwrk = max( minwrk, maxwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELSS', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           eps = stdlib_dlamch( 'P' )
           sfmin = stdlib_dlamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_dlaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              call stdlib_dlaset( 'F', minmn, 1_ilp, zero, zero, s, minmn )
              rank = 0_ilp
              go to 70
           end if
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_dlange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! overdetermined case
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 iwork = itau + n
                 ! compute a=q*r
                 ! (workspace: need 2*n, prefer n+n*nb)
                 call stdlib_dgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                           info )
                 ! multiply b by transpose(q)
                 ! (workspace: need n+nrhs, prefer n+nrhs*nb)
                 call stdlib_dormqr( 'L', 'T', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           iwork ), lwork-iwork+1, info )
                 ! zero out below r
                 if( n>1_ilp )call stdlib_dlaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ), lda )
              end if
              ie = 1_ilp
              itauq = ie + n
              itaup = itauq + n
              iwork = itaup + n
              ! bidiagonalize r in a
              ! (workspace: need 3*n+mm, prefer 3*n+(mm+n)*nb)
              call stdlib_dgebrd( mm, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work(&
                         iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r
              ! (workspace: need 3*n+nrhs, prefer 3*n+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in a
              ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
              call stdlib_dorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              iwork = ie + n
              ! perform bidiagonal qr iteration
                ! multiply b by transpose of left singular vectors
                ! compute right singular vectors in a
              ! (workspace: need bdspac)
              call stdlib_dbdsqr( 'U', n, n, 0_ilp, nrhs, s, work( ie ), a, lda, dum,1_ilp, b, ldb, work( &
                        iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, n
                 if( s( i )>thr ) then
                    call stdlib_drscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_dlaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors
              ! (workspace: need n, prefer n*nrhs)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_dgemm( 'T', 'N', n, nrhs, n, one, a, lda, b, ldb, zero,work, ldb )
                           
                 call stdlib_dlacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_dgemm( 'T', 'N', n, bl, n, one, a, lda, b( 1_ilp, i ),ldb, zero, work,&
                               n )
                    call stdlib_dlacpy( 'G', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_dgemv( 'T', n, n, one, a, lda, b, 1_ilp, zero, work, 1_ilp )
                 call stdlib_dcopy( n, work, 1_ilp, b, 1_ilp )
              end if
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) ) then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs ) )ldwork = &
                        lda
              itau = 1_ilp
              iwork = m + 1_ilp
              ! compute a=l*q
              ! (workspace: need 2*m, prefer m+m*nb)
              call stdlib_dgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, info )
                        
              il = iwork
              ! copy l to work(il), zeroing out above it
              call stdlib_dlacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_dlaset( 'U', m-1, m-1, zero, zero, work( il+ldwork ),ldwork )
              ie = il + ldwork*m
              itauq = ie + m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize l in work(il)
              ! (workspace: need m*m+5*m, prefer m*m+4*m+2*m*nb)
              call stdlib_dgebrd( m, m, work( il ), ldwork, s, work( ie ),work( itauq ), work( &
                        itaup ), work( iwork ),lwork-iwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l
              ! (workspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( iwork ),lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in work(il)
              ! (workspace: need m*m+5*m-1, prefer m*m+4*m+(m-1)*nb)
              call stdlib_dorgbr( 'P', m, m, m, work( il ), ldwork, work( itaup ),work( iwork ), &
                        lwork-iwork+1, info )
              iwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of l in work(il) and
                 ! multiplying b by transpose of left singular vectors
              ! (workspace: need m*m+m+bdspac)
              call stdlib_dbdsqr( 'U', m, m, 0_ilp, nrhs, s, work( ie ), work( il ),ldwork, a, lda, b,&
                         ldb, work( iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_drscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_dlaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              iwork = ie
              ! multiply b by right singular vectors of l in work(il)
              ! (workspace: need m*m+2*m, prefer m*m+m+m*nrhs)
              if( lwork>=ldb*nrhs+iwork-1 .and. nrhs>1_ilp ) then
                 call stdlib_dgemm( 'T', 'N', m, nrhs, m, one, work( il ), ldwork,b, ldb, zero, &
                           work( iwork ), ldb )
                 call stdlib_dlacpy( 'G', m, nrhs, work( iwork ), ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = ( lwork-iwork+1 ) / m
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_dgemm( 'T', 'N', m, bl, m, one, work( il ), ldwork,b( 1_ilp, i ), ldb,&
                               zero, work( iwork ), m )
                    call stdlib_dlacpy( 'G', m, bl, work( iwork ), m, b( 1_ilp, i ),ldb )
                 end do
              else
                 call stdlib_dgemv( 'T', m, m, one, work( il ), ldwork, b( 1_ilp, 1_ilp ),1_ilp, zero, work( &
                           iwork ), 1_ilp )
                 call stdlib_dcopy( m, work( iwork ), 1_ilp, b( 1_ilp, 1_ilp ), 1_ilp )
              end if
              ! zero out below first m rows of b
              call stdlib_dlaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
              iwork = itau + m
              ! multiply transpose(q) by b
              ! (workspace: need m+nrhs, prefer m+nrhs*nb)
              call stdlib_dormlq( 'L', 'T', n, nrhs, m, a, lda, work( itau ), b,ldb, work( iwork )&
                        , lwork-iwork+1, info )
           else
              ! path 2 - remaining underdetermined cases
              ie = 1_ilp
              itauq = ie + m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize a
              ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
              call stdlib_dgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work( &
                        iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors
              ! (workspace: need 3*m+nrhs, prefer 3*m+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors in a
              ! (workspace: need 4*m, prefer 3*m+m*nb)
              call stdlib_dorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              iwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of a in a and
                 ! multiplying b by transpose of left singular vectors
              ! (workspace: need bdspac)
              call stdlib_dbdsqr( 'L', m, n, 0_ilp, nrhs, s, work( ie ), a, lda, dum,1_ilp, b, ldb, work( &
                        iwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_drscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_dlaset( 'F', 1_ilp, nrhs, zero, zero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors of a
              ! (workspace: need n, prefer n*nrhs)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_dgemm( 'T', 'N', n, nrhs, m, one, a, lda, b, ldb, zero,work, ldb )
                           
                 call stdlib_dlacpy( 'F', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_dgemm( 'T', 'N', n, bl, m, one, a, lda, b( 1_ilp, i ),ldb, zero, work,&
                               n )
                    call stdlib_dlacpy( 'F', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_dgemv( 'T', m, n, one, a, lda, b, 1_ilp, zero, work, 1_ilp )
                 call stdlib_dcopy( n, work, 1_ilp, b, 1_ilp )
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgelss


     module subroutine stdlib_cgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
     !! CGELSS computes the minimum norm solution to a complex linear
     !! least squares problem:
     !! Minimize 2-norm(| b - A*x |).
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
     !! X.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: bl, chunk, i, iascl, ibscl, ie, il, irwork, itau, itaup, itauq, iwork, &
                     ldwork, maxmn, maxwrk, minmn, minwrk, mm, mnthr
           integer(ilp) :: lwork_cgeqrf, lwork_cunmqr, lwork_cgebrd, lwork_cunmbr, lwork_cungbr, &
                     lwork_cunmlq, lwork_cgelqf
           real(sp) :: anrm, bignum, bnrm, eps, sfmin, smlnum, thr
           ! Local Arrays 
           complex(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace refers
             ! to real workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( minmn>0_ilp ) then
                 mm = m
                 mnthr = stdlib_ilaenv( 6_ilp, 'CGELSS', ' ', m, n, nrhs, -1_ilp )
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns
                    ! compute space needed for stdlib_cgeqrf
                    call stdlib_cgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, info )
                    lwork_cgeqrf = real( dum(1_ilp),KIND=sp)
                    ! compute space needed for stdlib_cunmqr
                    call stdlib_cunmqr( 'L', 'C', m, nrhs, n, a, lda, dum(1_ilp), b,ldb, dum(1_ilp), -1_ilp, &
                              info )
                    lwork_cunmqr = real( dum(1_ilp),KIND=sp)
                    mm = n
                    maxwrk = max( maxwrk, n + n*stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m,n, -1_ilp, -1_ilp ) )
                              
                    maxwrk = max( maxwrk, n + nrhs*stdlib_ilaenv( 1_ilp, 'CUNMQR', 'LC',m, nrhs, n, -&
                              1_ilp ) )
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined
                    ! compute space needed for stdlib_cgebrd
                    call stdlib_cgebrd( mm, n, a, lda, s, s, dum(1_ilp), dum(1_ilp), dum(1_ilp),-1_ilp, info )
                              
                    lwork_cgebrd = real( dum(1_ilp),KIND=sp)
                    ! compute space needed for stdlib_cunmbr
                    call stdlib_cunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, dum(1_ilp),b, ldb, dum(1_ilp),&
                               -1_ilp, info )
                    lwork_cunmbr = real( dum(1_ilp),KIND=sp)
                    ! compute space needed for stdlib_cungbr
                    call stdlib_cungbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                    lwork_cungbr = real( dum(1_ilp),KIND=sp)
                    ! compute total workspace needed
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_cgebrd )
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_cunmbr )
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_cungbr )
                    maxwrk = max( maxwrk, n*nrhs )
                    minwrk = 2_ilp*n + max( nrhs, m )
                 end if
                 if( n>m ) then
                    minwrk = 2_ilp*m + max( nrhs, n )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                       ! than rows
                       ! compute space needed for stdlib_cgelqf
                       call stdlib_cgelqf( m, n, a, lda, dum(1_ilp), dum(1_ilp),-1_ilp, info )
                       lwork_cgelqf = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cgebrd
                       call stdlib_cgebrd( m, m, a, lda, s, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), -1_ilp, info )
                                 
                       lwork_cgebrd = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cunmbr
                       call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_cunmbr = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cungbr
                       call stdlib_cungbr( 'P', m, m, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_cungbr = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cunmlq
                       call stdlib_cunmlq( 'L', 'C', n, nrhs, m, a, lda, dum(1_ilp),b, ldb, dum(1_ilp), -&
                                 1_ilp, info )
                       lwork_cunmlq = real( dum(1_ilp),KIND=sp)
                       ! compute total workspace needed
                       maxwrk = m + lwork_cgelqf
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_cgebrd )
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_cunmbr )
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_cungbr )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m + lwork_cunmlq )
                    else
                       ! path 2 - underdetermined
                       ! compute space needed for stdlib_cgebrd
                       call stdlib_cgebrd( m, n, a, lda, s, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), -1_ilp, info )
                                 
                       lwork_cgebrd = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cunmbr
                       call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, m, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_cunmbr = real( dum(1_ilp),KIND=sp)
                       ! compute space needed for stdlib_cungbr
                       call stdlib_cungbr( 'P', m, n, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_cungbr = real( dum(1_ilp),KIND=sp)
                       maxwrk = 2_ilp*m + lwork_cgebrd
                       maxwrk = max( maxwrk, 2_ilp*m + lwork_cunmbr )
                       maxwrk = max( maxwrk, 2_ilp*m + lwork_cungbr )
                       maxwrk = max( maxwrk, n*nrhs )
                    end if
                 end if
                 maxwrk = max( minwrk, maxwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELSS', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           eps = stdlib_slamch( 'P' )
           sfmin = stdlib_slamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_claset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              call stdlib_slaset( 'F', minmn, 1_ilp, zero, zero, s, minmn )
              rank = 0_ilp
              go to 70
           end if
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_clange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! overdetermined case
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 iwork = itau + n
                 ! compute a=q*r
                 ! (cworkspace: need 2*n, prefer n+n*nb)
                 ! (rworkspace: none)
                 call stdlib_cgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                           info )
                 ! multiply b by transpose(q)
                 ! (cworkspace: need n+nrhs, prefer n+nrhs*nb)
                 ! (rworkspace: none)
                 call stdlib_cunmqr( 'L', 'C', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           iwork ), lwork-iwork+1, info )
                 ! zero out below r
                 if( n>1_ilp )call stdlib_claset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
              end if
              ie = 1_ilp
              itauq = 1_ilp
              itaup = itauq + n
              iwork = itaup + n
              ! bidiagonalize r in a
              ! (cworkspace: need 2*n+mm, prefer 2*n+(mm+n)*nb)
              ! (rworkspace: need n)
              call stdlib_cgebrd( mm, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                        work( iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r
              ! (cworkspace: need 2*n+nrhs, prefer 2*n+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_cunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in a
              ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_cungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              irwork = ie + n
              ! perform bidiagonal qr iteration
                ! multiply b by transpose of left singular vectors
                ! compute right singular vectors in a
              ! (cworkspace: none)
              ! (rworkspace: need bdspac)
              call stdlib_cbdsqr( 'U', n, n, 0_ilp, nrhs, s, rwork( ie ), a, lda, dum,1_ilp, b, ldb, &
                        rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, n
                 if( s( i )>thr ) then
                    call stdlib_csrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_claset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors
              ! (cworkspace: need n, prefer n*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_cgemm( 'C', 'N', n, nrhs, n, cone, a, lda, b, ldb,czero, work, ldb )
                           
                 call stdlib_clacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_cgemm( 'C', 'N', n, bl, n, cone, a, lda, b( 1_ilp, i ),ldb, czero, &
                              work, n )
                    call stdlib_clacpy( 'G', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_cgemv( 'C', n, n, cone, a, lda, b, 1_ilp, czero, work, 1_ilp )
                 call stdlib_ccopy( n, work, 1_ilp, b, 1_ilp )
              end if
           else if( n>=mnthr .and. lwork>=3_ilp*m+m*m+max( m, nrhs, n-2*m ) )then
              ! underdetermined case, m much less than n
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm
              ldwork = m
              if( lwork>=3_ilp*m+m*lda+max( m, nrhs, n-2*m ) )ldwork = lda
              itau = 1_ilp
              iwork = m + 1_ilp
              ! compute a=l*q
              ! (cworkspace: need 2*m, prefer m+m*nb)
              ! (rworkspace: none)
              call stdlib_cgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, info )
                        
              il = iwork
              ! copy l to work(il), zeroing out above it
              call stdlib_clacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_claset( 'U', m-1, m-1, czero, czero, work( il+ldwork ),ldwork )
              ie = 1_ilp
              itauq = il + ldwork*m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize l in work(il)
              ! (cworkspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
              ! (rworkspace: need m)
              call stdlib_cgebrd( m, m, work( il ), ldwork, s, rwork( ie ),work( itauq ), work( &
                        itaup ), work( iwork ),lwork-iwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l
              ! (cworkspace: need m*m+3*m+nrhs, prefer m*m+3*m+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( iwork ),lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in work(il)
              ! (cworkspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
              ! (rworkspace: none)
              call stdlib_cungbr( 'P', m, m, m, work( il ), ldwork, work( itaup ),work( iwork ), &
                        lwork-iwork+1, info )
              irwork = ie + m
              ! perform bidiagonal qr iteration, computing right singular
              ! vectors of l in work(il) and multiplying b by transpose of
              ! left singular vectors
              ! (cworkspace: need m*m)
              ! (rworkspace: need bdspac)
              call stdlib_cbdsqr( 'U', m, m, 0_ilp, nrhs, s, rwork( ie ), work( il ),ldwork, a, lda, &
                        b, ldb, rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_csrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_claset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              iwork = il + m*ldwork
              ! multiply b by right singular vectors of l in work(il)
              ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs+iwork-1 .and. nrhs>1_ilp ) then
                 call stdlib_cgemm( 'C', 'N', m, nrhs, m, cone, work( il ), ldwork,b, ldb, czero, &
                           work( iwork ), ldb )
                 call stdlib_clacpy( 'G', m, nrhs, work( iwork ), ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = ( lwork-iwork+1 ) / m
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_cgemm( 'C', 'N', m, bl, m, cone, work( il ), ldwork,b( 1_ilp, i ), &
                              ldb, czero, work( iwork ), m )
                    call stdlib_clacpy( 'G', m, bl, work( iwork ), m, b( 1_ilp, i ),ldb )
                 end do
              else
                 call stdlib_cgemv( 'C', m, m, cone, work( il ), ldwork, b( 1_ilp, 1_ilp ),1_ilp, czero, work(&
                            iwork ), 1_ilp )
                 call stdlib_ccopy( m, work( iwork ), 1_ilp, b( 1_ilp, 1_ilp ), 1_ilp )
              end if
              ! zero out below first m rows of b
              call stdlib_claset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
              iwork = itau + m
              ! multiply transpose(q) by b
              ! (cworkspace: need m+nrhs, prefer m+nhrs*nb)
              ! (rworkspace: none)
              call stdlib_cunmlq( 'L', 'C', n, nrhs, m, a, lda, work( itau ), b,ldb, work( iwork )&
                        , lwork-iwork+1, info )
           else
              ! path 2 - remaining underdetermined cases
              ie = 1_ilp
              itauq = 1_ilp
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize a
              ! (cworkspace: need 3*m, prefer 2*m+(m+n)*nb)
              ! (rworkspace: need n)
              call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), work(&
                         iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors
              ! (cworkspace: need 2*m+nrhs, prefer 2*m+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors in a
              ! (cworkspace: need 3*m, prefer 2*m+m*nb)
              ! (rworkspace: none)
              call stdlib_cungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              irwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of a in a and
                 ! multiplying b by transpose of left singular vectors
              ! (cworkspace: none)
              ! (rworkspace: need bdspac)
              call stdlib_cbdsqr( 'L', m, n, 0_ilp, nrhs, s, rwork( ie ), a, lda, dum,1_ilp, b, ldb, &
                        rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_csrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_claset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors of a
              ! (cworkspace: need n, prefer n*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_cgemm( 'C', 'N', n, nrhs, m, cone, a, lda, b, ldb,czero, work, ldb )
                           
                 call stdlib_clacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_cgemm( 'C', 'N', n, bl, m, cone, a, lda, b( 1_ilp, i ),ldb, czero, &
                              work, n )
                    call stdlib_clacpy( 'F', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_cgemv( 'C', m, n, cone, a, lda, b, 1_ilp, czero, work, 1_ilp )
                 call stdlib_ccopy( n, work, 1_ilp, b, 1_ilp )
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgelss

     module subroutine stdlib_zgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
     !! ZGELSS computes the minimum norm solution to a complex linear
     !! least squares problem:
     !! Minimize 2-norm(| b - A*x |).
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
     !! X.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: bl, chunk, i, iascl, ibscl, ie, il, irwork, itau, itaup, itauq, iwork, &
                     ldwork, maxmn, maxwrk, minmn, minwrk, mm, mnthr
           integer(ilp) :: lwork_zgeqrf, lwork_zunmqr, lwork_zgebrd, lwork_zunmbr, lwork_zungbr, &
                     lwork_zunmlq, lwork_zgelqf
           real(dp) :: anrm, bignum, bnrm, eps, sfmin, smlnum, thr
           ! Local Arrays 
           complex(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace refers
             ! to real workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( minmn>0_ilp ) then
                 mm = m
                 mnthr = stdlib_ilaenv( 6_ilp, 'ZGELSS', ' ', m, n, nrhs, -1_ilp )
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns
                    ! compute space needed for stdlib_zgeqrf
                    call stdlib_zgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, info )
                    lwork_zgeqrf = real( dum(1_ilp),KIND=dp)
                    ! compute space needed for stdlib_zunmqr
                    call stdlib_zunmqr( 'L', 'C', m, nrhs, n, a, lda, dum(1_ilp), b,ldb, dum(1_ilp), -1_ilp, &
                              info )
                    lwork_zunmqr = real( dum(1_ilp),KIND=dp)
                    mm = n
                    maxwrk = max( maxwrk, n + n*stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m,n, -1_ilp, -1_ilp ) )
                              
                    maxwrk = max( maxwrk, n + nrhs*stdlib_ilaenv( 1_ilp, 'ZUNMQR', 'LC',m, nrhs, n, -&
                              1_ilp ) )
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined
                    ! compute space needed for stdlib_zgebrd
                    call stdlib_zgebrd( mm, n, a, lda, s, s, dum(1_ilp), dum(1_ilp), dum(1_ilp),-1_ilp, info )
                              
                    lwork_zgebrd = real( dum(1_ilp),KIND=dp)
                    ! compute space needed for stdlib_zunmbr
                    call stdlib_zunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, dum(1_ilp),b, ldb, dum(1_ilp),&
                               -1_ilp, info )
                    lwork_zunmbr = real( dum(1_ilp),KIND=dp)
                    ! compute space needed for stdlib_zungbr
                    call stdlib_zungbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                    lwork_zungbr = real( dum(1_ilp),KIND=dp)
                    ! compute total workspace needed
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_zgebrd )
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_zunmbr )
                    maxwrk = max( maxwrk, 2_ilp*n + lwork_zungbr )
                    maxwrk = max( maxwrk, n*nrhs )
                    minwrk = 2_ilp*n + max( nrhs, m )
                 end if
                 if( n>m ) then
                    minwrk = 2_ilp*m + max( nrhs, n )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                       ! than rows
                       ! compute space needed for stdlib_zgelqf
                       call stdlib_zgelqf( m, n, a, lda, dum(1_ilp), dum(1_ilp),-1_ilp, info )
                       lwork_zgelqf = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zgebrd
                       call stdlib_zgebrd( m, m, a, lda, s, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), -1_ilp, info )
                                 
                       lwork_zgebrd = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zunmbr
                       call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_zunmbr = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zungbr
                       call stdlib_zungbr( 'P', m, m, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_zungbr = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zunmlq
                       call stdlib_zunmlq( 'L', 'C', n, nrhs, m, a, lda, dum(1_ilp),b, ldb, dum(1_ilp), -&
                                 1_ilp, info )
                       lwork_zunmlq = real( dum(1_ilp),KIND=dp)
                       ! compute total workspace needed
                       maxwrk = m + lwork_zgelqf
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_zgebrd )
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_zunmbr )
                       maxwrk = max( maxwrk, 3_ilp*m + m*m + lwork_zungbr )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m + lwork_zunmlq )
                    else
                       ! path 2 - underdetermined
                       ! compute space needed for stdlib_zgebrd
                       call stdlib_zgebrd( m, n, a, lda, s, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), -1_ilp, info )
                                 
                       lwork_zgebrd = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zunmbr
                       call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, m, a, lda,dum(1_ilp), b, ldb, dum(&
                                 1_ilp), -1_ilp, info )
                       lwork_zunmbr = real( dum(1_ilp),KIND=dp)
                       ! compute space needed for stdlib_zungbr
                       call stdlib_zungbr( 'P', m, n, m, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, info )
                       lwork_zungbr = real( dum(1_ilp),KIND=dp)
                       maxwrk = 2_ilp*m + lwork_zgebrd
                       maxwrk = max( maxwrk, 2_ilp*m + lwork_zunmbr )
                       maxwrk = max( maxwrk, 2_ilp*m + lwork_zungbr )
                       maxwrk = max( maxwrk, n*nrhs )
                    end if
                 end if
                 maxwrk = max( minwrk, maxwrk )
              end if
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery )info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELSS', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           eps = stdlib_dlamch( 'P' )
           sfmin = stdlib_dlamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_zlaset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              call stdlib_dlaset( 'F', minmn, 1_ilp, zero, zero, s, minmn )
              rank = 0_ilp
              go to 70
           end if
           ! scale b if max element outside range [smlnum,bignum]
           bnrm = stdlib_zlange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! overdetermined case
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 iwork = itau + n
                 ! compute a=q*r
                 ! (cworkspace: need 2*n, prefer n+n*nb)
                 ! (rworkspace: none)
                 call stdlib_zgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                           info )
                 ! multiply b by transpose(q)
                 ! (cworkspace: need n+nrhs, prefer n+nrhs*nb)
                 ! (rworkspace: none)
                 call stdlib_zunmqr( 'L', 'C', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           iwork ), lwork-iwork+1, info )
                 ! zero out below r
                 if( n>1_ilp )call stdlib_zlaset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
              end if
              ie = 1_ilp
              itauq = 1_ilp
              itaup = itauq + n
              iwork = itaup + n
              ! bidiagonalize r in a
              ! (cworkspace: need 2*n+mm, prefer 2*n+(mm+n)*nb)
              ! (rworkspace: need n)
              call stdlib_zgebrd( mm, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                        work( iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r
              ! (cworkspace: need 2*n+nrhs, prefer 2*n+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_zunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in a
              ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
              ! (rworkspace: none)
              call stdlib_zungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              irwork = ie + n
              ! perform bidiagonal qr iteration
                ! multiply b by transpose of left singular vectors
                ! compute right singular vectors in a
              ! (cworkspace: none)
              ! (rworkspace: need bdspac)
              call stdlib_zbdsqr( 'U', n, n, 0_ilp, nrhs, s, rwork( ie ), a, lda, dum,1_ilp, b, ldb, &
                        rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, n
                 if( s( i )>thr ) then
                    call stdlib_zdrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_zlaset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors
              ! (cworkspace: need n, prefer n*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_zgemm( 'C', 'N', n, nrhs, n, cone, a, lda, b, ldb,czero, work, ldb )
                           
                 call stdlib_zlacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_zgemm( 'C', 'N', n, bl, n, cone, a, lda, b( 1_ilp, i ),ldb, czero, &
                              work, n )
                    call stdlib_zlacpy( 'G', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_zgemv( 'C', n, n, cone, a, lda, b, 1_ilp, czero, work, 1_ilp )
                 call stdlib_zcopy( n, work, 1_ilp, b, 1_ilp )
              end if
           else if( n>=mnthr .and. lwork>=3_ilp*m+m*m+max( m, nrhs, n-2*m ) )then
              ! underdetermined case, m much less than n
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm
              ldwork = m
              if( lwork>=3_ilp*m+m*lda+max( m, nrhs, n-2*m ) )ldwork = lda
              itau = 1_ilp
              iwork = m + 1_ilp
              ! compute a=l*q
              ! (cworkspace: need 2*m, prefer m+m*nb)
              ! (rworkspace: none)
              call stdlib_zgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, info )
                        
              il = iwork
              ! copy l to work(il), zeroing out above it
              call stdlib_zlacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_zlaset( 'U', m-1, m-1, czero, czero, work( il+ldwork ),ldwork )
              ie = 1_ilp
              itauq = il + ldwork*m
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize l in work(il)
              ! (cworkspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
              ! (rworkspace: need m)
              call stdlib_zgebrd( m, m, work( il ), ldwork, s, rwork( ie ),work( itauq ), work( &
                        itaup ), work( iwork ),lwork-iwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l
              ! (cworkspace: need m*m+3*m+nrhs, prefer m*m+3*m+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( iwork ),lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors of r in work(il)
              ! (cworkspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
              ! (rworkspace: none)
              call stdlib_zungbr( 'P', m, m, m, work( il ), ldwork, work( itaup ),work( iwork ), &
                        lwork-iwork+1, info )
              irwork = ie + m
              ! perform bidiagonal qr iteration, computing right singular
              ! vectors of l in work(il) and multiplying b by transpose of
              ! left singular vectors
              ! (cworkspace: need m*m)
              ! (rworkspace: need bdspac)
              call stdlib_zbdsqr( 'U', m, m, 0_ilp, nrhs, s, rwork( ie ), work( il ),ldwork, a, lda, &
                        b, ldb, rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_zdrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_zlaset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              iwork = il + m*ldwork
              ! multiply b by right singular vectors of l in work(il)
              ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs+iwork-1 .and. nrhs>1_ilp ) then
                 call stdlib_zgemm( 'C', 'N', m, nrhs, m, cone, work( il ), ldwork,b, ldb, czero, &
                           work( iwork ), ldb )
                 call stdlib_zlacpy( 'G', m, nrhs, work( iwork ), ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = ( lwork-iwork+1 ) / m
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_zgemm( 'C', 'N', m, bl, m, cone, work( il ), ldwork,b( 1_ilp, i ), &
                              ldb, czero, work( iwork ), m )
                    call stdlib_zlacpy( 'G', m, bl, work( iwork ), m, b( 1_ilp, i ),ldb )
                 end do
              else
                 call stdlib_zgemv( 'C', m, m, cone, work( il ), ldwork, b( 1_ilp, 1_ilp ),1_ilp, czero, work(&
                            iwork ), 1_ilp )
                 call stdlib_zcopy( m, work( iwork ), 1_ilp, b( 1_ilp, 1_ilp ), 1_ilp )
              end if
              ! zero out below first m rows of b
              call stdlib_zlaset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
              iwork = itau + m
              ! multiply transpose(q) by b
              ! (cworkspace: need m+nrhs, prefer m+nhrs*nb)
              ! (rworkspace: none)
              call stdlib_zunmlq( 'L', 'C', n, nrhs, m, a, lda, work( itau ), b,ldb, work( iwork )&
                        , lwork-iwork+1, info )
           else
              ! path 2 - remaining underdetermined cases
              ie = 1_ilp
              itauq = 1_ilp
              itaup = itauq + m
              iwork = itaup + m
              ! bidiagonalize a
              ! (cworkspace: need 3*m, prefer 2*m+(m+n)*nb)
              ! (rworkspace: need n)
              call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), work(&
                         iwork ), lwork-iwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors
              ! (cworkspace: need 2*m+nrhs, prefer 2*m+nrhs*nb)
              ! (rworkspace: none)
              call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        iwork ), lwork-iwork+1, info )
              ! generate right bidiagonalizing vectors in a
              ! (cworkspace: need 3*m, prefer 2*m+m*nb)
              ! (rworkspace: none)
              call stdlib_zungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-iwork+&
                        1_ilp, info )
              irwork = ie + m
              ! perform bidiagonal qr iteration,
                 ! computing right singular vectors of a in a and
                 ! multiplying b by transpose of left singular vectors
              ! (cworkspace: none)
              ! (rworkspace: need bdspac)
              call stdlib_zbdsqr( 'L', m, n, 0_ilp, nrhs, s, rwork( ie ), a, lda, dum,1_ilp, b, ldb, &
                        rwork( irwork ), info )
              if( info/=0 )go to 70
              ! multiply b by reciprocals of singular values
              thr = max( rcond*s( 1_ilp ), sfmin )
              if( rcond<zero )thr = max( eps*s( 1_ilp ), sfmin )
              rank = 0_ilp
              do i = 1, m
                 if( s( i )>thr ) then
                    call stdlib_zdrscl( nrhs, s( i ), b( i, 1_ilp ), ldb )
                    rank = rank + 1_ilp
                 else
                    call stdlib_zlaset( 'F', 1_ilp, nrhs, czero, czero, b( i, 1_ilp ), ldb )
                 end if
              end do
              ! multiply b by right singular vectors of a
              ! (cworkspace: need n, prefer n*nrhs)
              ! (rworkspace: none)
              if( lwork>=ldb*nrhs .and. nrhs>1_ilp ) then
                 call stdlib_zgemm( 'C', 'N', n, nrhs, m, cone, a, lda, b, ldb,czero, work, ldb )
                           
                 call stdlib_zlacpy( 'G', n, nrhs, work, ldb, b, ldb )
              else if( nrhs>1_ilp ) then
                 chunk = lwork / n
                 do i = 1, nrhs, chunk
                    bl = min( nrhs-i+1, chunk )
                    call stdlib_zgemm( 'C', 'N', n, bl, m, cone, a, lda, b( 1_ilp, i ),ldb, czero, &
                              work, n )
                    call stdlib_zlacpy( 'F', n, bl, work, n, b( 1_ilp, i ), ldb )
                 end do
              else
                 call stdlib_zgemv( 'C', m, n, cone, a, lda, b, 1_ilp, czero, work, 1_ilp )
                 call stdlib_zcopy( n, work, 1_ilp, b, 1_ilp )
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgelss




     module subroutine stdlib_sgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, info )
     !! SGELSY computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize || A * X - B ||
     !! using a complete orthogonal factorization of A.  A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The routine first computes a QR factorization with column pivoting:
     !! A * P = Q * [ R11 R12 ]
     !! [  0  R22 ]
     !! with R11 defined as the largest leading submatrix whose estimated
     !! condition number is less than 1/RCOND.  The order of R11, RANK,
     !! is the effective rank of A.
     !! Then, R22 is considered to be negligible, and R12 is annihilated
     !! by orthogonal transformations from the right, arriving at the
     !! complete orthogonal factorization:
     !! A * P = Q * [ T11 0 ] * Z
     !! [  0  0 ]
     !! The minimum-norm solution is then
     !! X = P * Z**T [ inv(T11)*Q1**T*B ]
     !! [        0         ]
     !! where Q1 consists of the first RANK columns of Q.
     !! This routine is basically identical to the original xGELSX except
     !! three differences:
     !! o The call to the subroutine xGEQPF has been substituted by the
     !! the call to the subroutine xGEQP3. This subroutine is a Blas-3
     !! version of the QR factorization with column pivoting.
     !! o Matrix B (the right hand side) is updated with Blas-3.
     !! o The permutation of matrix B (the right hand side) is faster and
     !! more simple.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: imax = 1_ilp
           integer(ilp), parameter :: imin = 2_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iascl, ibscl, ismax, ismin, j, lwkmin, lwkopt, mn, nb, nb1, nb2, &
                     nb3, nb4
           real(sp) :: anrm, bignum, bnrm, c1, c2, s1, s2, smax, smaxpr, smin, sminpr, smlnum, &
                     wsize
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m, n )
           ismin = mn + 1_ilp
           ismax = 2_ilp*mn + 1_ilp
           ! test the input arguments.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -7_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp ) then
              if( mn==0_ilp .or. nrhs==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', m, n, nrhs, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'SORMRQ', ' ', m, n, nrhs, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = mn + max( 2_ilp*mn, n + 1_ilp, mn + nrhs )
                 lwkopt = max( lwkmin,mn + 2_ilp*n + nb*( n + 1_ilp ), 2_ilp*mn + nb*nrhs )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELSY', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( mn==0_ilp .or. nrhs==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max entries outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_slaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              rank = 0_ilp
              go to 70
           end if
           bnrm = stdlib_slange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! compute qr factorization with column pivoting of a:
              ! a * p = q * r
           call stdlib_sgeqp3( m, n, a, lda, jpvt, work( 1_ilp ), work( mn+1 ),lwork-mn, info )
                     
           wsize = mn + work( mn+1 )
           ! workspace: mn+2*n+nb*(n+1).
           ! details of householder rotations stored in work(1:mn).
           ! determine rank using incremental condition estimation
           work( ismin ) = one
           work( ismax ) = one
           smax = abs( a( 1_ilp, 1_ilp ) )
           smin = smax
           if( abs( a( 1_ilp, 1_ilp ) )==zero ) then
              rank = 0_ilp
              call stdlib_slaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              go to 70
           else
              rank = 1_ilp
           end if
           10 continue
           if( rank<mn ) then
              i = rank + 1_ilp
              call stdlib_slaic1( imin, rank, work( ismin ), smin, a( 1_ilp, i ),a( i, i ), sminpr, &
                        s1, c1 )
              call stdlib_slaic1( imax, rank, work( ismax ), smax, a( 1_ilp, i ),a( i, i ), smaxpr, &
                        s2, c2 )
              if( smaxpr*rcond<=sminpr ) then
                 do i = 1, rank
                    work( ismin+i-1 ) = s1*work( ismin+i-1 )
                    work( ismax+i-1 ) = s2*work( ismax+i-1 )
                 end do
                 work( ismin+rank ) = c1
                 work( ismax+rank ) = c2
                 smin = sminpr
                 smax = smaxpr
                 rank = rank + 1_ilp
                 go to 10
              end if
           end if
           ! workspace: 3*mn.
           ! logically partition r = [ r11 r12 ]
                                   ! [  0  r22 ]
           ! where r11 = r(1:rank,1:rank)
           ! [r11,r12] = [ t11, 0 ] * y
           if( rank<n )call stdlib_stzrzf( rank, n, a, lda, work( mn+1 ), work( 2_ilp*mn+1 ),lwork-&
                     2_ilp*mn, info )
           ! workspace: 2*mn.
           ! details of householder rotations stored in work(mn+1:2*mn)
           ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
           call stdlib_sormqr( 'LEFT', 'TRANSPOSE', m, nrhs, mn, a, lda, work( 1_ilp ),b, ldb, work( &
                     2_ilp*mn+1 ), lwork-2*mn, info )
           wsize = max( wsize, 2_ilp*mn+work( 2_ilp*mn+1 ) )
           ! workspace: 2*mn+nb*nrhs.
           ! b(1:rank,1:nrhs) := inv(t11) * b(1:rank,1:nrhs)
           call stdlib_strsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', rank,nrhs, one, a, lda,&
                      b, ldb )
           do j = 1, nrhs
              do i = rank + 1, n
                 b( i, j ) = zero
              end do
           end do
           ! b(1:n,1:nrhs) := y**t * b(1:n,1:nrhs)
           if( rank<n ) then
              call stdlib_sormrz( 'LEFT', 'TRANSPOSE', n, nrhs, rank, n-rank, a,lda, work( mn+1 ),&
                         b, ldb, work( 2_ilp*mn+1 ),lwork-2*mn, info )
           end if
           ! workspace: 2*mn+nrhs.
           ! b(1:n,1:nrhs) := p * b(1:n,1:nrhs)
           do j = 1, nrhs
              do i = 1, n
                 work( jpvt( i ) ) = b( i, j )
              end do
              call stdlib_scopy( n, work( 1_ilp ), 1_ilp, b( 1_ilp, j ), 1_ilp )
           end do
           ! workspace: n.
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'U', 0_ilp, 0_ilp, smlnum, anrm, rank, rank, a, lda,info )
           else if( iascl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'U', 0_ilp, 0_ilp, bignum, anrm, rank, rank, a, lda,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sgelsy

     module subroutine stdlib_dgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, info )
     !! DGELSY computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize || A * X - B ||
     !! using a complete orthogonal factorization of A.  A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The routine first computes a QR factorization with column pivoting:
     !! A * P = Q * [ R11 R12 ]
     !! [  0  R22 ]
     !! with R11 defined as the largest leading submatrix whose estimated
     !! condition number is less than 1/RCOND.  The order of R11, RANK,
     !! is the effective rank of A.
     !! Then, R22 is considered to be negligible, and R12 is annihilated
     !! by orthogonal transformations from the right, arriving at the
     !! complete orthogonal factorization:
     !! A * P = Q * [ T11 0 ] * Z
     !! [  0  0 ]
     !! The minimum-norm solution is then
     !! X = P * Z**T [ inv(T11)*Q1**T*B ]
     !! [        0         ]
     !! where Q1 consists of the first RANK columns of Q.
     !! This routine is basically identical to the original xGELSX except
     !! three differences:
     !! o The call to the subroutine xGEQPF has been substituted by the
     !! the call to the subroutine xGEQP3. This subroutine is a Blas-3
     !! version of the QR factorization with column pivoting.
     !! o Matrix B (the right hand side) is updated with Blas-3.
     !! o The permutation of matrix B (the right hand side) is faster and
     !! more simple.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: imax = 1_ilp
           integer(ilp), parameter :: imin = 2_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iascl, ibscl, ismax, ismin, j, lwkmin, lwkopt, mn, nb, nb1, nb2, &
                     nb3, nb4
           real(dp) :: anrm, bignum, bnrm, c1, c2, s1, s2, smax, smaxpr, smin, sminpr, smlnum, &
                     wsize
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m, n )
           ismin = mn + 1_ilp
           ismax = 2_ilp*mn + 1_ilp
           ! test the input arguments.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -7_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp ) then
              if( mn==0_ilp .or. nrhs==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', m, n, nrhs, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'DORMRQ', ' ', m, n, nrhs, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = mn + max( 2_ilp*mn, n + 1_ilp, mn + nrhs )
                 lwkopt = max( lwkmin,mn + 2_ilp*n + nb*( n + 1_ilp ), 2_ilp*mn + nb*nrhs )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELSY', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( mn==0_ilp .or. nrhs==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max entries outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_dlaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              rank = 0_ilp
              go to 70
           end if
           bnrm = stdlib_dlange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! compute qr factorization with column pivoting of a:
              ! a * p = q * r
           call stdlib_dgeqp3( m, n, a, lda, jpvt, work( 1_ilp ), work( mn+1 ),lwork-mn, info )
                     
           wsize = mn + work( mn+1 )
           ! workspace: mn+2*n+nb*(n+1).
           ! details of householder rotations stored in work(1:mn).
           ! determine rank using incremental condition estimation
           work( ismin ) = one
           work( ismax ) = one
           smax = abs( a( 1_ilp, 1_ilp ) )
           smin = smax
           if( abs( a( 1_ilp, 1_ilp ) )==zero ) then
              rank = 0_ilp
              call stdlib_dlaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              go to 70
           else
              rank = 1_ilp
           end if
           10 continue
           if( rank<mn ) then
              i = rank + 1_ilp
              call stdlib_dlaic1( imin, rank, work( ismin ), smin, a( 1_ilp, i ),a( i, i ), sminpr, &
                        s1, c1 )
              call stdlib_dlaic1( imax, rank, work( ismax ), smax, a( 1_ilp, i ),a( i, i ), smaxpr, &
                        s2, c2 )
              if( smaxpr*rcond<=sminpr ) then
                 do i = 1, rank
                    work( ismin+i-1 ) = s1*work( ismin+i-1 )
                    work( ismax+i-1 ) = s2*work( ismax+i-1 )
                 end do
                 work( ismin+rank ) = c1
                 work( ismax+rank ) = c2
                 smin = sminpr
                 smax = smaxpr
                 rank = rank + 1_ilp
                 go to 10
              end if
           end if
           ! workspace: 3*mn.
           ! logically partition r = [ r11 r12 ]
                                   ! [  0  r22 ]
           ! where r11 = r(1:rank,1:rank)
           ! [r11,r12] = [ t11, 0 ] * y
           if( rank<n )call stdlib_dtzrzf( rank, n, a, lda, work( mn+1 ), work( 2_ilp*mn+1 ),lwork-&
                     2_ilp*mn, info )
           ! workspace: 2*mn.
           ! details of householder rotations stored in work(mn+1:2*mn)
           ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
           call stdlib_dormqr( 'LEFT', 'TRANSPOSE', m, nrhs, mn, a, lda, work( 1_ilp ),b, ldb, work( &
                     2_ilp*mn+1 ), lwork-2*mn, info )
           wsize = max( wsize, 2_ilp*mn+work( 2_ilp*mn+1 ) )
           ! workspace: 2*mn+nb*nrhs.
           ! b(1:rank,1:nrhs) := inv(t11) * b(1:rank,1:nrhs)
           call stdlib_dtrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', rank,nrhs, one, a, lda,&
                      b, ldb )
           do j = 1, nrhs
              do i = rank + 1, n
                 b( i, j ) = zero
              end do
           end do
           ! b(1:n,1:nrhs) := y**t * b(1:n,1:nrhs)
           if( rank<n ) then
              call stdlib_dormrz( 'LEFT', 'TRANSPOSE', n, nrhs, rank, n-rank, a,lda, work( mn+1 ),&
                         b, ldb, work( 2_ilp*mn+1 ),lwork-2*mn, info )
           end if
           ! workspace: 2*mn+nrhs.
           ! b(1:n,1:nrhs) := p * b(1:n,1:nrhs)
           do j = 1, nrhs
              do i = 1, n
                 work( jpvt( i ) ) = b( i, j )
              end do
              call stdlib_dcopy( n, work( 1_ilp ), 1_ilp, b( 1_ilp, j ), 1_ilp )
           end do
           ! workspace: n.
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'U', 0_ilp, 0_ilp, smlnum, anrm, rank, rank, a, lda,info )
           else if( iascl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'U', 0_ilp, 0_ilp, bignum, anrm, rank, rank, a, lda,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dgelsy


     module subroutine stdlib_cgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, rwork, &
     !! CGELSY computes the minimum-norm solution to a complex linear least
     !! squares problem:
     !! minimize || A * X - B ||
     !! using a complete orthogonal factorization of A.  A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The routine first computes a QR factorization with column pivoting:
     !! A * P = Q * [ R11 R12 ]
     !! [  0  R22 ]
     !! with R11 defined as the largest leading submatrix whose estimated
     !! condition number is less than 1/RCOND.  The order of R11, RANK,
     !! is the effective rank of A.
     !! Then, R22 is considered to be negligible, and R12 is annihilated
     !! by unitary transformations from the right, arriving at the
     !! complete orthogonal factorization:
     !! A * P = Q * [ T11 0 ] * Z
     !! [  0  0 ]
     !! The minimum-norm solution is then
     !! X = P * Z**H [ inv(T11)*Q1**H*B ]
     !! [        0         ]
     !! where Q1 consists of the first RANK columns of Q.
     !! This routine is basically identical to the original xGELSX except
     !! three differences:
     !! o The permutation of matrix B (the right hand side) is faster and
     !! more simple.
     !! o The call to the subroutine xGEQPF has been substituted by the
     !! the call to the subroutine xGEQP3. This subroutine is a Blas-3
     !! version of the QR factorization with column pivoting.
     !! o Matrix B (the right hand side) is updated with Blas-3.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: imax = 1_ilp
           integer(ilp), parameter :: imin = 2_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iascl, ibscl, ismax, ismin, j, lwkopt, mn, nb, nb1, nb2, nb3, &
                     nb4
           real(sp) :: anrm, bignum, bnrm, smax, smaxpr, smin, sminpr, smlnum, wsize
           complex(sp) :: c1, c2, s1, s2
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m, n )
           ismin = mn + 1_ilp
           ismax = 2_ilp*mn + 1_ilp
           ! test the input arguments.
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', m, n, nrhs, -1_ilp )
           nb4 = stdlib_ilaenv( 1_ilp, 'CUNMRQ', ' ', m, n, nrhs, -1_ilp )
           nb = max( nb1, nb2, nb3, nb4 )
           lwkopt = max( 1_ilp, mn+2*n+nb*(n+1), 2_ilp*mn+nb*nrhs )
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -7_ilp
           else if( lwork<( mn+max( 2_ilp*mn, n+1, mn+nrhs ) ) .and..not.lquery ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELSY', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max entries outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_claset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              rank = 0_ilp
              go to 70
           end if
           bnrm = stdlib_clange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! compute qr factorization with column pivoting of a:
              ! a * p = q * r
           call stdlib_cgeqp3( m, n, a, lda, jpvt, work( 1_ilp ), work( mn+1 ),lwork-mn, rwork, info )
                     
           wsize = mn + real( work( mn+1 ),KIND=sp)
           ! complex workspace: mn+nb*(n+1). real workspace 2*n.
           ! details of householder rotations stored in work(1:mn).
           ! determine rank using incremental condition estimation
           work( ismin ) = cone
           work( ismax ) = cone
           smax = abs( a( 1_ilp, 1_ilp ) )
           smin = smax
           if( abs( a( 1_ilp, 1_ilp ) )==zero ) then
              rank = 0_ilp
              call stdlib_claset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              go to 70
           else
              rank = 1_ilp
           end if
           10 continue
           if( rank<mn ) then
              i = rank + 1_ilp
              call stdlib_claic1( imin, rank, work( ismin ), smin, a( 1_ilp, i ),a( i, i ), sminpr, &
                        s1, c1 )
              call stdlib_claic1( imax, rank, work( ismax ), smax, a( 1_ilp, i ),a( i, i ), smaxpr, &
                        s2, c2 )
              if( smaxpr*rcond<=sminpr ) then
                 do i = 1, rank
                    work( ismin+i-1 ) = s1*work( ismin+i-1 )
                    work( ismax+i-1 ) = s2*work( ismax+i-1 )
                 end do
                 work( ismin+rank ) = c1
                 work( ismax+rank ) = c2
                 smin = sminpr
                 smax = smaxpr
                 rank = rank + 1_ilp
                 go to 10
              end if
           end if
           ! complex workspace: 3*mn.
           ! logically partition r = [ r11 r12 ]
                                   ! [  0  r22 ]
           ! where r11 = r(1:rank,1:rank)
           ! [r11,r12] = [ t11, 0 ] * y
           if( rank<n )call stdlib_ctzrzf( rank, n, a, lda, work( mn+1 ), work( 2_ilp*mn+1 ),lwork-&
                     2_ilp*mn, info )
           ! complex workspace: 2*mn.
           ! details of householder rotations stored in work(mn+1:2*mn)
           ! b(1:m,1:nrhs) := q**h * b(1:m,1:nrhs)
           call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, nrhs, mn, a, lda,work( 1_ilp ), b, &
                     ldb, work( 2_ilp*mn+1 ), lwork-2*mn, info )
           wsize = max( wsize, 2_ilp*mn+real( work( 2_ilp*mn+1 ),KIND=sp) )
           ! complex workspace: 2*mn+nb*nrhs.
           ! b(1:rank,1:nrhs) := inv(t11) * b(1:rank,1:nrhs)
           call stdlib_ctrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', rank,nrhs, cone, a, &
                     lda, b, ldb )
           do j = 1, nrhs
              do i = rank + 1, n
                 b( i, j ) = czero
              end do
           end do
           ! b(1:n,1:nrhs) := y**h * b(1:n,1:nrhs)
           if( rank<n ) then
              call stdlib_cunmrz( 'LEFT', 'CONJUGATE TRANSPOSE', n, nrhs, rank,n-rank, a, lda, &
                        work( mn+1 ), b, ldb,work( 2_ilp*mn+1 ), lwork-2*mn, info )
           end if
           ! complex workspace: 2*mn+nrhs.
           ! b(1:n,1:nrhs) := p * b(1:n,1:nrhs)
           do j = 1, nrhs
              do i = 1, n
                 work( jpvt( i ) ) = b( i, j )
              end do
              call stdlib_ccopy( n, work( 1_ilp ), 1_ilp, b( 1_ilp, j ), 1_ilp )
           end do
           ! complex workspace: n.
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, smlnum, anrm, rank, rank, a, lda,info )
           else if( iascl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_clascl( 'U', 0_ilp, 0_ilp, bignum, anrm, rank, rank, a, lda,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cgelsy

     module subroutine stdlib_zgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, rwork, &
     !! ZGELSY computes the minimum-norm solution to a complex linear least
     !! squares problem:
     !! minimize || A * X - B ||
     !! using a complete orthogonal factorization of A.  A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The routine first computes a QR factorization with column pivoting:
     !! A * P = Q * [ R11 R12 ]
     !! [  0  R22 ]
     !! with R11 defined as the largest leading submatrix whose estimated
     !! condition number is less than 1/RCOND.  The order of R11, RANK,
     !! is the effective rank of A.
     !! Then, R22 is considered to be negligible, and R12 is annihilated
     !! by unitary transformations from the right, arriving at the
     !! complete orthogonal factorization:
     !! A * P = Q * [ T11 0 ] * Z
     !! [  0  0 ]
     !! The minimum-norm solution is then
     !! X = P * Z**H [ inv(T11)*Q1**H*B ]
     !! [        0         ]
     !! where Q1 consists of the first RANK columns of Q.
     !! This routine is basically identical to the original xGELSX except
     !! three differences:
     !! o The permutation of matrix B (the right hand side) is faster and
     !! more simple.
     !! o The call to the subroutine xGEQPF has been substituted by the
     !! the call to the subroutine xGEQP3. This subroutine is a Blas-3
     !! version of the QR factorization with column pivoting.
     !! o Matrix B (the right hand side) is updated with Blas-3.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: imax = 1_ilp
           integer(ilp), parameter :: imin = 2_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iascl, ibscl, ismax, ismin, j, lwkopt, mn, nb, nb1, nb2, nb3, &
                     nb4
           real(dp) :: anrm, bignum, bnrm, smax, smaxpr, smin, sminpr, smlnum, wsize
           complex(dp) :: c1, c2, s1, s2
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m, n )
           ismin = mn + 1_ilp
           ismax = 2_ilp*mn + 1_ilp
           ! test the input arguments.
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', m, n, nrhs, -1_ilp )
           nb4 = stdlib_ilaenv( 1_ilp, 'ZUNMRQ', ' ', m, n, nrhs, -1_ilp )
           nb = max( nb1, nb2, nb3, nb4 )
           lwkopt = max( 1_ilp, mn+2*n+nb*( n+1 ), 2_ilp*mn+nb*nrhs )
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -7_ilp
           else if( lwork<( mn+max( 2_ilp*mn, n+1, mn+nrhs ) ) .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELSY', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max entries outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_zlaset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              rank = 0_ilp
              go to 70
           end if
           bnrm = stdlib_zlange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! compute qr factorization with column pivoting of a:
              ! a * p = q * r
           call stdlib_zgeqp3( m, n, a, lda, jpvt, work( 1_ilp ), work( mn+1 ),lwork-mn, rwork, info )
                     
           wsize = mn + real( work( mn+1 ),KIND=dp)
           ! complex workspace: mn+nb*(n+1). real workspace 2*n.
           ! details of householder rotations stored in work(1:mn).
           ! determine rank using incremental condition estimation
           work( ismin ) = cone
           work( ismax ) = cone
           smax = abs( a( 1_ilp, 1_ilp ) )
           smin = smax
           if( abs( a( 1_ilp, 1_ilp ) )==zero ) then
              rank = 0_ilp
              call stdlib_zlaset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              go to 70
           else
              rank = 1_ilp
           end if
           10 continue
           if( rank<mn ) then
              i = rank + 1_ilp
              call stdlib_zlaic1( imin, rank, work( ismin ), smin, a( 1_ilp, i ),a( i, i ), sminpr, &
                        s1, c1 )
              call stdlib_zlaic1( imax, rank, work( ismax ), smax, a( 1_ilp, i ),a( i, i ), smaxpr, &
                        s2, c2 )
              if( smaxpr*rcond<=sminpr ) then
                 do i = 1, rank
                    work( ismin+i-1 ) = s1*work( ismin+i-1 )
                    work( ismax+i-1 ) = s2*work( ismax+i-1 )
                 end do
                 work( ismin+rank ) = c1
                 work( ismax+rank ) = c2
                 smin = sminpr
                 smax = smaxpr
                 rank = rank + 1_ilp
                 go to 10
              end if
           end if
           ! complex workspace: 3*mn.
           ! logically partition r = [ r11 r12 ]
                                   ! [  0  r22 ]
           ! where r11 = r(1:rank,1:rank)
           ! [r11,r12] = [ t11, 0 ] * y
           if( rank<n )call stdlib_ztzrzf( rank, n, a, lda, work( mn+1 ), work( 2_ilp*mn+1 ),lwork-&
                     2_ilp*mn, info )
           ! complex workspace: 2*mn.
           ! details of householder rotations stored in work(mn+1:2*mn)
           ! b(1:m,1:nrhs) := q**h * b(1:m,1:nrhs)
           call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, nrhs, mn, a, lda,work( 1_ilp ), b, &
                     ldb, work( 2_ilp*mn+1 ), lwork-2*mn, info )
           wsize = max( wsize, 2_ilp*mn+real( work( 2_ilp*mn+1 ),KIND=dp) )
           ! complex workspace: 2*mn+nb*nrhs.
           ! b(1:rank,1:nrhs) := inv(t11) * b(1:rank,1:nrhs)
           call stdlib_ztrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', rank,nrhs, cone, a, &
                     lda, b, ldb )
           do j = 1, nrhs
              do i = rank + 1, n
                 b( i, j ) = czero
              end do
           end do
           ! b(1:n,1:nrhs) := y**h * b(1:n,1:nrhs)
           if( rank<n ) then
              call stdlib_zunmrz( 'LEFT', 'CONJUGATE TRANSPOSE', n, nrhs, rank,n-rank, a, lda, &
                        work( mn+1 ), b, ldb,work( 2_ilp*mn+1 ), lwork-2*mn, info )
           end if
           ! complex workspace: 2*mn+nrhs.
           ! b(1:n,1:nrhs) := p * b(1:n,1:nrhs)
           do j = 1, nrhs
              do i = 1, n
                 work( jpvt( i ) ) = b( i, j )
              end do
              call stdlib_zcopy( n, work( 1_ilp ), 1_ilp, b( 1_ilp, j ), 1_ilp )
           end do
           ! complex workspace: n.
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, smlnum, anrm, rank, rank, a, lda,info )
           else if( iascl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_zlascl( 'U', 0_ilp, 0_ilp, bignum, anrm, rank, rank, a, lda,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           70 continue
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zgelsy




     module subroutine stdlib_sgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
     !! SGELS solves overdetermined or underdetermined real linear systems
     !! involving an M-by-N matrix A, or its transpose, using a QR or LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
     !! an underdetermined system A**T * X = B.
     !! 4. If TRANS = 'T' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, tpsd
           integer(ilp) :: brow, i, iascl, ibscl, j, mn, nb, scllen, wsize
           real(sp) :: anrm, bignum, bnrm, smlnum
           ! Local Arrays 
           real(sp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or. stdlib_lsame( trans, 'T' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, mn + max( mn, nrhs ) ) .and..not.lquery ) then
              info = -10_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp .or. info==-10_ilp ) then
              tpsd = .true.
              if( stdlib_lsame( trans, 'N' ) )tpsd = .false.
              if( m>=n ) then
                 nb = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMQR', 'LN', m, nrhs, n,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMQR', 'LT', m, nrhs, n,-1_ilp ) )
                 end if
              else
                 nb = stdlib_ilaenv( 1_ilp, 'SGELQF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMLQ', 'LT', n, nrhs, m,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'SORMLQ', 'LN', n, nrhs, m,-1_ilp ) )
                 end if
              end if
              wsize = max( 1_ilp, mn + max( mn, nrhs )*nb )
              work( 1_ilp ) = real( wsize,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELS ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              call stdlib_slaset( 'FULL', max( m, n ), nrhs, zero, zero, b, ldb )
              return
           end if
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_slaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              go to 50
           end if
           brow = m
           if( tpsd )brow = n
           bnrm = stdlib_slange( 'M', brow, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if( m>=n ) then
              ! compute qr factorization of a
              call stdlib_sgeqrf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least n, optimally n*nb
              if( .not.tpsd ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
                 call stdlib_sormqr( 'LEFT', 'TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb, &
                           work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
                 call stdlib_strtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = n
              else
                 ! underdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_strtrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_sormqr( 'LEFT', 'NO TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_sgelqf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least m, optimally m*nb.
              if( .not.tpsd ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_strtrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_sormlq( 'LEFT', 'TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb, &
                           work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_sormlq( 'LEFT', 'NO TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_strtrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( wsize,KIND=sp)
           return
     end subroutine stdlib_sgels

     module subroutine stdlib_dgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
     !! DGELS solves overdetermined or underdetermined real linear systems
     !! involving an M-by-N matrix A, or its transpose, using a QR or LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
     !! an underdetermined system A**T * X = B.
     !! 4. If TRANS = 'T' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, tpsd
           integer(ilp) :: brow, i, iascl, ibscl, j, mn, nb, scllen, wsize
           real(dp) :: anrm, bignum, bnrm, smlnum
           ! Local Arrays 
           real(dp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or. stdlib_lsame( trans, 'T' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, mn+max( mn, nrhs ) ) .and. .not.lquery )then
              info = -10_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp .or. info==-10_ilp ) then
              tpsd = .true.
              if( stdlib_lsame( trans, 'N' ) )tpsd = .false.
              if( m>=n ) then
                 nb = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMQR', 'LN', m, nrhs, n,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMQR', 'LT', m, nrhs, n,-1_ilp ) )
                 end if
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DGELQF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMLQ', 'LT', n, nrhs, m,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'DORMLQ', 'LN', n, nrhs, m,-1_ilp ) )
                 end if
              end if
              wsize = max( 1_ilp, mn+max( mn, nrhs )*nb )
              work( 1_ilp ) = real( wsize,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELS ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              call stdlib_dlaset( 'FULL', max( m, n ), nrhs, zero, zero, b, ldb )
              return
           end if
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_dlaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              go to 50
           end if
           brow = m
           if( tpsd )brow = n
           bnrm = stdlib_dlange( 'M', brow, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if( m>=n ) then
              ! compute qr factorization of a
              call stdlib_dgeqrf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least n, optimally n*nb
              if( .not.tpsd ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
                 call stdlib_dormqr( 'LEFT', 'TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb, &
                           work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
                 call stdlib_dtrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = n
              else
                 ! underdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_dtrtrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_dormqr( 'LEFT', 'NO TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_dgelqf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least m, optimally m*nb.
              if( .not.tpsd ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_dtrtrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_dormlq( 'LEFT', 'TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb, &
                           work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_dormlq( 'LEFT', 'NO TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_dtrtrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( wsize,KIND=dp)
           return
     end subroutine stdlib_dgels


     module subroutine stdlib_cgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
     !! CGELS solves overdetermined or underdetermined complex linear systems
     !! involving an M-by-N matrix A, or its conjugate-transpose, using a QR
     !! or LQ factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
     !! an underdetermined system A**H * X = B.
     !! 4. If TRANS = 'C' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**H * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, tpsd
           integer(ilp) :: brow, i, iascl, ibscl, j, mn, nb, scllen, wsize
           real(sp) :: anrm, bignum, bnrm, smlnum
           ! Local Arrays 
           real(sp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or. stdlib_lsame( trans, 'C' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, mn+max( mn, nrhs ) ) .and..not.lquery ) then
              info = -10_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp .or. info==-10_ilp ) then
              tpsd = .true.
              if( stdlib_lsame( trans, 'N' ) )tpsd = .false.
              if( m>=n ) then
                 nb = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMQR', 'LN', m, nrhs, n,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMQR', 'LC', m, nrhs, n,-1_ilp ) )
                 end if
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CGELQF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMLQ', 'LC', n, nrhs, m,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'CUNMLQ', 'LN', n, nrhs, m,-1_ilp ) )
                 end if
              end if
              wsize = max( 1_ilp, mn + max( mn, nrhs )*nb )
              work( 1_ilp ) = real( wsize,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELS ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              call stdlib_claset( 'FULL', max( m, n ), nrhs, czero, czero, b, ldb )
              return
           end if
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_claset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              go to 50
           end if
           brow = m
           if( tpsd )brow = n
           bnrm = stdlib_clange( 'M', brow, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if( m>=n ) then
              ! compute qr factorization of a
              call stdlib_cgeqrf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least n, optimally n*nb
              if( .not.tpsd ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**h * b(1:m,1:nrhs)
                 call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, nrhs, n, a,lda, work( 1_ilp ), &
                           b, ldb, work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
                 call stdlib_ctrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = n
              else
                 ! underdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**h) * b(1:n,1:nrhs)
                 call stdlib_ctrtrs( 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT',n, nrhs, a, lda, b,&
                            ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_cunmqr( 'LEFT', 'NO TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_cgelqf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least m, optimally m*nb.
              if( .not.tpsd ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_ctrtrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**h * b(1:m,1:nrhs)
                 call stdlib_cunmlq( 'LEFT', 'CONJUGATE TRANSPOSE', n, nrhs, m, a,lda, work( 1_ilp ), &
                           b, ldb, work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**h * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_cunmlq( 'LEFT', 'NO TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**h) * b(1:m,1:nrhs)
                 call stdlib_ctrtrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',m, nrhs, a, lda, &
                           b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( wsize,KIND=sp)
           return
     end subroutine stdlib_cgels

     module subroutine stdlib_zgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
     !! ZGELS solves overdetermined or underdetermined complex linear systems
     !! involving an M-by-N matrix A, or its conjugate-transpose, using a QR
     !! or LQ factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
     !! an underdetermined system A**H * X = B.
     !! 4. If TRANS = 'C' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**H * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, tpsd
           integer(ilp) :: brow, i, iascl, ibscl, j, mn, nb, scllen, wsize
           real(dp) :: anrm, bignum, bnrm, smlnum
           ! Local Arrays 
           real(dp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or. stdlib_lsame( trans, 'C' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, mn+max( mn, nrhs ) ) .and. .not.lquery )then
              info = -10_ilp
           end if
           ! figure out optimal block size
           if( info==0_ilp .or. info==-10_ilp ) then
              tpsd = .true.
              if( stdlib_lsame( trans, 'N' ) )tpsd = .false.
              if( m>=n ) then
                 nb = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMQR', 'LN', m, nrhs, n,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMQR', 'LC', m, nrhs, n,-1_ilp ) )
                 end if
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZGELQF', ' ', m, n, -1_ilp, -1_ilp )
                 if( tpsd ) then
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMLQ', 'LC', n, nrhs, m,-1_ilp ) )
                 else
                    nb = max( nb, stdlib_ilaenv( 1_ilp, 'ZUNMLQ', 'LN', n, nrhs, m,-1_ilp ) )
                 end if
              end if
              wsize = max( 1_ilp, mn+max( mn, nrhs )*nb )
              work( 1_ilp ) = real( wsize,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELS ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
              call stdlib_zlaset( 'FULL', max( m, n ), nrhs, czero, czero, b, ldb )
              return
           end if
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_zlaset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              go to 50
           end if
           brow = m
           if( tpsd )brow = n
           bnrm = stdlib_zlange( 'M', brow, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if( m>=n ) then
              ! compute qr factorization of a
              call stdlib_zgeqrf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least n, optimally n*nb
              if( .not.tpsd ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**h * b(1:m,1:nrhs)
                 call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, nrhs, n, a,lda, work( 1_ilp ), &
                           b, ldb, work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
                 call stdlib_ztrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = n
              else
                 ! underdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**h) * b(1:n,1:nrhs)
                 call stdlib_ztrtrs( 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT',n, nrhs, a, lda, b,&
                            ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_zunmqr( 'LEFT', 'NO TRANSPOSE', m, nrhs, n, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_zgelqf( m, n, a, lda, work( 1_ilp ), work( mn+1 ), lwork-mn,info )
              ! workspace at least m, optimally m*nb.
              if( .not.tpsd ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_ztrtrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**h * b(1:m,1:nrhs)
                 call stdlib_zunmlq( 'LEFT', 'CONJUGATE TRANSPOSE', n, nrhs, m, a,lda, work( 1_ilp ), &
                           b, ldb, work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**h * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_zunmlq( 'LEFT', 'NO TRANSPOSE', n, nrhs, m, a, lda,work( 1_ilp ), b, ldb,&
                            work( mn+1 ), lwork-mn,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**h) * b(1:m,1:nrhs)
                 call stdlib_ztrtrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',m, nrhs, a, lda, &
                           b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( wsize,KIND=dp)
           return
     end subroutine stdlib_zgels




     module subroutine stdlib_sgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond,rank, work, lwork, iwork, &
     !! SGELSD computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize 2-norm(| b - A*x |)
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The problem is solved in three steps:
     !! (1) Reduce the coefficient matrix A to bidiagonal form with
     !! Householder transformations, reducing the original problem
     !! into a "bidiagonal least squares problem" (BLS)
     !! (2) Solve the BLS using a divide and conquer approach.
     !! (3) Apply back all the Householder transformations to solve
     !! the original least squares problem.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
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
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: s(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iascl, ibscl, ie, il, itau, itaup, itauq, ldwork, liwork, maxmn, &
                     maxwrk, minmn, minwrk, mm, mnthr, nlvl, nwork, smlsiz, wlalsd
           real(sp) :: anrm, bignum, bnrm, eps, sfmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace.
           ! (note: comments in the code beginning "workspace:" describe the
           ! minimal amount of workspace needed at that point in the code,
           ! as well as the preferred amount for good performance.
           ! nb refers to the optimal block size for the immediately
           ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              liwork = 1_ilp
              if( minmn>0_ilp ) then
                 smlsiz = stdlib_ilaenv( 9_ilp, 'SGELSD', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
                 mnthr = stdlib_ilaenv( 6_ilp, 'SGELSD', ' ', m, n, nrhs, -1_ilp )
                 nlvl = max( int( log( real( minmn,KIND=sp) / real( smlsiz + 1_ilp,KIND=sp) ) /log( &
                           two ),KIND=ilp) + 1_ilp, 0_ilp )
                 liwork = 3_ilp*minmn*nlvl + 11_ilp*minmn
                 mm = m
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns.
                    mm = n
                    maxwrk = max( maxwrk, n + n*stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m,n, -1_ilp, -1_ilp ) )
                              
                    maxwrk = max( maxwrk, n + nrhs*stdlib_ilaenv( 1_ilp, 'SORMQR', 'LT',m, nrhs, n, -&
                              1_ilp ) )
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined.
                    maxwrk = max( maxwrk, 3_ilp*n + ( mm + n )*stdlib_ilaenv( 1_ilp,'SGEBRD', ' ', mm, n, &
                              -1_ilp, -1_ilp ) )
                    maxwrk = max( maxwrk, 3_ilp*n + nrhs*stdlib_ilaenv( 1_ilp, 'SORMBR','QLT', mm, nrhs, &
                              n, -1_ilp ) )
                    maxwrk = max( maxwrk, 3_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORMBR', 'PLN', n, &
                              nrhs, n, -1_ilp ) )
                    wlalsd = 9_ilp*n + 2_ilp*n*smlsiz + 8_ilp*n*nlvl + n*nrhs +( smlsiz + 1_ilp )**2_ilp
                    maxwrk = max( maxwrk, 3_ilp*n + wlalsd )
                    minwrk = max( 3_ilp*n + mm, 3_ilp*n + nrhs, 3_ilp*n + wlalsd )
                 end if
                 if( n>m ) then
                    wlalsd = 9_ilp*m + 2_ilp*m*smlsiz + 8_ilp*m*nlvl + m*nrhs +( smlsiz + 1_ilp )**2_ilp
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                                 ! than rows.
                       maxwrk = m + m*stdlib_ilaenv( 1_ilp, 'SGELQF', ' ', m, n, -1_ilp,-1_ilp )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + 2_ilp*m*stdlib_ilaenv( 1_ilp,'SGEBRD', ' ', m, m,&
                                  -1_ilp, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + nrhs*stdlib_ilaenv( 1_ilp,'SORMBR', 'QLT', m,&
                                  nrhs, m, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + ( m - 1_ilp )*stdlib_ilaenv( 1_ilp,'SORMBR', &
                                 'PLN', m, nrhs, m, -1_ilp ) )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m + nrhs*stdlib_ilaenv( 1_ilp, 'SORMLQ','LT', n, nrhs, m,&
                                  -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + wlalsd )
           ! xxx: ensure the path 2a case below is triggered.  the workspace
           ! calculation should use queries for all routines eventually.
                       maxwrk = max( maxwrk,4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) )
                    else
                       ! path 2 - remaining underdetermined cases.
                       maxwrk = 3_ilp*m + ( n + m )*stdlib_ilaenv( 1_ilp, 'SGEBRD', ' ', m,n, -1_ilp, -1_ilp )
                                 
                       maxwrk = max( maxwrk, 3_ilp*m + nrhs*stdlib_ilaenv( 1_ilp, 'SORMBR','QLT', m, nrhs,&
                                  n, -1_ilp ) )
                       maxwrk = max( maxwrk, 3_ilp*m + m*stdlib_ilaenv( 1_ilp, 'SORMBR','PLN', n, nrhs, m,&
                                  -1_ilp ) )
                       maxwrk = max( maxwrk, 3_ilp*m + wlalsd )
                    end if
                    minwrk = max( 3_ilp*m + nrhs, 3_ilp*m + m, 3_ilp*m + wlalsd )
                 end if
              end if
              minwrk = min( minwrk, maxwrk )
              work( 1_ilp ) = maxwrk
              iwork( 1_ilp ) = liwork
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters.
           eps = stdlib_slamch( 'P' )
           sfmin = stdlib_slamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a if max entry outside range [smlnum,bignum].
           anrm = stdlib_slange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_slaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              call stdlib_slaset( 'F', minmn, 1_ilp, zero, zero, s, 1_ilp )
              rank = 0_ilp
              go to 10
           end if
           ! scale b if max entry outside range [smlnum,bignum].
           bnrm = stdlib_slange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! if m < n make sure certain entries of b are zero.
           if( m<n )call stdlib_slaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
           ! overdetermined case.
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined.
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns.
                 mm = n
                 itau = 1_ilp
                 nwork = itau + n
                 ! compute a=q*r.
                 ! (workspace: need 2*n, prefer n+n*nb)
                 call stdlib_sgeqrf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, &
                           info )
                 ! multiply b by transpose(q).
                 ! (workspace: need n+nrhs, prefer n+nrhs*nb)
                 call stdlib_sormqr( 'L', 'T', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           nwork ), lwork-nwork+1, info )
                 ! zero out below r.
                 if( n>1_ilp ) then
                    call stdlib_slaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ), lda )
                 end if
              end if
              ie = 1_ilp
              itauq = ie + n
              itaup = itauq + n
              nwork = itaup + n
              ! bidiagonalize r in a.
              ! (workspace: need 3*n+mm, prefer 3*n+(mm+n)*nb)
              call stdlib_sgebrd( mm, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work(&
                         nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r.
              ! (workspace: need 3*n+nrhs, prefer 3*n+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_slalsd( 'U', smlsiz, n, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of r.
              call stdlib_sormbr( 'P', 'L', 'N', n, nrhs, n, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m, wlalsd ) ) &
                     then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm.
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs, 4_ilp*m+m*lda+&
                        wlalsd ) )ldwork = lda
              itau = 1_ilp
              nwork = m + 1_ilp
              ! compute a=l*q.
              ! (workspace: need 2*m, prefer m+m*nb)
              call stdlib_sgelqf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, info )
                        
              il = nwork
              ! copy l to work(il), zeroing out above its diagonal.
              call stdlib_slacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_slaset( 'U', m-1, m-1, zero, zero, work( il+ldwork ),ldwork )
              ie = il + ldwork*m
              itauq = ie + m
              itaup = itauq + m
              nwork = itaup + m
              ! bidiagonalize l in work(il).
              ! (workspace: need m*m+5*m, prefer m*m+4*m+2*m*nb)
              call stdlib_sgebrd( m, m, work( il ), ldwork, s, work( ie ),work( itauq ), work( &
                        itaup ), work( nwork ),lwork-nwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l.
              ! (workspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_slalsd( 'U', smlsiz, m, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of l.
              call stdlib_sormbr( 'P', 'L', 'N', m, nrhs, m, work( il ), ldwork,work( itaup ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! zero out below first m rows of b.
              call stdlib_slaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
              nwork = itau + m
              ! multiply transpose(q) by b.
              ! (workspace: need m+nrhs, prefer m+nrhs*nb)
              call stdlib_sormlq( 'L', 'T', n, nrhs, m, a, lda, work( itau ), b,ldb, work( nwork )&
                        , lwork-nwork+1, info )
           else
              ! path 2 - remaining underdetermined cases.
              ie = 1_ilp
              itauq = ie + m
              itaup = itauq + m
              nwork = itaup + m
              ! bidiagonalize a.
              ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
              call stdlib_sgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work( &
                        nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors.
              ! (workspace: need 3*m+nrhs, prefer 3*m+nrhs*nb)
              call stdlib_sormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_slalsd( 'L', smlsiz, m, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of a.
              call stdlib_sormbr( 'P', 'L', 'N', n, nrhs, m, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           end if
           ! undo scaling.
           if( iascl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           10 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwork
           return
     end subroutine stdlib_sgelsd

     module subroutine stdlib_dgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, iwork, &
     !! DGELSD computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize 2-norm(| b - A*x |)
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The problem is solved in three steps:
     !! (1) Reduce the coefficient matrix A to bidiagonal form with
     !! Householder transformations, reducing the original problem
     !! into a "bidiagonal least squares problem" (BLS)
     !! (2) Solve the BLS using a divide and conquer approach.
     !! (3) Apply back all the Householder transformations to solve
     !! the original least squares problem.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
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
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: s(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iascl, ibscl, ie, il, itau, itaup, itauq, ldwork, liwork, maxmn, &
                     maxwrk, minmn, minwrk, mm, mnthr, nlvl, nwork, smlsiz, wlalsd
           real(dp) :: anrm, bignum, bnrm, eps, sfmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           mnthr = stdlib_ilaenv( 6_ilp, 'DGELSD', ' ', m, n, nrhs, -1_ilp )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           smlsiz = stdlib_ilaenv( 9_ilp, 'DGELSD', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
           ! compute workspace.
           ! (note: comments in the code beginning "workspace:" describe the
           ! minimal amount of workspace needed at that point in the code,
           ! as well as the preferred amount for good performance.
           ! nb refers to the optimal block size for the immediately
           ! following subroutine, as returned by stdlib_ilaenv.)
           minwrk = 1_ilp
           liwork = 1_ilp
           minmn = max( 1_ilp, minmn )
           nlvl = max( int( log( real( minmn,KIND=dp) / real( smlsiz+1,KIND=dp) ) /log( two ),&
                     KIND=ilp) + 1_ilp, 0_ilp )
           if( info==0_ilp ) then
              maxwrk = 0_ilp
              liwork = 3_ilp*minmn*nlvl + 11_ilp*minmn
              mm = m
              if( m>=n .and. m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns.
                 mm = n
                 maxwrk = max( maxwrk, n+n*stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n,-1_ilp, -1_ilp ) )
                           
                 maxwrk = max( maxwrk, n+nrhs*stdlib_ilaenv( 1_ilp, 'DORMQR', 'LT', m, nrhs, n, -1_ilp ) )
                           
              end if
              if( m>=n ) then
                 ! path 1 - overdetermined or exactly determined.
                 maxwrk = max( maxwrk, 3_ilp*n+( mm+n )*stdlib_ilaenv( 1_ilp, 'DGEBRD', ' ', mm, n, -1_ilp, -&
                           1_ilp ) )
                 maxwrk = max( maxwrk, 3_ilp*n+nrhs*stdlib_ilaenv( 1_ilp, 'DORMBR', 'QLT', mm, nrhs, n, -&
                           1_ilp ) )
                 maxwrk = max( maxwrk, 3_ilp*n+( n-1 )*stdlib_ilaenv( 1_ilp, 'DORMBR', 'PLN', n, nrhs, n, &
                           -1_ilp ) )
                 wlalsd = 9_ilp*n+2*n*smlsiz+8*n*nlvl+n*nrhs+(smlsiz+1)**2_ilp
                 maxwrk = max( maxwrk, 3_ilp*n+wlalsd )
                 minwrk = max( 3_ilp*n+mm, 3_ilp*n+nrhs, 3_ilp*n+wlalsd )
              end if
              if( n>m ) then
                 wlalsd = 9_ilp*m+2*m*smlsiz+8*m*nlvl+m*nrhs+(smlsiz+1)**2_ilp
                 if( n>=mnthr ) then
                    ! path 2a - underdetermined, with many more columns
                    ! than rows.
                    maxwrk = m + m*stdlib_ilaenv( 1_ilp, 'DGELQF', ' ', m, n, -1_ilp, -1_ilp )
                    maxwrk = max( maxwrk, m*m+4*m+2*m*stdlib_ilaenv( 1_ilp, 'DGEBRD', ' ', m, m, -1_ilp, -&
                              1_ilp ) )
                    maxwrk = max( maxwrk, m*m+4*m+nrhs*stdlib_ilaenv( 1_ilp, 'DORMBR', 'QLT', m, nrhs,&
                               m, -1_ilp ) )
                    maxwrk = max( maxwrk, m*m+4*m+( m-1 )*stdlib_ilaenv( 1_ilp, 'DORMBR', 'PLN', m, &
                              nrhs, m, -1_ilp ) )
                    if( nrhs>1_ilp ) then
                       maxwrk = max( maxwrk, m*m+m+m*nrhs )
                    else
                       maxwrk = max( maxwrk, m*m+2*m )
                    end if
                    maxwrk = max( maxwrk, m+nrhs*stdlib_ilaenv( 1_ilp, 'DORMLQ', 'LT', n, nrhs, m, -1_ilp &
                              ) )
                    maxwrk = max( maxwrk, m*m+4*m+wlalsd )
           ! xxx: ensure the path 2a case below is triggered.  the workspace
           ! calculation should use queries for all routines eventually.
                    maxwrk = max( maxwrk,4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) )
                 else
                    ! path 2 - remaining underdetermined cases.
                    maxwrk = 3_ilp*m + ( n+m )*stdlib_ilaenv( 1_ilp, 'DGEBRD', ' ', m, n,-1_ilp, -1_ilp )
                    maxwrk = max( maxwrk, 3_ilp*m+nrhs*stdlib_ilaenv( 1_ilp, 'DORMBR', 'QLT', m, nrhs, n, &
                              -1_ilp ) )
                    maxwrk = max( maxwrk, 3_ilp*m+m*stdlib_ilaenv( 1_ilp, 'DORMBR', 'PLN', n, nrhs, m, -1_ilp &
                              ) )
                    maxwrk = max( maxwrk, 3_ilp*m+wlalsd )
                 end if
                 minwrk = max( 3_ilp*m+nrhs, 3_ilp*m+m, 3_ilp*m+wlalsd )
              end if
              minwrk = min( minwrk, maxwrk )
              work( 1_ilp ) = maxwrk
              iwork( 1_ilp ) = liwork
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELSD', -info )
              return
           else if( lquery ) then
              go to 10
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters.
           eps = stdlib_dlamch( 'P' )
           sfmin = stdlib_dlamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a if max entry outside range [smlnum,bignum].
           anrm = stdlib_dlange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_dlaset( 'F', max( m, n ), nrhs, zero, zero, b, ldb )
              call stdlib_dlaset( 'F', minmn, 1_ilp, zero, zero, s, 1_ilp )
              rank = 0_ilp
              go to 10
           end if
           ! scale b if max entry outside range [smlnum,bignum].
           bnrm = stdlib_dlange( 'M', m, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! if m < n make sure certain entries of b are zero.
           if( m<n )call stdlib_dlaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
           ! overdetermined case.
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined.
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns.
                 mm = n
                 itau = 1_ilp
                 nwork = itau + n
                 ! compute a=q*r.
                 ! (workspace: need 2*n, prefer n+n*nb)
                 call stdlib_dgeqrf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, &
                           info )
                 ! multiply b by transpose(q).
                 ! (workspace: need n+nrhs, prefer n+nrhs*nb)
                 call stdlib_dormqr( 'L', 'T', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           nwork ), lwork-nwork+1, info )
                 ! zero out below r.
                 if( n>1_ilp ) then
                    call stdlib_dlaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ), lda )
                 end if
              end if
              ie = 1_ilp
              itauq = ie + n
              itaup = itauq + n
              nwork = itaup + n
              ! bidiagonalize r in a.
              ! (workspace: need 3*n+mm, prefer 3*n+(mm+n)*nb)
              call stdlib_dgebrd( mm, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work(&
                         nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r.
              ! (workspace: need 3*n+nrhs, prefer 3*n+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_dlalsd( 'U', smlsiz, n, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of r.
              call stdlib_dormbr( 'P', 'L', 'N', n, nrhs, n, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m, wlalsd ) ) &
                     then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm.
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs, 4_ilp*m+m*lda+&
                        wlalsd ) )ldwork = lda
              itau = 1_ilp
              nwork = m + 1_ilp
              ! compute a=l*q.
              ! (workspace: need 2*m, prefer m+m*nb)
              call stdlib_dgelqf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, info )
                        
              il = nwork
              ! copy l to work(il), zeroing out above its diagonal.
              call stdlib_dlacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_dlaset( 'U', m-1, m-1, zero, zero, work( il+ldwork ),ldwork )
              ie = il + ldwork*m
              itauq = ie + m
              itaup = itauq + m
              nwork = itaup + m
              ! bidiagonalize l in work(il).
              ! (workspace: need m*m+5*m, prefer m*m+4*m+2*m*nb)
              call stdlib_dgebrd( m, m, work( il ), ldwork, s, work( ie ),work( itauq ), work( &
                        itaup ), work( nwork ),lwork-nwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l.
              ! (workspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_dlalsd( 'U', smlsiz, m, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of l.
              call stdlib_dormbr( 'P', 'L', 'N', m, nrhs, m, work( il ), ldwork,work( itaup ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! zero out below first m rows of b.
              call stdlib_dlaset( 'F', n-m, nrhs, zero, zero, b( m+1, 1_ilp ), ldb )
              nwork = itau + m
              ! multiply transpose(q) by b.
              ! (workspace: need m+nrhs, prefer m+nrhs*nb)
              call stdlib_dormlq( 'L', 'T', n, nrhs, m, a, lda, work( itau ), b,ldb, work( nwork )&
                        , lwork-nwork+1, info )
           else
              ! path 2 - remaining underdetermined cases.
              ie = 1_ilp
              itauq = ie + m
              itaup = itauq + m
              nwork = itaup + m
              ! bidiagonalize a.
              ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
              call stdlib_dgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), work( &
                        nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors.
              ! (workspace: need 3*m+nrhs, prefer 3*m+nrhs*nb)
              call stdlib_dormbr( 'Q', 'L', 'T', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_dlalsd( 'L', smlsiz, m, nrhs, s, work( ie ), b, ldb,rcond, rank, work( &
                        nwork ), iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of a.
              call stdlib_dormbr( 'P', 'L', 'N', n, nrhs, m, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           end if
           ! undo scaling.
           if( iascl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           10 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwork
           return
     end subroutine stdlib_dgelsd


     module subroutine stdlib_cgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
     !! CGELSD computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize 2-norm(| b - A*x |)
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The problem is solved in three steps:
     !! (1) Reduce the coefficient matrix A to bidiagonal form with
     !! Householder transformations, reducing the original problem
     !! into a "bidiagonal least squares problem" (BLS)
     !! (2) Solve the BLS using a divide and conquer approach.
     !! (3) Apply back all the Householder transformations to solve
     !! the original least squares problem.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iascl, ibscl, ie, il, itau, itaup, itauq, ldwork, liwork, lrwork, &
                     maxmn, maxwrk, minmn, minwrk, mm, mnthr, nlvl, nrwork, nwork, smlsiz
           real(sp) :: anrm, bignum, bnrm, eps, sfmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace.
           ! (note: comments in the code beginning "workspace:" describe the
           ! minimal amount of workspace needed at that point in the code,
           ! as well as the preferred amount for good performance.
           ! nb refers to the optimal block size for the immediately
           ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              liwork = 1_ilp
              lrwork = 1_ilp
              if( minmn>0_ilp ) then
                 smlsiz = stdlib_ilaenv( 9_ilp, 'CGELSD', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
                 mnthr = stdlib_ilaenv( 6_ilp, 'CGELSD', ' ', m, n, nrhs, -1_ilp )
                 nlvl = max( int( log( real( minmn,KIND=sp) / real( smlsiz + 1_ilp,KIND=sp) ) /log( &
                           two ),KIND=ilp) + 1_ilp, 0_ilp )
                 liwork = 3_ilp*minmn*nlvl + 11_ilp*minmn
                 mm = m
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns.
                    mm = n
                    maxwrk = max( maxwrk, n*stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n,-1_ilp, -1_ilp ) )
                              
                    maxwrk = max( maxwrk, nrhs*stdlib_ilaenv( 1_ilp, 'CUNMQR', 'LC', m,nrhs, n, -1_ilp ) )
                              
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined.
                    lrwork = 10_ilp*n + 2_ilp*n*smlsiz + 8_ilp*n*nlvl + 3_ilp*smlsiz*nrhs +max( (smlsiz+1)**2_ilp, n*(&
                              1_ilp+nrhs) + 2_ilp*nrhs )
                    maxwrk = max( maxwrk, 2_ilp*n + ( mm + n )*stdlib_ilaenv( 1_ilp,'CGEBRD', ' ', mm, n, &
                              -1_ilp, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + nrhs*stdlib_ilaenv( 1_ilp, 'CUNMBR','QLC', mm, nrhs, &
                              n, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'CUNMBR', 'PLN', n, &
                              nrhs, n, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + n*nrhs )
                    minwrk = max( 2_ilp*n + mm, 2_ilp*n + n*nrhs )
                 end if
                 if( n>m ) then
                    lrwork = 10_ilp*m + 2_ilp*m*smlsiz + 8_ilp*m*nlvl + 3_ilp*smlsiz*nrhs +max( (smlsiz+1)**2_ilp, n*(&
                              1_ilp+nrhs) + 2_ilp*nrhs )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                                 ! than rows.
                       maxwrk = m + m*stdlib_ilaenv( 1_ilp, 'CGELQF', ' ', m, n, -1_ilp,-1_ilp )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + 2_ilp*m*stdlib_ilaenv( 1_ilp,'CGEBRD', ' ', m, m,&
                                  -1_ilp, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + nrhs*stdlib_ilaenv( 1_ilp,'CUNMBR', 'QLC', m,&
                                  nrhs, m, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + ( m - 1_ilp )*stdlib_ilaenv( 1_ilp,'CUNMLQ', &
                                 'LC', n, nrhs, m, -1_ilp ) )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + m*nrhs )
           ! xxx: ensure the path 2a case below is triggered.  the workspace
           ! calculation should use queries for all routines eventually.
                       maxwrk = max( maxwrk,4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) )
                    else
                       ! path 2 - underdetermined.
                       maxwrk = 2_ilp*m + ( n + m )*stdlib_ilaenv( 1_ilp, 'CGEBRD', ' ', m,n, -1_ilp, -1_ilp )
                                 
                       maxwrk = max( maxwrk, 2_ilp*m + nrhs*stdlib_ilaenv( 1_ilp, 'CUNMBR','QLC', m, nrhs,&
                                  m, -1_ilp ) )
                       maxwrk = max( maxwrk, 2_ilp*m + m*stdlib_ilaenv( 1_ilp, 'CUNMBR','PLN', n, nrhs, m,&
                                  -1_ilp ) )
                       maxwrk = max( maxwrk, 2_ilp*m + m*nrhs )
                    end if
                    minwrk = max( 2_ilp*m + n, 2_ilp*m + m*nrhs )
                 end if
              end if
              minwrk = min( minwrk, maxwrk )
              work( 1_ilp ) = maxwrk
              iwork( 1_ilp ) = liwork
              rwork( 1_ilp ) = lrwork
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters.
           eps = stdlib_slamch( 'P' )
           sfmin = stdlib_slamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! scale a if max entry outside range [smlnum,bignum].
           anrm = stdlib_clange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_claset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              call stdlib_slaset( 'F', minmn, 1_ilp, zero, zero, s, 1_ilp )
              rank = 0_ilp
              go to 10
           end if
           ! scale b if max entry outside range [smlnum,bignum].
           bnrm = stdlib_clange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! if m < n make sure b(m+1:n,:) = 0
           if( m<n )call stdlib_claset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
           ! overdetermined case.
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined.
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 nwork = itau + n
                 ! compute a=q*r.
                 ! (rworkspace: need n)
                 ! (cworkspace: need n, prefer n*nb)
                 call stdlib_cgeqrf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, &
                           info )
                 ! multiply b by transpose(q).
                 ! (rworkspace: need n)
                 ! (cworkspace: need nrhs, prefer nrhs*nb)
                 call stdlib_cunmqr( 'L', 'C', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           nwork ), lwork-nwork+1, info )
                 ! zero out below r.
                 if( n>1_ilp ) then
                    call stdlib_claset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
                 end if
              end if
              itauq = 1_ilp
              itaup = itauq + n
              nwork = itaup + n
              ie = 1_ilp
              nrwork = ie + n
              ! bidiagonalize r in a.
              ! (rworkspace: need n)
              ! (cworkspace: need 2*n+mm, prefer 2*n+(mm+n)*nb)
              call stdlib_cgebrd( mm, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                        work( nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r.
              ! (cworkspace: need 2*n+nrhs, prefer 2*n+nrhs*nb)
              call stdlib_cunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_clalsd( 'U', smlsiz, n, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of r.
              call stdlib_cunmbr( 'P', 'L', 'N', n, nrhs, n, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) ) then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm.
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs ) )ldwork = &
                        lda
              itau = 1_ilp
              nwork = m + 1_ilp
              ! compute a=l*q.
              ! (cworkspace: need 2*m, prefer m+m*nb)
              call stdlib_cgelqf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, info )
                        
              il = nwork
              ! copy l to work(il), zeroing out above its diagonal.
              call stdlib_clacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_claset( 'U', m-1, m-1, czero, czero, work( il+ldwork ),ldwork )
              itauq = il + ldwork*m
              itaup = itauq + m
              nwork = itaup + m
              ie = 1_ilp
              nrwork = ie + m
              ! bidiagonalize l in work(il).
              ! (rworkspace: need m)
              ! (cworkspace: need m*m+4*m, prefer m*m+4*m+2*m*nb)
              call stdlib_cgebrd( m, m, work( il ), ldwork, s, rwork( ie ),work( itauq ), work( &
                        itaup ), work( nwork ),lwork-nwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l.
              ! (cworkspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_clalsd( 'U', smlsiz, m, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of l.
              call stdlib_cunmbr( 'P', 'L', 'N', m, nrhs, m, work( il ), ldwork,work( itaup ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! zero out below first m rows of b.
              call stdlib_claset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
              nwork = itau + m
              ! multiply transpose(q) by b.
              ! (cworkspace: need nrhs, prefer nrhs*nb)
              call stdlib_cunmlq( 'L', 'C', n, nrhs, m, a, lda, work( itau ), b,ldb, work( nwork )&
                        , lwork-nwork+1, info )
           else
              ! path 2 - remaining underdetermined cases.
              itauq = 1_ilp
              itaup = itauq + m
              nwork = itaup + m
              ie = 1_ilp
              nrwork = ie + m
              ! bidiagonalize a.
              ! (rworkspace: need m)
              ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
              call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), work(&
                         nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors.
              ! (cworkspace: need 2*m+nrhs, prefer 2*m+nrhs*nb)
              call stdlib_cunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_clalsd( 'L', smlsiz, m, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of a.
              call stdlib_cunmbr( 'P', 'L', 'N', n, nrhs, m, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           end if
           ! undo scaling.
           if( iascl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           10 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwork
           rwork( 1_ilp ) = lrwork
           return
     end subroutine stdlib_cgelsd

     module subroutine stdlib_zgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
     !! ZGELSD computes the minimum-norm solution to a real linear least
     !! squares problem:
     !! minimize 2-norm(| b - A*x |)
     !! using the singular value decomposition (SVD) of A. A is an M-by-N
     !! matrix which may be rank-deficient.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
     !! The problem is solved in three steps:
     !! (1) Reduce the coefficient matrix A to bidiagonal form with
     !! Householder transformations, reducing the original problem
     !! into a "bidiagonal least squares problem" (BLS)
     !! (2) Solve the BLS using a divide and conquer approach.
     !! (3) Apply back all the Householder transformations to solve
     !! the original least squares problem.
     !! The effective rank of A is determined by treating as zero those
     !! singular values which are less than RCOND times the largest singular
     !! value.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iascl, ibscl, ie, il, itau, itaup, itauq, ldwork, liwork, lrwork, &
                     maxmn, maxwrk, minmn, minwrk, mm, mnthr, nlvl, nrwork, nwork, smlsiz
           real(dp) :: anrm, bignum, bnrm, eps, sfmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           minmn = min( m, n )
           maxmn = max( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, maxmn ) ) then
              info = -7_ilp
           end if
           ! compute workspace.
           ! (note: comments in the code beginning "workspace:" describe the
           ! minimal amount of workspace needed at that point in the code,
           ! as well as the preferred amount for good performance.
           ! nb refers to the optimal block size for the immediately
           ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              liwork = 1_ilp
              lrwork = 1_ilp
              if( minmn>0_ilp ) then
                 smlsiz = stdlib_ilaenv( 9_ilp, 'ZGELSD', ' ', 0_ilp, 0_ilp, 0_ilp, 0_ilp )
                 mnthr = stdlib_ilaenv( 6_ilp, 'ZGELSD', ' ', m, n, nrhs, -1_ilp )
                 nlvl = max( int( log( real( minmn,KIND=dp) / real( smlsiz + 1_ilp,KIND=dp) ) /log( &
                           two ),KIND=ilp) + 1_ilp, 0_ilp )
                 liwork = 3_ilp*minmn*nlvl + 11_ilp*minmn
                 mm = m
                 if( m>=n .and. m>=mnthr ) then
                    ! path 1a - overdetermined, with many more rows than
                              ! columns.
                    mm = n
                    maxwrk = max( maxwrk, n*stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n,-1_ilp, -1_ilp ) )
                              
                    maxwrk = max( maxwrk, nrhs*stdlib_ilaenv( 1_ilp, 'ZUNMQR', 'LC', m,nrhs, n, -1_ilp ) )
                              
                 end if
                 if( m>=n ) then
                    ! path 1 - overdetermined or exactly determined.
                    lrwork = 10_ilp*n + 2_ilp*n*smlsiz + 8_ilp*n*nlvl + 3_ilp*smlsiz*nrhs +max( (smlsiz+1)**2_ilp, n*(&
                              1_ilp+nrhs) + 2_ilp*nrhs )
                    maxwrk = max( maxwrk, 2_ilp*n + ( mm + n )*stdlib_ilaenv( 1_ilp,'ZGEBRD', ' ', mm, n, &
                              -1_ilp, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + nrhs*stdlib_ilaenv( 1_ilp, 'ZUNMBR','QLC', mm, nrhs, &
                              n, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + ( n - 1_ilp )*stdlib_ilaenv( 1_ilp,'ZUNMBR', 'PLN', n, &
                              nrhs, n, -1_ilp ) )
                    maxwrk = max( maxwrk, 2_ilp*n + n*nrhs )
                    minwrk = max( 2_ilp*n + mm, 2_ilp*n + n*nrhs )
                 end if
                 if( n>m ) then
                    lrwork = 10_ilp*m + 2_ilp*m*smlsiz + 8_ilp*m*nlvl + 3_ilp*smlsiz*nrhs +max( (smlsiz+1)**2_ilp, n*(&
                              1_ilp+nrhs) + 2_ilp*nrhs )
                    if( n>=mnthr ) then
                       ! path 2a - underdetermined, with many more columns
                                 ! than rows.
                       maxwrk = m + m*stdlib_ilaenv( 1_ilp, 'ZGELQF', ' ', m, n, -1_ilp,-1_ilp )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + 2_ilp*m*stdlib_ilaenv( 1_ilp,'ZGEBRD', ' ', m, m,&
                                  -1_ilp, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + nrhs*stdlib_ilaenv( 1_ilp,'ZUNMBR', 'QLC', m,&
                                  nrhs, m, -1_ilp ) )
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + ( m - 1_ilp )*stdlib_ilaenv( 1_ilp,'ZUNMLQ', &
                                 'LC', n, nrhs, m, -1_ilp ) )
                       if( nrhs>1_ilp ) then
                          maxwrk = max( maxwrk, m*m + m + m*nrhs )
                       else
                          maxwrk = max( maxwrk, m*m + 2_ilp*m )
                       end if
                       maxwrk = max( maxwrk, m*m + 4_ilp*m + m*nrhs )
           ! xxx: ensure the path 2a case below is triggered.  the workspace
           ! calculation should use queries for all routines eventually.
                       maxwrk = max( maxwrk,4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) )
                    else
                       ! path 2 - underdetermined.
                       maxwrk = 2_ilp*m + ( n + m )*stdlib_ilaenv( 1_ilp, 'ZGEBRD', ' ', m,n, -1_ilp, -1_ilp )
                                 
                       maxwrk = max( maxwrk, 2_ilp*m + nrhs*stdlib_ilaenv( 1_ilp, 'ZUNMBR','QLC', m, nrhs,&
                                  m, -1_ilp ) )
                       maxwrk = max( maxwrk, 2_ilp*m + m*stdlib_ilaenv( 1_ilp, 'ZUNMBR','PLN', n, nrhs, m,&
                                  -1_ilp ) )
                       maxwrk = max( maxwrk, 2_ilp*m + m*nrhs )
                    end if
                    minwrk = max( 2_ilp*m + n, 2_ilp*m + m*nrhs )
                 end if
              end if
              minwrk = min( minwrk, maxwrk )
              work( 1_ilp ) = maxwrk
              iwork( 1_ilp ) = liwork
              rwork( 1_ilp ) = lrwork
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rank = 0_ilp
              return
           end if
           ! get machine parameters.
           eps = stdlib_dlamch( 'P' )
           sfmin = stdlib_dlamch( 'S' )
           smlnum = sfmin / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! scale a if max entry outside range [smlnum,bignum].
           anrm = stdlib_zlange( 'M', m, n, a, lda, rwork )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_zlaset( 'F', max( m, n ), nrhs, czero, czero, b, ldb )
              call stdlib_dlaset( 'F', minmn, 1_ilp, zero, zero, s, 1_ilp )
              rank = 0_ilp
              go to 10
           end if
           ! scale b if max entry outside range [smlnum,bignum].
           bnrm = stdlib_zlange( 'M', m, nrhs, b, ldb, rwork )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum.
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, m, nrhs, b, ldb, info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum.
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, m, nrhs, b, ldb, info )
              ibscl = 2_ilp
           end if
           ! if m < n make sure b(m+1:n,:) = 0
           if( m<n )call stdlib_zlaset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
           ! overdetermined case.
           if( m>=n ) then
              ! path 1 - overdetermined or exactly determined.
              mm = m
              if( m>=mnthr ) then
                 ! path 1a - overdetermined, with many more rows than columns
                 mm = n
                 itau = 1_ilp
                 nwork = itau + n
                 ! compute a=q*r.
                 ! (rworkspace: need n)
                 ! (cworkspace: need n, prefer n*nb)
                 call stdlib_zgeqrf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, &
                           info )
                 ! multiply b by transpose(q).
                 ! (rworkspace: need n)
                 ! (cworkspace: need nrhs, prefer nrhs*nb)
                 call stdlib_zunmqr( 'L', 'C', m, nrhs, n, a, lda, work( itau ), b,ldb, work( &
                           nwork ), lwork-nwork+1, info )
                 ! zero out below r.
                 if( n>1_ilp ) then
                    call stdlib_zlaset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
                 end if
              end if
              itauq = 1_ilp
              itaup = itauq + n
              nwork = itaup + n
              ie = 1_ilp
              nrwork = ie + n
              ! bidiagonalize r in a.
              ! (rworkspace: need n)
              ! (cworkspace: need 2*n+mm, prefer 2*n+(mm+n)*nb)
              call stdlib_zgebrd( mm, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                        work( nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors of r.
              ! (cworkspace: need 2*n+nrhs, prefer 2*n+nrhs*nb)
              call stdlib_zunmbr( 'Q', 'L', 'C', mm, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_zlalsd( 'U', smlsiz, n, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of r.
              call stdlib_zunmbr( 'P', 'L', 'N', n, nrhs, n, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           else if( n>=mnthr .and. lwork>=4_ilp*m+m*m+max( m, 2_ilp*m-4, nrhs, n-3*m ) ) then
              ! path 2a - underdetermined, with many more columns than rows
              ! and sufficient workspace for an efficient algorithm.
              ldwork = m
              if( lwork>=max( 4_ilp*m+m*lda+max( m, 2_ilp*m-4, nrhs, n-3*m ),m*lda+m+m*nrhs ) )ldwork = &
                        lda
              itau = 1_ilp
              nwork = m + 1_ilp
              ! compute a=l*q.
              ! (cworkspace: need 2*m, prefer m+m*nb)
              call stdlib_zgelqf( m, n, a, lda, work( itau ), work( nwork ),lwork-nwork+1, info )
                        
              il = nwork
              ! copy l to work(il), zeroing out above its diagonal.
              call stdlib_zlacpy( 'L', m, m, a, lda, work( il ), ldwork )
              call stdlib_zlaset( 'U', m-1, m-1, czero, czero, work( il+ldwork ),ldwork )
              itauq = il + ldwork*m
              itaup = itauq + m
              nwork = itaup + m
              ie = 1_ilp
              nrwork = ie + m
              ! bidiagonalize l in work(il).
              ! (rworkspace: need m)
              ! (cworkspace: need m*m+4*m, prefer m*m+4*m+2*m*nb)
              call stdlib_zgebrd( m, m, work( il ), ldwork, s, rwork( ie ),work( itauq ), work( &
                        itaup ), work( nwork ),lwork-nwork+1, info )
              ! multiply b by transpose of left bidiagonalizing vectors of l.
              ! (cworkspace: need m*m+4*m+nrhs, prefer m*m+4*m+nrhs*nb)
              call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, m, work( il ), ldwork,work( itauq ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_zlalsd( 'U', smlsiz, m, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of l.
              call stdlib_zunmbr( 'P', 'L', 'N', m, nrhs, m, work( il ), ldwork,work( itaup ), b, &
                        ldb, work( nwork ),lwork-nwork+1, info )
              ! zero out below first m rows of b.
              call stdlib_zlaset( 'F', n-m, nrhs, czero, czero, b( m+1, 1_ilp ), ldb )
              nwork = itau + m
              ! multiply transpose(q) by b.
              ! (cworkspace: need nrhs, prefer nrhs*nb)
              call stdlib_zunmlq( 'L', 'C', n, nrhs, m, a, lda, work( itau ), b,ldb, work( nwork )&
                        , lwork-nwork+1, info )
           else
              ! path 2 - remaining underdetermined cases.
              itauq = 1_ilp
              itaup = itauq + m
              nwork = itaup + m
              ie = 1_ilp
              nrwork = ie + m
              ! bidiagonalize a.
              ! (rworkspace: need m)
              ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
              call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), work(&
                         nwork ), lwork-nwork+1,info )
              ! multiply b by transpose of left bidiagonalizing vectors.
              ! (cworkspace: need 2*m+nrhs, prefer 2*m+nrhs*nb)
              call stdlib_zunmbr( 'Q', 'L', 'C', m, nrhs, n, a, lda, work( itauq ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
              ! solve the bidiagonal least squares problem.
              call stdlib_zlalsd( 'L', smlsiz, m, nrhs, s, rwork( ie ), b, ldb,rcond, rank, work( &
                        nwork ), rwork( nrwork ),iwork, info )
              if( info/=0_ilp ) then
                 go to 10
              end if
              ! multiply b by right bidiagonalizing vectors of a.
              call stdlib_zunmbr( 'P', 'L', 'N', n, nrhs, m, a, lda, work( itaup ),b, ldb, work( &
                        nwork ), lwork-nwork+1, info )
           end if
           ! undo scaling.
           if( iascl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,info )
           else if( iascl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, n, nrhs, b, ldb, info )
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,info )
           end if
           if( ibscl==1_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, n, nrhs, b, ldb, info )
           else if( ibscl==2_ilp ) then
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, n, nrhs, b, ldb, info )
           end if
           10 continue
           work( 1_ilp ) = maxwrk
           iwork( 1_ilp ) = liwork
           rwork( 1_ilp ) = lrwork
           return
     end subroutine stdlib_zgelsd




     module subroutine stdlib_sgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
     !! SGETSLS solves overdetermined or underdetermined real linear systems
     !! involving an M-by-N matrix A, using a tall skinny QR or short wide LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
     !! an undetermined system A**T * X = B.
     !! 4. If TRANS = 'T' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, tran
           integer(ilp) :: i, iascl, ibscl, j, maxmn, brow, scllen, tszo, tszm, lwo, lwm, lw1, &
                     lw2, wsizeo, wsizem, info2
           real(sp) :: anrm, bignum, bnrm, smlnum, tq(5_ilp), workq(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           maxmn = max( m, n )
           tran  = stdlib_lsame( trans, 'T' )
           lquery = ( lwork==-1_ilp .or. lwork==-2_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or.stdlib_lsame( trans, 'T' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
           ! determine the optimum and minimum lwork
            if( m>=n ) then
              call stdlib_sgeqr( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_sgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_sgeqr( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_sgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            else
              call stdlib_sgelq( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_sgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_sgelq( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_sgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm  = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            end if
            if( ( lwork<wsizem ).and.( .not.lquery ) ) then
               info = -10_ilp
            end if
            work( 1_ilp ) = real( wsizeo,KIND=sp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SGETSLS', -info )
             return
           end if
           if( lquery ) then
             if( lwork==-2_ilp ) work( 1_ilp ) = real( wsizem,KIND=sp)
             return
           end if
           if( lwork<wsizeo ) then
             lw1 = tszm
             lw2 = lwm
           else
             lw1 = tszo
             lw2 = lwo
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
                call stdlib_slaset( 'FULL', max( m, n ), nrhs, zero, zero,b, ldb )
                return
           end if
           ! get machine parameters
            smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
            bignum = one / smlnum
            call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_slaset( 'F', maxmn, nrhs, zero, zero, b, ldb )
              go to 50
           end if
           brow = m
           if ( tran ) then
             brow = n
           end if
           bnrm = stdlib_slange( 'M', brow, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if ( m>=n ) then
              ! compute qr factorization of a
             call stdlib_sgeqr( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
             if ( .not.tran ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
               call stdlib_sgemqr( 'L' , 'T', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, work(&
                          1_ilp ), lw2,info )
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
               call stdlib_strtrs( 'U', 'N', 'N', n, nrhs,a, lda, b, ldb, info )
               if( info>0_ilp ) then
                 return
               end if
               scllen = n
             else
                 ! overdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_strtrs( 'U', 'T', 'N', n, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_sgemqr( 'L', 'N', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_sgelq( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
              ! workspace at least m, optimally m*nb.
              if( .not.tran ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_strtrs( 'L', 'N', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_sgemlq( 'L', 'T', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_sgemlq( 'L', 'N', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_strtrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
             call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
             call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
             call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
             call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( tszo + lwo,KIND=sp)
           return
     end subroutine stdlib_sgetsls

     module subroutine stdlib_dgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
     !! DGETSLS solves overdetermined or underdetermined real linear systems
     !! involving an M-by-N matrix A, using a tall skinny QR or short wide LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
     !! an undetermined system A**T * X = B.
     !! 4. If TRANS = 'T' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, tran
           integer(ilp) :: i, iascl, ibscl, j, maxmn, brow, scllen, tszo, tszm, lwo, lwm, lw1, &
                     lw2, wsizeo, wsizem, info2
           real(dp) :: anrm, bignum, bnrm, smlnum, tq(5_ilp), workq(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           maxmn = max( m, n )
           tran  = stdlib_lsame( trans, 'T' )
           lquery = ( lwork==-1_ilp .or. lwork==-2_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or.stdlib_lsame( trans, 'T' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
           ! determine the optimum and minimum lwork
            if( m>=n ) then
              call stdlib_dgeqr( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_dgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_dgeqr( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_dgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            else
              call stdlib_dgelq( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_dgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_dgelq( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_dgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm  = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            end if
            if( ( lwork<wsizem ).and.( .not.lquery ) ) then
               info = -10_ilp
            end if
            work( 1_ilp ) = real( wsizeo,KIND=dp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DGETSLS', -info )
             return
           end if
           if( lquery ) then
             if( lwork==-2_ilp ) work( 1_ilp ) = real( wsizem,KIND=dp)
             return
           end if
           if( lwork<wsizeo ) then
             lw1 = tszm
             lw2 = lwm
           else
             lw1 = tszo
             lw2 = lwo
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
                call stdlib_dlaset( 'FULL', max( m, n ), nrhs, zero, zero,b, ldb )
                return
           end if
           ! get machine parameters
            smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
            bignum = one / smlnum
            call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', m, n, a, lda, work )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_dlaset( 'F', maxmn, nrhs, zero, zero, b, ldb )
              go to 50
           end if
           brow = m
           if ( tran ) then
             brow = n
           end if
           bnrm = stdlib_dlange( 'M', brow, nrhs, b, ldb, work )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if ( m>=n ) then
              ! compute qr factorization of a
             call stdlib_dgeqr( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
             if ( .not.tran ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
               call stdlib_dgemqr( 'L' , 'T', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, work(&
                          1_ilp ), lw2,info )
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
               call stdlib_dtrtrs( 'U', 'N', 'N', n, nrhs,a, lda, b, ldb, info )
               if( info>0_ilp ) then
                 return
               end if
               scllen = n
             else
                 ! overdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_dtrtrs( 'U', 'T', 'N', n, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = zero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_dgemqr( 'L', 'N', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_dgelq( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
              ! workspace at least m, optimally m*nb.
              if( .not.tran ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_dtrtrs( 'L', 'N', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = zero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_dgemlq( 'L', 'T', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_dgemlq( 'L', 'N', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_dtrtrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', m, nrhs,a, lda, b, ldb, &
                           info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
             call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
             call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
             call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
             call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( tszo + lwo,KIND=dp)
           return
     end subroutine stdlib_dgetsls


     module subroutine stdlib_cgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
     !! CGETSLS solves overdetermined or underdetermined complex linear systems
     !! involving an M-by-N matrix A, using a tall skinny QR or short wide LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
     !! an undetermined system A**T * X = B.
     !! 4. If TRANS = 'C' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, tran
           integer(ilp) :: i, iascl, ibscl, j, maxmn, brow, scllen, tszo, tszm, lwo, lwm, lw1, &
                     lw2, wsizeo, wsizem, info2
           real(sp) :: anrm, bignum, bnrm, smlnum, dum(1_ilp)
           complex(sp) :: tq(5_ilp), workq(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           maxmn = max( m, n )
           tran  = stdlib_lsame( trans, 'C' )
           lquery = ( lwork==-1_ilp .or. lwork==-2_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or.stdlib_lsame( trans, 'C' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
           ! determine the optimum and minimum lwork
            if( m>=n ) then
              call stdlib_cgeqr( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_cgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_cgeqr( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_cgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            else
              call stdlib_cgelq( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_cgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_cgelq( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_cgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm  = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            end if
            if( ( lwork<wsizem ).and.( .not.lquery ) ) then
               info = -10_ilp
            end if
            work( 1_ilp ) = real( wsizeo,KIND=sp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CGETSLS', -info )
             return
           end if
           if( lquery ) then
             if( lwork==-2_ilp ) work( 1_ilp ) = real( wsizem,KIND=sp)
             return
           end if
           if( lwork<wsizeo ) then
             lw1 = tszm
             lw2 = lwm
           else
             lw1 = tszo
             lw2 = lwo
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
                call stdlib_claset( 'FULL', max( m, n ), nrhs, czero, czero,b, ldb )
                return
           end if
           ! get machine parameters
            smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'P' )
            bignum = one / smlnum
            call stdlib_slabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', m, n, a, lda, dum )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_claset( 'F', maxmn, nrhs, czero, czero, b, ldb )
              go to 50
           end if
           brow = m
           if ( tran ) then
             brow = n
           end if
           bnrm = stdlib_clange( 'M', brow, nrhs, b, ldb, dum )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if ( m>=n ) then
              ! compute qr factorization of a
             call stdlib_cgeqr( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
             if ( .not.tran ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
               call stdlib_cgemqr( 'L' , 'C', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, work(&
                          1_ilp ), lw2,info )
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
               call stdlib_ctrtrs( 'U', 'N', 'N', n, nrhs,a, lda, b, ldb, info )
               if( info>0_ilp ) then
                 return
               end if
               scllen = n
             else
                 ! overdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_ctrtrs( 'U', 'C', 'N', n, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = czero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_cgemqr( 'L', 'N', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_cgelq( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
              ! workspace at least m, optimally m*nb.
              if( .not.tran ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_ctrtrs( 'L', 'N', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_cgemlq( 'L', 'C', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_cgemlq( 'L', 'N', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_ctrtrs( 'L', 'C', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
             call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
             call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
             call stdlib_clascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
             call stdlib_clascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( tszo + lwo,KIND=sp)
           return
     end subroutine stdlib_cgetsls

     module subroutine stdlib_zgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
     !! ZGETSLS solves overdetermined or underdetermined complex linear systems
     !! involving an M-by-N matrix A, using a tall skinny QR or short wide LQ
     !! factorization of A.  It is assumed that A has full rank.
     !! The following options are provided:
     !! 1. If TRANS = 'N' and m >= n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A*X ||.
     !! 2. If TRANS = 'N' and m < n:  find the minimum norm solution of
     !! an underdetermined system A * X = B.
     !! 3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
     !! an undetermined system A**T * X = B.
     !! 4. If TRANS = 'C' and m < n:  find the least squares solution of
     !! an overdetermined system, i.e., solve the least squares problem
     !! minimize || B - A**T * X ||.
     !! Several right hand side vectors b and solution vectors x can be
     !! handled in a single call; they are stored as the columns of the
     !! M-by-NRHS right hand side matrix B and the N-by-NRHS solution
     !! matrix X.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, tran
           integer(ilp) :: i, iascl, ibscl, j, maxmn, brow, scllen, tszo, tszm, lwo, lwm, lw1, &
                     lw2, wsizeo, wsizem, info2
           real(dp) :: anrm, bignum, bnrm, smlnum, dum(1_ilp)
           complex(dp) :: tq(5_ilp), workq(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           maxmn = max( m, n )
           tran  = stdlib_lsame( trans, 'C' )
           lquery = ( lwork==-1_ilp .or. lwork==-2_ilp )
           if( .not.( stdlib_lsame( trans, 'N' ) .or.stdlib_lsame( trans, 'C' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
           ! determine the optimum and minimum lwork
            if( m>=n ) then
              call stdlib_zgeqr( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_zgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_zgeqr( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_zgemqr( 'L', trans, m, nrhs, n, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            else
              call stdlib_zgelq( m, n, a, lda, tq, -1_ilp, workq, -1_ilp, info2 )
              tszo = int( tq( 1_ilp ),KIND=ilp)
              lwo  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_zgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszo, b, ldb, workq, -1_ilp, &
                        info2 )
              lwo  = max( lwo, int( workq( 1_ilp ),KIND=ilp) )
              call stdlib_zgelq( m, n, a, lda, tq, -2_ilp, workq, -2_ilp, info2 )
              tszm = int( tq( 1_ilp ),KIND=ilp)
              lwm  = int( workq( 1_ilp ),KIND=ilp)
              call stdlib_zgemlq( 'L', trans, n, nrhs, m, a, lda, tq,tszm, b, ldb, workq, -1_ilp, &
                        info2 )
              lwm  = max( lwm, int( workq( 1_ilp ),KIND=ilp) )
              wsizeo = tszo + lwo
              wsizem = tszm + lwm
            end if
            if( ( lwork<wsizem ).and.( .not.lquery ) ) then
               info = -10_ilp
            end if
            work( 1_ilp ) = real( wsizeo,KIND=dp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZGETSLS', -info )
             return
           end if
           if( lquery ) then
             if( lwork==-2_ilp ) work( 1_ilp ) = real( wsizem,KIND=dp)
             return
           end if
           if( lwork<wsizeo ) then
             lw1 = tszm
             lw2 = lwm
           else
             lw1 = tszo
             lw2 = lwo
           end if
           ! quick return if possible
           if( min( m, n, nrhs )==0_ilp ) then
                call stdlib_zlaset( 'FULL', max( m, n ), nrhs, czero, czero,b, ldb )
                return
           end if
           ! get machine parameters
            smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'P' )
            bignum = one / smlnum
            call stdlib_dlabad( smlnum, bignum )
           ! scale a, b if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', m, n, a, lda, dum )
           iascl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, info )
              iascl = 1_ilp
           else if( anrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, info )
              iascl = 2_ilp
           else if( anrm==zero ) then
              ! matrix all zero. return zero solution.
              call stdlib_zlaset( 'F', maxmn, nrhs, czero, czero, b, ldb )
              go to 50
           end if
           brow = m
           if ( tran ) then
             brow = n
           end if
           bnrm = stdlib_zlange( 'M', brow, nrhs, b, ldb, dum )
           ibscl = 0_ilp
           if( bnrm>zero .and. bnrm<smlnum ) then
              ! scale matrix norm up to smlnum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, smlnum, brow, nrhs, b, ldb,info )
              ibscl = 1_ilp
           else if( bnrm>bignum ) then
              ! scale matrix norm down to bignum
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bnrm, bignum, brow, nrhs, b, ldb,info )
              ibscl = 2_ilp
           end if
           if ( m>=n ) then
              ! compute qr factorization of a
             call stdlib_zgeqr( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
             if ( .not.tran ) then
                 ! least-squares problem min || a * x - b ||
                 ! b(1:m,1:nrhs) := q**t * b(1:m,1:nrhs)
               call stdlib_zgemqr( 'L' , 'C', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, work(&
                          1_ilp ), lw2,info )
                 ! b(1:n,1:nrhs) := inv(r) * b(1:n,1:nrhs)
               call stdlib_ztrtrs( 'U', 'N', 'N', n, nrhs,a, lda, b, ldb, info )
               if( info>0_ilp ) then
                 return
               end if
               scllen = n
             else
                 ! overdetermined system of equations a**t * x = b
                 ! b(1:n,1:nrhs) := inv(r**t) * b(1:n,1:nrhs)
                 call stdlib_ztrtrs( 'U', 'C', 'N', n, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(n+1:m,1:nrhs) = czero
                 do j = 1, nrhs
                    do i = n + 1, m
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:m,1:nrhs) := q(1:n,:) * b(1:n,1:nrhs)
                 call stdlib_zgemqr( 'L', 'N', m, nrhs, n, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 scllen = m
              end if
           else
              ! compute lq factorization of a
              call stdlib_zgelq( m, n, a, lda, work( lw2+1 ), lw1,work( 1_ilp ), lw2, info )
              ! workspace at least m, optimally m*nb.
              if( .not.tran ) then
                 ! underdetermined system of equations a * x = b
                 ! b(1:m,1:nrhs) := inv(l) * b(1:m,1:nrhs)
                 call stdlib_ztrtrs( 'L', 'N', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 ! b(m+1:n,1:nrhs) = 0
                 do j = 1, nrhs
                    do i = m + 1, n
                       b( i, j ) = czero
                    end do
                 end do
                 ! b(1:n,1:nrhs) := q(1:n,:)**t * b(1:m,1:nrhs)
                 call stdlib_zgemlq( 'L', 'C', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 scllen = n
              else
                 ! overdetermined system min || a**t * x - b ||
                 ! b(1:n,1:nrhs) := q * b(1:n,1:nrhs)
                 call stdlib_zgemlq( 'L', 'N', n, nrhs, m, a, lda,work( lw2+1 ), lw1, b, ldb, &
                           work( 1_ilp ), lw2,info )
                 ! workspace at least nrhs, optimally nrhs*nb
                 ! b(1:m,1:nrhs) := inv(l**t) * b(1:m,1:nrhs)
                 call stdlib_ztrtrs( 'L', 'C', 'N', m, nrhs,a, lda, b, ldb, info )
                 if( info>0_ilp ) then
                    return
                 end if
                 scllen = m
              end if
           end if
           ! undo scaling
           if( iascl==1_ilp ) then
             call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, scllen, nrhs, b, ldb,info )
           else if( iascl==2_ilp ) then
             call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, scllen, nrhs, b, ldb,info )
           end if
           if( ibscl==1_ilp ) then
             call stdlib_zlascl( 'G', 0_ilp, 0_ilp, smlnum, bnrm, scllen, nrhs, b, ldb,info )
           else if( ibscl==2_ilp ) then
             call stdlib_zlascl( 'G', 0_ilp, 0_ilp, bignum, bnrm, scllen, nrhs, b, ldb,info )
           end if
           50 continue
           work( 1_ilp ) = real( tszo + lwo,KIND=dp)
           return
     end subroutine stdlib_zgetsls



end submodule stdlib_lapack_lsq
