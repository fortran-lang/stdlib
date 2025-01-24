submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_comp2
  implicit none


  contains

     pure module subroutine stdlib_stgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
     !! STGSEN reorders the generalized real Schur decomposition of a real
     !! matrix pair (A, B) (in terms of an orthonormal equivalence trans-
     !! formation Q**T * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the upper quasi-triangular
     !! matrix A and the upper triangular B. The leading columns of Q and
     !! Z form orthonormal bases of the corresponding left and right eigen-
     !! spaces (deflating subspaces). (A, B) must be in generalized real
     !! Schur canonical form (as returned by SGGES), i.e. A is block upper
     !! triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
     !! triangular.
     !! STGSEN also computes the generalized eigenvalues
     !! w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, STGSEN computes the estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantd, wantd1, wantd2, wantp
           integer(ilp) :: i, ierr, ijb, k, kase, kk, ks, liwmin, lwmin, mn2, n1, n2
           real(sp) :: dscale, dsum, eps, rdscal, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ijob<0_ilp .or. ijob>5_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -14_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSEN', -info )
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           ierr = 0_ilp
           wantp = ijob==1_ilp .or. ijob>=4_ilp
           wantd1 = ijob==2_ilp .or. ijob==4_ilp
           wantd2 = ijob==3_ilp .or. ijob==5_ilp
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp
           pair = .false.
           if( .not.lquery .or. ijob/=0_ilp ) then
           do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )==zero ) then
                       if( select( k ) )m = m + 1_ilp
                    else
                       pair = .true.
                       if( select( k ) .or. select( k+1 ) )m = m + 2_ilp
                    end if
                 else
                    if( select( n ) )m = m + 1_ilp
                 end if
              end if
           end do
           end if
           if( ijob==1_ilp .or. ijob==2_ilp .or. ijob==4_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*n+16, 2_ilp*m*(n-m) )
              liwmin = max( 1_ilp, n+6 )
           else if( ijob==3_ilp .or. ijob==5_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*n+16, 4_ilp*m*(n-m) )
              liwmin = max( 1_ilp, 2_ilp*m*(n-m), n+6 )
           else
              lwmin = max( 1_ilp, 4_ilp*n+16 )
              liwmin = 1_ilp
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -22_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -24_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_slassq( n, a( 1_ilp, i ), 1_ilp, dscale, dsum )
                    call stdlib_slassq( n, b( 1_ilp, i ), 1_ilp, dscale, dsum )
                 end do
                 dif( 1_ilp ) = dscale*sqrt( dsum )
                 dif( 2_ilp ) = dif( 1_ilp )
              end if
              go to 60
           end if
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp
           pair = .false.
           loop_30: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp
                    ! swap the k-th block to position ks.
                    ! perform the reordering of diagonal blocks in (a, b)
                    ! by orthogonal transformation matrices and update
                    ! q and z accordingly (if requested):
                    kk = k
                    if( k/=ks )call stdlib_stgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz,&
                               kk, ks, work, lwork, ierr )
                    if( ierr>0_ilp ) then
                       ! swap is rejected: exit.
                       info = 1_ilp
                       if( wantp ) then
                          pl = zero
                          pr = zero
                       end if
                       if( wantd ) then
                          dif( 1_ilp ) = zero
                          dif( 2_ilp ) = zero
                       end if
                       go to 60
                    end if
                    if( pair )ks = ks + 1_ilp
                 end if
              end if
           end do loop_30
           if( wantp ) then
              ! solve generalized sylvester equation for r and l
              ! and compute pl and pr.
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp
              ijb = 0_ilp
              call stdlib_slacpy( 'FULL', n1, n2, a( 1_ilp, i ), lda, work, n1 )
              call stdlib_slacpy( 'FULL', n1, n2, b( 1_ilp, i ), ldb, work( n1*n2+1 ),n1 )
              call stdlib_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto left
              ! and right eigenspaces.
              rdscal = zero
              dsum = one
              call stdlib_slassq( n1*n2, work, 1_ilp, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_slassq( n1*n2, work( n1*n2+1 ), 1_ilp, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates of difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = idifjb
                 ! frobenius norm-based difu-estimate.
                 call stdlib_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp ), work( 2_ilp*n1*n2+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl-estimate.
                 call stdlib_stgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp ), work( 2_ilp*n1*n2+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_slacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = 0_ilp
                 mn2 = 2_ilp*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_slacn2( mn2, work( mn2+1 ), work, iwork, dif( 1_ilp ),kase, isave )
                           
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation.
                       call stdlib_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_stgsyl( 'T', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp ) = dscale / dif( 1_ilp )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_slacn2( mn2, work( mn2+1 ), work, iwork, dif( 2_ilp ),kase, isave )
                           
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation.
                       call stdlib_stgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_stgsyl( 'T', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp ) = dscale / dif( 2_ilp )
              end if
           end if
           60 continue
           ! compute generalized eigenvalues of reordered pair (a, b) and
           ! normalize the generalized schur form.
           pair = .false.
           loop_70: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                    end if
                 end if
                 if( pair ) then
                   ! compute the eigenvalue(s) at position k.
                    work( 1_ilp ) = a( k, k )
                    work( 2_ilp ) = a( k+1, k )
                    work( 3_ilp ) = a( k, k+1 )
                    work( 4_ilp ) = a( k+1, k+1 )
                    work( 5_ilp ) = b( k, k )
                    work( 6_ilp ) = b( k+1, k )
                    work( 7_ilp ) = b( k, k+1 )
                    work( 8_ilp ) = b( k+1, k+1 )
                    call stdlib_slag2( work, 2_ilp, work( 5_ilp ), 2_ilp, smlnum*eps, beta( k ),beta( k+1 ), &
                              alphar( k ), alphar( k+1 ),alphai( k ) )
                    alphai( k+1 ) = -alphai( k )
                 else
                    if( sign( one, b( k, k ) )<zero ) then
                       ! if b(k,k) is negative, make it positive
                       do i = 1, n
                          a( k, i ) = -a( k, i )
                          b( k, i ) = -b( k, i )
                          if( wantq ) q( i, k ) = -q( i, k )
                       end do
                    end if
                    alphar( k ) = a( k, k )
                    alphai( k ) = zero
                    beta( k ) = b( k, k )
                 end if
              end if
           end do loop_70
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_stgsen

     pure module subroutine stdlib_dtgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
     !! DTGSEN reorders the generalized real Schur decomposition of a real
     !! matrix pair (A, B) (in terms of an orthonormal equivalence trans-
     !! formation Q**T * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the upper quasi-triangular
     !! matrix A and the upper triangular B. The leading columns of Q and
     !! Z form orthonormal bases of the corresponding left and right eigen-
     !! spaces (deflating subspaces). (A, B) must be in generalized real
     !! Schur canonical form (as returned by DGGES), i.e. A is block upper
     !! triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
     !! triangular.
     !! DTGSEN also computes the generalized eigenvalues
     !! w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, DTGSEN computes the estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantd, wantd1, wantd2, wantp
           integer(ilp) :: i, ierr, ijb, k, kase, kk, ks, liwmin, lwmin, mn2, n1, n2
           real(dp) :: dscale, dsum, eps, rdscal, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ijob<0_ilp .or. ijob>5_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -14_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSEN', -info )
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           ierr = 0_ilp
           wantp = ijob==1_ilp .or. ijob>=4_ilp
           wantd1 = ijob==2_ilp .or. ijob==4_ilp
           wantd2 = ijob==3_ilp .or. ijob==5_ilp
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp
           pair = .false.
           if( .not.lquery .or. ijob/=0_ilp ) then
           do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )==zero ) then
                       if( select( k ) )m = m + 1_ilp
                    else
                       pair = .true.
                       if( select( k ) .or. select( k+1 ) )m = m + 2_ilp
                    end if
                 else
                    if( select( n ) )m = m + 1_ilp
                 end if
              end if
           end do
           end if
           if( ijob==1_ilp .or. ijob==2_ilp .or. ijob==4_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*n+16, 2_ilp*m*( n-m ) )
              liwmin = max( 1_ilp, n+6 )
           else if( ijob==3_ilp .or. ijob==5_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*n+16, 4_ilp*m*( n-m ) )
              liwmin = max( 1_ilp, 2_ilp*m*( n-m ), n+6 )
           else
              lwmin = max( 1_ilp, 4_ilp*n+16 )
              liwmin = 1_ilp
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -22_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -24_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_dlassq( n, a( 1_ilp, i ), 1_ilp, dscale, dsum )
                    call stdlib_dlassq( n, b( 1_ilp, i ), 1_ilp, dscale, dsum )
                 end do
                 dif( 1_ilp ) = dscale*sqrt( dsum )
                 dif( 2_ilp ) = dif( 1_ilp )
              end if
              go to 60
           end if
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp
           pair = .false.
           loop_30: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp
                    ! swap the k-th block to position ks.
                    ! perform the reordering of diagonal blocks in (a, b)
                    ! by orthogonal transformation matrices and update
                    ! q and z accordingly (if requested):
                    kk = k
                    if( k/=ks )call stdlib_dtgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz,&
                               kk, ks, work, lwork, ierr )
                    if( ierr>0_ilp ) then
                       ! swap is rejected: exit.
                       info = 1_ilp
                       if( wantp ) then
                          pl = zero
                          pr = zero
                       end if
                       if( wantd ) then
                          dif( 1_ilp ) = zero
                          dif( 2_ilp ) = zero
                       end if
                       go to 60
                    end if
                    if( pair )ks = ks + 1_ilp
                 end if
              end if
           end do loop_30
           if( wantp ) then
              ! solve generalized sylvester equation for r and l
              ! and compute pl and pr.
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp
              ijb = 0_ilp
              call stdlib_dlacpy( 'FULL', n1, n2, a( 1_ilp, i ), lda, work, n1 )
              call stdlib_dlacpy( 'FULL', n1, n2, b( 1_ilp, i ), ldb, work( n1*n2+1 ),n1 )
              call stdlib_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto left
              ! and right eigenspaces.
              rdscal = zero
              dsum = one
              call stdlib_dlassq( n1*n2, work, 1_ilp, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_dlassq( n1*n2, work( n1*n2+1 ), 1_ilp, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates of difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = idifjb
                 ! frobenius norm-based difu-estimate.
                 call stdlib_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp ), work( 2_ilp*n1*n2+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl-estimate.
                 call stdlib_dtgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp ), work( 2_ilp*n1*n2+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_dlacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = 0_ilp
                 mn2 = 2_ilp*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_dlacn2( mn2, work( mn2+1 ), work, iwork, dif( 1_ilp ),kase, isave )
                           
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation.
                       call stdlib_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_dtgsyl( 'T', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp ) = dscale / dif( 1_ilp )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_dlacn2( mn2, work( mn2+1 ), work, iwork, dif( 2_ilp ),kase, isave )
                           
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation.
                       call stdlib_dtgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_dtgsyl( 'T', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( 2_ilp*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp ) = dscale / dif( 2_ilp )
              end if
           end if
           60 continue
           ! compute generalized eigenvalues of reordered pair (a, b) and
           ! normalize the generalized schur form.
           pair = .false.
           loop_80: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                    end if
                 end if
                 if( pair ) then
                   ! compute the eigenvalue(s) at position k.
                    work( 1_ilp ) = a( k, k )
                    work( 2_ilp ) = a( k+1, k )
                    work( 3_ilp ) = a( k, k+1 )
                    work( 4_ilp ) = a( k+1, k+1 )
                    work( 5_ilp ) = b( k, k )
                    work( 6_ilp ) = b( k+1, k )
                    work( 7_ilp ) = b( k, k+1 )
                    work( 8_ilp ) = b( k+1, k+1 )
                    call stdlib_dlag2( work, 2_ilp, work( 5_ilp ), 2_ilp, smlnum*eps, beta( k ),beta( k+1 ), &
                              alphar( k ), alphar( k+1 ),alphai( k ) )
                    alphai( k+1 ) = -alphai( k )
                 else
                    if( sign( one, b( k, k ) )<zero ) then
                       ! if b(k,k) is negative, make it positive
                       do i = 1, n
                          a( k, i ) = -a( k, i )
                          b( k, i ) = -b( k, i )
                          if( wantq ) q( i, k ) = -q( i, k )
                       end do
                    end if
                    alphar( k ) = a( k, k )
                    alphai( k ) = zero
                    beta( k ) = b( k, k )
                 end if
              end if
           end do loop_80
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dtgsen


     pure module subroutine stdlib_ctgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
     !! CTGSEN reorders the generalized Schur decomposition of a complex
     !! matrix pair (A, B) (in terms of an unitary equivalence trans-
     !! formation Q**H * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the pair (A,B). The leading
     !! columns of Q and Z form unitary bases of the corresponding left and
     !! right eigenspaces (deflating subspaces). (A, B) must be in
     !! generalized Schur canonical form, that is, A and B are both upper
     !! triangular.
     !! CTGSEN also computes the generalized eigenvalues
     !! w(j)= ALPHA(j) / BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, the routine computes estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(sp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, swap, wantd, wantd1, wantd2, wantp
           integer(ilp) :: i, ierr, ijb, k, kase, ks, liwmin, lwmin, mn2, n1, n2
           real(sp) :: dscale, dsum, rdscal, safmin
           complex(sp) :: temp1, temp2
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ijob<0_ilp .or. ijob>5_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -13_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSEN', -info )
              return
           end if
           ierr = 0_ilp
           wantp = ijob==1_ilp .or. ijob>=4_ilp
           wantd1 = ijob==2_ilp .or. ijob==4_ilp
           wantd2 = ijob==3_ilp .or. ijob==5_ilp
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp
           if( .not.lquery .or. ijob/=0_ilp ) then
           do k = 1, n
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              if( k<n ) then
                 if( select( k ) )m = m + 1_ilp
              else
                 if( select( n ) )m = m + 1_ilp
              end if
           end do
           end if
           if( ijob==1_ilp .or. ijob==2_ilp .or. ijob==4_ilp ) then
              lwmin = max( 1_ilp, 2_ilp*m*(n-m) )
              liwmin = max( 1_ilp, n+2 )
           else if( ijob==3_ilp .or. ijob==5_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*m*(n-m) )
              liwmin = max( 1_ilp, 2_ilp*m*(n-m), n+2 )
           else
              lwmin = 1_ilp
              liwmin = 1_ilp
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -21_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -23_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_classq( n, a( 1_ilp, i ), 1_ilp, dscale, dsum )
                    call stdlib_classq( n, b( 1_ilp, i ), 1_ilp, dscale, dsum )
                 end do
                 dif( 1_ilp ) = dscale*sqrt( dsum )
                 dif( 2_ilp ) = dif( 1_ilp )
              end if
              go to 70
           end if
           ! get machine constant
           safmin = stdlib_slamch( 'S' )
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp
           do k = 1, n
              swap = select( k )
              if( swap ) then
                 ks = ks + 1_ilp
                 ! swap the k-th block to position ks. compute unitary q
                 ! and z that will swap adjacent diagonal blocks in (a, b).
                 if( k/=ks )call stdlib_ctgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, k,&
                            ks, ierr )
                 if( ierr>0_ilp ) then
                    ! swap is rejected: exit.
                    info = 1_ilp
                    if( wantp ) then
                       pl = zero
                       pr = zero
                    end if
                    if( wantd ) then
                       dif( 1_ilp ) = zero
                       dif( 2_ilp ) = zero
                    end if
                    go to 70
                 end if
              end if
           end do
           if( wantp ) then
              ! solve generalized sylvester equation for r and l:
                         ! a11 * r - l * a22 = a12
                         ! b11 * r - l * b22 = b12
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp
              call stdlib_clacpy( 'FULL', n1, n2, a( 1_ilp, i ), lda, work, n1 )
              call stdlib_clacpy( 'FULL', n1, n2, b( 1_ilp, i ), ldb, work( n1*n2+1 ),n1 )
              ijb = 0_ilp
              call stdlib_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto
              ! left and right eigenspaces
              rdscal = zero
              dsum = one
              call stdlib_classq( n1*n2, work, 1_ilp, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_classq( n1*n2, work( n1*n2+1 ), 1_ilp, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = idifjb
                 ! frobenius norm-based difu estimate.
                 call stdlib_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl estimate.
                 call stdlib_ctgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp ), work( n1*n2*2_ilp+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_clacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = 0_ilp
                 mn2 = 2_ilp*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_clacn2( mn2, work( mn2+1 ), work, dif( 1_ilp ), kase,isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation
                       call stdlib_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_ctgsyl( 'C', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp ) = dscale / dif( 1_ilp )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_clacn2( mn2, work( mn2+1 ), work, dif( 2_ilp ), kase,isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation
                       call stdlib_ctgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_ctgsyl( 'C', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp ) = dscale / dif( 2_ilp )
              end if
           end if
           ! if b(k,k) is complex, make it real and positive (normalization
           ! of the generalized schur form) and store the generalized
           ! eigenvalues of reordered pair (a, b)
           do k = 1, n
              dscale = abs( b( k, k ) )
              if( dscale>safmin ) then
                 temp1 = conjg( b( k, k ) / dscale )
                 temp2 = b( k, k ) / dscale
                 b( k, k ) = dscale
                 call stdlib_cscal( n-k, temp1, b( k, k+1 ), ldb )
                 call stdlib_cscal( n-k+1, temp1, a( k, k ), lda )
                 if( wantq )call stdlib_cscal( n, temp2, q( 1_ilp, k ), 1_ilp )
              else
                 b( k, k ) = cmplx( zero, zero,KIND=sp)
              end if
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
           end do
           70 continue
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_ctgsen

     pure module subroutine stdlib_ztgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
     !! ZTGSEN reorders the generalized Schur decomposition of a complex
     !! matrix pair (A, B) (in terms of an unitary equivalence trans-
     !! formation Q**H * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the pair (A,B). The leading
     !! columns of Q and Z form unitary bases of the corresponding left and
     !! right eigenspaces (deflating subspaces). (A, B) must be in
     !! generalized Schur canonical form, that is, A and B are both upper
     !! triangular.
     !! ZTGSEN also computes the generalized eigenvalues
     !! w(j)= ALPHA(j) / BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, the routine computes estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(dp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, swap, wantd, wantd1, wantd2, wantp
           integer(ilp) :: i, ierr, ijb, k, kase, ks, liwmin, lwmin, mn2, n1, n2
           real(dp) :: dscale, dsum, rdscal, safmin
           complex(dp) :: temp1, temp2
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           if( ijob<0_ilp .or. ijob>5_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -13_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSEN', -info )
              return
           end if
           ierr = 0_ilp
           wantp = ijob==1_ilp .or. ijob>=4_ilp
           wantd1 = ijob==2_ilp .or. ijob==4_ilp
           wantd2 = ijob==3_ilp .or. ijob==5_ilp
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp
           if( .not.lquery .or. ijob/=0_ilp ) then
           do k = 1, n
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              if( k<n ) then
                 if( select( k ) )m = m + 1_ilp
              else
                 if( select( n ) )m = m + 1_ilp
              end if
           end do
           end if
           if( ijob==1_ilp .or. ijob==2_ilp .or. ijob==4_ilp ) then
              lwmin = max( 1_ilp, 2_ilp*m*( n-m ) )
              liwmin = max( 1_ilp, n+2 )
           else if( ijob==3_ilp .or. ijob==5_ilp ) then
              lwmin = max( 1_ilp, 4_ilp*m*( n-m ) )
              liwmin = max( 1_ilp, 2_ilp*m*( n-m ), n+2 )
           else
              lwmin = 1_ilp
              liwmin = 1_ilp
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -21_ilp
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -23_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_zlassq( n, a( 1_ilp, i ), 1_ilp, dscale, dsum )
                    call stdlib_zlassq( n, b( 1_ilp, i ), 1_ilp, dscale, dsum )
                 end do
                 dif( 1_ilp ) = dscale*sqrt( dsum )
                 dif( 2_ilp ) = dif( 1_ilp )
              end if
              go to 70
           end if
           ! get machine constant
           safmin = stdlib_dlamch( 'S' )
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp
           do k = 1, n
              swap = select( k )
              if( swap ) then
                 ks = ks + 1_ilp
                 ! swap the k-th block to position ks. compute unitary q
                 ! and z that will swap adjacent diagonal blocks in (a, b).
                 if( k/=ks )call stdlib_ztgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, k,&
                            ks, ierr )
                 if( ierr>0_ilp ) then
                    ! swap is rejected: exit.
                    info = 1_ilp
                    if( wantp ) then
                       pl = zero
                       pr = zero
                    end if
                    if( wantd ) then
                       dif( 1_ilp ) = zero
                       dif( 2_ilp ) = zero
                    end if
                    go to 70
                 end if
              end if
           end do
           if( wantp ) then
              ! solve generalized sylvester equation for r and l:
                         ! a11 * r - l * a22 = a12
                         ! b11 * r - l * b22 = b12
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp
              call stdlib_zlacpy( 'FULL', n1, n2, a( 1_ilp, i ), lda, work, n1 )
              call stdlib_zlacpy( 'FULL', n1, n2, b( 1_ilp, i ), ldb, work( n1*n2+1 ),n1 )
              ijb = 0_ilp
              call stdlib_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto
              ! left and right eigenspaces
              rdscal = zero
              dsum = one
              call stdlib_zlassq( n1*n2, work, 1_ilp, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_zlassq( n1*n2, work( n1*n2+1 ), 1_ilp, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = idifjb
                 ! frobenius norm-based difu estimate.
                 call stdlib_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp ), work( n1*n2*2_ilp+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl estimate.
                 call stdlib_ztgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp ), work( n1*n2*2_ilp+1 ),lwork-&
                            2_ilp*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_zlacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp
                 ijb = 0_ilp
                 mn2 = 2_ilp*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_zlacn2( mn2, work( mn2+1 ), work, dif( 1_ilp ), kase,isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation
                       call stdlib_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_ztgsyl( 'C', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp ) = dscale / dif( 1_ilp )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_zlacn2( mn2, work( mn2+1 ), work, dif( 2_ilp ), kase,isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve generalized sylvester equation
                       call stdlib_ztgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_ztgsyl( 'C', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp ),work( n1*n2*2_ilp+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp ) = dscale / dif( 2_ilp )
              end if
           end if
           ! if b(k,k) is complex, make it real and positive (normalization
           ! of the generalized schur form) and store the generalized
           ! eigenvalues of reordered pair (a, b)
           do k = 1, n
              dscale = abs( b( k, k ) )
              if( dscale>safmin ) then
                 temp1 = conjg( b( k, k ) / dscale )
                 temp2 = b( k, k ) / dscale
                 b( k, k ) = dscale
                 call stdlib_zscal( n-k, temp1, b( k, k+1 ), ldb )
                 call stdlib_zscal( n-k+1, temp1, a( k, k ), lda )
                 if( wantq )call stdlib_zscal( n, temp2, q( 1_ilp, k ), 1_ilp )
              else
                 b( k, k ) = cmplx( zero, zero,KIND=dp)
              end if
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
           end do
           70 continue
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_ztgsen




     pure module subroutine stdlib_stgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! STGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B) in
     !! generalized real Schur canonical form (or of any matrix pair
     !! (Q*A*Z**T, Q*B*Z**T) with orthogonal matrices Q and Z, where
     !! Z**T denotes the transpose of Z.
     !! (A, B) must be in generalized real Schur form (as returned by SGGES),
     !! i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
     !! blocks. B is upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: dif(*), s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: difdri = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, somcon, wantbh, wantdf, wants
           integer(ilp) :: i, ierr, ifst, ilst, iz, k, ks, lwmin, n1, n2
           real(sp) :: alphai, alphar, alprqt, beta, c1, c2, cond, eps, lnrm, rnrm, root1, root2, &
                     scale, smlnum, tmpii, tmpir, tmpri, tmprr, uhav, uhavi, uhbv, uhbvi
           ! Local Arrays 
           real(sp) :: dummy(1_ilp), dummy1(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( wants .and. ldvl<n ) then
              info = -10_ilp
           else if( wants .and. ldvr<n ) then
              info = -12_ilp
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp
                 pair = .false.
                 do k = 1, n
                    if( pair ) then
                       pair = .false.
                    else
                       if( k<n ) then
                          if( a( k+1, k )==zero ) then
                             if( select( k ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( k ) .or. select( k+1 ) )m = m + 2_ilp
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( n==0_ilp ) then
                 lwmin = 1_ilp
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp*n*( n + 2_ilp ) + 16_ilp
              else
                 lwmin = n
              end if
              work( 1_ilp ) = lwmin
              if( mm<m ) then
                 info = -15_ilp
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           ks = 0_ilp
           pair = .false.
           loop_20: do k = 1, n
              ! determine whether a(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_20
              else
                 if( k<n )pair = a( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_20
                 else
                    if( .not.select( k ) )cycle loop_20
                 end if
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( pair ) then
                    ! complex eigenvalue pair.
                    rnrm = stdlib_slapy2( stdlib_snrm2( n, vr( 1_ilp, ks ), 1_ilp ),stdlib_snrm2( n, vr( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    lnrm = stdlib_slapy2( stdlib_snrm2( n, vl( 1_ilp, ks ), 1_ilp ),stdlib_snrm2( n, vl( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    call stdlib_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    tmprr = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    tmpri = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    call stdlib_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks+1 ), 1_ilp,zero, work, 1_ilp )
                              
                    tmpii = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    tmpir = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    uhav = tmprr + tmpii
                    uhavi = tmpir - tmpri
                    call stdlib_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    tmprr = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    tmpri = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    call stdlib_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks+1 ), 1_ilp,zero, work, 1_ilp )
                              
                    tmpii = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    tmpir = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    uhbv = tmprr + tmpii
                    uhbvi = tmpir - tmpri
                    uhav = stdlib_slapy2( uhav, uhavi )
                    uhbv = stdlib_slapy2( uhbv, uhbvi )
                    cond = stdlib_slapy2( uhav, uhbv )
                    s( ks ) = cond / ( rnrm*lnrm )
                    s( ks+1 ) = s( ks )
                 else
                    ! real eigenvalue.
                    rnrm = stdlib_snrm2( n, vr( 1_ilp, ks ), 1_ilp )
                    lnrm = stdlib_snrm2( n, vl( 1_ilp, ks ), 1_ilp )
                    call stdlib_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    uhav = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    call stdlib_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    uhbv = stdlib_sdot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    cond = stdlib_slapy2( uhav, uhbv )
                    if( cond==zero ) then
                       s( ks ) = -one
                    else
                       s( ks ) = cond / ( rnrm*lnrm )
                    end if
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp ) then
                    dif( ks ) = stdlib_slapy2( a( 1_ilp, 1_ilp ), b( 1_ilp, 1_ilp ) )
                    cycle loop_20
                 end if
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvectors.
                 if( pair ) then
                    ! copy the  2-by 2 pencil beginning at (a(k,k), b(k, k)).
                    ! compute the eigenvalue(s) at position k.
                    work( 1_ilp ) = a( k, k )
                    work( 2_ilp ) = a( k+1, k )
                    work( 3_ilp ) = a( k, k+1 )
                    work( 4_ilp ) = a( k+1, k+1 )
                    work( 5_ilp ) = b( k, k )
                    work( 6_ilp ) = b( k+1, k )
                    work( 7_ilp ) = b( k, k+1 )
                    work( 8_ilp ) = b( k+1, k+1 )
                    call stdlib_slag2( work, 2_ilp, work( 5_ilp ), 2_ilp, smlnum*eps, beta,dummy1( 1_ilp ), &
                              alphar, dummy( 1_ilp ), alphai )
                    alprqt = one
                    c1 = two*( alphar*alphar+alphai*alphai+beta*beta )
                    c2 = four*beta*beta*alphai*alphai
                    root1 = c1 + sqrt( c1*c1-4.0_sp*c2 )
                    root2 = c2 / root1
                    root1 = root1 / two
                    cond = min( sqrt( root1 ), sqrt( root2 ) )
                 end if
                 ! copy the matrix (a, b) to the array work and swap the
                 ! diagonal block beginning at a(k,k) to the (1,1) position.
                 call stdlib_slacpy( 'FULL', n, n, a, lda, work, n )
                 call stdlib_slacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                 ifst = k
                 ilst = 1_ilp
                 call stdlib_stgexc( .false., .false., n, work, n, work( n*n+1 ), n,dummy, 1_ilp, &
                           dummy1, 1_ilp, ifst, ilst,work( n*n*2_ilp+1 ), lwork-2*n*n, ierr )
                 if( ierr>0_ilp ) then
                    ! ill-conditioned problem - swap rejected.
                    dif( ks ) = zero
                 else
                    ! reordering successful, solve generalized sylvester
                    ! equation for r and l,
                               ! a22 * r - l * a11 = a12
                               ! b22 * r - l * b11 = b12,
                    ! and compute estimate of difl((a11,b11), (a22, b22)).
                    n1 = 1_ilp
                    if( work( 2_ilp )/=zero )n1 = 2_ilp
                    n2 = n - n1
                    if( n2==0_ilp ) then
                       dif( ks ) = cond
                    else
                       i = n*n + 1_ilp
                       iz = 2_ilp*n*n + 1_ilp
                       call stdlib_stgsyl( 'N', difdri, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ),work( iz+1 ), lwork-2*n*n, iwork, ierr )
                       if( pair )dif( ks ) = min( max( one, alprqt )*dif( ks ),cond )
                    end if
                 end if
                 if( pair )dif( ks+1 ) = dif( ks )
              end if
              if( pair )ks = ks + 1_ilp
           end do loop_20
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_stgsna

     pure module subroutine stdlib_dtgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! DTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B) in
     !! generalized real Schur canonical form (or of any matrix pair
     !! (Q*A*Z**T, Q*B*Z**T) with orthogonal matrices Q and Z, where
     !! Z**T denotes the transpose of Z.
     !! (A, B) must be in generalized real Schur form (as returned by DGGES),
     !! i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
     !! blocks. B is upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: dif(*), s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: difdri = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, somcon, wantbh, wantdf, wants
           integer(ilp) :: i, ierr, ifst, ilst, iz, k, ks, lwmin, n1, n2
           real(dp) :: alphai, alphar, alprqt, beta, c1, c2, cond, eps, lnrm, rnrm, root1, root2, &
                     scale, smlnum, tmpii, tmpir, tmpri, tmprr, uhav, uhavi, uhbv, uhbvi
           ! Local Arrays 
           real(dp) :: dummy(1_ilp), dummy1(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( wants .and. ldvl<n ) then
              info = -10_ilp
           else if( wants .and. ldvr<n ) then
              info = -12_ilp
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp
                 pair = .false.
                 do k = 1, n
                    if( pair ) then
                       pair = .false.
                    else
                       if( k<n ) then
                          if( a( k+1, k )==zero ) then
                             if( select( k ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( k ) .or. select( k+1 ) )m = m + 2_ilp
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( n==0_ilp ) then
                 lwmin = 1_ilp
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp*n*( n + 2_ilp ) + 16_ilp
              else
                 lwmin = n
              end if
              work( 1_ilp ) = lwmin
              if( mm<m ) then
                 info = -15_ilp
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           ks = 0_ilp
           pair = .false.
           loop_20: do k = 1, n
              ! determine whether a(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_20
              else
                 if( k<n )pair = a( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_20
                 else
                    if( .not.select( k ) )cycle loop_20
                 end if
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( pair ) then
                    ! complex eigenvalue pair.
                    rnrm = stdlib_dlapy2( stdlib_dnrm2( n, vr( 1_ilp, ks ), 1_ilp ),stdlib_dnrm2( n, vr( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    lnrm = stdlib_dlapy2( stdlib_dnrm2( n, vl( 1_ilp, ks ), 1_ilp ),stdlib_dnrm2( n, vl( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    call stdlib_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    tmprr = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    tmpri = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    call stdlib_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks+1 ), 1_ilp,zero, work, 1_ilp )
                              
                    tmpii = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    tmpir = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    uhav = tmprr + tmpii
                    uhavi = tmpir - tmpri
                    call stdlib_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    tmprr = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    tmpri = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    call stdlib_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks+1 ), 1_ilp,zero, work, 1_ilp )
                              
                    tmpii = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks+1 ), 1_ilp )
                    tmpir = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    uhbv = tmprr + tmpii
                    uhbvi = tmpir - tmpri
                    uhav = stdlib_dlapy2( uhav, uhavi )
                    uhbv = stdlib_dlapy2( uhbv, uhbvi )
                    cond = stdlib_dlapy2( uhav, uhbv )
                    s( ks ) = cond / ( rnrm*lnrm )
                    s( ks+1 ) = s( ks )
                 else
                    ! real eigenvalue.
                    rnrm = stdlib_dnrm2( n, vr( 1_ilp, ks ), 1_ilp )
                    lnrm = stdlib_dnrm2( n, vl( 1_ilp, ks ), 1_ilp )
                    call stdlib_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    uhav = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    call stdlib_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp, ks ), 1_ilp, zero,work, 1_ilp )
                              
                    uhbv = stdlib_ddot( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    cond = stdlib_dlapy2( uhav, uhbv )
                    if( cond==zero ) then
                       s( ks ) = -one
                    else
                       s( ks ) = cond / ( rnrm*lnrm )
                    end if
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp ) then
                    dif( ks ) = stdlib_dlapy2( a( 1_ilp, 1_ilp ), b( 1_ilp, 1_ilp ) )
                    cycle loop_20
                 end if
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvectors.
                 if( pair ) then
                    ! copy the  2-by 2 pencil beginning at (a(k,k), b(k, k)).
                    ! compute the eigenvalue(s) at position k.
                    work( 1_ilp ) = a( k, k )
                    work( 2_ilp ) = a( k+1, k )
                    work( 3_ilp ) = a( k, k+1 )
                    work( 4_ilp ) = a( k+1, k+1 )
                    work( 5_ilp ) = b( k, k )
                    work( 6_ilp ) = b( k+1, k )
                    work( 7_ilp ) = b( k, k+1 )
                    work( 8_ilp ) = b( k+1, k+1 )
                    call stdlib_dlag2( work, 2_ilp, work( 5_ilp ), 2_ilp, smlnum*eps, beta,dummy1( 1_ilp ), &
                              alphar, dummy( 1_ilp ), alphai )
                    alprqt = one
                    c1 = two*( alphar*alphar+alphai*alphai+beta*beta )
                    c2 = four*beta*beta*alphai*alphai
                    root1 = c1 + sqrt( c1*c1-4.0_dp*c2 )
                    root2 = c2 / root1
                    root1 = root1 / two
                    cond = min( sqrt( root1 ), sqrt( root2 ) )
                 end if
                 ! copy the matrix (a, b) to the array work and swap the
                 ! diagonal block beginning at a(k,k) to the (1,1) position.
                 call stdlib_dlacpy( 'FULL', n, n, a, lda, work, n )
                 call stdlib_dlacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                 ifst = k
                 ilst = 1_ilp
                 call stdlib_dtgexc( .false., .false., n, work, n, work( n*n+1 ), n,dummy, 1_ilp, &
                           dummy1, 1_ilp, ifst, ilst,work( n*n*2_ilp+1 ), lwork-2*n*n, ierr )
                 if( ierr>0_ilp ) then
                    ! ill-conditioned problem - swap rejected.
                    dif( ks ) = zero
                 else
                    ! reordering successful, solve generalized sylvester
                    ! equation for r and l,
                               ! a22 * r - l * a11 = a12
                               ! b22 * r - l * b11 = b12,
                    ! and compute estimate of difl((a11,b11), (a22, b22)).
                    n1 = 1_ilp
                    if( work( 2_ilp )/=zero )n1 = 2_ilp
                    n2 = n - n1
                    if( n2==0_ilp ) then
                       dif( ks ) = cond
                    else
                       i = n*n + 1_ilp
                       iz = 2_ilp*n*n + 1_ilp
                       call stdlib_dtgsyl( 'N', difdri, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ),work( iz+1 ), lwork-2*n*n, iwork, ierr )
                       if( pair )dif( ks ) = min( max( one, alprqt )*dif( ks ),cond )
                    end if
                 end if
                 if( pair )dif( ks+1 ) = dif( ks )
              end if
              if( pair )ks = ks + 1_ilp
           end do loop_20
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_dtgsna


     pure module subroutine stdlib_ctgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! CTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B).
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*), s(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, somcon, wantbh, wantdf, wants
           integer(ilp) :: i, ierr, ifst, ilst, k, ks, lwmin, n1, n2
           real(sp) :: bignum, cond, eps, lnrm, rnrm, scale, smlnum
           complex(sp) :: yhax, yhbx
           ! Local Arrays 
           complex(sp) :: dummy(1_ilp), dummy1(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( wants .and. ldvl<n ) then
              info = -10_ilp
           else if( wants .and. ldvr<n ) then
              info = -12_ilp
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp
                 do k = 1, n
                    if( select( k ) )m = m + 1_ilp
                 end do
              else
                 m = n
              end if
              if( n==0_ilp ) then
                 lwmin = 1_ilp
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp*n*n
              else
                 lwmin = n
              end if
              work( 1_ilp ) = lwmin
              if( mm<m ) then
                 info = -15_ilp
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ks = 0_ilp
           loop_20: do k = 1, n
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_20
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 rnrm = stdlib_scnrm2( n, vr( 1_ilp, ks ), 1_ilp )
                 lnrm = stdlib_scnrm2( n, vl( 1_ilp, ks ), 1_ilp )
                 call stdlib_cgemv( 'N', n, n, cmplx( one, zero,KIND=sp), a, lda,vr( 1_ilp, ks ), 1_ilp, &
                           cmplx( zero, zero,KIND=sp), work, 1_ilp )
                 yhax = stdlib_cdotc( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 call stdlib_cgemv( 'N', n, n, cmplx( one, zero,KIND=sp), b, ldb,vr( 1_ilp, ks ), 1_ilp, &
                           cmplx( zero, zero,KIND=sp), work, 1_ilp )
                 yhbx = stdlib_cdotc( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 cond = stdlib_slapy2( abs( yhax ), abs( yhbx ) )
                 if( cond==zero ) then
                    s( ks ) = -one
                 else
                    s( ks ) = cond / ( rnrm*lnrm )
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp ) then
                    dif( ks ) = stdlib_slapy2( abs( a( 1_ilp, 1_ilp ) ), abs( b( 1_ilp, 1_ilp ) ) )
                 else
                    ! estimate the reciprocal condition number of the k-th
                    ! eigenvectors.
                    ! copy the matrix (a, b) to the array work and move the
                    ! (k,k)th pair to the (1,1) position.
                    call stdlib_clacpy( 'FULL', n, n, a, lda, work, n )
                    call stdlib_clacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                    ifst = k
                    ilst = 1_ilp
                    call stdlib_ctgexc( .false., .false., n, work, n, work( n*n+1 ),n, dummy, 1_ilp, &
                              dummy1, 1_ilp, ifst, ilst, ierr )
                    if( ierr>0_ilp ) then
                       ! ill-conditioned problem - swap rejected.
                       dif( ks ) = zero
                    else
                       ! reordering successful, solve generalized sylvester
                       ! equation for r and l,
                                  ! a22 * r - l * a11 = a12
                                  ! b22 * r - l * b11 = b12,
                       ! and compute estimate of difl[(a11,b11), (a22, b22)].
                       n1 = 1_ilp
                       n2 = n - n1
                       i = n*n + 1_ilp
                       call stdlib_ctgsyl( 'N', idifjb, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ), dummy,1_ilp, iwork, ierr )
                    end if
                 end if
              end if
           end do loop_20
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ctgsna

     pure module subroutine stdlib_ztgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! ZTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B).
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*), s(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: idifjb = 3_ilp
           
           
           ! Local Scalars 
           logical(lk) :: lquery, somcon, wantbh, wantdf, wants
           integer(ilp) :: i, ierr, ifst, ilst, k, ks, lwmin, n1, n2
           real(dp) :: bignum, cond, eps, lnrm, rnrm, scale, smlnum
           complex(dp) :: yhax, yhbx
           ! Local Arrays 
           complex(dp) :: dummy(1_ilp), dummy1(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( wants .and. ldvl<n ) then
              info = -10_ilp
           else if( wants .and. ldvr<n ) then
              info = -12_ilp
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp
                 do k = 1, n
                    if( select( k ) )m = m + 1_ilp
                 end do
              else
                 m = n
              end if
              if( n==0_ilp ) then
                 lwmin = 1_ilp
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp*n*n
              else
                 lwmin = n
              end if
              work( 1_ilp ) = lwmin
              if( mm<m ) then
                 info = -15_ilp
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ks = 0_ilp
           loop_20: do k = 1, n
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_20
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 rnrm = stdlib_dznrm2( n, vr( 1_ilp, ks ), 1_ilp )
                 lnrm = stdlib_dznrm2( n, vl( 1_ilp, ks ), 1_ilp )
                 call stdlib_zgemv( 'N', n, n, cmplx( one, zero,KIND=dp), a, lda,vr( 1_ilp, ks ), 1_ilp, &
                           cmplx( zero, zero,KIND=dp), work, 1_ilp )
                 yhax = stdlib_zdotc( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 call stdlib_zgemv( 'N', n, n, cmplx( one, zero,KIND=dp), b, ldb,vr( 1_ilp, ks ), 1_ilp, &
                           cmplx( zero, zero,KIND=dp), work, 1_ilp )
                 yhbx = stdlib_zdotc( n, work, 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 cond = stdlib_dlapy2( abs( yhax ), abs( yhbx ) )
                 if( cond==zero ) then
                    s( ks ) = -one
                 else
                    s( ks ) = cond / ( rnrm*lnrm )
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp ) then
                    dif( ks ) = stdlib_dlapy2( abs( a( 1_ilp, 1_ilp ) ), abs( b( 1_ilp, 1_ilp ) ) )
                 else
                    ! estimate the reciprocal condition number of the k-th
                    ! eigenvectors.
                    ! copy the matrix (a, b) to the array work and move the
                    ! (k,k)th pair to the (1,1) position.
                    call stdlib_zlacpy( 'FULL', n, n, a, lda, work, n )
                    call stdlib_zlacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                    ifst = k
                    ilst = 1_ilp
                    call stdlib_ztgexc( .false., .false., n, work, n, work( n*n+1 ),n, dummy, 1_ilp, &
                              dummy1, 1_ilp, ifst, ilst, ierr )
                    if( ierr>0_ilp ) then
                       ! ill-conditioned problem - swap rejected.
                       dif( ks ) = zero
                    else
                       ! reordering successful, solve generalized sylvester
                       ! equation for r and l,
                                  ! a22 * r - l * a11 = a12
                                  ! b22 * r - l * b11 = b12,
                       ! and compute estimate of difl[(a11,b11), (a22, b22)].
                       n1 = 1_ilp
                       n2 = n - n1
                       i = n*n + 1_ilp
                       call stdlib_ztgsyl( 'N', idifjb, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ), dummy,1_ilp, iwork, ierr )
                    end if
                 end if
              end if
           end do loop_20
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ztgsna




     pure module subroutine stdlib_stgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! STGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                 (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with real entries. (A, D) and (B, E) must be in
     !! generalized (real) Schur canonical form, i.e. A, B are upper quasi
     !! triangular and D, E are upper triangular.
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve  Zx = scale b, where
     !! Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]         (2)
     !! [ kron(In, D)  -kron(E**T, Im) ].
     !! Here Ik is the identity matrix of size k and X**T is the transpose of
     !! X. kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'T', STGSYL solves the transposed system Z**T*y = scale*b,
     !! which is equivalent to solve for R and L in
     !! A**T * R + D**T * L = scale * C           (3)
     !! R * B**T + L * E**T = scale * -F
     !! This case (TRANS = 'T') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using SLACON.
     !! If IJOB >= 1, STGSYL computes a Frobenius norm-based estimate
     !! of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z. See [1-2] for more
     !! information.
     !! This is a level 3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_scopy by calls to stdlib_slaset.
        ! sven hammarling, 1/5/02.
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, ppqq, pq, q
           real(sp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>4_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( notran ) then
                 if( ijob==1_ilp .or. ijob==2_ilp ) then
                    lwmin = max( 1_ilp, 2_ilp*m*n )
                 else
                    lwmin = 1_ilp
                 end if
              else
                 lwmin = 1_ilp
              end if
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              scale = 1_ilp
              if( notran ) then
                 if( ijob/=0_ilp ) then
                    dif = 0_ilp
                 end if
              end if
              return
           end if
           ! determine optimal block sizes mb and nb
           mb = stdlib_ilaenv( 2_ilp, 'STGSYL', trans, m, n, -1_ilp, -1_ilp )
           nb = stdlib_ilaenv( 5_ilp, 'STGSYL', trans, m, n, -1_ilp, -1_ilp )
           isolve = 1_ilp
           ifunc = 0_ilp
           if( notran ) then
              if( ijob>=3_ilp ) then
                 ifunc = ijob - 2_ilp
                 call stdlib_slaset( 'F', m, n, zero, zero, c, ldc )
                 call stdlib_slaset( 'F', m, n, zero, zero, f, ldf )
              else if( ijob>=1_ilp .and. notran ) then
                 isolve = 2_ilp
              end if
           end if
           if( ( mb<=1_ilp .and. nb<=1_ilp ) .or. ( mb>=m .and. nb>=n ) )then
              loop_30: do iround = 1, isolve
                 ! use unblocked level 2 solver
                 dscale = zero
                 dsum = one
                 pq = 0_ilp
                 call stdlib_stgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,iwork, pq, info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_slacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_slacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_slaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_slaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_slacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_slacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp
           i = 1_ilp
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           if( a( i, i-1 )/=zero )i = i + 1_ilp
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           if( b( j, j-1 )/=zero )j = j + 1_ilp
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j)-subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1,..., 1; j = 1, 2,..., q
                 dscale = zero
                 dsum = one
                 pq = 0_ilp
                 scale = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp
                       mb = ie - is + 1_ilp
                       ppqq = 0_ilp
                       call stdlib_stgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,iwork( q+2 ), ppqq, linfo )
                       if( linfo>0_ilp )info = linfo
                       pq = pq + ppqq
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          do k = js, je
                             call stdlib_sscal( is-1, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( is-1, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          do k = js, je
                             call stdlib_sscal( m-ie, scaloc, c( ie+1, k ), 1_ilp )
                             call stdlib_sscal( m-ie, scaloc, f( ie+1, k ), 1_ilp )
                          end do
                          do k = je + 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_sgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp, is ), lda, c( is, &
                                    js ), ldc, one,c( 1_ilp, js ), ldc )
                          call stdlib_sgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp, is ), ldd, c( is, &
                                    js ), ldc, one,f( 1_ilp, js ), ldf )
                       end if
                       if( j<q ) then
                          call stdlib_sgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, b( js, &
                                    je+1 ), ldb,one, c( is, je+1 ), ldc )
                          call stdlib_sgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, e( js, &
                                    je+1 ), lde,one, f( is, je+1 ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_slacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_slacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_slaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_slaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_slacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_slacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                   ! a(i, i)**t * r(i, j)  + d(i, i)**t * l(i, j)  =  c(i, j)
                   ! r(i, j)  * b(j, j)**t + l(i, j)  * e(j, j)**t = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    call stdlib_stgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,iwork( q+2 ), ppqq, linfo )
                    if( linfo>0_ilp )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       do k = js, je
                          call stdlib_sscal( is-1, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_sscal( is-1, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       do k = js, je
                          call stdlib_sscal( m-ie, scaloc, c( ie+1, k ), 1_ilp )
                          call stdlib_sscal( m-ie, scaloc, f( ie+1, k ), 1_ilp )
                       end do
                       do k = je + 1, n
                          call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_sgemm( 'N', 'T', mb, js-1, nb, one, c( is, js ),ldc, b( 1_ilp, js )&
                                 , ldb, one, f( is, 1_ilp ),ldf )
                       call stdlib_sgemm( 'N', 'T', mb, js-1, nb, one, f( is, js ),ldf, e( 1_ilp, js )&
                                 , lde, one, f( is, 1_ilp ),ldf )
                    end if
                    if( i<p ) then
                       call stdlib_sgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( is, &
                                 js ), ldc, one,c( ie+1, js ), ldc )
                       call stdlib_sgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( is, &
                                 js ), ldf, one,c( ie+1, js ), ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_stgsyl

     pure module subroutine stdlib_dtgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! DTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                 (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with real entries. (A, D) and (B, E) must be in
     !! generalized (real) Schur canonical form, i.e. A, B are upper quasi
     !! triangular and D, E are upper triangular.
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve  Zx = scale b, where
     !! Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]         (2)
     !! [ kron(In, D)  -kron(E**T, Im) ].
     !! Here Ik is the identity matrix of size k and X**T is the transpose of
     !! X. kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'T', DTGSYL solves the transposed system Z**T*y = scale*b,
     !! which is equivalent to solve for R and L in
     !! A**T * R + D**T * L = scale * C           (3)
     !! R * B**T + L * E**T = scale * -F
     !! This case (TRANS = 'T') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using DLACON.
     !! If IJOB >= 1, DTGSYL computes a Frobenius norm-based estimate
     !! of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z. See [1-2] for more
     !! information.
     !! This is a level 3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_dcopy by calls to stdlib_dlaset.
        ! sven hammarling, 1/5/02.
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, ppqq, pq, q
           real(dp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>4_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( notran ) then
                 if( ijob==1_ilp .or. ijob==2_ilp ) then
                    lwmin = max( 1_ilp, 2_ilp*m*n )
                 else
                    lwmin = 1_ilp
                 end if
              else
                 lwmin = 1_ilp
              end if
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              scale = 1_ilp
              if( notran ) then
                 if( ijob/=0_ilp ) then
                    dif = 0_ilp
                 end if
              end if
              return
           end if
           ! determine optimal block sizes mb and nb
           mb = stdlib_ilaenv( 2_ilp, 'DTGSYL', trans, m, n, -1_ilp, -1_ilp )
           nb = stdlib_ilaenv( 5_ilp, 'DTGSYL', trans, m, n, -1_ilp, -1_ilp )
           isolve = 1_ilp
           ifunc = 0_ilp
           if( notran ) then
              if( ijob>=3_ilp ) then
                 ifunc = ijob - 2_ilp
                 call stdlib_dlaset( 'F', m, n, zero, zero, c, ldc )
                 call stdlib_dlaset( 'F', m, n, zero, zero, f, ldf )
              else if( ijob>=1_ilp ) then
                 isolve = 2_ilp
              end if
           end if
           if( ( mb<=1_ilp .and. nb<=1_ilp ) .or. ( mb>=m .and. nb>=n ) )then
              loop_30: do iround = 1, isolve
                 ! use unblocked level 2 solver
                 dscale = zero
                 dsum = one
                 pq = 0_ilp
                 call stdlib_dtgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,iwork, pq, info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_dlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_dlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_dlaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_dlaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_dlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_dlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp
           i = 1_ilp
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           if( a( i, i-1 )/=zero )i = i + 1_ilp
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           if( b( j, j-1 )/=zero )j = j + 1_ilp
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j)-subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1,..., 1; j = 1, 2,..., q
                 dscale = zero
                 dsum = one
                 pq = 0_ilp
                 scale = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp
                       mb = ie - is + 1_ilp
                       ppqq = 0_ilp
                       call stdlib_dtgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,iwork( q+2 ), ppqq, linfo )
                       if( linfo>0_ilp )info = linfo
                       pq = pq + ppqq
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          do k = js, je
                             call stdlib_dscal( is-1, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( is-1, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          do k = js, je
                             call stdlib_dscal( m-ie, scaloc, c( ie+1, k ), 1_ilp )
                             call stdlib_dscal( m-ie, scaloc, f( ie+1, k ), 1_ilp )
                          end do
                          do k = je + 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_dgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp, is ), lda, c( is, &
                                    js ), ldc, one,c( 1_ilp, js ), ldc )
                          call stdlib_dgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp, is ), ldd, c( is, &
                                    js ), ldc, one,f( 1_ilp, js ), ldf )
                       end if
                       if( j<q ) then
                          call stdlib_dgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, b( js, &
                                    je+1 ), ldb,one, c( is, je+1 ), ldc )
                          call stdlib_dgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, e( js, &
                                    je+1 ), lde,one, f( is, je+1 ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_dlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_dlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_dlaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_dlaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_dlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_dlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                   ! a(i, i)**t * r(i, j)  + d(i, i)**t * l(i, j)  =  c(i, j)
                   ! r(i, j)  * b(j, j)**t + l(i, j)  * e(j, j)**t = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    call stdlib_dtgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,iwork( q+2 ), ppqq, linfo )
                    if( linfo>0_ilp )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       do k = js, je
                          call stdlib_dscal( is-1, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_dscal( is-1, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       do k = js, je
                          call stdlib_dscal( m-ie, scaloc, c( ie+1, k ), 1_ilp )
                          call stdlib_dscal( m-ie, scaloc, f( ie+1, k ), 1_ilp )
                       end do
                       do k = je + 1, n
                          call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                          call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_dgemm( 'N', 'T', mb, js-1, nb, one, c( is, js ),ldc, b( 1_ilp, js )&
                                 , ldb, one, f( is, 1_ilp ),ldf )
                       call stdlib_dgemm( 'N', 'T', mb, js-1, nb, one, f( is, js ),ldf, e( 1_ilp, js )&
                                 , lde, one, f( is, 1_ilp ),ldf )
                    end if
                    if( i<p ) then
                       call stdlib_dgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( is, &
                                 js ), ldc, one,c( ie+1, js ), ldc )
                       call stdlib_dgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( is, &
                                 js ), ldf, one,c( ie+1, js ), ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_dtgsyl


     pure module subroutine stdlib_ctgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! CTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C            (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with complex entries. A, B, D and E are upper
     !! triangular (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
     !! is an output scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
     !! is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]        (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Here Ix is the identity matrix of size x and X**H is the conjugate
     !! transpose of X. Kron(X, Y) is the Kronecker product between the
     !! matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H *y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R + D**H * L = scale * C           (3)
     !! R * B**H + L * E**H = scale * -F
     !! This case (TRANS = 'C') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using CLACON.
     !! If IJOB >= 1, CTGSYL computes a Frobenius norm-based estimate of
     !! Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z.
     !! This is a level-3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_ccopy by calls to stdlib_claset.
        ! sven hammarling, 1/5/02.
           
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, pq, q
           real(sp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>4_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( notran ) then
                 if( ijob==1_ilp .or. ijob==2_ilp ) then
                    lwmin = max( 1_ilp, 2_ilp*m*n )
                 else
                    lwmin = 1_ilp
                 end if
              else
                 lwmin = 1_ilp
              end if
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              scale = 1_ilp
              if( notran ) then
                 if( ijob/=0_ilp ) then
                    dif = 0_ilp
                 end if
              end if
              return
           end if
           ! determine  optimal block sizes mb and nb
           mb = stdlib_ilaenv( 2_ilp, 'CTGSYL', trans, m, n, -1_ilp, -1_ilp )
           nb = stdlib_ilaenv( 5_ilp, 'CTGSYL', trans, m, n, -1_ilp, -1_ilp )
           isolve = 1_ilp
           ifunc = 0_ilp
           if( notran ) then
              if( ijob>=3_ilp ) then
                 ifunc = ijob - 2_ilp
                 call stdlib_claset( 'F', m, n, czero, czero, c, ldc )
                 call stdlib_claset( 'F', m, n, czero, czero, f, ldf )
              else if( ijob>=1_ilp .and. notran ) then
                 isolve = 2_ilp
              end if
           end if
           if( ( mb<=1_ilp .and. nb<=1_ilp ) .or. ( mb>=m .and. nb>=n ) )then
              ! use unblocked level 2 solver
              loop_30: do iround = 1, isolve
                 scale = one
                 dscale = zero
                 dsum = one
                 pq = m*n
                 call stdlib_ctgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_clacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_clacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_claset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_claset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_clacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_clacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp
           i = 1_ilp
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j) - subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
                 pq = 0_ilp
                 scale = one
                 dscale = zero
                 dsum = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp
                       mb = ie - is + 1_ilp
                       call stdlib_ctgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,linfo )
                       if( linfo>0_ilp )info = linfo
                       pq = pq + mb*nb
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                       
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                       
                          end do
                          do k = js, je
                             call stdlib_cscal( is-1, cmplx( scaloc, zero,KIND=sp),c( 1_ilp, k ), 1_ilp )
                                       
                             call stdlib_cscal( is-1, cmplx( scaloc, zero,KIND=sp),f( 1_ilp, k ), 1_ilp )
                                       
                          end do
                          do k = js, je
                             call stdlib_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),c( ie+1, k ), &
                                       1_ilp )
                             call stdlib_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),f( ie+1, k ), &
                                       1_ilp )
                          end do
                          do k = je + 1, n
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                       
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i,j) and l(i,j) into remaining equation.
                       if( i>1_ilp ) then
                          call stdlib_cgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=sp), a(&
                           1_ilp, is ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=sp),c( 1_ilp, js ), &
                                     ldc )
                          call stdlib_cgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=sp), d(&
                           1_ilp, is ), ldd,c( is, js ), ldc, cmplx( one, zero,KIND=sp),f( 1_ilp, js ), &
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_cgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=sp), f( &
                          is, js ), ldf,b( js, je+1 ), ldb, cmplx( one, zero,KIND=sp),c( is, je+1 &
                                    ), ldc )
                          call stdlib_cgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=sp), f( &
                          is, js ), ldf,e( js, je+1 ), lde, cmplx( one, zero,KIND=sp),f( is, je+1 &
                                    ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_clacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_clacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_claset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_claset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_clacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_clacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                  ! a(i, i)**h * r(i, j) + d(i, i)**h * l(i, j) = c(i, j)
                  ! r(i, j) * b(j, j)  + l(i, j) * e(j, j) = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    call stdlib_ctgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,linfo )
                    if( linfo>0_ilp )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       do k = js, je
                          call stdlib_cscal( is-1, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_cscal( is-1, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       do k = js, je
                          call stdlib_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),c( ie+1, k ), 1_ilp )
                                    
                          call stdlib_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),f( ie+1, k ), 1_ilp )
                                    
                       end do
                       do k = je + 1, n
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i,j) and l(i,j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_cgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=sp), c( is,&
                        js ), ldc,b( 1_ilp, js ), ldb, cmplx( one, zero,KIND=sp),f( is, 1_ilp ), ldf )
                                  
                       call stdlib_cgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=sp), f( is,&
                        js ), ldf,e( 1_ilp, js ), lde, cmplx( one, zero,KIND=sp),f( is, 1_ilp ), ldf )
                                  
                    end if
                    if( i<p ) then
                       call stdlib_cgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=sp), a( &
                       is, ie+1 ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=sp),c( ie+1, js ), &
                                 ldc )
                       call stdlib_cgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=sp), d( &
                       is, ie+1 ), ldd,f( is, js ), ldf, cmplx( one, zero,KIND=sp),c( ie+1, js ), &
                                 ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ctgsyl

     pure module subroutine stdlib_ztgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! ZTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C            (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with complex entries. A, B, D and E are upper
     !! triangular (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
     !! is an output scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
     !! is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]        (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Here Ix is the identity matrix of size x and X**H is the conjugate
     !! transpose of X. Kron(X, Y) is the Kronecker product between the
     !! matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H *y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R + D**H * L = scale * C           (3)
     !! R * B**H + L * E**H = scale * -F
     !! This case (TRANS = 'C') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using ZLACON.
     !! If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of
     !! Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z.
     !! This is a level-3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_ccopy by calls to stdlib_claset.
        ! sven hammarling, 1/5/02.
           
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, pq, q
           real(dp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>4_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( notran ) then
                 if( ijob==1_ilp .or. ijob==2_ilp ) then
                    lwmin = max( 1_ilp, 2_ilp*m*n )
                 else
                    lwmin = 1_ilp
                 end if
              else
                 lwmin = 1_ilp
              end if
              work( 1_ilp ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              scale = 1_ilp
              if( notran ) then
                 if( ijob/=0_ilp ) then
                    dif = 0_ilp
                 end if
              end if
              return
           end if
           ! determine  optimal block sizes mb and nb
           mb = stdlib_ilaenv( 2_ilp, 'ZTGSYL', trans, m, n, -1_ilp, -1_ilp )
           nb = stdlib_ilaenv( 5_ilp, 'ZTGSYL', trans, m, n, -1_ilp, -1_ilp )
           isolve = 1_ilp
           ifunc = 0_ilp
           if( notran ) then
              if( ijob>=3_ilp ) then
                 ifunc = ijob - 2_ilp
                 call stdlib_zlaset( 'F', m, n, czero, czero, c, ldc )
                 call stdlib_zlaset( 'F', m, n, czero, czero, f, ldf )
              else if( ijob>=1_ilp .and. notran ) then
                 isolve = 2_ilp
              end if
           end if
           if( ( mb<=1_ilp .and. nb<=1_ilp ) .or. ( mb>=m .and. nb>=n ) )then
              ! use unblocked level 2 solver
              loop_30: do iround = 1, isolve
                 scale = one
                 dscale = zero
                 dsum = one
                 pq = m*n
                 call stdlib_ztgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_zlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_zlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_zlaset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_zlaset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_zlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_zlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp
           i = 1_ilp
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j) - subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
                 pq = 0_ilp
                 scale = one
                 dscale = zero
                 dsum = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp
                       mb = ie - is + 1_ilp
                       call stdlib_ztgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,linfo )
                       if( linfo>0_ilp )info = linfo
                       pq = pq + mb*nb
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp, k ), 1_ilp )
                                       
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp, k ), 1_ilp )
                                       
                          end do
                          do k = js, je
                             call stdlib_zscal( is-1, cmplx( scaloc, zero,KIND=dp),c( 1_ilp, k ), 1_ilp )
                                       
                             call stdlib_zscal( is-1, cmplx( scaloc, zero,KIND=dp),f( 1_ilp, k ), 1_ilp )
                                       
                          end do
                          do k = js, je
                             call stdlib_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),c( ie+1, k ), &
                                       1_ilp )
                             call stdlib_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),f( ie+1, k ), &
                                       1_ilp )
                          end do
                          do k = je + 1, n
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp, k ), 1_ilp )
                                       
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp, k ), 1_ilp )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i,j) and l(i,j) into remaining equation.
                       if( i>1_ilp ) then
                          call stdlib_zgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=dp), a(&
                           1_ilp, is ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=dp),c( 1_ilp, js ), &
                                     ldc )
                          call stdlib_zgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=dp), d(&
                           1_ilp, is ), ldd,c( is, js ), ldc, cmplx( one, zero,KIND=dp),f( 1_ilp, js ), &
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_zgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=dp), f( &
                          is, js ), ldf,b( js, je+1 ), ldb,cmplx( one, zero,KIND=dp), c( is, je+1 &
                                    ),ldc )
                          call stdlib_zgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=dp), f( &
                          is, js ), ldf,e( js, je+1 ), lde,cmplx( one, zero,KIND=dp), f( is, je+1 &
                                    ),ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp .or. ijob==3_ilp ) then
                       dif = sqrt( real( 2_ilp*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp .and. iround==1_ilp ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_zlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_zlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_zlaset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_zlaset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp .and. iround==2_ilp ) then
                    call stdlib_zlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_zlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                  ! a(i, i)**h * r(i, j) + d(i, i)**h * l(i, j) = c(i, j)
                  ! r(i, j) * b(j, j)  + l(i, j) * e(j, j) = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    call stdlib_ztgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,linfo )
                    if( linfo>0_ilp )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       do k = js, je
                          call stdlib_zscal( is-1, cmplx( scaloc, zero,KIND=dp),c( 1_ilp, k ), 1_ilp )
                                    
                          call stdlib_zscal( is-1, cmplx( scaloc, zero,KIND=dp),f( 1_ilp, k ), 1_ilp )
                                    
                       end do
                       do k = js, je
                          call stdlib_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),c( ie+1, k ), 1_ilp )
                                    
                          call stdlib_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),f( ie+1, k ), 1_ilp )
                                    
                       end do
                       do k = je + 1, n
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i,j) and l(i,j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_zgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=dp), c( is,&
                        js ), ldc,b( 1_ilp, js ), ldb, cmplx( one, zero,KIND=dp),f( is, 1_ilp ), ldf )
                                  
                       call stdlib_zgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=dp), f( is,&
                        js ), ldf,e( 1_ilp, js ), lde, cmplx( one, zero,KIND=dp),f( is, 1_ilp ), ldf )
                                  
                    end if
                    if( i<p ) then
                       call stdlib_zgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=dp), a( &
                       is, ie+1 ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=dp),c( ie+1, js ), &
                                 ldc )
                       call stdlib_zgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=dp), d( &
                       is, ie+1 ), ldd,f( is, js ), ldf, cmplx( one, zero,KIND=dp),c( ie+1, js ), &
                                 ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ztgsyl




     pure module subroutine stdlib_stgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! STGSY2 solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                (1)
     !! D * R - L * E = scale * F,
     !! using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)
     !! must be in generalized Schur canonical form, i.e. A, B are upper
     !! quasi triangular and D, E are upper triangular. The solution (R, L)
     !! overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor
     !! chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Z*x = scale*b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**T, Im) ],
     !! Ik is the identity matrix of size k and X**T is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! In the process of solving (1), we solve a number of such systems
     !! where Dim(In), Dim(In) = 1 or 2.
     !! If TRANS = 'T', solve the transposed system Z**T*y = scale*b for y,
     !! which is equivalent to solve for R and L in
     !! A**T * R  + D**T * L   = scale * C           (3)
     !! R  * B**T + L  * E**T  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! sigma_min(Z) using reverse communication with SLACON.
     !! STGSY2 also (IJOB >= 1) contributes to the computation in STGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of the matrix pair in
     !! STGSYL. See STGSYL for details.
               ldf, scale, rdsum, rdscal,iwork, pq, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info, pq
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_scopy by calls to stdlib_slaset.
        ! sven hammarling, 27/5/02.
           ! Parameters 
           integer(ilp), parameter :: ldz = 8_ilp
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ie, ierr, ii, is, isp1, j, je, jj, js, jsp1, k, mb, nb, p, q, &
                     zdim
           real(sp) :: alpha, scaloc
           ! Local Arrays 
           integer(ilp) :: ipiv(ldz), jpiv(ldz)
           real(sp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           ierr = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>2_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSY2', -info )
              return
           end if
           ! determine block structure of a
           pq = 0_ilp
           p = 0_ilp
           i = 1_ilp
           10 continue
           if( i>m )go to 20
           p = p + 1_ilp
           iwork( p ) = i
           if( i==m )go to 20
           if( a( i+1, i )/=zero ) then
              i = i + 2_ilp
           else
              i = i + 1_ilp
           end if
           go to 10
           20 continue
           iwork( p+1 ) = m + 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           30 continue
           if( j>n )go to 40
           q = q + 1_ilp
           iwork( q ) = j
           if( j==n )go to 40
           if( b( j+1, j )/=zero ) then
              j = j + 2_ilp
           else
              j = j + 1_ilp
           end if
           go to 30
           40 continue
           iwork( q+1 ) = n + 1_ilp
           pq = p*( q-p-1 )
           if( notran ) then
              ! solve (i, j) - subsystem
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
              scale = one
              scaloc = one
              loop_120: do j = p + 2, q
                 js = iwork( j )
                 jsp1 = js + 1_ilp
                 je = iwork( j+1 ) - 1_ilp
                 nb = je - js + 1_ilp
                 loop_110: do i = p, 1, -1
                    is = iwork( i )
                    isp1 = is + 1_ilp
                    ie = iwork( i+1 ) - 1_ilp
                    mb = ie - is + 1_ilp
                    zdim = mb*nb*2_ilp
                    if( ( mb==1_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 2-by-2 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = d( is, is )
                       z( 1_ilp, 2_ilp ) = -b( js, js )
                       z( 2_ilp, 2_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = f( is, js )
                       ! solve z * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       f( is, js ) = rhs( 2_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          alpha = -rhs( 1_ilp )
                          call stdlib_saxpy( is-1, alpha, a( 1_ilp, is ), 1_ilp, c( 1_ilp, js ),1_ilp )
                          call stdlib_saxpy( is-1, alpha, d( 1_ilp, is ), 1_ilp, f( 1_ilp, js ),1_ilp )
                       end if
                       if( j<q ) then
                          call stdlib_saxpy( n-je, rhs( 2_ilp ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_saxpy( n-je, rhs( 2_ilp ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==1_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = zero
                       z( 3_ilp, 1_ilp ) = d( is, is )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = zero
                       z( 2_ilp, 2_ilp ) = a( is, is )
                       z( 3_ilp, 2_ilp ) = zero
                       z( 4_ilp, 2_ilp ) = d( is, is )
                       z( 1_ilp, 3_ilp ) = -b( js, js )
                       z( 2_ilp, 3_ilp ) = -b( js, jsp1 )
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = -e( js, jsp1 )
                       z( 1_ilp, 4_ilp ) = -b( jsp1, js )
                       z( 2_ilp, 4_ilp ) = -b( jsp1, jsp1 )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( is, jsp1 )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( is, jsp1 )
                       ! solve z * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( is, jsp1 ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( is, jsp1 ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_sger( is-1, nb, -one, a( 1_ilp, is ), 1_ilp, rhs( 1_ilp ),1_ilp, c( 1_ilp, js ),&
                                     ldc )
                          call stdlib_sger( is-1, nb, -one, d( 1_ilp, is ), 1_ilp, rhs( 1_ilp ),1_ilp, f( 1_ilp, js ),&
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_saxpy( n-je, rhs( 3_ilp ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_saxpy( n-je, rhs( 3_ilp ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                          call stdlib_saxpy( n-je, rhs( 4_ilp ), b( jsp1, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_saxpy( n-je, rhs( 4_ilp ), e( jsp1, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( isp1, is )
                       z( 3_ilp, 1_ilp ) = d( is, is )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = a( is, isp1 )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 3_ilp, 2_ilp ) = d( is, isp1 )
                       z( 4_ilp, 2_ilp ) = d( isp1, isp1 )
                       z( 1_ilp, 3_ilp ) = -b( js, js )
                       z( 2_ilp, 3_ilp ) = zero
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = -b( js, js )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( isp1, js )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( isp1, js )
                       ! solve z * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( isp1, js ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( isp1, js ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_sgemv( 'N', is-1, mb, -one, a( 1_ilp, is ), lda,rhs( 1_ilp ), 1_ilp, &
                                    one, c( 1_ilp, js ), 1_ilp )
                          call stdlib_sgemv( 'N', is-1, mb, -one, d( 1_ilp, is ), ldd,rhs( 1_ilp ), 1_ilp, &
                                    one, f( 1_ilp, js ), 1_ilp )
                       end if
                       if( j<q ) then
                          call stdlib_sger( mb, n-je, one, rhs( 3_ilp ), 1_ilp,b( js, je+1 ), ldb, c( is, &
                                    je+1 ), ldc )
                          call stdlib_sger( mb, n-je, one, rhs( 3_ilp ), 1_ilp,e( js, je+1 ), lde, f( is, &
                                    je+1 ), ldf )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build an 8-by-8 system z * x = rhs
                       call stdlib_slaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( isp1, is )
                       z( 5_ilp, 1_ilp ) = d( is, is )
                       z( 1_ilp, 2_ilp ) = a( is, isp1 )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 5_ilp, 2_ilp ) = d( is, isp1 )
                       z( 6_ilp, 2_ilp ) = d( isp1, isp1 )
                       z( 3_ilp, 3_ilp ) = a( is, is )
                       z( 4_ilp, 3_ilp ) = a( isp1, is )
                       z( 7_ilp, 3_ilp ) = d( is, is )
                       z( 3_ilp, 4_ilp ) = a( is, isp1 )
                       z( 4_ilp, 4_ilp ) = a( isp1, isp1 )
                       z( 7_ilp, 4_ilp ) = d( is, isp1 )
                       z( 8_ilp, 4_ilp ) = d( isp1, isp1 )
                       z( 1_ilp, 5_ilp ) = -b( js, js )
                       z( 3_ilp, 5_ilp ) = -b( js, jsp1 )
                       z( 5_ilp, 5_ilp ) = -e( js, js )
                       z( 7_ilp, 5_ilp ) = -e( js, jsp1 )
                       z( 2_ilp, 6_ilp ) = -b( js, js )
                       z( 4_ilp, 6_ilp ) = -b( js, jsp1 )
                       z( 6_ilp, 6_ilp ) = -e( js, js )
                       z( 8_ilp, 6_ilp ) = -e( js, jsp1 )
                       z( 1_ilp, 7_ilp ) = -b( jsp1, js )
                       z( 3_ilp, 7_ilp ) = -b( jsp1, jsp1 )
                       z( 7_ilp, 7_ilp ) = -e( jsp1, jsp1 )
                       z( 2_ilp, 8_ilp ) = -b( jsp1, js )
                       z( 4_ilp, 8_ilp ) = -b( jsp1, jsp1 )
                       z( 8_ilp, 8_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_scopy( mb, c( is, js+jj ), 1_ilp, rhs( k ), 1_ilp )
                          call stdlib_scopy( mb, f( is, js+jj ), 1_ilp, rhs( ii ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_scopy( mb, rhs( k ), 1_ilp, c( is, js+jj ), 1_ilp )
                          call stdlib_scopy( mb, rhs( ii ), 1_ilp, f( is, js+jj ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_sgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp, is ), lda, rhs( 1_ilp &
                                    ), mb, one,c( 1_ilp, js ), ldc )
                          call stdlib_sgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp, is ), ldd, rhs( 1_ilp &
                                    ), mb, one,f( 1_ilp, js ), ldf )
                       end if
                       if( j<q ) then
                          k = mb*nb + 1_ilp
                          call stdlib_sgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, b( js, je+&
                                    1_ilp ), ldb, one,c( is, je+1 ), ldc )
                          call stdlib_sgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, e( js, je+&
                                    1_ilp ), lde, one,f( is, je+1 ), ldf )
                       end if
                    end if
                 end do loop_110
              end do loop_120
           else
              ! solve (i, j) - subsystem
                   ! a(i, i)**t * r(i, j) + d(i, i)**t * l(j, j)  =  c(i, j)
                   ! r(i, i)  * b(j, j) + l(i, j)  * e(j, j)  = -f(i, j)
              ! for i = 1, 2, ..., p, j = q, q - 1, ..., 1
              scale = one
              scaloc = one
              loop_200: do i = 1, p
                 is = iwork( i )
                 isp1 = is + 1_ilp
                 ie = iwork( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_190: do j = q, p + 2, -1
                    js = iwork( j )
                    jsp1 = js + 1_ilp
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    zdim = mb*nb*2_ilp
                    if( ( mb==1_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 2-by-2 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = -b( js, js )
                       z( 1_ilp, 2_ilp ) = d( is, is )
                       z( 2_ilp, 2_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = f( is, js )
                       ! solve z**t * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       f( is, js ) = rhs( 2_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          alpha = rhs( 1_ilp )
                          call stdlib_saxpy( js-1, alpha, b( 1_ilp, js ), 1_ilp, f( is, 1_ilp ),ldf )
                          alpha = rhs( 2_ilp )
                          call stdlib_saxpy( js-1, alpha, e( 1_ilp, js ), 1_ilp, f( is, 1_ilp ),ldf )
                       end if
                       if( i<p ) then
                          alpha = -rhs( 1_ilp )
                          call stdlib_saxpy( m-ie, alpha, a( is, ie+1 ), lda,c( ie+1, js ), 1_ilp )
                                    
                          alpha = -rhs( 2_ilp )
                          call stdlib_saxpy( m-ie, alpha, d( is, ie+1 ), ldd,c( ie+1, js ), 1_ilp )
                                    
                       end if
                    else if( ( mb==1_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = zero
                       z( 3_ilp, 1_ilp ) = -b( js, js )
                       z( 4_ilp, 1_ilp ) = -b( jsp1, js )
                       z( 1_ilp, 2_ilp ) = zero
                       z( 2_ilp, 2_ilp ) = a( is, is )
                       z( 3_ilp, 2_ilp ) = -b( js, jsp1 )
                       z( 4_ilp, 2_ilp ) = -b( jsp1, jsp1 )
                       z( 1_ilp, 3_ilp ) = d( is, is )
                       z( 2_ilp, 3_ilp ) = zero
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = d( is, is )
                       z( 3_ilp, 4_ilp ) = -e( js, jsp1 )
                       z( 4_ilp, 4_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( is, jsp1 )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( is, jsp1 )
                       ! solve z**t * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( is, jsp1 ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( is, jsp1 ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_saxpy( js-1, rhs( 1_ilp ), b( 1_ilp, js ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_saxpy( js-1, rhs( 2_ilp ), b( 1_ilp, jsp1 ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_saxpy( js-1, rhs( 3_ilp ), e( 1_ilp, js ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_saxpy( js-1, rhs( 4_ilp ), e( 1_ilp, jsp1 ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                       end if
                       if( i<p ) then
                          call stdlib_sger( m-ie, nb, -one, a( is, ie+1 ), lda,rhs( 1_ilp ), 1_ilp, c( ie+&
                                    1_ilp, js ), ldc )
                          call stdlib_sger( m-ie, nb, -one, d( is, ie+1 ), ldd,rhs( 3_ilp ), 1_ilp, c( ie+&
                                    1_ilp, js ), ldc )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( is, isp1 )
                       z( 3_ilp, 1_ilp ) = -b( js, js )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = a( isp1, is )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 3_ilp, 2_ilp ) = zero
                       z( 4_ilp, 2_ilp ) = -b( js, js )
                       z( 1_ilp, 3_ilp ) = d( is, is )
                       z( 2_ilp, 3_ilp ) = d( is, isp1 )
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = d( isp1, isp1 )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( isp1, js )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( isp1, js )
                       ! solve z**t * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( isp1, js ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( isp1, js ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_sger( mb, js-1, one, rhs( 1_ilp ), 1_ilp, b( 1_ilp, js ),1_ilp, f( is, 1_ilp ), &
                                    ldf )
                          call stdlib_sger( mb, js-1, one, rhs( 3_ilp ), 1_ilp, e( 1_ilp, js ),1_ilp, f( is, 1_ilp ), &
                                    ldf )
                       end if
                       if( i<p ) then
                          call stdlib_sgemv( 'T', mb, m-ie, -one, a( is, ie+1 ),lda, rhs( 1_ilp ), 1_ilp, &
                                    one, c( ie+1, js ),1_ilp )
                          call stdlib_sgemv( 'T', mb, m-ie, -one, d( is, ie+1 ),ldd, rhs( 3_ilp ), 1_ilp, &
                                    one, c( ie+1, js ),1_ilp )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build an 8-by-8 system z**t * x = rhs
                       call stdlib_slaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( is, isp1 )
                       z( 5_ilp, 1_ilp ) = -b( js, js )
                       z( 7_ilp, 1_ilp ) = -b( jsp1, js )
                       z( 1_ilp, 2_ilp ) = a( isp1, is )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 6_ilp, 2_ilp ) = -b( js, js )
                       z( 8_ilp, 2_ilp ) = -b( jsp1, js )
                       z( 3_ilp, 3_ilp ) = a( is, is )
                       z( 4_ilp, 3_ilp ) = a( is, isp1 )
                       z( 5_ilp, 3_ilp ) = -b( js, jsp1 )
                       z( 7_ilp, 3_ilp ) = -b( jsp1, jsp1 )
                       z( 3_ilp, 4_ilp ) = a( isp1, is )
                       z( 4_ilp, 4_ilp ) = a( isp1, isp1 )
                       z( 6_ilp, 4_ilp ) = -b( js, jsp1 )
                       z( 8_ilp, 4_ilp ) = -b( jsp1, jsp1 )
                       z( 1_ilp, 5_ilp ) = d( is, is )
                       z( 2_ilp, 5_ilp ) = d( is, isp1 )
                       z( 5_ilp, 5_ilp ) = -e( js, js )
                       z( 2_ilp, 6_ilp ) = d( isp1, isp1 )
                       z( 6_ilp, 6_ilp ) = -e( js, js )
                       z( 3_ilp, 7_ilp ) = d( is, is )
                       z( 4_ilp, 7_ilp ) = d( is, isp1 )
                       z( 5_ilp, 7_ilp ) = -e( js, jsp1 )
                       z( 7_ilp, 7_ilp ) = -e( jsp1, jsp1 )
                       z( 4_ilp, 8_ilp ) = d( isp1, isp1 )
                       z( 6_ilp, 8_ilp ) = -e( js, jsp1 )
                       z( 8_ilp, 8_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_scopy( mb, c( is, js+jj ), 1_ilp, rhs( k ), 1_ilp )
                          call stdlib_scopy( mb, f( is, js+jj ), 1_ilp, rhs( ii ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z**t * x = rhs
                       call stdlib_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_sscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_scopy( mb, rhs( k ), 1_ilp, c( is, js+jj ), 1_ilp )
                          call stdlib_scopy( mb, rhs( ii ), 1_ilp, f( is, js+jj ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_sgemm( 'N', 'T', mb, js-1, nb, one,c( is, js ), ldc, b( 1_ilp, &
                                    js ), ldb, one,f( is, 1_ilp ), ldf )
                          call stdlib_sgemm( 'N', 'T', mb, js-1, nb, one,f( is, js ), ldf, e( 1_ilp, &
                                    js ), lde, one,f( is, 1_ilp ), ldf )
                       end if
                       if( i<p ) then
                          call stdlib_sgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( &
                                    is, js ), ldc,one, c( ie+1, js ), ldc )
                          call stdlib_sgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( &
                                    is, js ), ldf,one, c( ie+1, js ), ldc )
                       end if
                    end if
                 end do loop_190
              end do loop_200
           end if
           return
     end subroutine stdlib_stgsy2

     pure module subroutine stdlib_dtgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! DTGSY2 solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                (1)
     !! D * R - L * E = scale * F,
     !! using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)
     !! must be in generalized Schur canonical form, i.e. A, B are upper
     !! quasi triangular and D, E are upper triangular. The solution (R, L)
     !! overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor
     !! chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Z*x = scale*b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**T, Im) ],
     !! Ik is the identity matrix of size k and X**T is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! In the process of solving (1), we solve a number of such systems
     !! where Dim(In), Dim(In) = 1 or 2.
     !! If TRANS = 'T', solve the transposed system Z**T*y = scale*b for y,
     !! which is equivalent to solve for R and L in
     !! A**T * R  + D**T * L   = scale * C           (3)
     !! R  * B**T + L  * E**T  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! sigma_min(Z) using reverse communication with DLACON.
     !! DTGSY2 also (IJOB >= 1) contributes to the computation in DTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of the matrix pair in
     !! DTGSYL. See DTGSYL for details.
               ldf, scale, rdsum, rdscal,iwork, pq, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info, pq
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_dcopy by calls to stdlib_dlaset.
        ! sven hammarling, 27/5/02.
           ! Parameters 
           integer(ilp), parameter :: ldz = 8_ilp
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ie, ierr, ii, is, isp1, j, je, jj, js, jsp1, k, mb, nb, p, q, &
                     zdim
           real(dp) :: alpha, scaloc
           ! Local Arrays 
           integer(ilp) :: ipiv(ldz), jpiv(ldz)
           real(dp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           ierr = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>2_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSY2', -info )
              return
           end if
           ! determine block structure of a
           pq = 0_ilp
           p = 0_ilp
           i = 1_ilp
           10 continue
           if( i>m )go to 20
           p = p + 1_ilp
           iwork( p ) = i
           if( i==m )go to 20
           if( a( i+1, i )/=zero ) then
              i = i + 2_ilp
           else
              i = i + 1_ilp
           end if
           go to 10
           20 continue
           iwork( p+1 ) = m + 1_ilp
           ! determine block structure of b
           q = p + 1_ilp
           j = 1_ilp
           30 continue
           if( j>n )go to 40
           q = q + 1_ilp
           iwork( q ) = j
           if( j==n )go to 40
           if( b( j+1, j )/=zero ) then
              j = j + 2_ilp
           else
              j = j + 1_ilp
           end if
           go to 30
           40 continue
           iwork( q+1 ) = n + 1_ilp
           pq = p*( q-p-1 )
           if( notran ) then
              ! solve (i, j) - subsystem
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
              scale = one
              scaloc = one
              loop_120: do j = p + 2, q
                 js = iwork( j )
                 jsp1 = js + 1_ilp
                 je = iwork( j+1 ) - 1_ilp
                 nb = je - js + 1_ilp
                 loop_110: do i = p, 1, -1
                    is = iwork( i )
                    isp1 = is + 1_ilp
                    ie = iwork( i+1 ) - 1_ilp
                    mb = ie - is + 1_ilp
                    zdim = mb*nb*2_ilp
                    if( ( mb==1_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 2-by-2 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = d( is, is )
                       z( 1_ilp, 2_ilp ) = -b( js, js )
                       z( 2_ilp, 2_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = f( is, js )
                       ! solve z * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       f( is, js ) = rhs( 2_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          alpha = -rhs( 1_ilp )
                          call stdlib_daxpy( is-1, alpha, a( 1_ilp, is ), 1_ilp, c( 1_ilp, js ),1_ilp )
                          call stdlib_daxpy( is-1, alpha, d( 1_ilp, is ), 1_ilp, f( 1_ilp, js ),1_ilp )
                       end if
                       if( j<q ) then
                          call stdlib_daxpy( n-je, rhs( 2_ilp ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_daxpy( n-je, rhs( 2_ilp ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==1_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = zero
                       z( 3_ilp, 1_ilp ) = d( is, is )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = zero
                       z( 2_ilp, 2_ilp ) = a( is, is )
                       z( 3_ilp, 2_ilp ) = zero
                       z( 4_ilp, 2_ilp ) = d( is, is )
                       z( 1_ilp, 3_ilp ) = -b( js, js )
                       z( 2_ilp, 3_ilp ) = -b( js, jsp1 )
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = -e( js, jsp1 )
                       z( 1_ilp, 4_ilp ) = -b( jsp1, js )
                       z( 2_ilp, 4_ilp ) = -b( jsp1, jsp1 )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( is, jsp1 )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( is, jsp1 )
                       ! solve z * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( is, jsp1 ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( is, jsp1 ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_dger( is-1, nb, -one, a( 1_ilp, is ), 1_ilp, rhs( 1_ilp ),1_ilp, c( 1_ilp, js ),&
                                     ldc )
                          call stdlib_dger( is-1, nb, -one, d( 1_ilp, is ), 1_ilp, rhs( 1_ilp ),1_ilp, f( 1_ilp, js ),&
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_daxpy( n-je, rhs( 3_ilp ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_daxpy( n-je, rhs( 3_ilp ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                          call stdlib_daxpy( n-je, rhs( 4_ilp ), b( jsp1, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_daxpy( n-je, rhs( 4_ilp ), e( jsp1, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( isp1, is )
                       z( 3_ilp, 1_ilp ) = d( is, is )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = a( is, isp1 )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 3_ilp, 2_ilp ) = d( is, isp1 )
                       z( 4_ilp, 2_ilp ) = d( isp1, isp1 )
                       z( 1_ilp, 3_ilp ) = -b( js, js )
                       z( 2_ilp, 3_ilp ) = zero
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = -b( js, js )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( isp1, js )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( isp1, js )
                       ! solve z * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( isp1, js ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( isp1, js ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_dgemv( 'N', is-1, mb, -one, a( 1_ilp, is ), lda,rhs( 1_ilp ), 1_ilp, &
                                    one, c( 1_ilp, js ), 1_ilp )
                          call stdlib_dgemv( 'N', is-1, mb, -one, d( 1_ilp, is ), ldd,rhs( 1_ilp ), 1_ilp, &
                                    one, f( 1_ilp, js ), 1_ilp )
                       end if
                       if( j<q ) then
                          call stdlib_dger( mb, n-je, one, rhs( 3_ilp ), 1_ilp,b( js, je+1 ), ldb, c( is, &
                                    je+1 ), ldc )
                          call stdlib_dger( mb, n-je, one, rhs( 3_ilp ), 1_ilp,e( js, je+1 ), lde, f( is, &
                                    je+1 ), ldf )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build an 8-by-8 system z * x = rhs
                       call stdlib_dlaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( isp1, is )
                       z( 5_ilp, 1_ilp ) = d( is, is )
                       z( 1_ilp, 2_ilp ) = a( is, isp1 )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 5_ilp, 2_ilp ) = d( is, isp1 )
                       z( 6_ilp, 2_ilp ) = d( isp1, isp1 )
                       z( 3_ilp, 3_ilp ) = a( is, is )
                       z( 4_ilp, 3_ilp ) = a( isp1, is )
                       z( 7_ilp, 3_ilp ) = d( is, is )
                       z( 3_ilp, 4_ilp ) = a( is, isp1 )
                       z( 4_ilp, 4_ilp ) = a( isp1, isp1 )
                       z( 7_ilp, 4_ilp ) = d( is, isp1 )
                       z( 8_ilp, 4_ilp ) = d( isp1, isp1 )
                       z( 1_ilp, 5_ilp ) = -b( js, js )
                       z( 3_ilp, 5_ilp ) = -b( js, jsp1 )
                       z( 5_ilp, 5_ilp ) = -e( js, js )
                       z( 7_ilp, 5_ilp ) = -e( js, jsp1 )
                       z( 2_ilp, 6_ilp ) = -b( js, js )
                       z( 4_ilp, 6_ilp ) = -b( js, jsp1 )
                       z( 6_ilp, 6_ilp ) = -e( js, js )
                       z( 8_ilp, 6_ilp ) = -e( js, jsp1 )
                       z( 1_ilp, 7_ilp ) = -b( jsp1, js )
                       z( 3_ilp, 7_ilp ) = -b( jsp1, jsp1 )
                       z( 7_ilp, 7_ilp ) = -e( jsp1, jsp1 )
                       z( 2_ilp, 8_ilp ) = -b( jsp1, js )
                       z( 4_ilp, 8_ilp ) = -b( jsp1, jsp1 )
                       z( 8_ilp, 8_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_dcopy( mb, c( is, js+jj ), 1_ilp, rhs( k ), 1_ilp )
                          call stdlib_dcopy( mb, f( is, js+jj ), 1_ilp, rhs( ii ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       if( ijob==0_ilp ) then
                          call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                                call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_dcopy( mb, rhs( k ), 1_ilp, c( is, js+jj ), 1_ilp )
                          call stdlib_dcopy( mb, rhs( ii ), 1_ilp, f( is, js+jj ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp ) then
                          call stdlib_dgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp, is ), lda, rhs( 1_ilp &
                                    ), mb, one,c( 1_ilp, js ), ldc )
                          call stdlib_dgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp, is ), ldd, rhs( 1_ilp &
                                    ), mb, one,f( 1_ilp, js ), ldf )
                       end if
                       if( j<q ) then
                          k = mb*nb + 1_ilp
                          call stdlib_dgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, b( js, je+&
                                    1_ilp ), ldb, one,c( is, je+1 ), ldc )
                          call stdlib_dgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, e( js, je+&
                                    1_ilp ), lde, one,f( is, je+1 ), ldf )
                       end if
                    end if
                 end do loop_110
              end do loop_120
           else
              ! solve (i, j) - subsystem
                   ! a(i, i)**t * r(i, j) + d(i, i)**t * l(j, j)  =  c(i, j)
                   ! r(i, i)  * b(j, j) + l(i, j)  * e(j, j)  = -f(i, j)
              ! for i = 1, 2, ..., p, j = q, q - 1, ..., 1
              scale = one
              scaloc = one
              loop_200: do i = 1, p
                 is = iwork( i )
                 isp1 = is + 1_ilp
                 ie = iwork ( i+1 ) - 1_ilp
                 mb = ie - is + 1_ilp
                 loop_190: do j = q, p + 2, -1
                    js = iwork( j )
                    jsp1 = js + 1_ilp
                    je = iwork( j+1 ) - 1_ilp
                    nb = je - js + 1_ilp
                    zdim = mb*nb*2_ilp
                    if( ( mb==1_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 2-by-2 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = -b( js, js )
                       z( 1_ilp, 2_ilp ) = d( is, is )
                       z( 2_ilp, 2_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = f( is, js )
                       ! solve z**t * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       f( is, js ) = rhs( 2_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          alpha = rhs( 1_ilp )
                          call stdlib_daxpy( js-1, alpha, b( 1_ilp, js ), 1_ilp, f( is, 1_ilp ),ldf )
                          alpha = rhs( 2_ilp )
                          call stdlib_daxpy( js-1, alpha, e( 1_ilp, js ), 1_ilp, f( is, 1_ilp ),ldf )
                       end if
                       if( i<p ) then
                          alpha = -rhs( 1_ilp )
                          call stdlib_daxpy( m-ie, alpha, a( is, ie+1 ), lda,c( ie+1, js ), 1_ilp )
                                    
                          alpha = -rhs( 2_ilp )
                          call stdlib_daxpy( m-ie, alpha, d( is, ie+1 ), ldd,c( ie+1, js ), 1_ilp )
                                    
                       end if
                    else if( ( mb==1_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = zero
                       z( 3_ilp, 1_ilp ) = -b( js, js )
                       z( 4_ilp, 1_ilp ) = -b( jsp1, js )
                       z( 1_ilp, 2_ilp ) = zero
                       z( 2_ilp, 2_ilp ) = a( is, is )
                       z( 3_ilp, 2_ilp ) = -b( js, jsp1 )
                       z( 4_ilp, 2_ilp ) = -b( jsp1, jsp1 )
                       z( 1_ilp, 3_ilp ) = d( is, is )
                       z( 2_ilp, 3_ilp ) = zero
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = d( is, is )
                       z( 3_ilp, 4_ilp ) = -e( js, jsp1 )
                       z( 4_ilp, 4_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( is, jsp1 )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( is, jsp1 )
                       ! solve z**t * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( is, jsp1 ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( is, jsp1 ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_daxpy( js-1, rhs( 1_ilp ), b( 1_ilp, js ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_daxpy( js-1, rhs( 2_ilp ), b( 1_ilp, jsp1 ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_daxpy( js-1, rhs( 3_ilp ), e( 1_ilp, js ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                          call stdlib_daxpy( js-1, rhs( 4_ilp ), e( 1_ilp, jsp1 ), 1_ilp,f( is, 1_ilp ), ldf )
                                    
                       end if
                       if( i<p ) then
                          call stdlib_dger( m-ie, nb, -one, a( is, ie+1 ), lda,rhs( 1_ilp ), 1_ilp, c( ie+&
                                    1_ilp, js ), ldc )
                          call stdlib_dger( m-ie, nb, -one, d( is, ie+1 ), ldd,rhs( 3_ilp ), 1_ilp, c( ie+&
                                    1_ilp, js ), ldc )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==1_ilp ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( is, isp1 )
                       z( 3_ilp, 1_ilp ) = -b( js, js )
                       z( 4_ilp, 1_ilp ) = zero
                       z( 1_ilp, 2_ilp ) = a( isp1, is )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 3_ilp, 2_ilp ) = zero
                       z( 4_ilp, 2_ilp ) = -b( js, js )
                       z( 1_ilp, 3_ilp ) = d( is, is )
                       z( 2_ilp, 3_ilp ) = d( is, isp1 )
                       z( 3_ilp, 3_ilp ) = -e( js, js )
                       z( 4_ilp, 3_ilp ) = zero
                       z( 1_ilp, 4_ilp ) = zero
                       z( 2_ilp, 4_ilp ) = d( isp1, isp1 )
                       z( 3_ilp, 4_ilp ) = zero
                       z( 4_ilp, 4_ilp ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp ) = c( is, js )
                       rhs( 2_ilp ) = c( isp1, js )
                       rhs( 3_ilp ) = f( is, js )
                       rhs( 4_ilp ) = f( isp1, js )
                       ! solve z**t * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp )
                       c( isp1, js ) = rhs( 2_ilp )
                       f( is, js ) = rhs( 3_ilp )
                       f( isp1, js ) = rhs( 4_ilp )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_dger( mb, js-1, one, rhs( 1_ilp ), 1_ilp, b( 1_ilp, js ),1_ilp, f( is, 1_ilp ), &
                                    ldf )
                          call stdlib_dger( mb, js-1, one, rhs( 3_ilp ), 1_ilp, e( 1_ilp, js ),1_ilp, f( is, 1_ilp ), &
                                    ldf )
                       end if
                       if( i<p ) then
                          call stdlib_dgemv( 'T', mb, m-ie, -one, a( is, ie+1 ),lda, rhs( 1_ilp ), 1_ilp, &
                                    one, c( ie+1, js ),1_ilp )
                          call stdlib_dgemv( 'T', mb, m-ie, -one, d( is, ie+1 ),ldd, rhs( 3_ilp ), 1_ilp, &
                                    one, c( ie+1, js ),1_ilp )
                       end if
                    else if( ( mb==2_ilp ) .and. ( nb==2_ilp ) ) then
                       ! build an 8-by-8 system z**t * x = rhs
                       call stdlib_dlaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp, 1_ilp ) = a( is, is )
                       z( 2_ilp, 1_ilp ) = a( is, isp1 )
                       z( 5_ilp, 1_ilp ) = -b( js, js )
                       z( 7_ilp, 1_ilp ) = -b( jsp1, js )
                       z( 1_ilp, 2_ilp ) = a( isp1, is )
                       z( 2_ilp, 2_ilp ) = a( isp1, isp1 )
                       z( 6_ilp, 2_ilp ) = -b( js, js )
                       z( 8_ilp, 2_ilp ) = -b( jsp1, js )
                       z( 3_ilp, 3_ilp ) = a( is, is )
                       z( 4_ilp, 3_ilp ) = a( is, isp1 )
                       z( 5_ilp, 3_ilp ) = -b( js, jsp1 )
                       z( 7_ilp, 3_ilp ) = -b( jsp1, jsp1 )
                       z( 3_ilp, 4_ilp ) = a( isp1, is )
                       z( 4_ilp, 4_ilp ) = a( isp1, isp1 )
                       z( 6_ilp, 4_ilp ) = -b( js, jsp1 )
                       z( 8_ilp, 4_ilp ) = -b( jsp1, jsp1 )
                       z( 1_ilp, 5_ilp ) = d( is, is )
                       z( 2_ilp, 5_ilp ) = d( is, isp1 )
                       z( 5_ilp, 5_ilp ) = -e( js, js )
                       z( 2_ilp, 6_ilp ) = d( isp1, isp1 )
                       z( 6_ilp, 6_ilp ) = -e( js, js )
                       z( 3_ilp, 7_ilp ) = d( is, is )
                       z( 4_ilp, 7_ilp ) = d( is, isp1 )
                       z( 5_ilp, 7_ilp ) = -e( js, jsp1 )
                       z( 7_ilp, 7_ilp ) = -e( jsp1, jsp1 )
                       z( 4_ilp, 8_ilp ) = d( isp1, isp1 )
                       z( 6_ilp, 8_ilp ) = -e( js, jsp1 )
                       z( 8_ilp, 8_ilp ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_dcopy( mb, c( is, js+jj ), 1_ilp, rhs( k ), 1_ilp )
                          call stdlib_dcopy( mb, f( is, js+jj ), 1_ilp, rhs( ii ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z**t * x = rhs
                       call stdlib_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp )info = ierr
                       call stdlib_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, k ), 1_ilp )
                             call stdlib_dscal( m, scaloc, f( 1_ilp, k ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp
                       ii = mb*nb + 1_ilp
                       do jj = 0, nb - 1
                          call stdlib_dcopy( mb, rhs( k ), 1_ilp, c( is, js+jj ), 1_ilp )
                          call stdlib_dcopy( mb, rhs( ii ), 1_ilp, f( is, js+jj ), 1_ilp )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_dgemm( 'N', 'T', mb, js-1, nb, one,c( is, js ), ldc, b( 1_ilp, &
                                    js ), ldb, one,f( is, 1_ilp ), ldf )
                          call stdlib_dgemm( 'N', 'T', mb, js-1, nb, one,f( is, js ), ldf, e( 1_ilp, &
                                    js ), lde, one,f( is, 1_ilp ), ldf )
                       end if
                       if( i<p ) then
                          call stdlib_dgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( &
                                    is, js ), ldc,one, c( ie+1, js ), ldc )
                          call stdlib_dgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( &
                                    is, js ), ldf,one, c( ie+1, js ), ldc )
                       end if
                    end if
                 end do loop_190
              end do loop_200
           end if
           return
     end subroutine stdlib_dtgsy2


     pure module subroutine stdlib_ctgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! CTGSY2 solves the generalized Sylvester equation
     !! A * R - L * B = scale *  C               (1)
     !! D * R - L * E = scale * F
     !! using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
     !! (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Zx = scale * b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Ik is the identity matrix of size k and X**H is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H*y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R  + D**H * L   = scale * C           (3)
     !! R  * B**H + L  * E**H  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! = sigma_min(Z) using reverse communication with CLACON.
     !! CTGSY2 also (IJOB >= 1) contributes to the computation in CTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of two matrix pairs in
     !! CTGSYL.
               ldf, scale, rdsum, rdscal,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ldz = 2_ilp
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ierr, j, k
           real(sp) :: scaloc
           complex(sp) :: alpha
           ! Local Arrays 
           integer(ilp) :: ipiv(ldz), jpiv(ldz)
           complex(sp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           ierr = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>2_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSY2', -info )
              return
           end if
           if( notran ) then
              ! solve (i, j) - system
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = m, m - 1, ..., 1; j = 1, 2, ..., n
              scale = one
              scaloc = one
              loop_30: do j = 1, n
                 loop_20: do i = m, 1, -1
                    ! build 2 by 2 system
                    z( 1_ilp, 1_ilp ) = a( i, i )
                    z( 2_ilp, 1_ilp ) = d( i, i )
                    z( 1_ilp, 2_ilp ) = -b( j, j )
                    z( 2_ilp, 2_ilp ) = -e( j, j )
                    ! set up right hand side(s)
                    rhs( 1_ilp ) = c( i, j )
                    rhs( 2_ilp ) = f( i, j )
                    ! solve z * x = rhs
                    call stdlib_cgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp )info = ierr
                    if( ijob==0_ilp ) then
                       call stdlib_cgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                       
                             call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                    else
                       call stdlib_clatdf( ijob, ldz, z, ldz, rhs, rdsum, rdscal,ipiv, jpiv )
                                 
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp )
                    f( i, j ) = rhs( 2_ilp )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( i>1_ilp ) then
                       alpha = -rhs( 1_ilp )
                       call stdlib_caxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, c( 1_ilp, j ), 1_ilp )
                       call stdlib_caxpy( i-1, alpha, d( 1_ilp, i ), 1_ilp, f( 1_ilp, j ), 1_ilp )
                    end if
                    if( j<n ) then
                       call stdlib_caxpy( n-j, rhs( 2_ilp ), b( j, j+1 ), ldb,c( i, j+1 ), ldc )
                                 
                       call stdlib_caxpy( n-j, rhs( 2_ilp ), e( j, j+1 ), lde,f( i, j+1 ), ldf )
                                 
                    end if
                 end do loop_20
              end do loop_30
           else
              ! solve transposed (i, j) - system:
                 ! a(i, i)**h * r(i, j) + d(i, i)**h * l(j, j) = c(i, j)
                 ! r(i, i) * b(j, j) + l(i, j) * e(j, j)   = -f(i, j)
              ! for i = 1, 2, ..., m, j = n, n - 1, ..., 1
              scale = one
              scaloc = one
              loop_80: do i = 1, m
                 loop_70: do j = n, 1, -1
                    ! build 2 by 2 system z**h
                    z( 1_ilp, 1_ilp ) = conjg( a( i, i ) )
                    z( 2_ilp, 1_ilp ) = -conjg( b( j, j ) )
                    z( 1_ilp, 2_ilp ) = conjg( d( i, i ) )
                    z( 2_ilp, 2_ilp ) = -conjg( e( j, j ) )
                    ! set up right hand side(s)
                    rhs( 1_ilp ) = c( i, j )
                    rhs( 2_ilp ) = f( i, j )
                    ! solve z**h * x = rhs
                    call stdlib_cgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp )info = ierr
                    call stdlib_cgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                    if( scaloc/=one ) then
                       do k = 1, n
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp )
                    f( i, j ) = rhs( 2_ilp )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    do k = 1, j - 1
                       f( i, k ) = f( i, k ) + rhs( 1_ilp )*conjg( b( k, j ) ) +rhs( 2_ilp )*conjg( e( k, &
                                 j ) )
                    end do
                    do k = i + 1, m
                       c( k, j ) = c( k, j ) - conjg( a( i, k ) )*rhs( 1_ilp ) -conjg( d( i, k ) )&
                                 *rhs( 2_ilp )
                    end do
                 end do loop_70
              end do loop_80
           end if
           return
     end subroutine stdlib_ctgsy2

     pure module subroutine stdlib_ztgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! ZTGSY2 solves the generalized Sylvester equation
     !! A * R - L * B = scale * C               (1)
     !! D * R - L * E = scale * F
     !! using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
     !! (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Zx = scale * b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Ik is the identity matrix of size k and X**H is the conjuguate transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H*y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R  + D**H * L   = scale * C           (3)
     !! R  * B**H + L  * E**H  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! = sigma_min(Z) using reverse communication with ZLACON.
     !! ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of two matrix pairs in
     !! ZTGSYL.
               ldf, scale, rdsum, rdscal,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ldz = 2_ilp
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ierr, j, k
           real(dp) :: scaloc
           complex(dp) :: alpha
           ! Local Arrays 
           integer(ilp) :: ipiv(ldz), jpiv(ldz)
           complex(dp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp
           ierr = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp
           else if( notran ) then
              if( ( ijob<0_ilp ) .or. ( ijob>2_ilp ) ) then
                 info = -2_ilp
              end if
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 info = -3_ilp
              else if( n<=0_ilp ) then
                 info = -4_ilp
              else if( lda<max( 1_ilp, m ) ) then
                 info = -6_ilp
              else if( ldb<max( 1_ilp, n ) ) then
                 info = -8_ilp
              else if( ldc<max( 1_ilp, m ) ) then
                 info = -10_ilp
              else if( ldd<max( 1_ilp, m ) ) then
                 info = -12_ilp
              else if( lde<max( 1_ilp, n ) ) then
                 info = -14_ilp
              else if( ldf<max( 1_ilp, m ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSY2', -info )
              return
           end if
           if( notran ) then
              ! solve (i, j) - system
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = m, m - 1, ..., 1; j = 1, 2, ..., n
              scale = one
              scaloc = one
              loop_30: do j = 1, n
                 loop_20: do i = m, 1, -1
                    ! build 2 by 2 system
                    z( 1_ilp, 1_ilp ) = a( i, i )
                    z( 2_ilp, 1_ilp ) = d( i, i )
                    z( 1_ilp, 2_ilp ) = -b( j, j )
                    z( 2_ilp, 2_ilp ) = -e( j, j )
                    ! set up right hand side(s)
                    rhs( 1_ilp ) = c( i, j )
                    rhs( 2_ilp ) = f( i, j )
                    ! solve z * x = rhs
                    call stdlib_zgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp )info = ierr
                    if( ijob==0_ilp ) then
                       call stdlib_zgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp, k ), 1_ilp )
                                       
                             call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp, k ), 1_ilp )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                    else
                       call stdlib_zlatdf( ijob, ldz, z, ldz, rhs, rdsum, rdscal,ipiv, jpiv )
                                 
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp )
                    f( i, j ) = rhs( 2_ilp )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( i>1_ilp ) then
                       alpha = -rhs( 1_ilp )
                       call stdlib_zaxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, c( 1_ilp, j ), 1_ilp )
                       call stdlib_zaxpy( i-1, alpha, d( 1_ilp, i ), 1_ilp, f( 1_ilp, j ), 1_ilp )
                    end if
                    if( j<n ) then
                       call stdlib_zaxpy( n-j, rhs( 2_ilp ), b( j, j+1 ), ldb,c( i, j+1 ), ldc )
                                 
                       call stdlib_zaxpy( n-j, rhs( 2_ilp ), e( j, j+1 ), lde,f( i, j+1 ), ldf )
                                 
                    end if
                 end do loop_20
              end do loop_30
           else
              ! solve transposed (i, j) - system:
                 ! a(i, i)**h * r(i, j) + d(i, i)**h * l(j, j) = c(i, j)
                 ! r(i, i) * b(j, j) + l(i, j) * e(j, j)   = -f(i, j)
              ! for i = 1, 2, ..., m, j = n, n - 1, ..., 1
              scale = one
              scaloc = one
              loop_80: do i = 1, m
                 loop_70: do j = n, 1, -1
                    ! build 2 by 2 system z**h
                    z( 1_ilp, 1_ilp ) = conjg( a( i, i ) )
                    z( 2_ilp, 1_ilp ) = -conjg( b( j, j ) )
                    z( 1_ilp, 2_ilp ) = conjg( d( i, i ) )
                    z( 2_ilp, 2_ilp ) = -conjg( e( j, j ) )
                    ! set up right hand side(s)
                    rhs( 1_ilp ) = c( i, j )
                    rhs( 2_ilp ) = f( i, j )
                    ! solve z**h * x = rhs
                    call stdlib_zgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp )info = ierr
                    call stdlib_zgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                    if( scaloc/=one ) then
                       do k = 1, n
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp, k ),1_ilp )
                                    
                          call stdlib_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp, k ),1_ilp )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp )
                    f( i, j ) = rhs( 2_ilp )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    do k = 1, j - 1
                       f( i, k ) = f( i, k ) + rhs( 1_ilp )*conjg( b( k, j ) ) +rhs( 2_ilp )*conjg( e( k, &
                                 j ) )
                    end do
                    do k = i + 1, m
                       c( k, j ) = c( k, j ) - conjg( a( i, k ) )*rhs( 1_ilp ) -conjg( d( i, k ) )&
                                 *rhs( 2_ilp )
                    end do
                 end do loop_70
              end do loop_80
           end if
           return
     end subroutine stdlib_ztgsy2




     pure module subroutine stdlib_slagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
     !! SLAGV2 computes the Generalized Schur factorization of a real 2-by-2
     !! matrix pencil (A,B) where B is upper triangular. This routine
     !! computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
     !! SNR such that
     !! 1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
     !! types), then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],
     !! 2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
     !! then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]
     !! where b11 >= b22 > 0.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb
           real(sp), intent(out) :: csl, csr, snl, snr
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(2_ilp), alphar(2_ilp), beta(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: anorm, ascale, bnorm, bscale, h1, h2, h3, qq, r, rr, safmin, scale1, &
                     scale2, t, ulp, wi, wr1, wr2
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_slamch( 'S' )
           ulp = stdlib_slamch( 'P' )
           ! scale a
           anorm = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 2_ilp, 1_ilp ) ),abs( a( 1_ilp, 2_ilp ) )+abs( a( 2_ilp, 2_ilp ) ), &
                     safmin )
           ascale = one / anorm
           a( 1_ilp, 1_ilp ) = ascale*a( 1_ilp, 1_ilp )
           a( 1_ilp, 2_ilp ) = ascale*a( 1_ilp, 2_ilp )
           a( 2_ilp, 1_ilp ) = ascale*a( 2_ilp, 1_ilp )
           a( 2_ilp, 2_ilp ) = ascale*a( 2_ilp, 2_ilp )
           ! scale b
           bnorm = max( abs( b( 1_ilp, 1_ilp ) ), abs( b( 1_ilp, 2_ilp ) )+abs( b( 2_ilp, 2_ilp ) ),safmin )
           bscale = one / bnorm
           b( 1_ilp, 1_ilp ) = bscale*b( 1_ilp, 1_ilp )
           b( 1_ilp, 2_ilp ) = bscale*b( 1_ilp, 2_ilp )
           b( 2_ilp, 2_ilp ) = bscale*b( 2_ilp, 2_ilp )
           ! check if a can be deflated
           if( abs( a( 2_ilp, 1_ilp ) )<=ulp ) then
              csl = one
              snl = zero
              csr = one
              snr = zero
              a( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              wi = zero
           ! check if b is singular
           else if( abs( b( 1_ilp, 1_ilp ) )<=ulp ) then
              call stdlib_slartg( a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), csl, snl, r )
              csr = one
              snr = zero
              call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
              call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
              a( 2_ilp, 1_ilp ) = zero
              b( 1_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              wi = zero
           else if( abs( b( 2_ilp, 2_ilp ) )<=ulp ) then
              call stdlib_slartg( a( 2_ilp, 2_ilp ), a( 2_ilp, 1_ilp ), csr, snr, t )
              snr = -snr
              call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
              call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
              csl = one
              snl = zero
              a( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 2_ilp ) = zero
              wi = zero
           else
              ! b is nonsingular, first compute the eigenvalues of (a,b)
              call stdlib_slag2( a, lda, b, ldb, safmin, scale1, scale2, wr1, wr2,wi )
              if( wi==zero ) then
                 ! two real eigenvalues, compute s*a-w*b
                 h1 = scale1*a( 1_ilp, 1_ilp ) - wr1*b( 1_ilp, 1_ilp )
                 h2 = scale1*a( 1_ilp, 2_ilp ) - wr1*b( 1_ilp, 2_ilp )
                 h3 = scale1*a( 2_ilp, 2_ilp ) - wr1*b( 2_ilp, 2_ilp )
                 rr = stdlib_slapy2( h1, h2 )
                 qq = stdlib_slapy2( scale1*a( 2_ilp, 1_ilp ), h3 )
                 if( rr>qq ) then
                    ! find right rotation matrix to zero 1,1 element of
                    ! (sa - wb)
                    call stdlib_slartg( h2, h1, csr, snr, t )
                 else
                    ! find right rotation matrix to zero 2,1 element of
                    ! (sa - wb)
                    call stdlib_slartg( h3, scale1*a( 2_ilp, 1_ilp ), csr, snr, t )
                 end if
                 snr = -snr
                 call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 ! compute inf norms of a and b
                 h1 = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 1_ilp, 2_ilp ) ),abs( a( 2_ilp, 1_ilp ) )+abs( a( 2_ilp, 2_ilp ) ) )
                           
                 h2 = max( abs( b( 1_ilp, 1_ilp ) )+abs( b( 1_ilp, 2_ilp ) ),abs( b( 2_ilp, 1_ilp ) )+abs( b( 2_ilp, 2_ilp ) ) )
                           
                 if( ( scale1*h1 )>=abs( wr1 )*h2 ) then
                    ! find left rotation matrix q to zero out b(2,1)
                    call stdlib_slartg( b( 1_ilp, 1_ilp ), b( 2_ilp, 1_ilp ), csl, snl, r )
                 else
                    ! find left rotation matrix q to zero out a(2,1)
                    call stdlib_slartg( a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), csl, snl, r )
                 end if
                 call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
                 call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
                 a( 2_ilp, 1_ilp ) = zero
                 b( 2_ilp, 1_ilp ) = zero
              else
                 ! a pair of complex conjugate eigenvalues
                 ! first compute the svd of the matrix b
                 call stdlib_slasv2( b( 1_ilp, 1_ilp ), b( 1_ilp, 2_ilp ), b( 2_ilp, 2_ilp ), r, t, snr,csr, snl, csl )
                           
                 ! form (a,b) := q(a,b)z**t where q is left rotation matrix and
                 ! z is right rotation matrix computed from stdlib_slasv2
                 call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
                 call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
                 call stdlib_srot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 call stdlib_srot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 b( 2_ilp, 1_ilp ) = zero
                 b( 1_ilp, 2_ilp ) = zero
              end if
           end if
           ! unscaling
           a( 1_ilp, 1_ilp ) = anorm*a( 1_ilp, 1_ilp )
           a( 2_ilp, 1_ilp ) = anorm*a( 2_ilp, 1_ilp )
           a( 1_ilp, 2_ilp ) = anorm*a( 1_ilp, 2_ilp )
           a( 2_ilp, 2_ilp ) = anorm*a( 2_ilp, 2_ilp )
           b( 1_ilp, 1_ilp ) = bnorm*b( 1_ilp, 1_ilp )
           b( 2_ilp, 1_ilp ) = bnorm*b( 2_ilp, 1_ilp )
           b( 1_ilp, 2_ilp ) = bnorm*b( 1_ilp, 2_ilp )
           b( 2_ilp, 2_ilp ) = bnorm*b( 2_ilp, 2_ilp )
           if( wi==zero ) then
              alphar( 1_ilp ) = a( 1_ilp, 1_ilp )
              alphar( 2_ilp ) = a( 2_ilp, 2_ilp )
              alphai( 1_ilp ) = zero
              alphai( 2_ilp ) = zero
              beta( 1_ilp ) = b( 1_ilp, 1_ilp )
              beta( 2_ilp ) = b( 2_ilp, 2_ilp )
           else
              alphar( 1_ilp ) = anorm*wr1 / scale1 / bnorm
              alphai( 1_ilp ) = anorm*wi / scale1 / bnorm
              alphar( 2_ilp ) = alphar( 1_ilp )
              alphai( 2_ilp ) = -alphai( 1_ilp )
              beta( 1_ilp ) = one
              beta( 2_ilp ) = one
           end if
           return
     end subroutine stdlib_slagv2

     pure module subroutine stdlib_dlagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
     !! DLAGV2 computes the Generalized Schur factorization of a real 2-by-2
     !! matrix pencil (A,B) where B is upper triangular. This routine
     !! computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
     !! SNR such that
     !! 1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
     !! types), then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],
     !! 2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
     !! then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]
     !! where b11 >= b22 > 0.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb
           real(dp), intent(out) :: csl, csr, snl, snr
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(2_ilp), alphar(2_ilp), beta(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: anorm, ascale, bnorm, bscale, h1, h2, h3, qq, r, rr, safmin, scale1, &
                     scale2, t, ulp, wi, wr1, wr2
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_dlamch( 'S' )
           ulp = stdlib_dlamch( 'P' )
           ! scale a
           anorm = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 2_ilp, 1_ilp ) ),abs( a( 1_ilp, 2_ilp ) )+abs( a( 2_ilp, 2_ilp ) ), &
                     safmin )
           ascale = one / anorm
           a( 1_ilp, 1_ilp ) = ascale*a( 1_ilp, 1_ilp )
           a( 1_ilp, 2_ilp ) = ascale*a( 1_ilp, 2_ilp )
           a( 2_ilp, 1_ilp ) = ascale*a( 2_ilp, 1_ilp )
           a( 2_ilp, 2_ilp ) = ascale*a( 2_ilp, 2_ilp )
           ! scale b
           bnorm = max( abs( b( 1_ilp, 1_ilp ) ), abs( b( 1_ilp, 2_ilp ) )+abs( b( 2_ilp, 2_ilp ) ),safmin )
           bscale = one / bnorm
           b( 1_ilp, 1_ilp ) = bscale*b( 1_ilp, 1_ilp )
           b( 1_ilp, 2_ilp ) = bscale*b( 1_ilp, 2_ilp )
           b( 2_ilp, 2_ilp ) = bscale*b( 2_ilp, 2_ilp )
           ! check if a can be deflated
           if( abs( a( 2_ilp, 1_ilp ) )<=ulp ) then
              csl = one
              snl = zero
              csr = one
              snr = zero
              a( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              wi = zero
           ! check if b is singular
           else if( abs( b( 1_ilp, 1_ilp ) )<=ulp ) then
              call stdlib_dlartg( a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), csl, snl, r )
              csr = one
              snr = zero
              call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
              call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
              a( 2_ilp, 1_ilp ) = zero
              b( 1_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              wi = zero
           else if( abs( b( 2_ilp, 2_ilp ) )<=ulp ) then
              call stdlib_dlartg( a( 2_ilp, 2_ilp ), a( 2_ilp, 1_ilp ), csr, snr, t )
              snr = -snr
              call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
              call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
              csl = one
              snl = zero
              a( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 1_ilp ) = zero
              b( 2_ilp, 2_ilp ) = zero
              wi = zero
           else
              ! b is nonsingular, first compute the eigenvalues of (a,b)
              call stdlib_dlag2( a, lda, b, ldb, safmin, scale1, scale2, wr1, wr2,wi )
              if( wi==zero ) then
                 ! two real eigenvalues, compute s*a-w*b
                 h1 = scale1*a( 1_ilp, 1_ilp ) - wr1*b( 1_ilp, 1_ilp )
                 h2 = scale1*a( 1_ilp, 2_ilp ) - wr1*b( 1_ilp, 2_ilp )
                 h3 = scale1*a( 2_ilp, 2_ilp ) - wr1*b( 2_ilp, 2_ilp )
                 rr = stdlib_dlapy2( h1, h2 )
                 qq = stdlib_dlapy2( scale1*a( 2_ilp, 1_ilp ), h3 )
                 if( rr>qq ) then
                    ! find right rotation matrix to zero 1,1 element of
                    ! (sa - wb)
                    call stdlib_dlartg( h2, h1, csr, snr, t )
                 else
                    ! find right rotation matrix to zero 2,1 element of
                    ! (sa - wb)
                    call stdlib_dlartg( h3, scale1*a( 2_ilp, 1_ilp ), csr, snr, t )
                 end if
                 snr = -snr
                 call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 ! compute inf norms of a and b
                 h1 = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 1_ilp, 2_ilp ) ),abs( a( 2_ilp, 1_ilp ) )+abs( a( 2_ilp, 2_ilp ) ) )
                           
                 h2 = max( abs( b( 1_ilp, 1_ilp ) )+abs( b( 1_ilp, 2_ilp ) ),abs( b( 2_ilp, 1_ilp ) )+abs( b( 2_ilp, 2_ilp ) ) )
                           
                 if( ( scale1*h1 )>=abs( wr1 )*h2 ) then
                    ! find left rotation matrix q to zero out b(2,1)
                    call stdlib_dlartg( b( 1_ilp, 1_ilp ), b( 2_ilp, 1_ilp ), csl, snl, r )
                 else
                    ! find left rotation matrix q to zero out a(2,1)
                    call stdlib_dlartg( a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), csl, snl, r )
                 end if
                 call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
                 call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
                 a( 2_ilp, 1_ilp ) = zero
                 b( 2_ilp, 1_ilp ) = zero
              else
                 ! a pair of complex conjugate eigenvalues
                 ! first compute the svd of the matrix b
                 call stdlib_dlasv2( b( 1_ilp, 1_ilp ), b( 1_ilp, 2_ilp ), b( 2_ilp, 2_ilp ), r, t, snr,csr, snl, csl )
                           
                 ! form (a,b) := q(a,b)z**t where q is left rotation matrix and
                 ! z is right rotation matrix computed from stdlib_dlasv2
                 call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), lda, a( 2_ilp, 1_ilp ), lda, csl, snl )
                 call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), ldb, b( 2_ilp, 1_ilp ), ldb, csl, snl )
                 call stdlib_drot( 2_ilp, a( 1_ilp, 1_ilp ), 1_ilp, a( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 call stdlib_drot( 2_ilp, b( 1_ilp, 1_ilp ), 1_ilp, b( 1_ilp, 2_ilp ), 1_ilp, csr, snr )
                 b( 2_ilp, 1_ilp ) = zero
                 b( 1_ilp, 2_ilp ) = zero
              end if
           end if
           ! unscaling
           a( 1_ilp, 1_ilp ) = anorm*a( 1_ilp, 1_ilp )
           a( 2_ilp, 1_ilp ) = anorm*a( 2_ilp, 1_ilp )
           a( 1_ilp, 2_ilp ) = anorm*a( 1_ilp, 2_ilp )
           a( 2_ilp, 2_ilp ) = anorm*a( 2_ilp, 2_ilp )
           b( 1_ilp, 1_ilp ) = bnorm*b( 1_ilp, 1_ilp )
           b( 2_ilp, 1_ilp ) = bnorm*b( 2_ilp, 1_ilp )
           b( 1_ilp, 2_ilp ) = bnorm*b( 1_ilp, 2_ilp )
           b( 2_ilp, 2_ilp ) = bnorm*b( 2_ilp, 2_ilp )
           if( wi==zero ) then
              alphar( 1_ilp ) = a( 1_ilp, 1_ilp )
              alphar( 2_ilp ) = a( 2_ilp, 2_ilp )
              alphai( 1_ilp ) = zero
              alphai( 2_ilp ) = zero
              beta( 1_ilp ) = b( 1_ilp, 1_ilp )
              beta( 2_ilp ) = b( 2_ilp, 2_ilp )
           else
              alphar( 1_ilp ) = anorm*wr1 / scale1 / bnorm
              alphai( 1_ilp ) = anorm*wi / scale1 / bnorm
              alphar( 2_ilp ) = alphar( 1_ilp )
              alphai( 2_ilp ) = -alphai( 1_ilp )
              beta( 1_ilp ) = one
              beta( 2_ilp ) = one
           end if
           return
     end subroutine stdlib_dlagv2




     pure module subroutine stdlib_stgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! STGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of real matrices (S,P), where S is a quasi-triangular matrix
     !! and P is upper triangular.  Matrix pairs of this type are produced by
     !! the generalized Schur factorization of a matrix pair (A,B):
     !! A = Q*S*Z**T,  B = Q*P*Z**T
     !! as computed by SGGHRD + SHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal blocks of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the orthogonal factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(in) :: p(ldp,*), s(lds,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: safety = 1.0e+2_sp
           
           ! Local Scalars 
           logical(lk) :: compl, compr, il2by2, ilabad, ilall, ilback, ilbbad, ilcomp, ilcplx, &
                     lsa, lsb
           integer(ilp) :: i, ibeg, ieig, iend, ihwmny, iinfo, im, iside, j, ja, jc, je, jr, jw, &
                     na, nw
           real(sp) :: acoef, acoefa, anorm, ascale, bcoefa, bcoefi, bcoefr, big, bignum, bnorm, &
           bscale, cim2a, cim2b, cimaga, cimagb, cre2a, cre2b, creala, crealb, dmin, safmin, &
                     salfar, sbeta, scale, small, temp, temp2, temp2i, temp2r, ulp, xmax, xscale
           ! Local Arrays 
           real(sp) :: bdiag(2_ilp), sum(2_ilp,2_ilp), sums(2_ilp,2_ilp), sump(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp
              ilall = .true.
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp
           end if
           info = 0_ilp
           if( iside<0_ilp ) then
              info = -1_ilp
           else if( ihwmny<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lds<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldp<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGEVC', -info )
              return
           end if
           ! count the number of eigenvectors to be computed
           if( .not.ilall ) then
              im = 0_ilp
              ilcplx = .false.
              loop_10: do j = 1, n
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_10
                 end if
                 if( j<n ) then
                    if( s( j+1, j )/=zero )ilcplx = .true.
                 end if
                 if( ilcplx ) then
                    if( select( j ) .or. select( j+1 ) )im = im + 2_ilp
                 else
                    if( select( j ) )im = im + 1_ilp
                 end if
              end do loop_10
           else
              im = n
           end if
           ! check 2-by-2 diagonal blocks of a, b
           ilabad = .false.
           ilbbad = .false.
           do j = 1, n - 1
              if( s( j+1, j )/=zero ) then
                 if( p( j, j )==zero .or. p( j+1, j+1 )==zero .or.p( j, j+1 )/=zero )ilbbad = &
                           .true.
                 if( j<n-1 ) then
                    if( s( j+2, j+1 )/=zero )ilabad = .true.
                 end if
              end if
           end do
           if( ilabad ) then
              info = -5_ilp
           else if( ilbbad ) then
              info = -7_ilp
           else if( compl .and. ldvl<n .or. ldvl<1_ilp ) then
              info = -10_ilp
           else if( compr .and. ldvr<n .or. ldvr<1_ilp ) then
              info = -12_ilp
           else if( mm<im ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_slabad( safmin, big )
           ulp = stdlib_slamch( 'EPSILON' )*stdlib_slamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part (i.e., excluding all elements belonging to the diagonal
           ! blocks) of a and b to check for possible overflow in the
           ! triangular solver.
           anorm = abs( s( 1_ilp, 1_ilp ) )
           if( n>1_ilp )anorm = anorm + abs( s( 2_ilp, 1_ilp ) )
           bnorm = abs( p( 1_ilp, 1_ilp ) )
           work( 1_ilp ) = zero
           work( n+1 ) = zero
           do j = 2, n
              temp = zero
              temp2 = zero
              if( s( j, j-1 )==zero ) then
                 iend = j - 1_ilp
              else
                 iend = j - 2_ilp
              end if
              do i = 1, iend
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              work( j ) = temp
              work( n+j ) = temp2
              do i = iend + 1, min( j+1, n )
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              anorm = max( anorm, temp )
              bnorm = max( bnorm, temp2 )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_220: do je = 1, n
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at.
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_220
                 end if
                 nw = 1_ilp
                 if( je<n ) then
                    if( s( je+1, je )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je+1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_220
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       ieig = ieig + 1_ilp
                       do jr = 1, n
                          vl( jr, ieig ) = zero
                       end do
                       vl( ieig, ieig ) = one
                       cycle loop_220
                    end if
                 end if
                 ! clear vector
                 do jr = 1, nw*n
                    work( 2_ilp*n+jr ) = zero
                 end do
                                                       ! t
                 ! compute coefficients in  ( a a - b b )  y = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp*n+je ) = one
                    xmax = one
                 else
                    ! complex eigenvalue
                    call stdlib_slag2( s( je, je ), lds, p( je, je ), ldp,safmin*safety, acoef, &
                              temp, bcoefr, temp2,bcoefi )
                    bcoefi = -bcoefi
                    if( bcoefi==zero ) then
                       info = je
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    temp = acoef*s( je+1, je )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp*n+je ) = one
                       work( 3_ilp*n+je ) = zero
                       work( 2_ilp*n+je+1 ) = -temp2r / temp
                       work( 3_ilp*n+je+1 ) = -temp2i / temp
                    else
                       work( 2_ilp*n+je+1 ) = one
                       work( 3_ilp*n+je+1 ) = zero
                       temp = acoef*s( je, je+1 )
                       work( 2_ilp*n+je ) = ( bcoefr*p( je+1, je+1 )-acoef*s( je+1, je+1 ) ) / &
                                 temp
                       work( 3_ilp*n+je ) = bcoefi*p( je+1, je+1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp*n+je ) )+abs( work( 3_ilp*n+je ) ),abs( work( 2_ilp*n+je+1 ) &
                              )+abs( work( 3_ilp*n+je+1 ) ) )
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                 ! t
                 ! triangular solve of  (a a - b b)  y = 0
                                         ! t
                 ! (rowwise in  (a a - b b) , or columnwise in (a a - b b) )
                 il2by2 = .false.
                 loop_160: do j = je + nw, n
                    if( il2by2 ) then
                       il2by2 = .false.
                       cycle loop_160
                    end if
                    na = 1_ilp
                    bdiag( 1_ilp ) = p( j, j )
                    if( j<n ) then
                       if( s( j+1, j )/=zero ) then
                          il2by2 = .true.
                          bdiag( 2_ilp ) = p( j+1, j+1 )
                          na = 2_ilp
                       end if
                    end if
                    ! check whether scaling is necessary for dot products
                    xscale = one / max( one, xmax )
                    temp = max( work( j ), work( n+j ),acoefa*work( j )+bcoefa*work( n+j ) )
                              
                    if( il2by2 )temp = max( temp, work( j+1 ), work( n+j+1 ),acoefa*work( j+1 )+&
                              bcoefa*work( n+j+1 ) )
                    if( temp>bignum*xscale ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = xmax*xscale
                    end if
                    ! compute dot products
                          ! j-1
                    ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                          ! k=je
                    ! to reduce the op count, this is done as
                    ! _        j-1                  _        j-1
                    ! a*conjg( sum  s(k,j)*x(k) ) - b*conjg( sum  p(k,j)*x(k) )
                             ! k=je                          k=je
                    ! which may cause underflow problems if a or b are close
                    ! to underflow.  (e.g., less than small.)
                    do jw = 1, nw
                       do ja = 1, na
                          sums( ja, jw ) = zero
                          sump( ja, jw ) = zero
                          do jr = je, j - 1
                             sums( ja, jw ) = sums( ja, jw ) +s( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                             sump( ja, jw ) = sump( ja, jw ) +p( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                          end do
                       end do
                    end do
                    do ja = 1, na
                       if( ilcplx ) then
                          sum( ja, 1_ilp ) = -acoef*sums( ja, 1_ilp ) +bcoefr*sump( ja, 1_ilp ) -bcoefi*sump( &
                                    ja, 2_ilp )
                          sum( ja, 2_ilp ) = -acoef*sums( ja, 2_ilp ) +bcoefr*sump( ja, 2_ilp ) +bcoefi*sump( &
                                    ja, 1_ilp )
                       else
                          sum( ja, 1_ilp ) = -acoef*sums( ja, 1_ilp ) +bcoefr*sump( ja, 1_ilp )
                       end if
                    end do
                                        ! t
                    ! solve  ( a a - b b )  y = sum(,)
                    ! with scaling and perturbation of the denominator
                    call stdlib_slaln2( .true., na, nw, dmin, acoef, s( j, j ), lds,bdiag( 1_ilp ), &
                    bdiag( 2_ilp ), sum, 2_ilp, bcoefr,bcoefi, work( 2_ilp*n+j ), n, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = scale*xmax
                    end if
                    xmax = max( xmax, temp )
                 end do loop_160
                 ! copy eigenvector to vl, back transforming if
                 ! howmny='b'.
                 ieig = ieig + 1_ilp
                 if( ilback ) then
                    do jw = 0, nw - 1
                       call stdlib_sgemv( 'N', n, n+1-je, one, vl( 1_ilp, je ), ldvl,work( ( jw+2 )*n+&
                                 je ), 1_ilp, zero,work( ( jw+4 )*n+1 ), 1_ilp )
                    end do
                    call stdlib_slacpy( ' ', n, nw, work( 4_ilp*n+1 ), n, vl( 1_ilp, je ),ldvl )
                    ibeg = 1_ilp
                 else
                    call stdlib_slacpy( ' ', n, nw, work( 2_ilp*n+1 ), n, vl( 1_ilp, ieig ),ldvl )
                    ibeg = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) )+abs( vl( j, ieig+1 ) ) )
                    end do
                 else
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = ibeg, n
                          vl( jr, ieig+jw ) = xscale*vl( jr, ieig+jw )
                       end do
                    end do
                 end if
                 ieig = ieig + nw - 1_ilp
              end do loop_220
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_500: do je = n, 1, -1
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at -- if complex, select(je)
                 ! or select(je-1).
                 ! if this is a complex pair, the 2-by-2 diagonal block
                 ! corresponding to the eigenvalue is in rows/columns je-1:je
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_500
                 end if
                 nw = 1_ilp
                 if( je>1_ilp ) then
                    if( s( je, je-1 )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je-1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_500
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- unit eigenvector
                       ieig = ieig - 1_ilp
                       do jr = 1, n
                          vr( jr, ieig ) = zero
                       end do
                       vr( ieig, ieig ) = one
                       cycle loop_500
                    end if
                 end if
                 ! clear vector
                 do jw = 0, nw - 1
                    do jr = 1, n
                       work( ( jw+2 )*n+jr ) = zero
                    end do
                 end do
                 ! compute coefficients in  ( a a - b b ) x = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp*n+je ) = one
                    xmax = one
                    ! compute contribution from column je of a and b to sum
                    ! (see "further details", above.)
                    do jr = 1, je - 1
                       work( 2_ilp*n+jr ) = bcoefr*p( jr, je ) -acoef*s( jr, je )
                    end do
                 else
                    ! complex eigenvalue
                    call stdlib_slag2( s( je-1, je-1 ), lds, p( je-1, je-1 ), ldp,safmin*safety, &
                              acoef, temp, bcoefr, temp2,bcoefi )
                    if( bcoefi==zero ) then
                       info = je - 1_ilp
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    ! and contribution to sums
                    temp = acoef*s( je, je-1 )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>=abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp*n+je ) = one
                       work( 3_ilp*n+je ) = zero
                       work( 2_ilp*n+je-1 ) = -temp2r / temp
                       work( 3_ilp*n+je-1 ) = -temp2i / temp
                    else
                       work( 2_ilp*n+je-1 ) = one
                       work( 3_ilp*n+je-1 ) = zero
                       temp = acoef*s( je-1, je )
                       work( 2_ilp*n+je ) = ( bcoefr*p( je-1, je-1 )-acoef*s( je-1, je-1 ) ) / &
                                 temp
                       work( 3_ilp*n+je ) = bcoefi*p( je-1, je-1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp*n+je ) )+abs( work( 3_ilp*n+je ) ),abs( work( 2_ilp*n+je-1 ) &
                              )+abs( work( 3_ilp*n+je-1 ) ) )
                    ! compute contribution from columns je and je-1
                    ! of a and b to the sums.
                    creala = acoef*work( 2_ilp*n+je-1 )
                    cimaga = acoef*work( 3_ilp*n+je-1 )
                    crealb = bcoefr*work( 2_ilp*n+je-1 ) -bcoefi*work( 3_ilp*n+je-1 )
                    cimagb = bcoefi*work( 2_ilp*n+je-1 ) +bcoefr*work( 3_ilp*n+je-1 )
                    cre2a = acoef*work( 2_ilp*n+je )
                    cim2a = acoef*work( 3_ilp*n+je )
                    cre2b = bcoefr*work( 2_ilp*n+je ) - bcoefi*work( 3_ilp*n+je )
                    cim2b = bcoefi*work( 2_ilp*n+je ) + bcoefr*work( 3_ilp*n+je )
                    do jr = 1, je - 2
                       work( 2_ilp*n+jr ) = -creala*s( jr, je-1 ) +crealb*p( jr, je-1 ) -cre2a*s( jr, &
                                 je ) + cre2b*p( jr, je )
                       work( 3_ilp*n+jr ) = -cimaga*s( jr, je-1 ) +cimagb*p( jr, je-1 ) -cim2a*s( jr, &
                                 je ) + cim2b*p( jr, je )
                    end do
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                 ! columnwise triangular solve of  (a a - b b)  x = 0
                 il2by2 = .false.
                 loop_370: do j = je - nw, 1, -1
                    ! if a 2-by-2 block, is in position j-1:j, wait until
                    ! next iteration to process it (when it will be j:j+1)
                    if( .not.il2by2 .and. j>1_ilp ) then
                       if( s( j, j-1 )/=zero ) then
                          il2by2 = .true.
                          cycle loop_370
                       end if
                    end if
                    bdiag( 1_ilp ) = p( j, j )
                    if( il2by2 ) then
                       na = 2_ilp
                       bdiag( 2_ilp ) = p( j+1, j+1 )
                    else
                       na = 1_ilp
                    end if
                    ! compute x(j) (and x(j+1), if 2-by-2 block)
                    call stdlib_slaln2( .false., na, nw, dmin, acoef, s( j, j ),lds, bdiag( 1_ilp ), &
                    bdiag( 2_ilp ), work( 2_ilp*n+j ),n, bcoefr, bcoefi, sum, 2_ilp, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = 1, je
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                    end if
                    xmax = max( scale*xmax, temp )
                    do jw = 1, nw
                       do ja = 1, na
                          work( ( jw+1 )*n+j+ja-1 ) = sum( ja, jw )
                       end do
                    end do
                    ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                    if( j>1_ilp ) then
                       ! check whether scaling is necessary for sum.
                       xscale = one / max( one, xmax )
                       temp = acoefa*work( j ) + bcoefa*work( n+j )
                       if( il2by2 )temp = max( temp, acoefa*work( j+1 )+bcoefa*work( n+j+1 ) )
                                 
                       temp = max( temp, acoefa, bcoefa )
                       if( temp>bignum*xscale ) then
                          do jw = 0, nw - 1
                             do jr = 1, je
                                work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                             end do
                          end do
                          xmax = xmax*xscale
                       end if
                       ! compute the contributions of the off-diagonals of
                       ! column j (and j+1, if 2-by-2 block) of a and b to the
                       ! sums.
                       do ja = 1, na
                          if( ilcplx ) then
                             creala = acoef*work( 2_ilp*n+j+ja-1 )
                             cimaga = acoef*work( 3_ilp*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp*n+j+ja-1 ) -bcoefi*work( 3_ilp*n+j+ja-1 )
                             cimagb = bcoefi*work( 2_ilp*n+j+ja-1 ) +bcoefr*work( 3_ilp*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp*n+jr ) = work( 2_ilp*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                                work( 3_ilp*n+jr ) = work( 3_ilp*n+jr ) -cimaga*s( jr, j+ja-1 ) +cimagb*p(&
                                           jr, j+ja-1 )
                             end do
                          else
                             creala = acoef*work( 2_ilp*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp*n+jr ) = work( 2_ilp*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                             end do
                          end if
                       end do
                    end if
                    il2by2 = .false.
                 end do loop_370
                 ! copy eigenvector to vr, back transforming if
                 ! howmny='b'.
                 ieig = ieig - nw
                 if( ilback ) then
                    do jw = 0, nw - 1
                       do jr = 1, n
                          work( ( jw+4 )*n+jr ) = work( ( jw+2 )*n+1 )*vr( jr, 1_ilp )
                       end do
                       ! a series of compiler directives to defeat
                       ! vectorization for the next loop
                       do jc = 2, je
                          do jr = 1, n
                             work( ( jw+4 )*n+jr ) = work( ( jw+4 )*n+jr ) +work( ( jw+2 )*n+jc )&
                                       *vr( jr, jc )
                          end do
                       end do
                    end do
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+4 )*n+jr )
                       end do
                    end do
                    iend = n
                 else
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+2 )*n+jr )
                       end do
                    end do
                    iend = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) )+abs( vr( j, ieig+1 ) ) )
                    end do
                 else
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = 1, iend
                          vr( jr, ieig+jw ) = xscale*vr( jr, ieig+jw )
                       end do
                    end do
                 end if
              end do loop_500
           end if
           return
     end subroutine stdlib_stgevc

     pure module subroutine stdlib_dtgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! DTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of real matrices (S,P), where S is a quasi-triangular matrix
     !! and P is upper triangular.  Matrix pairs of this type are produced by
     !! the generalized Schur factorization of a matrix pair (A,B):
     !! A = Q*S*Z**T,  B = Q*P*Z**T
     !! as computed by DGGHRD + DHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal blocks of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the orthogonal factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(in) :: p(ldp,*), s(lds,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: safety = 1.0e+2_dp
           
           ! Local Scalars 
           logical(lk) :: compl, compr, il2by2, ilabad, ilall, ilback, ilbbad, ilcomp, ilcplx, &
                     lsa, lsb
           integer(ilp) :: i, ibeg, ieig, iend, ihwmny, iinfo, im, iside, j, ja, jc, je, jr, jw, &
                     na, nw
           real(dp) :: acoef, acoefa, anorm, ascale, bcoefa, bcoefi, bcoefr, big, bignum, bnorm, &
           bscale, cim2a, cim2b, cimaga, cimagb, cre2a, cre2b, creala, crealb, dmin, safmin, &
                     salfar, sbeta, scale, small, temp, temp2, temp2i, temp2r, ulp, xmax, xscale
           ! Local Arrays 
           real(dp) :: bdiag(2_ilp), sum(2_ilp,2_ilp), sums(2_ilp,2_ilp), sump(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp
              ilall = .true.
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp
           end if
           info = 0_ilp
           if( iside<0_ilp ) then
              info = -1_ilp
           else if( ihwmny<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lds<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldp<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors to be computed
           if( .not.ilall ) then
              im = 0_ilp
              ilcplx = .false.
              loop_10: do j = 1, n
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_10
                 end if
                 if( j<n ) then
                    if( s( j+1, j )/=zero )ilcplx = .true.
                 end if
                 if( ilcplx ) then
                    if( select( j ) .or. select( j+1 ) )im = im + 2_ilp
                 else
                    if( select( j ) )im = im + 1_ilp
                 end if
              end do loop_10
           else
              im = n
           end if
           ! check 2-by-2 diagonal blocks of a, b
           ilabad = .false.
           ilbbad = .false.
           do j = 1, n - 1
              if( s( j+1, j )/=zero ) then
                 if( p( j, j )==zero .or. p( j+1, j+1 )==zero .or.p( j, j+1 )/=zero )ilbbad = &
                           .true.
                 if( j<n-1 ) then
                    if( s( j+2, j+1 )/=zero )ilabad = .true.
                 end if
              end if
           end do
           if( ilabad ) then
              info = -5_ilp
           else if( ilbbad ) then
              info = -7_ilp
           else if( compl .and. ldvl<n .or. ldvl<1_ilp ) then
              info = -10_ilp
           else if( compr .and. ldvr<n .or. ldvr<1_ilp ) then
              info = -12_ilp
           else if( mm<im ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_dlabad( safmin, big )
           ulp = stdlib_dlamch( 'EPSILON' )*stdlib_dlamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part (i.e., excluding all elements belonging to the diagonal
           ! blocks) of a and b to check for possible overflow in the
           ! triangular solver.
           anorm = abs( s( 1_ilp, 1_ilp ) )
           if( n>1_ilp )anorm = anorm + abs( s( 2_ilp, 1_ilp ) )
           bnorm = abs( p( 1_ilp, 1_ilp ) )
           work( 1_ilp ) = zero
           work( n+1 ) = zero
           do j = 2, n
              temp = zero
              temp2 = zero
              if( s( j, j-1 )==zero ) then
                 iend = j - 1_ilp
              else
                 iend = j - 2_ilp
              end if
              do i = 1, iend
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              work( j ) = temp
              work( n+j ) = temp2
              do i = iend + 1, min( j+1, n )
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              anorm = max( anorm, temp )
              bnorm = max( bnorm, temp2 )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_220: do je = 1, n
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at.
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_220
                 end if
                 nw = 1_ilp
                 if( je<n ) then
                    if( s( je+1, je )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je+1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_220
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       ieig = ieig + 1_ilp
                       do jr = 1, n
                          vl( jr, ieig ) = zero
                       end do
                       vl( ieig, ieig ) = one
                       cycle loop_220
                    end if
                 end if
                 ! clear vector
                 do jr = 1, nw*n
                    work( 2_ilp*n+jr ) = zero
                 end do
                                                       ! t
                 ! compute coefficients in  ( a a - b b )  y = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp*n+je ) = one
                    xmax = one
                 else
                    ! complex eigenvalue
                    call stdlib_dlag2( s( je, je ), lds, p( je, je ), ldp,safmin*safety, acoef, &
                              temp, bcoefr, temp2,bcoefi )
                    bcoefi = -bcoefi
                    if( bcoefi==zero ) then
                       info = je
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    temp = acoef*s( je+1, je )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp*n+je ) = one
                       work( 3_ilp*n+je ) = zero
                       work( 2_ilp*n+je+1 ) = -temp2r / temp
                       work( 3_ilp*n+je+1 ) = -temp2i / temp
                    else
                       work( 2_ilp*n+je+1 ) = one
                       work( 3_ilp*n+je+1 ) = zero
                       temp = acoef*s( je, je+1 )
                       work( 2_ilp*n+je ) = ( bcoefr*p( je+1, je+1 )-acoef*s( je+1, je+1 ) ) / &
                                 temp
                       work( 3_ilp*n+je ) = bcoefi*p( je+1, je+1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp*n+je ) )+abs( work( 3_ilp*n+je ) ),abs( work( 2_ilp*n+je+1 ) &
                              )+abs( work( 3_ilp*n+je+1 ) ) )
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                 ! t
                 ! triangular solve of  (a a - b b)  y = 0
                                         ! t
                 ! (rowwise in  (a a - b b) , or columnwise in (a a - b b) )
                 il2by2 = .false.
                 loop_160: do j = je + nw, n
                    if( il2by2 ) then
                       il2by2 = .false.
                       cycle loop_160
                    end if
                    na = 1_ilp
                    bdiag( 1_ilp ) = p( j, j )
                    if( j<n ) then
                       if( s( j+1, j )/=zero ) then
                          il2by2 = .true.
                          bdiag( 2_ilp ) = p( j+1, j+1 )
                          na = 2_ilp
                       end if
                    end if
                    ! check whether scaling is necessary for dot products
                    xscale = one / max( one, xmax )
                    temp = max( work( j ), work( n+j ),acoefa*work( j )+bcoefa*work( n+j ) )
                              
                    if( il2by2 )temp = max( temp, work( j+1 ), work( n+j+1 ),acoefa*work( j+1 )+&
                              bcoefa*work( n+j+1 ) )
                    if( temp>bignum*xscale ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = xmax*xscale
                    end if
                    ! compute dot products
                          ! j-1
                    ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                          ! k=je
                    ! to reduce the op count, this is done as
                    ! _        j-1                  _        j-1
                    ! a*conjg( sum  s(k,j)*x(k) ) - b*conjg( sum  p(k,j)*x(k) )
                             ! k=je                          k=je
                    ! which may cause underflow problems if a or b are close
                    ! to underflow.  (e.g., less than small.)
                    do jw = 1, nw
                       do ja = 1, na
                          sums( ja, jw ) = zero
                          sump( ja, jw ) = zero
                          do jr = je, j - 1
                             sums( ja, jw ) = sums( ja, jw ) +s( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                             sump( ja, jw ) = sump( ja, jw ) +p( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                          end do
                       end do
                    end do
                    do ja = 1, na
                       if( ilcplx ) then
                          sum( ja, 1_ilp ) = -acoef*sums( ja, 1_ilp ) +bcoefr*sump( ja, 1_ilp ) -bcoefi*sump( &
                                    ja, 2_ilp )
                          sum( ja, 2_ilp ) = -acoef*sums( ja, 2_ilp ) +bcoefr*sump( ja, 2_ilp ) +bcoefi*sump( &
                                    ja, 1_ilp )
                       else
                          sum( ja, 1_ilp ) = -acoef*sums( ja, 1_ilp ) +bcoefr*sump( ja, 1_ilp )
                       end if
                    end do
                                        ! t
                    ! solve  ( a a - b b )  y = sum(,)
                    ! with scaling and perturbation of the denominator
                    call stdlib_dlaln2( .true., na, nw, dmin, acoef, s( j, j ), lds,bdiag( 1_ilp ), &
                    bdiag( 2_ilp ), sum, 2_ilp, bcoefr,bcoefi, work( 2_ilp*n+j ), n, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = scale*xmax
                    end if
                    xmax = max( xmax, temp )
                 end do loop_160
                 ! copy eigenvector to vl, back transforming if
                 ! howmny='b'.
                 ieig = ieig + 1_ilp
                 if( ilback ) then
                    do jw = 0, nw - 1
                       call stdlib_dgemv( 'N', n, n+1-je, one, vl( 1_ilp, je ), ldvl,work( ( jw+2 )*n+&
                                 je ), 1_ilp, zero,work( ( jw+4 )*n+1 ), 1_ilp )
                    end do
                    call stdlib_dlacpy( ' ', n, nw, work( 4_ilp*n+1 ), n, vl( 1_ilp, je ),ldvl )
                    ibeg = 1_ilp
                 else
                    call stdlib_dlacpy( ' ', n, nw, work( 2_ilp*n+1 ), n, vl( 1_ilp, ieig ),ldvl )
                    ibeg = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) )+abs( vl( j, ieig+1 ) ) )
                    end do
                 else
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = ibeg, n
                          vl( jr, ieig+jw ) = xscale*vl( jr, ieig+jw )
                       end do
                    end do
                 end if
                 ieig = ieig + nw - 1_ilp
              end do loop_220
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_500: do je = n, 1, -1
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at -- if complex, select(je)
                 ! or select(je-1).
                 ! if this is a complex pair, the 2-by-2 diagonal block
                 ! corresponding to the eigenvalue is in rows/columns je-1:je
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_500
                 end if
                 nw = 1_ilp
                 if( je>1_ilp ) then
                    if( s( je, je-1 )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je-1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_500
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- unit eigenvector
                       ieig = ieig - 1_ilp
                       do jr = 1, n
                          vr( jr, ieig ) = zero
                       end do
                       vr( ieig, ieig ) = one
                       cycle loop_500
                    end if
                 end if
                 ! clear vector
                 do jw = 0, nw - 1
                    do jr = 1, n
                       work( ( jw+2 )*n+jr ) = zero
                    end do
                 end do
                 ! compute coefficients in  ( a a - b b ) x = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp*n+je ) = one
                    xmax = one
                    ! compute contribution from column je of a and b to sum
                    ! (see "further details", above.)
                    do jr = 1, je - 1
                       work( 2_ilp*n+jr ) = bcoefr*p( jr, je ) -acoef*s( jr, je )
                    end do
                 else
                    ! complex eigenvalue
                    call stdlib_dlag2( s( je-1, je-1 ), lds, p( je-1, je-1 ), ldp,safmin*safety, &
                              acoef, temp, bcoefr, temp2,bcoefi )
                    if( bcoefi==zero ) then
                       info = je - 1_ilp
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    ! and contribution to sums
                    temp = acoef*s( je, je-1 )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>=abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp*n+je ) = one
                       work( 3_ilp*n+je ) = zero
                       work( 2_ilp*n+je-1 ) = -temp2r / temp
                       work( 3_ilp*n+je-1 ) = -temp2i / temp
                    else
                       work( 2_ilp*n+je-1 ) = one
                       work( 3_ilp*n+je-1 ) = zero
                       temp = acoef*s( je-1, je )
                       work( 2_ilp*n+je ) = ( bcoefr*p( je-1, je-1 )-acoef*s( je-1, je-1 ) ) / &
                                 temp
                       work( 3_ilp*n+je ) = bcoefi*p( je-1, je-1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp*n+je ) )+abs( work( 3_ilp*n+je ) ),abs( work( 2_ilp*n+je-1 ) &
                              )+abs( work( 3_ilp*n+je-1 ) ) )
                    ! compute contribution from columns je and je-1
                    ! of a and b to the sums.
                    creala = acoef*work( 2_ilp*n+je-1 )
                    cimaga = acoef*work( 3_ilp*n+je-1 )
                    crealb = bcoefr*work( 2_ilp*n+je-1 ) -bcoefi*work( 3_ilp*n+je-1 )
                    cimagb = bcoefi*work( 2_ilp*n+je-1 ) +bcoefr*work( 3_ilp*n+je-1 )
                    cre2a = acoef*work( 2_ilp*n+je )
                    cim2a = acoef*work( 3_ilp*n+je )
                    cre2b = bcoefr*work( 2_ilp*n+je ) - bcoefi*work( 3_ilp*n+je )
                    cim2b = bcoefi*work( 2_ilp*n+je ) + bcoefr*work( 3_ilp*n+je )
                    do jr = 1, je - 2
                       work( 2_ilp*n+jr ) = -creala*s( jr, je-1 ) +crealb*p( jr, je-1 ) -cre2a*s( jr, &
                                 je ) + cre2b*p( jr, je )
                       work( 3_ilp*n+jr ) = -cimaga*s( jr, je-1 ) +cimagb*p( jr, je-1 ) -cim2a*s( jr, &
                                 je ) + cim2b*p( jr, je )
                    end do
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                 ! columnwise triangular solve of  (a a - b b)  x = 0
                 il2by2 = .false.
                 loop_370: do j = je - nw, 1, -1
                    ! if a 2-by-2 block, is in position j-1:j, wait until
                    ! next iteration to process it (when it will be j:j+1)
                    if( .not.il2by2 .and. j>1_ilp ) then
                       if( s( j, j-1 )/=zero ) then
                          il2by2 = .true.
                          cycle loop_370
                       end if
                    end if
                    bdiag( 1_ilp ) = p( j, j )
                    if( il2by2 ) then
                       na = 2_ilp
                       bdiag( 2_ilp ) = p( j+1, j+1 )
                    else
                       na = 1_ilp
                    end if
                    ! compute x(j) (and x(j+1), if 2-by-2 block)
                    call stdlib_dlaln2( .false., na, nw, dmin, acoef, s( j, j ),lds, bdiag( 1_ilp ), &
                    bdiag( 2_ilp ), work( 2_ilp*n+j ),n, bcoefr, bcoefi, sum, 2_ilp, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = 1, je
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                    end if
                    xmax = max( scale*xmax, temp )
                    do jw = 1, nw
                       do ja = 1, na
                          work( ( jw+1 )*n+j+ja-1 ) = sum( ja, jw )
                       end do
                    end do
                    ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                    if( j>1_ilp ) then
                       ! check whether scaling is necessary for sum.
                       xscale = one / max( one, xmax )
                       temp = acoefa*work( j ) + bcoefa*work( n+j )
                       if( il2by2 )temp = max( temp, acoefa*work( j+1 )+bcoefa*work( n+j+1 ) )
                                 
                       temp = max( temp, acoefa, bcoefa )
                       if( temp>bignum*xscale ) then
                          do jw = 0, nw - 1
                             do jr = 1, je
                                work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                             end do
                          end do
                          xmax = xmax*xscale
                       end if
                       ! compute the contributions of the off-diagonals of
                       ! column j (and j+1, if 2-by-2 block) of a and b to the
                       ! sums.
                       do ja = 1, na
                          if( ilcplx ) then
                             creala = acoef*work( 2_ilp*n+j+ja-1 )
                             cimaga = acoef*work( 3_ilp*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp*n+j+ja-1 ) -bcoefi*work( 3_ilp*n+j+ja-1 )
                             cimagb = bcoefi*work( 2_ilp*n+j+ja-1 ) +bcoefr*work( 3_ilp*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp*n+jr ) = work( 2_ilp*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                                work( 3_ilp*n+jr ) = work( 3_ilp*n+jr ) -cimaga*s( jr, j+ja-1 ) +cimagb*p(&
                                           jr, j+ja-1 )
                             end do
                          else
                             creala = acoef*work( 2_ilp*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp*n+jr ) = work( 2_ilp*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                             end do
                          end if
                       end do
                    end if
                    il2by2 = .false.
                 end do loop_370
                 ! copy eigenvector to vr, back transforming if
                 ! howmny='b'.
                 ieig = ieig - nw
                 if( ilback ) then
                    do jw = 0, nw - 1
                       do jr = 1, n
                          work( ( jw+4 )*n+jr ) = work( ( jw+2 )*n+1 )*vr( jr, 1_ilp )
                       end do
                       ! a series of compiler directives to defeat
                       ! vectorization for the next loop
                       do jc = 2, je
                          do jr = 1, n
                             work( ( jw+4 )*n+jr ) = work( ( jw+4 )*n+jr ) +work( ( jw+2 )*n+jc )&
                                       *vr( jr, jc )
                          end do
                       end do
                    end do
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+4 )*n+jr )
                       end do
                    end do
                    iend = n
                 else
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+2 )*n+jr )
                       end do
                    end do
                    iend = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) )+abs( vr( j, ieig+1 ) ) )
                    end do
                 else
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = 1, iend
                          vr( jr, ieig+jw ) = xscale*vr( jr, ieig+jw )
                       end do
                    end do
                 end if
              end do loop_500
           end if
           return
     end subroutine stdlib_dtgevc


     pure module subroutine stdlib_ctgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! CTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of complex matrices (S,P), where S and P are upper triangular.
     !! Matrix pairs of this type are produced by the generalized Schur
     !! factorization of a complex matrix pair (A,B):
     !! A = Q*S*Z**H,  B = Q*P*Z**H
     !! as computed by CGGHRD + CHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal elements of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the unitary factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: p(ldp,*), s(lds,*)
           complex(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: compl, compr, ilall, ilback, ilbbad, ilcomp, lsa, lsb
           integer(ilp) :: i, ibeg, ieig, iend, ihwmny, im, iside, isrc, j, je, jr
           real(sp) :: acoefa, acoeff, anorm, ascale, bcoefa, big, bignum, bnorm, bscale, dmin, &
                     safmin, sbeta, scale, small, temp, ulp, xmax
           complex(sp) :: bcoeff, ca, cb, d, salpha, sum, suma, sumb, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp
           end if
           info = 0_ilp
           if( iside<0_ilp ) then
              info = -1_ilp
           else if( ihwmny<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lds<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldp<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors
           if( .not.ilall ) then
              im = 0_ilp
              do j = 1, n
                 if( select( j ) )im = im + 1_ilp
              end do
           else
              im = n
           end if
           ! check diagonal of b
           ilbbad = .false.
           do j = 1, n
              if( aimag( p( j, j ) )/=zero )ilbbad = .true.
           end do
           if( ilbbad ) then
              info = -7_ilp
           else if( compl .and. ldvl<n .or. ldvl<1_ilp ) then
              info = -10_ilp
           else if( compr .and. ldvr<n .or. ldvr<1_ilp ) then
              info = -12_ilp
           else if( mm<im ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_slabad( safmin, big )
           ulp = stdlib_slamch( 'EPSILON' )*stdlib_slamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part of a and b to check for possible overflow in the triangular
           ! solver.
           anorm = abs1( s( 1_ilp, 1_ilp ) )
           bnorm = abs1( p( 1_ilp, 1_ilp ) )
           rwork( 1_ilp ) = zero
           rwork( n+1 ) = zero
           do j = 2, n
              rwork( j ) = zero
              rwork( n+j ) = zero
              do i = 1, j - 1
                 rwork( j ) = rwork( j ) + abs1( s( i, j ) )
                 rwork( n+j ) = rwork( n+j ) + abs1( p( i, j ) )
              end do
              anorm = max( anorm, rwork( j )+abs1( s( j, j ) ) )
              bnorm = max( bnorm, rwork( n+j )+abs1( p( j, j ) ) )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp
              ! main loop over eigenvalues
              loop_140: do je = 1, n
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig + 1_ilp
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=sp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vl( jr, ieig ) = czero
                       end do
                       vl( ieig, ieig ) = cone
                       cycle loop_140
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                         ! h
                       ! y  ( a a - b b ) = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=sp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=sp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                    ! h
                    ! triangular solve of  (a a - b b)  y = 0
                                            ! h
                    ! (rowwise in  (a a - b b) , or columnwise in a a - b b)
                    loop_100: do j = je + 1, n
                       ! compute
                             ! j-1
                       ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                             ! k=je
                       ! (scale if necessary)
                       temp = one / xmax
                       if( acoefa*rwork( j )+bcoefa*rwork( n+j )>bignum*temp ) then
                          do jr = je, j - 1
                             work( jr ) = temp*work( jr )
                          end do
                          xmax = one
                       end if
                       suma = czero
                       sumb = czero
                       do jr = je, j - 1
                          suma = suma + conjg( s( jr, j ) )*work( jr )
                          sumb = sumb + conjg( p( jr, j ) )*work( jr )
                       end do
                       sum = acoeff*suma - conjg( bcoeff )*sumb
                       ! form x(j) = - sum / conjg( a*s(j,j) - b*p(j,j) )
                       ! with scaling and perturbation of the denominator
                       d = conjg( acoeff*s( j, j )-bcoeff*p( j, j ) )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=sp)
                       if( abs1( d )<one ) then
                          if( abs1( sum )>=bignum*abs1( d ) ) then
                             temp = one / abs1( sum )
                             do jr = je, j - 1
                                work( jr ) = temp*work( jr )
                             end do
                             xmax = temp*xmax
                             sum = temp*sum
                          end if
                       end if
                       work( j ) = stdlib_cladiv( -sum, d )
                       xmax = max( xmax, abs1( work( j ) ) )
                    end do loop_100
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_cgemv( 'N', n, n+1-je, cone, vl( 1_ilp, je ), ldvl,work( je ), 1_ilp, &
                                 czero, work( n+1 ), 1_ilp )
                       isrc = 2_ilp
                       ibeg = 1_ilp
                    else
                       isrc = 1_ilp
                       ibeg = je
                    end if
                    ! copy and scale eigenvector into column of vl
                    xmax = zero
                    do jr = ibeg, n
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = ibeg, n
                          vl( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       ibeg = n + 1_ilp
                    end if
                    do jr = 1, ibeg - 1
                       vl( jr, ieig ) = czero
                    end do
                 end if
              end do loop_140
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp
              ! main loop over eigenvalues
              loop_250: do je = n, 1, -1
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig - 1_ilp
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=sp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vr( jr, ieig ) = czero
                       end do
                       vr( ieig, ieig ) = cone
                       cycle loop_250
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                    ! ( a a - b b ) x  = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=sp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=sp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                    ! triangular solve of  (a a - b b) x = 0  (columnwise)
                    ! work(1:j-1) contains sums w,
                    ! work(j+1:je) contains x
                    do jr = 1, je - 1
                       work( jr ) = acoeff*s( jr, je ) - bcoeff*p( jr, je )
                    end do
                    work( je ) = cone
                    loop_210: do j = je - 1, 1, -1
                       ! form x(j) := - w(j) / d
                       ! with scaling and perturbation of the denominator
                       d = acoeff*s( j, j ) - bcoeff*p( j, j )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=sp)
                       if( abs1( d )<one ) then
                          if( abs1( work( j ) )>=bignum*abs1( d ) ) then
                             temp = one / abs1( work( j ) )
                             do jr = 1, je
                                work( jr ) = temp*work( jr )
                             end do
                          end if
                       end if
                       work( j ) = stdlib_cladiv( -work( j ), d )
                       if( j>1_ilp ) then
                          ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                          if( abs1( work( j ) )>one ) then
                             temp = one / abs1( work( j ) )
                             if( acoefa*rwork( j )+bcoefa*rwork( n+j )>=bignum*temp ) then
                                do jr = 1, je
                                   work( jr ) = temp*work( jr )
                                end do
                             end if
                          end if
                          ca = acoeff*work( j )
                          cb = bcoeff*work( j )
                          do jr = 1, j - 1
                             work( jr ) = work( jr ) + ca*s( jr, j ) -cb*p( jr, j )
                          end do
                       end if
                    end do loop_210
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_cgemv( 'N', n, je, cone, vr, ldvr, work, 1_ilp,czero, work( n+1 ), &
                                 1_ilp )
                       isrc = 2_ilp
                       iend = n
                    else
                       isrc = 1_ilp
                       iend = je
                    end if
                    ! copy and scale eigenvector into column of vr
                    xmax = zero
                    do jr = 1, iend
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = 1, iend
                          vr( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       iend = 0_ilp
                    end if
                    do jr = iend + 1, n
                       vr( jr, ieig ) = czero
                    end do
                 end if
              end do loop_250
           end if
           return
     end subroutine stdlib_ctgevc

     pure module subroutine stdlib_ztgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! ZTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of complex matrices (S,P), where S and P are upper triangular.
     !! Matrix pairs of this type are produced by the generalized Schur
     !! factorization of a complex matrix pair (A,B):
     !! A = Q*S*Z**H,  B = Q*P*Z**H
     !! as computed by ZGGHRD + ZHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal elements of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the unitary factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: p(ldp,*), s(lds,*)
           complex(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: compl, compr, ilall, ilback, ilbbad, ilcomp, lsa, lsb
           integer(ilp) :: i, ibeg, ieig, iend, ihwmny, im, iside, isrc, j, je, jr
           real(dp) :: acoefa, acoeff, anorm, ascale, bcoefa, big, bignum, bnorm, bscale, dmin, &
                     safmin, sbeta, scale, small, temp, ulp, xmax
           complex(dp) :: bcoeff, ca, cb, d, salpha, sum, suma, sumb, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp
           end if
           info = 0_ilp
           if( iside<0_ilp ) then
              info = -1_ilp
           else if( ihwmny<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lds<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldp<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors
           if( .not.ilall ) then
              im = 0_ilp
              do j = 1, n
                 if( select( j ) )im = im + 1_ilp
              end do
           else
              im = n
           end if
           ! check diagonal of b
           ilbbad = .false.
           do j = 1, n
              if( aimag( p( j, j ) )/=zero )ilbbad = .true.
           end do
           if( ilbbad ) then
              info = -7_ilp
           else if( compl .and. ldvl<n .or. ldvl<1_ilp ) then
              info = -10_ilp
           else if( compr .and. ldvr<n .or. ldvr<1_ilp ) then
              info = -12_ilp
           else if( mm<im ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_dlabad( safmin, big )
           ulp = stdlib_dlamch( 'EPSILON' )*stdlib_dlamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part of a and b to check for possible overflow in the triangular
           ! solver.
           anorm = abs1( s( 1_ilp, 1_ilp ) )
           bnorm = abs1( p( 1_ilp, 1_ilp ) )
           rwork( 1_ilp ) = zero
           rwork( n+1 ) = zero
           do j = 2, n
              rwork( j ) = zero
              rwork( n+j ) = zero
              do i = 1, j - 1
                 rwork( j ) = rwork( j ) + abs1( s( i, j ) )
                 rwork( n+j ) = rwork( n+j ) + abs1( p( i, j ) )
              end do
              anorm = max( anorm, rwork( j )+abs1( s( j, j ) ) )
              bnorm = max( bnorm, rwork( n+j )+abs1( p( j, j ) ) )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp
              ! main loop over eigenvalues
              loop_140: do je = 1, n
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig + 1_ilp
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=dp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vl( jr, ieig ) = czero
                       end do
                       vl( ieig, ieig ) = cone
                       cycle loop_140
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                         ! h
                       ! y  ( a a - b b ) = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=dp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=dp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                    ! h
                    ! triangular solve of  (a a - b b)  y = 0
                                            ! h
                    ! (rowwise in  (a a - b b) , or columnwise in a a - b b)
                    loop_100: do j = je + 1, n
                       ! compute
                             ! j-1
                       ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                             ! k=je
                       ! (scale if necessary)
                       temp = one / xmax
                       if( acoefa*rwork( j )+bcoefa*rwork( n+j )>bignum*temp ) then
                          do jr = je, j - 1
                             work( jr ) = temp*work( jr )
                          end do
                          xmax = one
                       end if
                       suma = czero
                       sumb = czero
                       do jr = je, j - 1
                          suma = suma + conjg( s( jr, j ) )*work( jr )
                          sumb = sumb + conjg( p( jr, j ) )*work( jr )
                       end do
                       sum = acoeff*suma - conjg( bcoeff )*sumb
                       ! form x(j) = - sum / conjg( a*s(j,j) - b*p(j,j) )
                       ! with scaling and perturbation of the denominator
                       d = conjg( acoeff*s( j, j )-bcoeff*p( j, j ) )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=dp)
                       if( abs1( d )<one ) then
                          if( abs1( sum )>=bignum*abs1( d ) ) then
                             temp = one / abs1( sum )
                             do jr = je, j - 1
                                work( jr ) = temp*work( jr )
                             end do
                             xmax = temp*xmax
                             sum = temp*sum
                          end if
                       end if
                       work( j ) = stdlib_zladiv( -sum, d )
                       xmax = max( xmax, abs1( work( j ) ) )
                    end do loop_100
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_zgemv( 'N', n, n+1-je, cone, vl( 1_ilp, je ), ldvl,work( je ), 1_ilp, &
                                 czero, work( n+1 ), 1_ilp )
                       isrc = 2_ilp
                       ibeg = 1_ilp
                    else
                       isrc = 1_ilp
                       ibeg = je
                    end if
                    ! copy and scale eigenvector into column of vl
                    xmax = zero
                    do jr = ibeg, n
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = ibeg, n
                          vl( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       ibeg = n + 1_ilp
                    end if
                    do jr = 1, ibeg - 1
                       vl( jr, ieig ) = czero
                    end do
                 end if
              end do loop_140
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp
              ! main loop over eigenvalues
              loop_250: do je = n, 1, -1
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig - 1_ilp
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=dp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vr( jr, ieig ) = czero
                       end do
                       vr( ieig, ieig ) = cone
                       cycle loop_250
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                    ! ( a a - b b ) x  = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=dp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=dp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                    ! triangular solve of  (a a - b b) x = 0  (columnwise)
                    ! work(1:j-1) contains sums w,
                    ! work(j+1:je) contains x
                    do jr = 1, je - 1
                       work( jr ) = acoeff*s( jr, je ) - bcoeff*p( jr, je )
                    end do
                    work( je ) = cone
                    loop_210: do j = je - 1, 1, -1
                       ! form x(j) := - w(j) / d
                       ! with scaling and perturbation of the denominator
                       d = acoeff*s( j, j ) - bcoeff*p( j, j )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=dp)
                       if( abs1( d )<one ) then
                          if( abs1( work( j ) )>=bignum*abs1( d ) ) then
                             temp = one / abs1( work( j ) )
                             do jr = 1, je
                                work( jr ) = temp*work( jr )
                             end do
                          end if
                       end if
                       work( j ) = stdlib_zladiv( -work( j ), d )
                       if( j>1_ilp ) then
                          ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                          if( abs1( work( j ) )>one ) then
                             temp = one / abs1( work( j ) )
                             if( acoefa*rwork( j )+bcoefa*rwork( n+j )>=bignum*temp ) then
                                do jr = 1, je
                                   work( jr ) = temp*work( jr )
                                end do
                             end if
                          end if
                          ca = acoeff*work( j )
                          cb = bcoeff*work( j )
                          do jr = 1, j - 1
                             work( jr ) = work( jr ) + ca*s( jr, j ) -cb*p( jr, j )
                          end do
                       end if
                    end do loop_210
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_zgemv( 'N', n, je, cone, vr, ldvr, work, 1_ilp,czero, work( n+1 ), &
                                 1_ilp )
                       isrc = 2_ilp
                       iend = n
                    else
                       isrc = 1_ilp
                       iend = je
                    end if
                    ! copy and scale eigenvector into column of vr
                    xmax = zero
                    do jr = 1, iend
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = 1, iend
                          vr( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       iend = 0_ilp
                    end if
                    do jr = iend + 1, n
                       vr( jr, ieig ) = czero
                    end do
                 end if
              end do loop_250
           end if
           return
     end subroutine stdlib_ztgevc




     pure module subroutine stdlib_stgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! STGEXC reorders the generalized real Schur decomposition of a real
     !! matrix pair (A,B) using an orthogonal equivalence transformation
     !! (A, B) = Q * (A, B) * Z**T,
     !! so that the diagonal block of (A, B) with row index IFST is moved
     !! to row ILST.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: here, lwmin, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldq<1_ilp .or. wantq .and. ( ldq<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. wantz .and. ( ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( ifst<1_ilp .or. ifst>n ) then
              info = -12_ilp
           else if( ilst<1_ilp .or. ilst>n ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
              else
                 lwmin = 4_ilp*n + 16_ilp
              end if
              work(1_ilp) = lwmin
              if (lwork<lwmin .and. .not.lquery) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGEXC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of the specified block and find out
           ! if it is 1-by-1 or 2-by-2.
           if( ifst>1_ilp ) then
              if( a( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp
           end if
           nbf = 1_ilp
           if( ifst<n ) then
              if( a( ifst+1, ifst )/=zero )nbf = 2_ilp
           end if
           ! determine the first row of the final block
           ! and find out if it is 1-by-1 or 2-by-2.
           if( ilst>1_ilp ) then
              if( a( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp
           end if
           nbl = 1_ilp
           if( ilst<n ) then
              if( a( ilst+1, ilst )/=zero )nbl = 2_ilp
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst.
              if( nbf==2_ilp .and. nbl==1_ilp )ilst = ilst - 1_ilp
              if( nbf==1_ilp .and. nbl==2_ilp )ilst = ilst + 1_ilp
              here = ifst
              10 continue
              ! swap with next one below.
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp
                 if( here+nbf+1<=n ) then
                    if( a( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, nbf, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp
                 if( here+3<=n ) then
                    if( a( here+3, here+2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here+1, 1_ilp, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, 1_ilp, &
                              1_ilp, work, lwork, info )
                    if( info/=0_ilp ) then
                       ilst = here
                       return
                    end if
                    here = here + 1_ilp
                 else
                    ! recompute nbnext in case of 2-by-2 split.
                    if( a( here+2, here+1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2-by-2 block did not split.
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, nbnext, work, lwork,info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp
                    else
                       ! 2-by-2 block did split.
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap with next one below.
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, nbf, work, lwork,info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, 1_ilp, work, lwork,info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, &
                              nbnext, 1_ilp, work, lwork, info )
                    if( info/=0_ilp ) then
                       ilst = here
                       return
                    end if
                    here = here - 1_ilp
                 else
                   ! recompute nbnext in case of 2-by-2 split.
                    if( a( here, here-1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2-by-2 block did not split.
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here-1,&
                                  2_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp
                    else
                       ! 2-by-2 block did split.
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp
                       call stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_stgexc

     pure module subroutine stdlib_dtgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! DTGEXC reorders the generalized real Schur decomposition of a real
     !! matrix pair (A,B) using an orthogonal equivalence transformation
     !! (A, B) = Q * (A, B) * Z**T,
     !! so that the diagonal block of (A, B) with row index IFST is moved
     !! to row ILST.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: here, lwmin, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldq<1_ilp .or. wantq .and. ( ldq<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. wantz .and. ( ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( ifst<1_ilp .or. ifst>n ) then
              info = -12_ilp
           else if( ilst<1_ilp .or. ilst>n ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
              else
                 lwmin = 4_ilp*n + 16_ilp
              end if
              work(1_ilp) = lwmin
              if (lwork<lwmin .and. .not.lquery) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGEXC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of the specified block and find out
           ! if it is 1-by-1 or 2-by-2.
           if( ifst>1_ilp ) then
              if( a( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp
           end if
           nbf = 1_ilp
           if( ifst<n ) then
              if( a( ifst+1, ifst )/=zero )nbf = 2_ilp
           end if
           ! determine the first row of the final block
           ! and find out if it is 1-by-1 or 2-by-2.
           if( ilst>1_ilp ) then
              if( a( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp
           end if
           nbl = 1_ilp
           if( ilst<n ) then
              if( a( ilst+1, ilst )/=zero )nbl = 2_ilp
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst.
              if( nbf==2_ilp .and. nbl==1_ilp )ilst = ilst - 1_ilp
              if( nbf==1_ilp .and. nbl==2_ilp )ilst = ilst + 1_ilp
              here = ifst
              10 continue
              ! swap with next one below.
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp
                 if( here+nbf+1<=n ) then
                    if( a( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, nbf, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp
                 if( here+3<=n ) then
                    if( a( here+3, here+2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here+1, 1_ilp, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, 1_ilp, &
                              1_ilp, work, lwork, info )
                    if( info/=0_ilp ) then
                       ilst = here
                       return
                    end if
                    here = here + 1_ilp
                 else
                    ! recompute nbnext in case of 2-by-2 split.
                    if( a( here+2, here+1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2-by-2 block did not split.
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, nbnext, work, lwork,info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp
                    else
                       ! 2-by-2 block did split.
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap with next one below.
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, nbf, work, lwork,info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, 1_ilp, work, lwork,info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, &
                              nbnext, 1_ilp, work, lwork, info )
                    if( info/=0_ilp ) then
                       ilst = here
                       return
                    end if
                    here = here - 1_ilp
                 else
                   ! recompute nbnext in case of 2-by-2 split.
                    if( a( here, here-1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2-by-2 block did not split.
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here-1,&
                                  2_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp
                    else
                       ! 2-by-2 block did split.
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp
                       call stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp, 1_ilp, work, lwork, info )
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_dtgexc


     pure module subroutine stdlib_ctgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! CTGEXC reorders the generalized Schur decomposition of a complex
     !! matrix pair (A,B), using an unitary equivalence transformation
     !! (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
     !! row index IFST is moved to row ILST.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp), intent(inout) :: ilst
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: here
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldq<1_ilp .or. wantq .and. ( ldq<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. wantz .and. ( ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( ifst<1_ilp .or. ifst>n ) then
              info = -12_ilp
           else if( ilst<1_ilp .or. ilst>n ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGEXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           if( ifst==ilst )return
           if( ifst<ilst ) then
              here = ifst
              10 continue
              ! swap with next one below
              call stdlib_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp ) then
                 ilst = here
                 return
              end if
              here = here + 1_ilp
              if( here<ilst )go to 10
              here = here - 1_ilp
           else
              here = ifst - 1_ilp
              20 continue
              ! swap with next one above
              call stdlib_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp ) then
                 ilst = here
                 return
              end if
              here = here - 1_ilp
              if( here>=ilst )go to 20
              here = here + 1_ilp
           end if
           ilst = here
           return
     end subroutine stdlib_ctgexc

     pure module subroutine stdlib_ztgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! ZTGEXC reorders the generalized Schur decomposition of a complex
     !! matrix pair (A,B), using an unitary equivalence transformation
     !! (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
     !! row index IFST is moved to row ILST.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp), intent(inout) :: ilst
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: here
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldq<1_ilp .or. wantq .and. ( ldq<max( 1_ilp, n ) ) ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. wantz .and. ( ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( ifst<1_ilp .or. ifst>n ) then
              info = -12_ilp
           else if( ilst<1_ilp .or. ilst>n ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGEXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           if( ifst==ilst )return
           if( ifst<ilst ) then
              here = ifst
              10 continue
              ! swap with next one below
              call stdlib_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp ) then
                 ilst = here
                 return
              end if
              here = here + 1_ilp
              if( here<ilst )go to 10
              here = here - 1_ilp
           else
              here = ifst - 1_ilp
              20 continue
              ! swap with next one above
              call stdlib_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp ) then
                 ilst = here
                 return
              end if
              here = here - 1_ilp
              if( here>=ilst )go to 20
              here = here + 1_ilp
           end if
           ilst = here
           return
     end subroutine stdlib_ztgexc




     pure module subroutine stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
     !! STGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
     !! of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
     !! (A, B) by an orthogonal equivalence transformation.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_scopy by calls to stdlib_slaset, or by do
        ! loops. sven hammarling, 1/5/02.
           ! Parameters 
           real(sp), parameter :: twenty = 2.0e+01_sp
           integer(ilp), parameter :: ldst = 4_ilp
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp) :: i, idum, linfo, m
           real(sp) :: bqra21, brqa21, ddum, dnorma, dnormb, dscale, dsum, eps, f, g, sa, sb, &
                     scale, smlnum, thresha, threshb
           ! Local Arrays 
           integer(ilp) :: iwork(ldst)
           real(sp) :: ai(2_ilp), ar(2_ilp), be(2_ilp), ir(ldst,ldst), ircop(ldst,ldst), li(ldst,ldst), licop(&
           ldst,ldst), s(ldst,ldst), scpy(ldst,ldst), t(ldst,ldst), taul(ldst), taur(ldst), tcpy(&
                     ldst,ldst)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=1 .or. n1<=0 .or. n2<=0 )return
           if( n1>n .or. ( j1+n1 )>n )return
           m = n1 + n2
           if( lwork<max( n*m, m*m*2_ilp ) ) then
              info = -16_ilp
              work( 1_ilp ) = max( n*m, m*m*2_ilp )
              return
           end if
           weak = .false.
           strong = .false.
           ! make a local copy of selected block
           call stdlib_slaset( 'FULL', ldst, ldst, zero, zero, li, ldst )
           call stdlib_slaset( 'FULL', ldst, ldst, zero, zero, ir, ldst )
           call stdlib_slacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute threshold for testing acceptance of swapping.
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           dscale = zero
           dsum = one
           call stdlib_slacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_slassq( m*m, work, 1_ilp, dscale, dsum )
           dnorma = dscale*sqrt( dsum )
           dscale = zero
           dsum = one
           call stdlib_slacpy( 'FULL', m, m, t, ldst, work, m )
           call stdlib_slassq( m*m, work, 1_ilp, dscale, dsum )
           dnormb = dscale*sqrt( dsum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*dnorma, smlnum )
           threshb = max( twenty*eps*dnormb, smlnum )
           if( m==2_ilp ) then
              ! case 1: swap 1-by-1 and 1-by-1 blocks.
              ! compute orthogonal ql and rq that swap 1-by-1 and 1-by-1 blocks
              ! using givens rotations and perform the swap tentatively.
              f = s( 2_ilp, 2_ilp )*t( 1_ilp, 1_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 1_ilp )
              g = s( 2_ilp, 2_ilp )*t( 1_ilp, 2_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 2_ilp )
              sa = abs( s( 2_ilp, 2_ilp ) ) * abs( t( 1_ilp, 1_ilp ) )
              sb = abs( s( 1_ilp, 1_ilp ) ) * abs( t( 2_ilp, 2_ilp ) )
              call stdlib_slartg( f, g, ir( 1_ilp, 2_ilp ), ir( 1_ilp, 1_ilp ), ddum )
              ir( 2_ilp, 1_ilp ) = -ir( 1_ilp, 2_ilp )
              ir( 2_ilp, 2_ilp ) = ir( 1_ilp, 1_ilp )
              call stdlib_srot( 2_ilp, s( 1_ilp, 1_ilp ), 1_ilp, s( 1_ilp, 2_ilp ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
              call stdlib_srot( 2_ilp, t( 1_ilp, 1_ilp ), 1_ilp, t( 1_ilp, 2_ilp ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
              if( sa>=sb ) then
                 call stdlib_slartg( s( 1_ilp, 1_ilp ), s( 2_ilp, 1_ilp ), li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp ),ddum )
              else
                 call stdlib_slartg( t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ), li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp ),ddum )
              end if
              call stdlib_srot( 2_ilp, s( 1_ilp, 1_ilp ), ldst, s( 2_ilp, 1_ilp ), ldst, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp ) )
                        
              call stdlib_srot( 2_ilp, t( 1_ilp, 1_ilp ), ldst, t( 2_ilp, 1_ilp ), ldst, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp ) )
                        
              li( 2_ilp, 2_ilp ) = li( 1_ilp, 1_ilp )
              li( 1_ilp, 2_ilp ) = -li( 2_ilp, 1_ilp )
              ! weak stability test: |s21| <= o(eps f-norm((a)))
                                 ! and  |t21| <= o(eps f-norm((b)))
              weak = abs( s( 2_ilp, 1_ilp ) ) <= thresha .and.abs( t( 2_ilp, 1_ilp ) ) <= threshb
              if( .not.weak )go to 70
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_slacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_sgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_sgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_slassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_sgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_sgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_slassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                     ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              call stdlib_srot( j1+1, a( 1_ilp, j1 ), 1_ilp, a( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
                        
              call stdlib_srot( j1+1, b( 1_ilp, j1 ), 1_ilp, b( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
                        
              call stdlib_srot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda,li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp &
                        ) )
              call stdlib_srot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb,li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp &
                        ) )
              ! set  n1-by-n2 (2,1) - blocks to zero.
              a( j1+1, j1 ) = zero
              b( j1+1, j1 ) = zero
              ! accumulate transformations into q and z if requested.
              if( wantz )call stdlib_srot( n, z( 1_ilp, j1 ), 1_ilp, z( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp &
                        ) )
              if( wantq )call stdlib_srot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j1+1 ), 1_ilp, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp &
                        ) )
              ! exit with info = 0 if swap was successfully performed.
              return
           else
              ! case 2: swap 1-by-1 and 2-by-2 blocks, or 2-by-2
                      ! and 2-by-2 blocks.
              ! solve the generalized sylvester equation
                       ! s11 * r - l * s22 = scale * s12
                       ! t11 * r - l * t22 = scale * t12
              ! for r and l. solutions in li and ir.
              call stdlib_slacpy( 'FULL', n1, n2, t( 1_ilp, n1+1 ), ldst, li, ldst )
              call stdlib_slacpy( 'FULL', n1, n2, s( 1_ilp, n1+1 ), ldst,ir( n2+1, n1+1 ), ldst )
                        
              call stdlib_stgsy2( 'N', 0_ilp, n1, n2, s, ldst, s( n1+1, n1+1 ), ldst,ir( n2+1, n1+1 ),&
               ldst, t, ldst, t( n1+1, n1+1 ),ldst, li, ldst, scale, dsum, dscale, iwork, idum,&
                         linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix ql:
                          ! ql**t * li = [ tl ]
                                       ! [ 0  ]
              ! where
                          ! li =  [      -l              ]
                                ! [ scale * identity(n2) ]
              do i = 1, n2
                 call stdlib_sscal( n1, -one, li( 1_ilp, i ), 1_ilp )
                 li( n1+i, i ) = scale
              end do
              call stdlib_sgeqr2( m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_sorg2r( m, m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix rq:
                          ! ir * rq**t =   [ 0  tr],
               ! where ir = [ scale * identity(n1), r ]
              do i = 1, n1
                 ir( n2+i, i ) = scale
              end do
              call stdlib_sgerq2( n1, m, ir( n2+1, 1_ilp ), ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_sorgr2( m, m, n1, ir, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              ! perform the swapping tentatively:
              call stdlib_sgemm( 'T', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
              call stdlib_sgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, s,ldst )
              call stdlib_sgemm( 'T', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
              call stdlib_sgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, t,ldst )
              call stdlib_slacpy( 'F', m, m, s, ldst, scpy, ldst )
              call stdlib_slacpy( 'F', m, m, t, ldst, tcpy, ldst )
              call stdlib_slacpy( 'F', m, m, ir, ldst, ircop, ldst )
              call stdlib_slacpy( 'F', m, m, li, ldst, licop, ldst )
              ! triangularize the b-part by an rq factorization.
              ! apply transformation (from left) to a-part, giving s.
              call stdlib_sgerq2( m, m, t, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_sormr2( 'R', 'T', m, m, m, t, ldst, taur, s, ldst, work,linfo )
              if( linfo/=0 )go to 70
              call stdlib_sormr2( 'L', 'N', m, m, m, t, ldst, taur, ir, ldst, work,linfo )
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in brqa21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_slassq( n1, s( n2+1, i ), 1_ilp, dscale, dsum )
              end do
              brqa21 = dscale*sqrt( dsum )
              ! triangularize the b-part by a qr factorization.
              ! apply transformation (from right) to a-part, giving s.
              call stdlib_sgeqr2( m, m, tcpy, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_sorm2r( 'L', 'T', m, m, m, tcpy, ldst, taul, scpy, ldst,work, info )
                        
              call stdlib_sorm2r( 'R', 'N', m, m, m, tcpy, ldst, taul, licop, ldst,work, info )
                        
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in bqra21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_slassq( n1, scpy( n2+1, i ), 1_ilp, dscale, dsum )
              end do
              bqra21 = dscale*sqrt( dsum )
              ! decide which method to use.
                ! weak stability test:
                   ! f-norm(s21) <= o(eps * f-norm((s)))
              if( bqra21<=brqa21 .and. bqra21<=thresha ) then
                 call stdlib_slacpy( 'F', m, m, scpy, ldst, s, ldst )
                 call stdlib_slacpy( 'F', m, m, tcpy, ldst, t, ldst )
                 call stdlib_slacpy( 'F', m, m, ircop, ldst, ir, ldst )
                 call stdlib_slacpy( 'F', m, m, licop, ldst, li, ldst )
              else if( brqa21>=thresha ) then
                 go to 70
              end if
              ! set lower triangle of b-part to zero
              if (m>1_ilp) call stdlib_slaset( 'LOWER', m-1, m-1, zero, zero, t(2_ilp,1_ilp), ldst )
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_slacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_sgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_sgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_slassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_sgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_sgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_slassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! if the swap is accepted ("weakly" and "strongly"), apply the
              ! transformations and set n1-by-n2 (2,1)-block to zero.
              call stdlib_slaset( 'FULL', n1, n2, zero, zero, s(n2+1,1_ilp), ldst )
              ! copy back m-by-m diagonal block starting at index j1 of (a, b)
              call stdlib_slacpy( 'F', m, m, s, ldst, a( j1, j1 ), lda )
              call stdlib_slacpy( 'F', m, m, t, ldst, b( j1, j1 ), ldb )
              call stdlib_slaset( 'FULL', ldst, ldst, zero, zero, t, ldst )
              ! standardize existing 2-by-2 blocks.
              call stdlib_slaset( 'FULL', m, m, zero, zero, work, m )
              work( 1_ilp ) = one
              t( 1_ilp, 1_ilp ) = one
              idum = lwork - m*m - 2_ilp
              if( n2>1_ilp ) then
                 call stdlib_slagv2( a( j1, j1 ), lda, b( j1, j1 ), ldb, ar, ai, be,work( 1_ilp ), &
                           work( 2_ilp ), t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ) )
                 work( m+1 ) = -work( 2_ilp )
                 work( m+2 ) = work( 1_ilp )
                 t( n2, n2 ) = t( 1_ilp, 1_ilp )
                 t( 1_ilp, 2_ilp ) = -t( 2_ilp, 1_ilp )
              end if
              work( m*m ) = one
              t( m, m ) = one
              if( n1>1_ilp ) then
                 call stdlib_slagv2( a( j1+n2, j1+n2 ), lda, b( j1+n2, j1+n2 ), ldb,taur, taul, &
                 work( m*m+1 ), work( n2*m+n2+1 ),work( n2*m+n2+2 ), t( n2+1, n2+1 ),t( m, m-1 ) )
                           
                 work( m*m ) = work( n2*m+n2+1 )
                 work( m*m-1 ) = -work( n2*m+n2+2 )
                 t( m, m ) = t( n2+1, n2+1 )
                 t( m-1, m ) = -t( m, m-1 )
              end if
              call stdlib_sgemm( 'T', 'N', n2, n1, n2, one, work, m, a( j1, j1+n2 ),lda, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_slacpy( 'FULL', n2, n1, work( m*m+1 ), n2, a( j1, j1+n2 ),lda )
              call stdlib_sgemm( 'T', 'N', n2, n1, n2, one, work, m, b( j1, j1+n2 ),ldb, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_slacpy( 'FULL', n2, n1, work( m*m+1 ), n2, b( j1, j1+n2 ),ldb )
              call stdlib_sgemm( 'N', 'N', m, m, m, one, li, ldst, work, m, zero,work( m*m+1 ), m &
                        )
              call stdlib_slacpy( 'FULL', m, m, work( m*m+1 ), m, li, ldst )
              call stdlib_sgemm( 'N', 'N', n2, n1, n1, one, a( j1, j1+n2 ), lda,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_slacpy( 'FULL', n2, n1, work, n2, a( j1, j1+n2 ), lda )
              call stdlib_sgemm( 'N', 'N', n2, n1, n1, one, b( j1, j1+n2 ), ldb,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_slacpy( 'FULL', n2, n1, work, n2, b( j1, j1+n2 ), ldb )
              call stdlib_sgemm( 'T', 'N', m, m, m, one, ir, ldst, t, ldst, zero,work, m )
              call stdlib_slacpy( 'FULL', m, m, work, m, ir, ldst )
              ! accumulate transformations into q and z if requested.
              if( wantq ) then
                 call stdlib_sgemm( 'N', 'N', n, m, m, one, q( 1_ilp, j1 ), ldq, li,ldst, zero, work, &
                           n )
                 call stdlib_slacpy( 'FULL', n, m, work, n, q( 1_ilp, j1 ), ldq )
              end if
              if( wantz ) then
                 call stdlib_sgemm( 'N', 'N', n, m, m, one, z( 1_ilp, j1 ), ldz, ir,ldst, zero, work, &
                           n )
                 call stdlib_slacpy( 'FULL', n, m, work, n, z( 1_ilp, j1 ), ldz )
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                      ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              i = j1 + m
              if( i<=n ) then
                 call stdlib_sgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,a( j1, i ), lda, zero, &
                           work, m )
                 call stdlib_slacpy( 'FULL', m, n-i+1, work, m, a( j1, i ), lda )
                 call stdlib_sgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,b( j1, i ), ldb, zero, &
                           work, m )
                 call stdlib_slacpy( 'FULL', m, n-i+1, work, m, b( j1, i ), ldb )
              end if
              i = j1 - 1_ilp
              if( i>0_ilp ) then
                 call stdlib_sgemm( 'N', 'N', i, m, m, one, a( 1_ilp, j1 ), lda, ir,ldst, zero, work, &
                           i )
                 call stdlib_slacpy( 'FULL', i, m, work, i, a( 1_ilp, j1 ), lda )
                 call stdlib_sgemm( 'N', 'N', i, m, m, one, b( 1_ilp, j1 ), ldb, ir,ldst, zero, work, &
                           i )
                 call stdlib_slacpy( 'FULL', i, m, work, i, b( 1_ilp, j1 ), ldb )
              end if
              ! exit with info = 0 if swap was successfully performed.
              return
           end if
           ! exit with info = 1 if swap was rejected.
           70 continue
           info = 1_ilp
           return
     end subroutine stdlib_stgex2

     pure module subroutine stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
     !! DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
     !! of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
     !! (A, B) by an orthogonal equivalence transformation.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_dcopy by calls to stdlib_dlaset, or by do
        ! loops. sven hammarling, 1/5/02.
           ! Parameters 
           real(dp), parameter :: twenty = 2.0e+01_dp
           integer(ilp), parameter :: ldst = 4_ilp
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp) :: i, idum, linfo, m
           real(dp) :: bqra21, brqa21, ddum, dnorma, dnormb, dscale, dsum, eps, f, g, sa, sb, &
                     scale, smlnum, thresha, threshb
           ! Local Arrays 
           integer(ilp) :: iwork(ldst)
           real(dp) :: ai(2_ilp), ar(2_ilp), be(2_ilp), ir(ldst,ldst), ircop(ldst,ldst), li(ldst,ldst), licop(&
           ldst,ldst), s(ldst,ldst), scpy(ldst,ldst), t(ldst,ldst), taul(ldst), taur(ldst), tcpy(&
                     ldst,ldst)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=1 .or. n1<=0 .or. n2<=0 )return
           if( n1>n .or. ( j1+n1 )>n )return
           m = n1 + n2
           if( lwork<max( 1_ilp, n*m, m*m*2_ilp ) ) then
              info = -16_ilp
              work( 1_ilp ) = max( 1_ilp, n*m, m*m*2_ilp )
              return
           end if
           weak = .false.
           strong = .false.
           ! make a local copy of selected block
           call stdlib_dlaset( 'FULL', ldst, ldst, zero, zero, li, ldst )
           call stdlib_dlaset( 'FULL', ldst, ldst, zero, zero, ir, ldst )
           call stdlib_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute threshold for testing acceptance of swapping.
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           dscale = zero
           dsum = one
           call stdlib_dlacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_dlassq( m*m, work, 1_ilp, dscale, dsum )
           dnorma = dscale*sqrt( dsum )
           dscale = zero
           dsum = one
           call stdlib_dlacpy( 'FULL', m, m, t, ldst, work, m )
           call stdlib_dlassq( m*m, work, 1_ilp, dscale, dsum )
           dnormb = dscale*sqrt( dsum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*dnorma, smlnum )
           threshb = max( twenty*eps*dnormb, smlnum )
           if( m==2_ilp ) then
              ! case 1: swap 1-by-1 and 1-by-1 blocks.
              ! compute orthogonal ql and rq that swap 1-by-1 and 1-by-1 blocks
              ! using givens rotations and perform the swap tentatively.
              f = s( 2_ilp, 2_ilp )*t( 1_ilp, 1_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 1_ilp )
              g = s( 2_ilp, 2_ilp )*t( 1_ilp, 2_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 2_ilp )
              sa = abs( s( 2_ilp, 2_ilp ) ) * abs( t( 1_ilp, 1_ilp ) )
              sb = abs( s( 1_ilp, 1_ilp ) ) * abs( t( 2_ilp, 2_ilp ) )
              call stdlib_dlartg( f, g, ir( 1_ilp, 2_ilp ), ir( 1_ilp, 1_ilp ), ddum )
              ir( 2_ilp, 1_ilp ) = -ir( 1_ilp, 2_ilp )
              ir( 2_ilp, 2_ilp ) = ir( 1_ilp, 1_ilp )
              call stdlib_drot( 2_ilp, s( 1_ilp, 1_ilp ), 1_ilp, s( 1_ilp, 2_ilp ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
              call stdlib_drot( 2_ilp, t( 1_ilp, 1_ilp ), 1_ilp, t( 1_ilp, 2_ilp ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
              if( sa>=sb ) then
                 call stdlib_dlartg( s( 1_ilp, 1_ilp ), s( 2_ilp, 1_ilp ), li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp ),ddum )
              else
                 call stdlib_dlartg( t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ), li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp ),ddum )
              end if
              call stdlib_drot( 2_ilp, s( 1_ilp, 1_ilp ), ldst, s( 2_ilp, 1_ilp ), ldst, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp ) )
                        
              call stdlib_drot( 2_ilp, t( 1_ilp, 1_ilp ), ldst, t( 2_ilp, 1_ilp ), ldst, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp ) )
                        
              li( 2_ilp, 2_ilp ) = li( 1_ilp, 1_ilp )
              li( 1_ilp, 2_ilp ) = -li( 2_ilp, 1_ilp )
              ! weak stability test: |s21| <= o(eps f-norm((a)))
                                 ! and  |t21| <= o(eps f-norm((b)))
              weak = abs( s( 2_ilp, 1_ilp ) ) <= thresha .and.abs( t( 2_ilp, 1_ilp ) ) <= threshb
              if( .not.weak )go to 70
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_dgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_dgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_dlassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_dgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_dgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_dlassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                     ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              call stdlib_drot( j1+1, a( 1_ilp, j1 ), 1_ilp, a( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
                        
              call stdlib_drot( j1+1, b( 1_ilp, j1 ), 1_ilp, b( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp ) )
                        
              call stdlib_drot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda,li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp &
                        ) )
              call stdlib_drot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb,li( 1_ilp, 1_ilp ), li( 2_ilp, 1_ilp &
                        ) )
              ! set  n1-by-n2 (2,1) - blocks to zero.
              a( j1+1, j1 ) = zero
              b( j1+1, j1 ) = zero
              ! accumulate transformations into q and z if requested.
              if( wantz )call stdlib_drot( n, z( 1_ilp, j1 ), 1_ilp, z( 1_ilp, j1+1 ), 1_ilp, ir( 1_ilp, 1_ilp ),ir( 2_ilp, 1_ilp &
                        ) )
              if( wantq )call stdlib_drot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j1+1 ), 1_ilp, li( 1_ilp, 1_ilp ),li( 2_ilp, 1_ilp &
                        ) )
              ! exit with info = 0 if swap was successfully performed.
              return
           else
              ! case 2: swap 1-by-1 and 2-by-2 blocks, or 2-by-2
                      ! and 2-by-2 blocks.
              ! solve the generalized sylvester equation
                       ! s11 * r - l * s22 = scale * s12
                       ! t11 * r - l * t22 = scale * t12
              ! for r and l. solutions in li and ir.
              call stdlib_dlacpy( 'FULL', n1, n2, t( 1_ilp, n1+1 ), ldst, li, ldst )
              call stdlib_dlacpy( 'FULL', n1, n2, s( 1_ilp, n1+1 ), ldst,ir( n2+1, n1+1 ), ldst )
                        
              call stdlib_dtgsy2( 'N', 0_ilp, n1, n2, s, ldst, s( n1+1, n1+1 ), ldst,ir( n2+1, n1+1 ),&
               ldst, t, ldst, t( n1+1, n1+1 ),ldst, li, ldst, scale, dsum, dscale, iwork, idum,&
                         linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix ql:
                          ! ql**t * li = [ tl ]
                                       ! [ 0  ]
              ! where
                          ! li =  [      -l              ]
                                ! [ scale * identity(n2) ]
              do i = 1, n2
                 call stdlib_dscal( n1, -one, li( 1_ilp, i ), 1_ilp )
                 li( n1+i, i ) = scale
              end do
              call stdlib_dgeqr2( m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_dorg2r( m, m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix rq:
                          ! ir * rq**t =   [ 0  tr],
               ! where ir = [ scale * identity(n1), r ]
              do i = 1, n1
                 ir( n2+i, i ) = scale
              end do
              call stdlib_dgerq2( n1, m, ir( n2+1, 1_ilp ), ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_dorgr2( m, m, n1, ir, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              ! perform the swapping tentatively:
              call stdlib_dgemm( 'T', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
              call stdlib_dgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, s,ldst )
              call stdlib_dgemm( 'T', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
              call stdlib_dgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, t,ldst )
              call stdlib_dlacpy( 'F', m, m, s, ldst, scpy, ldst )
              call stdlib_dlacpy( 'F', m, m, t, ldst, tcpy, ldst )
              call stdlib_dlacpy( 'F', m, m, ir, ldst, ircop, ldst )
              call stdlib_dlacpy( 'F', m, m, li, ldst, licop, ldst )
              ! triangularize the b-part by an rq factorization.
              ! apply transformation (from left) to a-part, giving s.
              call stdlib_dgerq2( m, m, t, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_dormr2( 'R', 'T', m, m, m, t, ldst, taur, s, ldst, work,linfo )
              if( linfo/=0 )go to 70
              call stdlib_dormr2( 'L', 'N', m, m, m, t, ldst, taur, ir, ldst, work,linfo )
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in brqa21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_dlassq( n1, s( n2+1, i ), 1_ilp, dscale, dsum )
              end do
              brqa21 = dscale*sqrt( dsum )
              ! triangularize the b-part by a qr factorization.
              ! apply transformation (from right) to a-part, giving s.
              call stdlib_dgeqr2( m, m, tcpy, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_dorm2r( 'L', 'T', m, m, m, tcpy, ldst, taul, scpy, ldst,work, info )
                        
              call stdlib_dorm2r( 'R', 'N', m, m, m, tcpy, ldst, taul, licop, ldst,work, info )
                        
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in bqra21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_dlassq( n1, scpy( n2+1, i ), 1_ilp, dscale, dsum )
              end do
              bqra21 = dscale*sqrt( dsum )
              ! decide which method to use.
                ! weak stability test:
                   ! f-norm(s21) <= o(eps * f-norm((s)))
              if( bqra21<=brqa21 .and. bqra21<=thresha ) then
                 call stdlib_dlacpy( 'F', m, m, scpy, ldst, s, ldst )
                 call stdlib_dlacpy( 'F', m, m, tcpy, ldst, t, ldst )
                 call stdlib_dlacpy( 'F', m, m, ircop, ldst, ir, ldst )
                 call stdlib_dlacpy( 'F', m, m, licop, ldst, li, ldst )
              else if( brqa21>=thresha ) then
                 go to 70
              end if
              ! set lower triangle of b-part to zero
              call stdlib_dlaset( 'LOWER', m-1, m-1, zero, zero, t(2_ilp,1_ilp), ldst )
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_dgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_dgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_dlassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_dgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_dgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_dlassq( m*m, work( m*m+1 ), 1_ilp, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! if the swap is accepted ("weakly" and "strongly"), apply the
              ! transformations and set n1-by-n2 (2,1)-block to zero.
              call stdlib_dlaset( 'FULL', n1, n2, zero, zero, s(n2+1,1_ilp), ldst )
              ! copy back m-by-m diagonal block starting at index j1 of (a, b)
              call stdlib_dlacpy( 'F', m, m, s, ldst, a( j1, j1 ), lda )
              call stdlib_dlacpy( 'F', m, m, t, ldst, b( j1, j1 ), ldb )
              call stdlib_dlaset( 'FULL', ldst, ldst, zero, zero, t, ldst )
              ! standardize existing 2-by-2 blocks.
              call stdlib_dlaset( 'FULL', m, m, zero, zero, work, m )
              work( 1_ilp ) = one
              t( 1_ilp, 1_ilp ) = one
              idum = lwork - m*m - 2_ilp
              if( n2>1_ilp ) then
                 call stdlib_dlagv2( a( j1, j1 ), lda, b( j1, j1 ), ldb, ar, ai, be,work( 1_ilp ), &
                           work( 2_ilp ), t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ) )
                 work( m+1 ) = -work( 2_ilp )
                 work( m+2 ) = work( 1_ilp )
                 t( n2, n2 ) = t( 1_ilp, 1_ilp )
                 t( 1_ilp, 2_ilp ) = -t( 2_ilp, 1_ilp )
              end if
              work( m*m ) = one
              t( m, m ) = one
              if( n1>1_ilp ) then
                 call stdlib_dlagv2( a( j1+n2, j1+n2 ), lda, b( j1+n2, j1+n2 ), ldb,taur, taul, &
                 work( m*m+1 ), work( n2*m+n2+1 ),work( n2*m+n2+2 ), t( n2+1, n2+1 ),t( m, m-1 ) )
                           
                 work( m*m ) = work( n2*m+n2+1 )
                 work( m*m-1 ) = -work( n2*m+n2+2 )
                 t( m, m ) = t( n2+1, n2+1 )
                 t( m-1, m ) = -t( m, m-1 )
              end if
              call stdlib_dgemm( 'T', 'N', n2, n1, n2, one, work, m, a( j1, j1+n2 ),lda, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_dlacpy( 'FULL', n2, n1, work( m*m+1 ), n2, a( j1, j1+n2 ),lda )
              call stdlib_dgemm( 'T', 'N', n2, n1, n2, one, work, m, b( j1, j1+n2 ),ldb, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_dlacpy( 'FULL', n2, n1, work( m*m+1 ), n2, b( j1, j1+n2 ),ldb )
              call stdlib_dgemm( 'N', 'N', m, m, m, one, li, ldst, work, m, zero,work( m*m+1 ), m &
                        )
              call stdlib_dlacpy( 'FULL', m, m, work( m*m+1 ), m, li, ldst )
              call stdlib_dgemm( 'N', 'N', n2, n1, n1, one, a( j1, j1+n2 ), lda,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_dlacpy( 'FULL', n2, n1, work, n2, a( j1, j1+n2 ), lda )
              call stdlib_dgemm( 'N', 'N', n2, n1, n1, one, b( j1, j1+n2 ), ldb,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_dlacpy( 'FULL', n2, n1, work, n2, b( j1, j1+n2 ), ldb )
              call stdlib_dgemm( 'T', 'N', m, m, m, one, ir, ldst, t, ldst, zero,work, m )
              call stdlib_dlacpy( 'FULL', m, m, work, m, ir, ldst )
              ! accumulate transformations into q and z if requested.
              if( wantq ) then
                 call stdlib_dgemm( 'N', 'N', n, m, m, one, q( 1_ilp, j1 ), ldq, li,ldst, zero, work, &
                           n )
                 call stdlib_dlacpy( 'FULL', n, m, work, n, q( 1_ilp, j1 ), ldq )
              end if
              if( wantz ) then
                 call stdlib_dgemm( 'N', 'N', n, m, m, one, z( 1_ilp, j1 ), ldz, ir,ldst, zero, work, &
                           n )
                 call stdlib_dlacpy( 'FULL', n, m, work, n, z( 1_ilp, j1 ), ldz )
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                      ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              i = j1 + m
              if( i<=n ) then
                 call stdlib_dgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,a( j1, i ), lda, zero, &
                           work, m )
                 call stdlib_dlacpy( 'FULL', m, n-i+1, work, m, a( j1, i ), lda )
                 call stdlib_dgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,b( j1, i ), ldb, zero, &
                           work, m )
                 call stdlib_dlacpy( 'FULL', m, n-i+1, work, m, b( j1, i ), ldb )
              end if
              i = j1 - 1_ilp
              if( i>0_ilp ) then
                 call stdlib_dgemm( 'N', 'N', i, m, m, one, a( 1_ilp, j1 ), lda, ir,ldst, zero, work, &
                           i )
                 call stdlib_dlacpy( 'FULL', i, m, work, i, a( 1_ilp, j1 ), lda )
                 call stdlib_dgemm( 'N', 'N', i, m, m, one, b( 1_ilp, j1 ), ldb, ir,ldst, zero, work, &
                           i )
                 call stdlib_dlacpy( 'FULL', i, m, work, i, b( 1_ilp, j1 ), ldb )
              end if
              ! exit with info = 0 if swap was successfully performed.
              return
           end if
           ! exit with info = 1 if swap was rejected.
           70 continue
           info = 1_ilp
           return
     end subroutine stdlib_dtgex2


     pure module subroutine stdlib_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
     !! CTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
     !! in an upper triangular matrix pair (A, B) by an unitary equivalence
     !! transformation.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: twenty = 2.0e+1_sp
           integer(ilp), parameter :: ldst = 2_ilp
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp) :: i, m
           real(sp) :: cq, cz, eps, sa, sb, scale, smlnum, sum, thresha, threshb
           complex(sp) :: cdum, f, g, sq, sz
           ! Local Arrays 
           complex(sp) :: s(ldst,ldst), t(ldst,ldst), work(8_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=1 )return
           m = ldst
           weak = .false.
           strong = .false.
           ! make a local copy of selected block in (a, b)
           call stdlib_clacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_clacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute the threshold for testing the acceptance of swapping.
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           scale = real( czero,KIND=sp)
           sum = real( cone,KIND=sp)
           call stdlib_clacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_clacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
           call stdlib_classq( m*m, work, 1_ilp, scale, sum )
           sa = scale*sqrt( sum )
           scale = real( czero,KIND=sp)
           sum = real( cone,KIND=sp)
           call stdlib_classq( m*m, work(m*m+1), 1_ilp, scale, sum )
           sb = scale*sqrt( sum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*sa, smlnum )
           threshb = max( twenty*eps*sb, smlnum )
           ! compute unitary ql and rq that swap 1-by-1 and 1-by-1 blocks
           ! using givens rotations and perform the swap tentatively.
           f = s( 2_ilp, 2_ilp )*t( 1_ilp, 1_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 1_ilp )
           g = s( 2_ilp, 2_ilp )*t( 1_ilp, 2_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 2_ilp )
           sa = abs( s( 2_ilp, 2_ilp ) ) * abs( t( 1_ilp, 1_ilp ) )
           sb = abs( s( 1_ilp, 1_ilp ) ) * abs( t( 2_ilp, 2_ilp ) )
           call stdlib_clartg( g, f, cz, sz, cdum )
           sz = -sz
           call stdlib_crot( 2_ilp, s( 1_ilp, 1_ilp ), 1_ilp, s( 1_ilp, 2_ilp ), 1_ilp, cz, conjg( sz ) )
           call stdlib_crot( 2_ilp, t( 1_ilp, 1_ilp ), 1_ilp, t( 1_ilp, 2_ilp ), 1_ilp, cz, conjg( sz ) )
           if( sa>=sb ) then
              call stdlib_clartg( s( 1_ilp, 1_ilp ), s( 2_ilp, 1_ilp ), cq, sq, cdum )
           else
              call stdlib_clartg( t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ), cq, sq, cdum )
           end if
           call stdlib_crot( 2_ilp, s( 1_ilp, 1_ilp ), ldst, s( 2_ilp, 1_ilp ), ldst, cq, sq )
           call stdlib_crot( 2_ilp, t( 1_ilp, 1_ilp ), ldst, t( 2_ilp, 1_ilp ), ldst, cq, sq )
           ! weak stability test: |s21| <= o(eps f-norm((a)))
                                ! and  |t21| <= o(eps f-norm((b)))
           weak = abs( s( 2_ilp, 1_ilp ) )<=thresha .and.abs( t( 2_ilp, 1_ilp ) )<=threshb
           if( .not.weak )go to 20
           if( wands ) then
              ! strong stability test:
                 ! f-norm((a-ql**h*s*qr, b-ql**h*t*qr)) <= o(eps*f-norm((a, b)))
              call stdlib_clacpy( 'FULL', m, m, s, ldst, work, m )
              call stdlib_clacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
              call stdlib_crot( 2_ilp, work, 1_ilp, work( 3_ilp ), 1_ilp, cz, -conjg( sz ) )
              call stdlib_crot( 2_ilp, work( 5_ilp ), 1_ilp, work( 7_ilp ), 1_ilp, cz, -conjg( sz ) )
              call stdlib_crot( 2_ilp, work, 2_ilp, work( 2_ilp ), 2_ilp, cq, -sq )
              call stdlib_crot( 2_ilp, work( 5_ilp ), 2_ilp, work( 6_ilp ), 2_ilp, cq, -sq )
              do i = 1, 2
                 work( i ) = work( i ) - a( j1+i-1, j1 )
                 work( i+2 ) = work( i+2 ) - a( j1+i-1, j1+1 )
                 work( i+4 ) = work( i+4 ) - b( j1+i-1, j1 )
                 work( i+6 ) = work( i+6 ) - b( j1+i-1, j1+1 )
              end do
              scale = real( czero,KIND=sp)
              sum = real( cone,KIND=sp)
              call stdlib_classq( m*m, work, 1_ilp, scale, sum )
              sa = scale*sqrt( sum )
              scale = real( czero,KIND=sp)
              sum = real( cone,KIND=sp)
              call stdlib_classq( m*m, work(m*m+1), 1_ilp, scale, sum )
              sb = scale*sqrt( sum )
              strong = sa<=thresha .and. sb<=threshb
              if( .not.strong )go to 20
           end if
           ! if the swap is accepted ("weakly" and "strongly"), apply the
           ! equivalence transformations to the original matrix pair (a,b)
           call stdlib_crot( j1+1, a( 1_ilp, j1 ), 1_ilp, a( 1_ilp, j1+1 ), 1_ilp, cz, conjg( sz ) )
           call stdlib_crot( j1+1, b( 1_ilp, j1 ), 1_ilp, b( 1_ilp, j1+1 ), 1_ilp, cz, conjg( sz ) )
           call stdlib_crot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda, cq, sq )
           call stdlib_crot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb, cq, sq )
           ! set  n1 by n2 (2,1) blocks to 0
           a( j1+1, j1 ) = czero
           b( j1+1, j1 ) = czero
           ! accumulate transformations into q and z if requested.
           if( wantz )call stdlib_crot( n, z( 1_ilp, j1 ), 1_ilp, z( 1_ilp, j1+1 ), 1_ilp, cz, conjg( sz ) )
                     
           if( wantq )call stdlib_crot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j1+1 ), 1_ilp, cq, conjg( sq ) )
                     
           ! exit with info = 0 if swap was successfully performed.
           return
           ! exit with info = 1 if swap was rejected.
           20 continue
           info = 1_ilp
           return
     end subroutine stdlib_ctgex2

     pure module subroutine stdlib_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
     !! ZTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
     !! in an upper triangular matrix pair (A, B) by an unitary equivalence
     !! transformation.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: twenty = 2.0e+1_dp
           integer(ilp), parameter :: ldst = 2_ilp
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp) :: i, m
           real(dp) :: cq, cz, eps, sa, sb, scale, smlnum, sum, thresha, threshb
           complex(dp) :: cdum, f, g, sq, sz
           ! Local Arrays 
           complex(dp) :: s(ldst,ldst), t(ldst,ldst), work(8_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n<=1 )return
           m = ldst
           weak = .false.
           strong = .false.
           ! make a local copy of selected block in (a, b)
           call stdlib_zlacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_zlacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute the threshold for testing the acceptance of swapping.
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           scale = real( czero,KIND=dp)
           sum = real( cone,KIND=dp)
           call stdlib_zlacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_zlacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
           call stdlib_zlassq( m*m, work, 1_ilp, scale, sum )
           sa = scale*sqrt( sum )
           scale = real( czero,KIND=dp)
           sum = real( cone,KIND=dp)
           call stdlib_zlassq( m*m, work(m*m+1), 1_ilp, scale, sum )
           sb = scale*sqrt( sum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*sa, smlnum )
           threshb = max( twenty*eps*sb, smlnum )
           ! compute unitary ql and rq that swap 1-by-1 and 1-by-1 blocks
           ! using givens rotations and perform the swap tentatively.
           f = s( 2_ilp, 2_ilp )*t( 1_ilp, 1_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 1_ilp )
           g = s( 2_ilp, 2_ilp )*t( 1_ilp, 2_ilp ) - t( 2_ilp, 2_ilp )*s( 1_ilp, 2_ilp )
           sa = abs( s( 2_ilp, 2_ilp ) ) * abs( t( 1_ilp, 1_ilp ) )
           sb = abs( s( 1_ilp, 1_ilp ) ) * abs( t( 2_ilp, 2_ilp ) )
           call stdlib_zlartg( g, f, cz, sz, cdum )
           sz = -sz
           call stdlib_zrot( 2_ilp, s( 1_ilp, 1_ilp ), 1_ilp, s( 1_ilp, 2_ilp ), 1_ilp, cz, conjg( sz ) )
           call stdlib_zrot( 2_ilp, t( 1_ilp, 1_ilp ), 1_ilp, t( 1_ilp, 2_ilp ), 1_ilp, cz, conjg( sz ) )
           if( sa>=sb ) then
              call stdlib_zlartg( s( 1_ilp, 1_ilp ), s( 2_ilp, 1_ilp ), cq, sq, cdum )
           else
              call stdlib_zlartg( t( 1_ilp, 1_ilp ), t( 2_ilp, 1_ilp ), cq, sq, cdum )
           end if
           call stdlib_zrot( 2_ilp, s( 1_ilp, 1_ilp ), ldst, s( 2_ilp, 1_ilp ), ldst, cq, sq )
           call stdlib_zrot( 2_ilp, t( 1_ilp, 1_ilp ), ldst, t( 2_ilp, 1_ilp ), ldst, cq, sq )
           ! weak stability test: |s21| <= o(eps f-norm((a)))
                                ! and  |t21| <= o(eps f-norm((b)))
           weak = abs( s( 2_ilp, 1_ilp ) )<=thresha .and.abs( t( 2_ilp, 1_ilp ) )<=threshb
           if( .not.weak )go to 20
           if( wands ) then
              ! strong stability test:
                 ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                 ! and
                 ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
              call stdlib_zlacpy( 'FULL', m, m, s, ldst, work, m )
              call stdlib_zlacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
              call stdlib_zrot( 2_ilp, work, 1_ilp, work( 3_ilp ), 1_ilp, cz, -conjg( sz ) )
              call stdlib_zrot( 2_ilp, work( 5_ilp ), 1_ilp, work( 7_ilp ), 1_ilp, cz, -conjg( sz ) )
              call stdlib_zrot( 2_ilp, work, 2_ilp, work( 2_ilp ), 2_ilp, cq, -sq )
              call stdlib_zrot( 2_ilp, work( 5_ilp ), 2_ilp, work( 6_ilp ), 2_ilp, cq, -sq )
              do i = 1, 2
                 work( i ) = work( i ) - a( j1+i-1, j1 )
                 work( i+2 ) = work( i+2 ) - a( j1+i-1, j1+1 )
                 work( i+4 ) = work( i+4 ) - b( j1+i-1, j1 )
                 work( i+6 ) = work( i+6 ) - b( j1+i-1, j1+1 )
              end do
              scale = real( czero,KIND=dp)
              sum = real( cone,KIND=dp)
              call stdlib_zlassq( m*m, work, 1_ilp, scale, sum )
              sa = scale*sqrt( sum )
              scale = real( czero,KIND=dp)
              sum = real( cone,KIND=dp)
              call stdlib_zlassq( m*m, work(m*m+1), 1_ilp, scale, sum )
              sb = scale*sqrt( sum )
              strong = sa<=thresha .and. sb<=threshb
              if( .not.strong )go to 20
           end if
           ! if the swap is accepted ("weakly" and "strongly"), apply the
           ! equivalence transformations to the original matrix pair (a,b)
           call stdlib_zrot( j1+1, a( 1_ilp, j1 ), 1_ilp, a( 1_ilp, j1+1 ), 1_ilp, cz,conjg( sz ) )
           call stdlib_zrot( j1+1, b( 1_ilp, j1 ), 1_ilp, b( 1_ilp, j1+1 ), 1_ilp, cz,conjg( sz ) )
           call stdlib_zrot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda, cq, sq )
           call stdlib_zrot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb, cq, sq )
           ! set  n1 by n2 (2,1) blocks to 0
           a( j1+1, j1 ) = czero
           b( j1+1, j1 ) = czero
           ! accumulate transformations into q and z if requested.
           if( wantz )call stdlib_zrot( n, z( 1_ilp, j1 ), 1_ilp, z( 1_ilp, j1+1 ), 1_ilp, cz,conjg( sz ) )
                     
           if( wantq )call stdlib_zrot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j1+1 ), 1_ilp, cq,conjg( sq ) )
                     
           ! exit with info = 0 if swap was successfully performed.
           return
           ! exit with info = 1 if swap was rejected.
           20 continue
           info = 1_ilp
           return
     end subroutine stdlib_ztgex2




     pure module subroutine stdlib_I64_stgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
     !! STGSEN reorders the generalized real Schur decomposition of a real
     !! matrix pair (A, B) (in terms of an orthonormal equivalence trans-
     !! formation Q**T * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the upper quasi-triangular
     !! matrix A and the upper triangular B. The leading columns of Q and
     !! Z form orthonormal bases of the corresponding left and right eigen-
     !! spaces (deflating subspaces). (A, B) must be in generalized real
     !! Schur canonical form (as returned by SGGES), i.e. A is block upper
     !! triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
     !! triangular.
     !! STGSEN also computes the generalized eigenvalues
     !! w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, STGSEN computes the estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantd, wantd1, wantd2, wantp
           integer(ilp64) :: i, ierr, ijb, k, kase, kk, ks, liwmin, lwmin, mn2, n1, n2
           real(sp) :: dscale, dsum, eps, rdscal, smlnum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           if( ijob<0_ilp64 .or. ijob>5_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldq<1_ilp64 .or. ( wantq .and. ldq<n ) ) then
              info = -14_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGSEN', -info )
              return
           end if
           ! get machine constants
           eps = stdlib_I64_slamch( 'P' )
           smlnum = stdlib_I64_slamch( 'S' ) / eps
           ierr = 0_ilp64
           wantp = ijob==1_ilp64 .or. ijob>=4_ilp64
           wantd1 = ijob==2_ilp64 .or. ijob==4_ilp64
           wantd2 = ijob==3_ilp64 .or. ijob==5_ilp64
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp64
           pair = .false.
           if( .not.lquery .or. ijob/=0_ilp64 ) then
           do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )==zero ) then
                       if( select( k ) )m = m + 1_ilp64
                    else
                       pair = .true.
                       if( select( k ) .or. select( k+1 ) )m = m + 2_ilp64
                    end if
                 else
                    if( select( n ) )m = m + 1_ilp64
                 end if
              end if
           end do
           end if
           if( ijob==1_ilp64 .or. ijob==2_ilp64 .or. ijob==4_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*n+16, 2_ilp64*m*(n-m) )
              liwmin = max( 1_ilp64, n+6 )
           else if( ijob==3_ilp64 .or. ijob==5_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*n+16, 4_ilp64*m*(n-m) )
              liwmin = max( 1_ilp64, 2_ilp64*m*(n-m), n+6 )
           else
              lwmin = max( 1_ilp64, 4_ilp64*n+16 )
              liwmin = 1_ilp64
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -22_ilp64
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -24_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp64 ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_I64_slassq( n, a( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                    call stdlib_I64_slassq( n, b( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                 end do
                 dif( 1_ilp64 ) = dscale*sqrt( dsum )
                 dif( 2_ilp64 ) = dif( 1_ilp64 )
              end if
              go to 60
           end if
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp64
           pair = .false.
           loop_30: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp64
                    ! swap the k-th block to position ks.
                    ! perform the reordering of diagonal blocks in (a, b)
                    ! by orthogonal transformation matrices and update
                    ! q and z accordingly (if requested):
                    kk = k
                    if( k/=ks )call stdlib_I64_stgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz,&
                               kk, ks, work, lwork, ierr )
                    if( ierr>0_ilp64 ) then
                       ! swap is rejected: exit.
                       info = 1_ilp64
                       if( wantp ) then
                          pl = zero
                          pr = zero
                       end if
                       if( wantd ) then
                          dif( 1_ilp64 ) = zero
                          dif( 2_ilp64 ) = zero
                       end if
                       go to 60
                    end if
                    if( pair )ks = ks + 1_ilp64
                 end if
              end if
           end do loop_30
           if( wantp ) then
              ! solve generalized sylvester equation for r and l
              ! and compute pl and pr.
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp64
              ijb = 0_ilp64
              call stdlib_I64_slacpy( 'FULL', n1, n2, a( 1_ilp64, i ), lda, work, n1 )
              call stdlib_I64_slacpy( 'FULL', n1, n2, b( 1_ilp64, i ), ldb, work( n1*n2+1 ),n1 )
              call stdlib_I64_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto left
              ! and right eigenspaces.
              rdscal = zero
              dsum = one
              call stdlib_I64_slassq( n1*n2, work, 1_ilp64, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_I64_slassq( n1*n2, work( n1*n2+1 ), 1_ilp64, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates of difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = idifjb
                 ! frobenius norm-based difu-estimate.
                 call stdlib_I64_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp64 ), work( 2_ilp64*n1*n2+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl-estimate.
                 call stdlib_I64_stgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp64 ), work( 2_ilp64*n1*n2+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_I64_slacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp64
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = 0_ilp64
                 mn2 = 2_ilp64*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_I64_slacn2( mn2, work( mn2+1 ), work, iwork, dif( 1_ilp64 ),kase, isave )
                           
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation.
                       call stdlib_I64_stgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_stgsyl( 'T', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp64 ) = dscale / dif( 1_ilp64 )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_I64_slacn2( mn2, work( mn2+1 ), work, iwork, dif( 2_ilp64 ),kase, isave )
                           
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation.
                       call stdlib_I64_stgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_stgsyl( 'T', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp64 ) = dscale / dif( 2_ilp64 )
              end if
           end if
           60 continue
           ! compute generalized eigenvalues of reordered pair (a, b) and
           ! normalize the generalized schur form.
           pair = .false.
           loop_70: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                    end if
                 end if
                 if( pair ) then
                   ! compute the eigenvalue(s) at position k.
                    work( 1_ilp64 ) = a( k, k )
                    work( 2_ilp64 ) = a( k+1, k )
                    work( 3_ilp64 ) = a( k, k+1 )
                    work( 4_ilp64 ) = a( k+1, k+1 )
                    work( 5_ilp64 ) = b( k, k )
                    work( 6_ilp64 ) = b( k+1, k )
                    work( 7_ilp64 ) = b( k, k+1 )
                    work( 8_ilp64 ) = b( k+1, k+1 )
                    call stdlib_I64_slag2( work, 2_ilp64, work( 5_ilp64 ), 2_ilp64, smlnum*eps, beta( k ),beta( k+1 ), &
                              alphar( k ), alphar( k+1 ),alphai( k ) )
                    alphai( k+1 ) = -alphai( k )
                 else
                    if( sign( one, b( k, k ) )<zero ) then
                       ! if b(k,k) is negative, make it positive
                       do i = 1, n
                          a( k, i ) = -a( k, i )
                          b( k, i ) = -b( k, i )
                          if( wantq ) q( i, k ) = -q( i, k )
                       end do
                    end if
                    alphar( k ) = a( k, k )
                    alphai( k ) = zero
                    beta( k ) = b( k, k )
                 end if
              end if
           end do loop_70
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_stgsen

     pure module subroutine stdlib_I64_dtgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
     !! DTGSEN reorders the generalized real Schur decomposition of a real
     !! matrix pair (A, B) (in terms of an orthonormal equivalence trans-
     !! formation Q**T * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the upper quasi-triangular
     !! matrix A and the upper triangular B. The leading columns of Q and
     !! Z form orthonormal bases of the corresponding left and right eigen-
     !! spaces (deflating subspaces). (A, B) must be in generalized real
     !! Schur canonical form (as returned by DGGES), i.e. A is block upper
     !! triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
     !! triangular.
     !! DTGSEN also computes the generalized eigenvalues
     !! w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, DTGSEN computes the estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantd, wantd1, wantd2, wantp
           integer(ilp64) :: i, ierr, ijb, k, kase, kk, ks, liwmin, lwmin, mn2, n1, n2
           real(dp) :: dscale, dsum, eps, rdscal, smlnum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           if( ijob<0_ilp64 .or. ijob>5_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldq<1_ilp64 .or. ( wantq .and. ldq<n ) ) then
              info = -14_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGSEN', -info )
              return
           end if
           ! get machine constants
           eps = stdlib_I64_dlamch( 'P' )
           smlnum = stdlib_I64_dlamch( 'S' ) / eps
           ierr = 0_ilp64
           wantp = ijob==1_ilp64 .or. ijob>=4_ilp64
           wantd1 = ijob==2_ilp64 .or. ijob==4_ilp64
           wantd2 = ijob==3_ilp64 .or. ijob==5_ilp64
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp64
           pair = .false.
           if( .not.lquery .or. ijob/=0_ilp64 ) then
           do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )==zero ) then
                       if( select( k ) )m = m + 1_ilp64
                    else
                       pair = .true.
                       if( select( k ) .or. select( k+1 ) )m = m + 2_ilp64
                    end if
                 else
                    if( select( n ) )m = m + 1_ilp64
                 end if
              end if
           end do
           end if
           if( ijob==1_ilp64 .or. ijob==2_ilp64 .or. ijob==4_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*n+16, 2_ilp64*m*( n-m ) )
              liwmin = max( 1_ilp64, n+6 )
           else if( ijob==3_ilp64 .or. ijob==5_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*n+16, 4_ilp64*m*( n-m ) )
              liwmin = max( 1_ilp64, 2_ilp64*m*( n-m ), n+6 )
           else
              lwmin = max( 1_ilp64, 4_ilp64*n+16 )
              liwmin = 1_ilp64
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -22_ilp64
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -24_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp64 ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_I64_dlassq( n, a( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                    call stdlib_I64_dlassq( n, b( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                 end do
                 dif( 1_ilp64 ) = dscale*sqrt( dsum )
                 dif( 2_ilp64 ) = dif( 1_ilp64 )
              end if
              go to 60
           end if
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp64
           pair = .false.
           loop_30: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp64
                    ! swap the k-th block to position ks.
                    ! perform the reordering of diagonal blocks in (a, b)
                    ! by orthogonal transformation matrices and update
                    ! q and z accordingly (if requested):
                    kk = k
                    if( k/=ks )call stdlib_I64_dtgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz,&
                               kk, ks, work, lwork, ierr )
                    if( ierr>0_ilp64 ) then
                       ! swap is rejected: exit.
                       info = 1_ilp64
                       if( wantp ) then
                          pl = zero
                          pr = zero
                       end if
                       if( wantd ) then
                          dif( 1_ilp64 ) = zero
                          dif( 2_ilp64 ) = zero
                       end if
                       go to 60
                    end if
                    if( pair )ks = ks + 1_ilp64
                 end if
              end if
           end do loop_30
           if( wantp ) then
              ! solve generalized sylvester equation for r and l
              ! and compute pl and pr.
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp64
              ijb = 0_ilp64
              call stdlib_I64_dlacpy( 'FULL', n1, n2, a( 1_ilp64, i ), lda, work, n1 )
              call stdlib_I64_dlacpy( 'FULL', n1, n2, b( 1_ilp64, i ), ldb, work( n1*n2+1 ),n1 )
              call stdlib_I64_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto left
              ! and right eigenspaces.
              rdscal = zero
              dsum = one
              call stdlib_I64_dlassq( n1*n2, work, 1_ilp64, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_I64_dlassq( n1*n2, work( n1*n2+1 ), 1_ilp64, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates of difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = idifjb
                 ! frobenius norm-based difu-estimate.
                 call stdlib_I64_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp64 ), work( 2_ilp64*n1*n2+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl-estimate.
                 call stdlib_I64_dtgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp64 ), work( 2_ilp64*n1*n2+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_I64_dlacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp64
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = 0_ilp64
                 mn2 = 2_ilp64*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_I64_dlacn2( mn2, work( mn2+1 ), work, iwork, dif( 1_ilp64 ),kase, isave )
                           
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation.
                       call stdlib_I64_dtgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_dtgsyl( 'T', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp64 ) = dscale / dif( 1_ilp64 )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_I64_dlacn2( mn2, work( mn2+1 ), work, iwork, dif( 2_ilp64 ),kase, isave )
                           
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation.
                       call stdlib_I64_dtgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_dtgsyl( 'T', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( 2_ilp64*n1*n2+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp64 ) = dscale / dif( 2_ilp64 )
              end if
           end if
           60 continue
           ! compute generalized eigenvalues of reordered pair (a, b) and
           ! normalize the generalized schur form.
           pair = .false.
           loop_80: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 if( k<n ) then
                    if( a( k+1, k )/=zero ) then
                       pair = .true.
                    end if
                 end if
                 if( pair ) then
                   ! compute the eigenvalue(s) at position k.
                    work( 1_ilp64 ) = a( k, k )
                    work( 2_ilp64 ) = a( k+1, k )
                    work( 3_ilp64 ) = a( k, k+1 )
                    work( 4_ilp64 ) = a( k+1, k+1 )
                    work( 5_ilp64 ) = b( k, k )
                    work( 6_ilp64 ) = b( k+1, k )
                    work( 7_ilp64 ) = b( k, k+1 )
                    work( 8_ilp64 ) = b( k+1, k+1 )
                    call stdlib_I64_dlag2( work, 2_ilp64, work( 5_ilp64 ), 2_ilp64, smlnum*eps, beta( k ),beta( k+1 ), &
                              alphar( k ), alphar( k+1 ),alphai( k ) )
                    alphai( k+1 ) = -alphai( k )
                 else
                    if( sign( one, b( k, k ) )<zero ) then
                       ! if b(k,k) is negative, make it positive
                       do i = 1, n
                          a( k, i ) = -a( k, i )
                          b( k, i ) = -b( k, i )
                          if( wantq ) q( i, k ) = -q( i, k )
                       end do
                    end if
                    alphar( k ) = a( k, k )
                    alphai( k ) = zero
                    beta( k ) = b( k, k )
                 end if
              end if
           end do loop_80
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_dtgsen


     pure module subroutine stdlib_I64_ctgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
     !! CTGSEN reorders the generalized Schur decomposition of a complex
     !! matrix pair (A, B) (in terms of an unitary equivalence trans-
     !! formation Q**H * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the pair (A,B). The leading
     !! columns of Q and Z form unitary bases of the corresponding left and
     !! right eigenspaces (deflating subspaces). (A, B) must be in
     !! generalized Schur canonical form, that is, A and B are both upper
     !! triangular.
     !! CTGSEN also computes the generalized eigenvalues
     !! w(j)= ALPHA(j) / BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, the routine computes estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(sp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, swap, wantd, wantd1, wantd2, wantp
           integer(ilp64) :: i, ierr, ijb, k, kase, ks, liwmin, lwmin, mn2, n1, n2
           real(sp) :: dscale, dsum, rdscal, safmin
           complex(sp) :: temp1, temp2
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           if( ijob<0_ilp64 .or. ijob>5_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldq<1_ilp64 .or. ( wantq .and. ldq<n ) ) then
              info = -13_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -15_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGSEN', -info )
              return
           end if
           ierr = 0_ilp64
           wantp = ijob==1_ilp64 .or. ijob>=4_ilp64
           wantd1 = ijob==2_ilp64 .or. ijob==4_ilp64
           wantd2 = ijob==3_ilp64 .or. ijob==5_ilp64
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp64
           if( .not.lquery .or. ijob/=0_ilp64 ) then
           do k = 1, n
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              if( k<n ) then
                 if( select( k ) )m = m + 1_ilp64
              else
                 if( select( n ) )m = m + 1_ilp64
              end if
           end do
           end if
           if( ijob==1_ilp64 .or. ijob==2_ilp64 .or. ijob==4_ilp64 ) then
              lwmin = max( 1_ilp64, 2_ilp64*m*(n-m) )
              liwmin = max( 1_ilp64, n+2 )
           else if( ijob==3_ilp64 .or. ijob==5_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*m*(n-m) )
              liwmin = max( 1_ilp64, 2_ilp64*m*(n-m), n+2 )
           else
              lwmin = 1_ilp64
              liwmin = 1_ilp64
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -21_ilp64
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -23_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp64 ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_I64_classq( n, a( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                    call stdlib_I64_classq( n, b( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                 end do
                 dif( 1_ilp64 ) = dscale*sqrt( dsum )
                 dif( 2_ilp64 ) = dif( 1_ilp64 )
              end if
              go to 70
           end if
           ! get machine constant
           safmin = stdlib_I64_slamch( 'S' )
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp64
           do k = 1, n
              swap = select( k )
              if( swap ) then
                 ks = ks + 1_ilp64
                 ! swap the k-th block to position ks. compute unitary q
                 ! and z that will swap adjacent diagonal blocks in (a, b).
                 if( k/=ks )call stdlib_I64_ctgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, k,&
                            ks, ierr )
                 if( ierr>0_ilp64 ) then
                    ! swap is rejected: exit.
                    info = 1_ilp64
                    if( wantp ) then
                       pl = zero
                       pr = zero
                    end if
                    if( wantd ) then
                       dif( 1_ilp64 ) = zero
                       dif( 2_ilp64 ) = zero
                    end if
                    go to 70
                 end if
              end if
           end do
           if( wantp ) then
              ! solve generalized sylvester equation for r and l:
                         ! a11 * r - l * a22 = a12
                         ! b11 * r - l * b22 = b12
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp64
              call stdlib_I64_clacpy( 'FULL', n1, n2, a( 1_ilp64, i ), lda, work, n1 )
              call stdlib_I64_clacpy( 'FULL', n1, n2, b( 1_ilp64, i ), ldb, work( n1*n2+1 ),n1 )
              ijb = 0_ilp64
              call stdlib_I64_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto
              ! left and right eigenspaces
              rdscal = zero
              dsum = one
              call stdlib_I64_classq( n1*n2, work, 1_ilp64, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_I64_classq( n1*n2, work( n1*n2+1 ), 1_ilp64, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = idifjb
                 ! frobenius norm-based difu estimate.
                 call stdlib_I64_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl estimate.
                 call stdlib_I64_ctgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_I64_clacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp64
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = 0_ilp64
                 mn2 = 2_ilp64*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_I64_clacn2( mn2, work( mn2+1 ), work, dif( 1_ilp64 ), kase,isave )
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation
                       call stdlib_I64_ctgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_ctgsyl( 'C', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp64 ) = dscale / dif( 1_ilp64 )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_I64_clacn2( mn2, work( mn2+1 ), work, dif( 2_ilp64 ), kase,isave )
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation
                       call stdlib_I64_ctgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_ctgsyl( 'C', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp64 ) = dscale / dif( 2_ilp64 )
              end if
           end if
           ! if b(k,k) is complex, make it real and positive (normalization
           ! of the generalized schur form) and store the generalized
           ! eigenvalues of reordered pair (a, b)
           do k = 1, n
              dscale = abs( b( k, k ) )
              if( dscale>safmin ) then
                 temp1 = conjg( b( k, k ) / dscale )
                 temp2 = b( k, k ) / dscale
                 b( k, k ) = dscale
                 call stdlib_I64_cscal( n-k, temp1, b( k, k+1 ), ldb )
                 call stdlib_I64_cscal( n-k+1, temp1, a( k, k ), lda )
                 if( wantq )call stdlib_I64_cscal( n, temp2, q( 1_ilp64, k ), 1_ilp64 )
              else
                 b( k, k ) = cmplx( zero, zero,KIND=sp)
              end if
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
           end do
           70 continue
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_ctgsen

     pure module subroutine stdlib_I64_ztgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
     !! ZTGSEN reorders the generalized Schur decomposition of a complex
     !! matrix pair (A, B) (in terms of an unitary equivalence trans-
     !! formation Q**H * (A, B) * Z), so that a selected cluster of eigenvalues
     !! appears in the leading diagonal blocks of the pair (A,B). The leading
     !! columns of Q and Z form unitary bases of the corresponding left and
     !! right eigenspaces (deflating subspaces). (A, B) must be in
     !! generalized Schur canonical form, that is, A and B are both upper
     !! triangular.
     !! ZTGSEN also computes the generalized eigenvalues
     !! w(j)= ALPHA(j) / BETA(j)
     !! of the reordered matrix pair (A, B).
     !! Optionally, the routine computes estimates of reciprocal condition
     !! numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
     !! (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
     !! between the matrix pairs (A11, B11) and (A22,B22) that correspond to
     !! the selected cluster and the eigenvalues outside the cluster, resp.,
     !! and norms of "projections" onto left and right eigenspaces w.r.t.
     !! the selected cluster in the (1,1)-block.
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(dp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, swap, wantd, wantd1, wantd2, wantp
           integer(ilp64) :: i, ierr, ijb, k, kase, ks, liwmin, lwmin, mn2, n1, n2
           real(dp) :: dscale, dsum, rdscal, safmin
           complex(dp) :: temp1, temp2
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           if( ijob<0_ilp64 .or. ijob>5_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldq<1_ilp64 .or. ( wantq .and. ldq<n ) ) then
              info = -13_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -15_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGSEN', -info )
              return
           end if
           ierr = 0_ilp64
           wantp = ijob==1_ilp64 .or. ijob>=4_ilp64
           wantd1 = ijob==2_ilp64 .or. ijob==4_ilp64
           wantd2 = ijob==3_ilp64 .or. ijob==5_ilp64
           wantd = wantd1 .or. wantd2
           ! set m to the dimension of the specified pair of deflating
           ! subspaces.
           m = 0_ilp64
           if( .not.lquery .or. ijob/=0_ilp64 ) then
           do k = 1, n
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              if( k<n ) then
                 if( select( k ) )m = m + 1_ilp64
              else
                 if( select( n ) )m = m + 1_ilp64
              end if
           end do
           end if
           if( ijob==1_ilp64 .or. ijob==2_ilp64 .or. ijob==4_ilp64 ) then
              lwmin = max( 1_ilp64, 2_ilp64*m*( n-m ) )
              liwmin = max( 1_ilp64, n+2 )
           else if( ijob==3_ilp64 .or. ijob==5_ilp64 ) then
              lwmin = max( 1_ilp64, 4_ilp64*m*( n-m ) )
              liwmin = max( 1_ilp64, 2_ilp64*m*( n-m ), n+2 )
           else
              lwmin = 1_ilp64
              liwmin = 1_ilp64
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           if( lwork<lwmin .and. .not.lquery ) then
              info = -21_ilp64
           else if( liwork<liwmin .and. .not.lquery ) then
              info = -23_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp64 ) then
              if( wantp ) then
                 pl = one
                 pr = one
              end if
              if( wantd ) then
                 dscale = zero
                 dsum = one
                 do i = 1, n
                    call stdlib_I64_zlassq( n, a( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                    call stdlib_I64_zlassq( n, b( 1_ilp64, i ), 1_ilp64, dscale, dsum )
                 end do
                 dif( 1_ilp64 ) = dscale*sqrt( dsum )
                 dif( 2_ilp64 ) = dif( 1_ilp64 )
              end if
              go to 70
           end if
           ! get machine constant
           safmin = stdlib_I64_dlamch( 'S' )
           ! collect the selected blocks at the top-left corner of (a, b).
           ks = 0_ilp64
           do k = 1, n
              swap = select( k )
              if( swap ) then
                 ks = ks + 1_ilp64
                 ! swap the k-th block to position ks. compute unitary q
                 ! and z that will swap adjacent diagonal blocks in (a, b).
                 if( k/=ks )call stdlib_I64_ztgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, k,&
                            ks, ierr )
                 if( ierr>0_ilp64 ) then
                    ! swap is rejected: exit.
                    info = 1_ilp64
                    if( wantp ) then
                       pl = zero
                       pr = zero
                    end if
                    if( wantd ) then
                       dif( 1_ilp64 ) = zero
                       dif( 2_ilp64 ) = zero
                    end if
                    go to 70
                 end if
              end if
           end do
           if( wantp ) then
              ! solve generalized sylvester equation for r and l:
                         ! a11 * r - l * a22 = a12
                         ! b11 * r - l * b22 = b12
              n1 = m
              n2 = n - m
              i = n1 + 1_ilp64
              call stdlib_I64_zlacpy( 'FULL', n1, n2, a( 1_ilp64, i ), lda, work, n1 )
              call stdlib_I64_zlacpy( 'FULL', n1, n2, b( 1_ilp64, i ), ldb, work( n1*n2+1 ),n1 )
              ijb = 0_ilp64
              call stdlib_I64_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b( i,&
               i ), ldb, work( n1*n2+1 ), n1,dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-2*n1*n2, &
                         iwork, ierr )
              ! estimate the reciprocal of norms of "projections" onto
              ! left and right eigenspaces
              rdscal = zero
              dsum = one
              call stdlib_I64_zlassq( n1*n2, work, 1_ilp64, rdscal, dsum )
              pl = rdscal*sqrt( dsum )
              if( pl==zero ) then
                 pl = one
              else
                 pl = dscale / ( sqrt( dscale*dscale / pl+pl )*sqrt( pl ) )
              end if
              rdscal = zero
              dsum = one
              call stdlib_I64_zlassq( n1*n2, work( n1*n2+1 ), 1_ilp64, rdscal, dsum )
              pr = rdscal*sqrt( dsum )
              if( pr==zero ) then
                 pr = one
              else
                 pr = dscale / ( sqrt( dscale*dscale / pr+pr )*sqrt( pr ) )
              end if
           end if
           if( wantd ) then
              ! compute estimates difu and difl.
              if( wantd1 ) then
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = idifjb
                 ! frobenius norm-based difu estimate.
                 call stdlib_I64_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda, work,n1, b, ldb, b(&
                  i, i ), ldb, work( n1*n2+1 ),n1, dscale, dif( 1_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
                 ! frobenius norm-based difl estimate.
                 call stdlib_I64_ztgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda, work,n2, b( i, i ),&
                  ldb, b, ldb, work( n1*n2+1 ),n2, dscale, dif( 2_ilp64 ), work( n1*n2*2_ilp64+1 ),lwork-&
                            2_ilp64*n1*n2, iwork, ierr )
              else
                 ! compute 1-norm-based estimates of difu and difl using
                 ! reversed communication with stdlib_I64_zlacn2. in each step a
                 ! generalized sylvester equation or a transposed variant
                 ! is solved.
                 kase = 0_ilp64
                 n1 = m
                 n2 = n - m
                 i = n1 + 1_ilp64
                 ijb = 0_ilp64
                 mn2 = 2_ilp64*n1*n2
                 ! 1-norm-based estimate of difu.
                 40 continue
                 call stdlib_I64_zlacn2( mn2, work( mn2+1 ), work, dif( 1_ilp64 ), kase,isave )
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation
                       call stdlib_I64_ztgsyl( 'N', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_ztgsyl( 'C', ijb, n1, n2, a, lda, a( i, i ), lda,work, n1, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n1, dscale, dif( 1_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 40
                 end if
                 dif( 1_ilp64 ) = dscale / dif( 1_ilp64 )
                 ! 1-norm-based estimate of difl.
                 50 continue
                 call stdlib_I64_zlacn2( mn2, work( mn2+1 ), work, dif( 2_ilp64 ), kase,isave )
                 if( kase/=0_ilp64 ) then
                    if( kase==1_ilp64 ) then
                       ! solve generalized sylvester equation
                       call stdlib_I64_ztgsyl( 'N', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b( &
                       i, i ), ldb, b, ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    else
                       ! solve the transposed variant.
                       call stdlib_I64_ztgsyl( 'C', ijb, n2, n1, a( i, i ), lda, a, lda,work, n2, b, &
                       ldb, b( i, i ), ldb,work( n1*n2+1 ), n2, dscale, dif( 2_ilp64 ),work( n1*n2*2_ilp64+1 )&
                                 , lwork-2*n1*n2, iwork,ierr )
                    end if
                    go to 50
                 end if
                 dif( 2_ilp64 ) = dscale / dif( 2_ilp64 )
              end if
           end if
           ! if b(k,k) is complex, make it real and positive (normalization
           ! of the generalized schur form) and store the generalized
           ! eigenvalues of reordered pair (a, b)
           do k = 1, n
              dscale = abs( b( k, k ) )
              if( dscale>safmin ) then
                 temp1 = conjg( b( k, k ) / dscale )
                 temp2 = b( k, k ) / dscale
                 b( k, k ) = dscale
                 call stdlib_I64_zscal( n-k, temp1, b( k, k+1 ), ldb )
                 call stdlib_I64_zscal( n-k+1, temp1, a( k, k ), lda )
                 if( wantq )call stdlib_I64_zscal( n, temp2, q( 1_ilp64, k ), 1_ilp64 )
              else
                 b( k, k ) = cmplx( zero, zero,KIND=dp)
              end if
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
           end do
           70 continue
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_ztgsen




     pure module subroutine stdlib_I64_stgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! STGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B) in
     !! generalized real Schur canonical form (or of any matrix pair
     !! (Q*A*Z**T, Q*B*Z**T) with orthogonal matrices Q and Z, where
     !! Z**T denotes the transpose of Z.
     !! (A, B) must be in generalized real Schur form (as returned by SGGES),
     !! i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
     !! blocks. B is upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: dif(*), s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: difdri = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, somcon, wantbh, wantdf, wants
           integer(ilp64) :: i, ierr, ifst, ilst, iz, k, ks, lwmin, n1, n2
           real(sp) :: alphai, alphar, alprqt, beta, c1, c2, cond, eps, lnrm, rnrm, root1, root2, &
                     scale, smlnum, tmpii, tmpir, tmpri, tmprr, uhav, uhavi, uhbv, uhbvi
           ! Local Arrays 
           real(sp) :: dummy(1_ilp64), dummy1(1_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( wants .and. ldvl<n ) then
              info = -10_ilp64
           else if( wants .and. ldvr<n ) then
              info = -12_ilp64
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp64
                 pair = .false.
                 do k = 1, n
                    if( pair ) then
                       pair = .false.
                    else
                       if( k<n ) then
                          if( a( k+1, k )==zero ) then
                             if( select( k ) )m = m + 1_ilp64
                          else
                             pair = .true.
                             if( select( k ) .or. select( k+1 ) )m = m + 2_ilp64
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp64
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( n==0_ilp64 ) then
                 lwmin = 1_ilp64
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp64*n*( n + 2_ilp64 ) + 16_ilp64
              else
                 lwmin = n
              end if
              work( 1_ilp64 ) = lwmin
              if( mm<m ) then
                 info = -15_ilp64
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_I64_slamch( 'P' )
           smlnum = stdlib_I64_slamch( 'S' ) / eps
           ks = 0_ilp64
           pair = .false.
           loop_20: do k = 1, n
              ! determine whether a(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_20
              else
                 if( k<n )pair = a( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_20
                 else
                    if( .not.select( k ) )cycle loop_20
                 end if
              end if
              ks = ks + 1_ilp64
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( pair ) then
                    ! complex eigenvalue pair.
                    rnrm = stdlib_I64_slapy2( stdlib_I64_snrm2( n, vr( 1_ilp64, ks ), 1_ilp64 ),stdlib_I64_snrm2( n, vr( &
                              1_ilp64, ks+1 ), 1_ilp64 ) )
                    lnrm = stdlib_I64_slapy2( stdlib_I64_snrm2( n, vl( 1_ilp64, ks ), 1_ilp64 ),stdlib_I64_snrm2( n, vl( &
                              1_ilp64, ks+1 ), 1_ilp64 ) )
                    call stdlib_I64_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    tmprr = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    tmpri = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    call stdlib_I64_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks+1 ), 1_ilp64,zero, work, 1_ilp64 )
                              
                    tmpii = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    tmpir = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    uhav = tmprr + tmpii
                    uhavi = tmpir - tmpri
                    call stdlib_I64_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    tmprr = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    tmpri = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    call stdlib_I64_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks+1 ), 1_ilp64,zero, work, 1_ilp64 )
                              
                    tmpii = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    tmpir = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    uhbv = tmprr + tmpii
                    uhbvi = tmpir - tmpri
                    uhav = stdlib_I64_slapy2( uhav, uhavi )
                    uhbv = stdlib_I64_slapy2( uhbv, uhbvi )
                    cond = stdlib_I64_slapy2( uhav, uhbv )
                    s( ks ) = cond / ( rnrm*lnrm )
                    s( ks+1 ) = s( ks )
                 else
                    ! real eigenvalue.
                    rnrm = stdlib_I64_snrm2( n, vr( 1_ilp64, ks ), 1_ilp64 )
                    lnrm = stdlib_I64_snrm2( n, vl( 1_ilp64, ks ), 1_ilp64 )
                    call stdlib_I64_sgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    uhav = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    call stdlib_I64_sgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    uhbv = stdlib_I64_sdot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    cond = stdlib_I64_slapy2( uhav, uhbv )
                    if( cond==zero ) then
                       s( ks ) = -one
                    else
                       s( ks ) = cond / ( rnrm*lnrm )
                    end if
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp64 ) then
                    dif( ks ) = stdlib_I64_slapy2( a( 1_ilp64, 1_ilp64 ), b( 1_ilp64, 1_ilp64 ) )
                    cycle loop_20
                 end if
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvectors.
                 if( pair ) then
                    ! copy the  2-by 2 pencil beginning at (a(k,k), b(k, k)).
                    ! compute the eigenvalue(s) at position k.
                    work( 1_ilp64 ) = a( k, k )
                    work( 2_ilp64 ) = a( k+1, k )
                    work( 3_ilp64 ) = a( k, k+1 )
                    work( 4_ilp64 ) = a( k+1, k+1 )
                    work( 5_ilp64 ) = b( k, k )
                    work( 6_ilp64 ) = b( k+1, k )
                    work( 7_ilp64 ) = b( k, k+1 )
                    work( 8_ilp64 ) = b( k+1, k+1 )
                    call stdlib_I64_slag2( work, 2_ilp64, work( 5_ilp64 ), 2_ilp64, smlnum*eps, beta,dummy1( 1_ilp64 ), &
                              alphar, dummy( 1_ilp64 ), alphai )
                    alprqt = one
                    c1 = two*( alphar*alphar+alphai*alphai+beta*beta )
                    c2 = four*beta*beta*alphai*alphai
                    root1 = c1 + sqrt( c1*c1-4.0_sp*c2 )
                    root2 = c2 / root1
                    root1 = root1 / two
                    cond = min( sqrt( root1 ), sqrt( root2 ) )
                 end if
                 ! copy the matrix (a, b) to the array work and swap the
                 ! diagonal block beginning at a(k,k) to the (1,1) position.
                 call stdlib_I64_slacpy( 'FULL', n, n, a, lda, work, n )
                 call stdlib_I64_slacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                 ifst = k
                 ilst = 1_ilp64
                 call stdlib_I64_stgexc( .false., .false., n, work, n, work( n*n+1 ), n,dummy, 1_ilp64, &
                           dummy1, 1_ilp64, ifst, ilst,work( n*n*2_ilp64+1 ), lwork-2*n*n, ierr )
                 if( ierr>0_ilp64 ) then
                    ! ill-conditioned problem - swap rejected.
                    dif( ks ) = zero
                 else
                    ! reordering successful, solve generalized sylvester
                    ! equation for r and l,
                               ! a22 * r - l * a11 = a12
                               ! b22 * r - l * b11 = b12,
                    ! and compute estimate of difl((a11,b11), (a22, b22)).
                    n1 = 1_ilp64
                    if( work( 2_ilp64 )/=zero )n1 = 2_ilp64
                    n2 = n - n1
                    if( n2==0_ilp64 ) then
                       dif( ks ) = cond
                    else
                       i = n*n + 1_ilp64
                       iz = 2_ilp64*n*n + 1_ilp64
                       call stdlib_I64_stgsyl( 'N', difdri, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ),work( iz+1 ), lwork-2*n*n, iwork, ierr )
                       if( pair )dif( ks ) = min( max( one, alprqt )*dif( ks ),cond )
                    end if
                 end if
                 if( pair )dif( ks+1 ) = dif( ks )
              end if
              if( pair )ks = ks + 1_ilp64
           end do loop_20
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_stgsna

     pure module subroutine stdlib_I64_dtgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! DTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B) in
     !! generalized real Schur canonical form (or of any matrix pair
     !! (Q*A*Z**T, Q*B*Z**T) with orthogonal matrices Q and Z, where
     !! Z**T denotes the transpose of Z.
     !! (A, B) must be in generalized real Schur form (as returned by DGGES),
     !! i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
     !! blocks. B is upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: dif(*), s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: difdri = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, somcon, wantbh, wantdf, wants
           integer(ilp64) :: i, ierr, ifst, ilst, iz, k, ks, lwmin, n1, n2
           real(dp) :: alphai, alphar, alprqt, beta, c1, c2, cond, eps, lnrm, rnrm, root1, root2, &
                     scale, smlnum, tmpii, tmpir, tmpri, tmprr, uhav, uhavi, uhbv, uhbvi
           ! Local Arrays 
           real(dp) :: dummy(1_ilp64), dummy1(1_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( wants .and. ldvl<n ) then
              info = -10_ilp64
           else if( wants .and. ldvr<n ) then
              info = -12_ilp64
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp64
                 pair = .false.
                 do k = 1, n
                    if( pair ) then
                       pair = .false.
                    else
                       if( k<n ) then
                          if( a( k+1, k )==zero ) then
                             if( select( k ) )m = m + 1_ilp64
                          else
                             pair = .true.
                             if( select( k ) .or. select( k+1 ) )m = m + 2_ilp64
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp64
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( n==0_ilp64 ) then
                 lwmin = 1_ilp64
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp64*n*( n + 2_ilp64 ) + 16_ilp64
              else
                 lwmin = n
              end if
              work( 1_ilp64 ) = lwmin
              if( mm<m ) then
                 info = -15_ilp64
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_I64_dlamch( 'P' )
           smlnum = stdlib_I64_dlamch( 'S' ) / eps
           ks = 0_ilp64
           pair = .false.
           loop_20: do k = 1, n
              ! determine whether a(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_20
              else
                 if( k<n )pair = a( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_20
                 else
                    if( .not.select( k ) )cycle loop_20
                 end if
              end if
              ks = ks + 1_ilp64
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( pair ) then
                    ! complex eigenvalue pair.
                    rnrm = stdlib_I64_dlapy2( stdlib_I64_dnrm2( n, vr( 1_ilp64, ks ), 1_ilp64 ),stdlib_I64_dnrm2( n, vr( &
                              1_ilp64, ks+1 ), 1_ilp64 ) )
                    lnrm = stdlib_I64_dlapy2( stdlib_I64_dnrm2( n, vl( 1_ilp64, ks ), 1_ilp64 ),stdlib_I64_dnrm2( n, vl( &
                              1_ilp64, ks+1 ), 1_ilp64 ) )
                    call stdlib_I64_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    tmprr = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    tmpri = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    call stdlib_I64_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks+1 ), 1_ilp64,zero, work, 1_ilp64 )
                              
                    tmpii = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    tmpir = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    uhav = tmprr + tmpii
                    uhavi = tmpir - tmpri
                    call stdlib_I64_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    tmprr = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    tmpri = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    call stdlib_I64_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks+1 ), 1_ilp64,zero, work, 1_ilp64 )
                              
                    tmpii = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks+1 ), 1_ilp64 )
                    tmpir = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    uhbv = tmprr + tmpii
                    uhbvi = tmpir - tmpri
                    uhav = stdlib_I64_dlapy2( uhav, uhavi )
                    uhbv = stdlib_I64_dlapy2( uhbv, uhbvi )
                    cond = stdlib_I64_dlapy2( uhav, uhbv )
                    s( ks ) = cond / ( rnrm*lnrm )
                    s( ks+1 ) = s( ks )
                 else
                    ! real eigenvalue.
                    rnrm = stdlib_I64_dnrm2( n, vr( 1_ilp64, ks ), 1_ilp64 )
                    lnrm = stdlib_I64_dnrm2( n, vl( 1_ilp64, ks ), 1_ilp64 )
                    call stdlib_I64_dgemv( 'N', n, n, one, a, lda, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    uhav = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    call stdlib_I64_dgemv( 'N', n, n, one, b, ldb, vr( 1_ilp64, ks ), 1_ilp64, zero,work, 1_ilp64 )
                              
                    uhbv = stdlib_I64_ddot( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                    cond = stdlib_I64_dlapy2( uhav, uhbv )
                    if( cond==zero ) then
                       s( ks ) = -one
                    else
                       s( ks ) = cond / ( rnrm*lnrm )
                    end if
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp64 ) then
                    dif( ks ) = stdlib_I64_dlapy2( a( 1_ilp64, 1_ilp64 ), b( 1_ilp64, 1_ilp64 ) )
                    cycle loop_20
                 end if
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvectors.
                 if( pair ) then
                    ! copy the  2-by 2 pencil beginning at (a(k,k), b(k, k)).
                    ! compute the eigenvalue(s) at position k.
                    work( 1_ilp64 ) = a( k, k )
                    work( 2_ilp64 ) = a( k+1, k )
                    work( 3_ilp64 ) = a( k, k+1 )
                    work( 4_ilp64 ) = a( k+1, k+1 )
                    work( 5_ilp64 ) = b( k, k )
                    work( 6_ilp64 ) = b( k+1, k )
                    work( 7_ilp64 ) = b( k, k+1 )
                    work( 8_ilp64 ) = b( k+1, k+1 )
                    call stdlib_I64_dlag2( work, 2_ilp64, work( 5_ilp64 ), 2_ilp64, smlnum*eps, beta,dummy1( 1_ilp64 ), &
                              alphar, dummy( 1_ilp64 ), alphai )
                    alprqt = one
                    c1 = two*( alphar*alphar+alphai*alphai+beta*beta )
                    c2 = four*beta*beta*alphai*alphai
                    root1 = c1 + sqrt( c1*c1-4.0_dp*c2 )
                    root2 = c2 / root1
                    root1 = root1 / two
                    cond = min( sqrt( root1 ), sqrt( root2 ) )
                 end if
                 ! copy the matrix (a, b) to the array work and swap the
                 ! diagonal block beginning at a(k,k) to the (1,1) position.
                 call stdlib_I64_dlacpy( 'FULL', n, n, a, lda, work, n )
                 call stdlib_I64_dlacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                 ifst = k
                 ilst = 1_ilp64
                 call stdlib_I64_dtgexc( .false., .false., n, work, n, work( n*n+1 ), n,dummy, 1_ilp64, &
                           dummy1, 1_ilp64, ifst, ilst,work( n*n*2_ilp64+1 ), lwork-2*n*n, ierr )
                 if( ierr>0_ilp64 ) then
                    ! ill-conditioned problem - swap rejected.
                    dif( ks ) = zero
                 else
                    ! reordering successful, solve generalized sylvester
                    ! equation for r and l,
                               ! a22 * r - l * a11 = a12
                               ! b22 * r - l * b11 = b12,
                    ! and compute estimate of difl((a11,b11), (a22, b22)).
                    n1 = 1_ilp64
                    if( work( 2_ilp64 )/=zero )n1 = 2_ilp64
                    n2 = n - n1
                    if( n2==0_ilp64 ) then
                       dif( ks ) = cond
                    else
                       i = n*n + 1_ilp64
                       iz = 2_ilp64*n*n + 1_ilp64
                       call stdlib_I64_dtgsyl( 'N', difdri, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ),work( iz+1 ), lwork-2*n*n, iwork, ierr )
                       if( pair )dif( ks ) = min( max( one, alprqt )*dif( ks ),cond )
                    end if
                 end if
                 if( pair )dif( ks+1 ) = dif( ks )
              end if
              if( pair )ks = ks + 1_ilp64
           end do loop_20
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_dtgsna


     pure module subroutine stdlib_I64_ctgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! CTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B).
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*), s(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, somcon, wantbh, wantdf, wants
           integer(ilp64) :: i, ierr, ifst, ilst, k, ks, lwmin, n1, n2
           real(sp) :: bignum, cond, eps, lnrm, rnrm, scale, smlnum
           complex(sp) :: yhax, yhbx
           ! Local Arrays 
           complex(sp) :: dummy(1_ilp64), dummy1(1_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( wants .and. ldvl<n ) then
              info = -10_ilp64
           else if( wants .and. ldvr<n ) then
              info = -12_ilp64
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp64
                 do k = 1, n
                    if( select( k ) )m = m + 1_ilp64
                 end do
              else
                 m = n
              end if
              if( n==0_ilp64 ) then
                 lwmin = 1_ilp64
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp64*n*n
              else
                 lwmin = n
              end if
              work( 1_ilp64 ) = lwmin
              if( mm<m ) then
                 info = -15_ilp64
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_I64_slamch( 'P' )
           smlnum = stdlib_I64_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_I64_slabad( smlnum, bignum )
           ks = 0_ilp64
           loop_20: do k = 1, n
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_20
              end if
              ks = ks + 1_ilp64
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 rnrm = stdlib_I64_scnrm2( n, vr( 1_ilp64, ks ), 1_ilp64 )
                 lnrm = stdlib_I64_scnrm2( n, vl( 1_ilp64, ks ), 1_ilp64 )
                 call stdlib_I64_cgemv( 'N', n, n, cmplx( one, zero,KIND=sp), a, lda,vr( 1_ilp64, ks ), 1_ilp64, &
                           cmplx( zero, zero,KIND=sp), work, 1_ilp64 )
                 yhax = stdlib_I64_cdotc( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                 call stdlib_I64_cgemv( 'N', n, n, cmplx( one, zero,KIND=sp), b, ldb,vr( 1_ilp64, ks ), 1_ilp64, &
                           cmplx( zero, zero,KIND=sp), work, 1_ilp64 )
                 yhbx = stdlib_I64_cdotc( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                 cond = stdlib_I64_slapy2( abs( yhax ), abs( yhbx ) )
                 if( cond==zero ) then
                    s( ks ) = -one
                 else
                    s( ks ) = cond / ( rnrm*lnrm )
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp64 ) then
                    dif( ks ) = stdlib_I64_slapy2( abs( a( 1_ilp64, 1_ilp64 ) ), abs( b( 1_ilp64, 1_ilp64 ) ) )
                 else
                    ! estimate the reciprocal condition number of the k-th
                    ! eigenvectors.
                    ! copy the matrix (a, b) to the array work and move the
                    ! (k,k)th pair to the (1,1) position.
                    call stdlib_I64_clacpy( 'FULL', n, n, a, lda, work, n )
                    call stdlib_I64_clacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                    ifst = k
                    ilst = 1_ilp64
                    call stdlib_I64_ctgexc( .false., .false., n, work, n, work( n*n+1 ),n, dummy, 1_ilp64, &
                              dummy1, 1_ilp64, ifst, ilst, ierr )
                    if( ierr>0_ilp64 ) then
                       ! ill-conditioned problem - swap rejected.
                       dif( ks ) = zero
                    else
                       ! reordering successful, solve generalized sylvester
                       ! equation for r and l,
                                  ! a22 * r - l * a11 = a12
                                  ! b22 * r - l * b11 = b12,
                       ! and compute estimate of difl[(a11,b11), (a22, b22)].
                       n1 = 1_ilp64
                       n2 = n - n1
                       i = n*n + 1_ilp64
                       call stdlib_I64_ctgsyl( 'N', idifjb, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ), dummy,1_ilp64, iwork, ierr )
                    end if
                 end if
              end if
           end do loop_20
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ctgsna

     pure module subroutine stdlib_I64_ztgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
     !! ZTGSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or eigenvectors of a matrix pair (A, B).
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
               dif, mm, m, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*), s(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: idifjb = 3_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: lquery, somcon, wantbh, wantdf, wants
           integer(ilp64) :: i, ierr, ifst, ilst, k, ks, lwmin, n1, n2
           real(dp) :: bignum, cond, eps, lnrm, rnrm, scale, smlnum
           complex(dp) :: yhax, yhbx
           ! Local Arrays 
           complex(dp) :: dummy(1_ilp64), dummy1(1_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantdf = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( .not.wants .and. .not.wantdf ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( wants .and. ldvl<n ) then
              info = -10_ilp64
           else if( wants .and. ldvr<n ) then
              info = -12_ilp64
           else
              ! set m to the number of eigenpairs for which condition numbers
              ! are required, and test mm.
              if( somcon ) then
                 m = 0_ilp64
                 do k = 1, n
                    if( select( k ) )m = m + 1_ilp64
                 end do
              else
                 m = n
              end if
              if( n==0_ilp64 ) then
                 lwmin = 1_ilp64
              else if( stdlib_lsame( job, 'V' ) .or. stdlib_lsame( job, 'B' ) ) then
                 lwmin = 2_ilp64*n*n
              else
                 lwmin = n
              end if
              work( 1_ilp64 ) = lwmin
              if( mm<m ) then
                 info = -15_ilp64
              else if( lwork<lwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGSNA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get machine constants
           eps = stdlib_I64_dlamch( 'P' )
           smlnum = stdlib_I64_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_I64_dlabad( smlnum, bignum )
           ks = 0_ilp64
           loop_20: do k = 1, n
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_20
              end if
              ks = ks + 1_ilp64
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 rnrm = stdlib_I64_dznrm2( n, vr( 1_ilp64, ks ), 1_ilp64 )
                 lnrm = stdlib_I64_dznrm2( n, vl( 1_ilp64, ks ), 1_ilp64 )
                 call stdlib_I64_zgemv( 'N', n, n, cmplx( one, zero,KIND=dp), a, lda,vr( 1_ilp64, ks ), 1_ilp64, &
                           cmplx( zero, zero,KIND=dp), work, 1_ilp64 )
                 yhax = stdlib_I64_zdotc( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                 call stdlib_I64_zgemv( 'N', n, n, cmplx( one, zero,KIND=dp), b, ldb,vr( 1_ilp64, ks ), 1_ilp64, &
                           cmplx( zero, zero,KIND=dp), work, 1_ilp64 )
                 yhbx = stdlib_I64_zdotc( n, work, 1_ilp64, vl( 1_ilp64, ks ), 1_ilp64 )
                 cond = stdlib_I64_dlapy2( abs( yhax ), abs( yhbx ) )
                 if( cond==zero ) then
                    s( ks ) = -one
                 else
                    s( ks ) = cond / ( rnrm*lnrm )
                 end if
              end if
              if( wantdf ) then
                 if( n==1_ilp64 ) then
                    dif( ks ) = stdlib_I64_dlapy2( abs( a( 1_ilp64, 1_ilp64 ) ), abs( b( 1_ilp64, 1_ilp64 ) ) )
                 else
                    ! estimate the reciprocal condition number of the k-th
                    ! eigenvectors.
                    ! copy the matrix (a, b) to the array work and move the
                    ! (k,k)th pair to the (1,1) position.
                    call stdlib_I64_zlacpy( 'FULL', n, n, a, lda, work, n )
                    call stdlib_I64_zlacpy( 'FULL', n, n, b, ldb, work( n*n+1 ), n )
                    ifst = k
                    ilst = 1_ilp64
                    call stdlib_I64_ztgexc( .false., .false., n, work, n, work( n*n+1 ),n, dummy, 1_ilp64, &
                              dummy1, 1_ilp64, ifst, ilst, ierr )
                    if( ierr>0_ilp64 ) then
                       ! ill-conditioned problem - swap rejected.
                       dif( ks ) = zero
                    else
                       ! reordering successful, solve generalized sylvester
                       ! equation for r and l,
                                  ! a22 * r - l * a11 = a12
                                  ! b22 * r - l * b11 = b12,
                       ! and compute estimate of difl[(a11,b11), (a22, b22)].
                       n1 = 1_ilp64
                       n2 = n - n1
                       i = n*n + 1_ilp64
                       call stdlib_I64_ztgsyl( 'N', idifjb, n2, n1, work( n*n1+n1+1 ),n, work, n, &
                       work( n1+1 ), n,work( n*n1+n1+i ), n, work( i ), n,work( n1+i ), n, scale, &
                                 dif( ks ), dummy,1_ilp64, iwork, ierr )
                    end if
                 end if
              end if
           end do loop_20
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ztgsna




     pure module subroutine stdlib_I64_stgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! STGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                 (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with real entries. (A, D) and (B, E) must be in
     !! generalized (real) Schur canonical form, i.e. A, B are upper quasi
     !! triangular and D, E are upper triangular.
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve  Zx = scale b, where
     !! Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]         (2)
     !! [ kron(In, D)  -kron(E**T, Im) ].
     !! Here Ik is the identity matrix of size k and X**T is the transpose of
     !! X. kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'T', STGSYL solves the transposed system Z**T*y = scale*b,
     !! which is equivalent to solve for R and L in
     !! A**T * R + D**T * L = scale * C           (3)
     !! R * B**T + L * E**T = scale * -F
     !! This case (TRANS = 'T') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using SLACON.
     !! If IJOB >= 1, STGSYL computes a Frobenius norm-based estimate
     !! of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z. See [1-2] for more
     !! information.
     !! This is a level 3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp64), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_scopy by calls to stdlib_I64_slaset.
        ! sven hammarling, 1/5/02.
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp64) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, ppqq, pq, q
           real(sp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>4_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( notran ) then
                 if( ijob==1_ilp64 .or. ijob==2_ilp64 ) then
                    lwmin = max( 1_ilp64, 2_ilp64*m*n )
                 else
                    lwmin = 1_ilp64
                 end if
              else
                 lwmin = 1_ilp64
              end if
              work( 1_ilp64 ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              scale = 1_ilp64
              if( notran ) then
                 if( ijob/=0_ilp64 ) then
                    dif = 0_ilp64
                 end if
              end if
              return
           end if
           ! determine optimal block sizes mb and nb
           mb = stdlib_I64_ilaenv( 2_ilp64, 'STGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           nb = stdlib_I64_ilaenv( 5_ilp64, 'STGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           isolve = 1_ilp64
           ifunc = 0_ilp64
           if( notran ) then
              if( ijob>=3_ilp64 ) then
                 ifunc = ijob - 2_ilp64
                 call stdlib_I64_slaset( 'F', m, n, zero, zero, c, ldc )
                 call stdlib_I64_slaset( 'F', m, n, zero, zero, f, ldf )
              else if( ijob>=1_ilp64 .and. notran ) then
                 isolve = 2_ilp64
              end if
           end if
           if( ( mb<=1_ilp64 .and. nb<=1_ilp64 ) .or. ( mb>=m .and. nb>=n ) )then
              loop_30: do iround = 1, isolve
                 ! use unblocked level 2 solver
                 dscale = zero
                 dsum = one
                 pq = 0_ilp64
                 call stdlib_I64_stgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,iwork, pq, info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_slacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_slacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_slaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_I64_slaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_slacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_slacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp64
           i = 1_ilp64
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp64
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           if( a( i, i-1 )/=zero )i = i + 1_ilp64
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp64
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp64
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           if( b( j, j-1 )/=zero )j = j + 1_ilp64
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp64
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp64
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j)-subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1,..., 1; j = 1, 2,..., q
                 dscale = zero
                 dsum = one
                 pq = 0_ilp64
                 scale = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp64
                       mb = ie - is + 1_ilp64
                       ppqq = 0_ilp64
                       call stdlib_I64_stgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,iwork( q+2 ), ppqq, linfo )
                       if( linfo>0_ilp64 )info = linfo
                       pq = pq + ppqq
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          do k = js, je
                             call stdlib_I64_sscal( is-1, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( is-1, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          do k = js, je
                             call stdlib_I64_sscal( m-ie, scaloc, c( ie+1, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m-ie, scaloc, f( ie+1, k ), 1_ilp64 )
                          end do
                          do k = je + 1, n
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_sgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp64, is ), lda, c( is, &
                                    js ), ldc, one,c( 1_ilp64, js ), ldc )
                          call stdlib_I64_sgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp64, is ), ldd, c( is, &
                                    js ), ldc, one,f( 1_ilp64, js ), ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_sgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, b( js, &
                                    je+1 ), ldb,one, c( is, je+1 ), ldc )
                          call stdlib_I64_sgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, e( js, &
                                    je+1 ), lde,one, f( is, je+1 ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_slacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_slacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_slaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_I64_slaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_slacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_slacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                   ! a(i, i)**t * r(i, j)  + d(i, i)**t * l(i, j)  =  c(i, j)
                   ! r(i, j)  * b(j, j)**t + l(i, j)  * e(j, j)**t = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    call stdlib_I64_stgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,iwork( q+2 ), ppqq, linfo )
                    if( linfo>0_ilp64 )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       do k = js, je
                          call stdlib_I64_sscal( is-1, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_sscal( is-1, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       do k = js, je
                          call stdlib_I64_sscal( m-ie, scaloc, c( ie+1, k ), 1_ilp64 )
                          call stdlib_I64_sscal( m-ie, scaloc, f( ie+1, k ), 1_ilp64 )
                       end do
                       do k = je + 1, n
                          call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_I64_sgemm( 'N', 'T', mb, js-1, nb, one, c( is, js ),ldc, b( 1_ilp64, js )&
                                 , ldb, one, f( is, 1_ilp64 ),ldf )
                       call stdlib_I64_sgemm( 'N', 'T', mb, js-1, nb, one, f( is, js ),ldf, e( 1_ilp64, js )&
                                 , lde, one, f( is, 1_ilp64 ),ldf )
                    end if
                    if( i<p ) then
                       call stdlib_I64_sgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( is, &
                                 js ), ldc, one,c( ie+1, js ), ldc )
                       call stdlib_I64_sgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( is, &
                                 js ), ldf, one,c( ie+1, js ), ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_stgsyl

     pure module subroutine stdlib_I64_dtgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! DTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                 (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with real entries. (A, D) and (B, E) must be in
     !! generalized (real) Schur canonical form, i.e. A, B are upper quasi
     !! triangular and D, E are upper triangular.
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve  Zx = scale b, where
     !! Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]         (2)
     !! [ kron(In, D)  -kron(E**T, Im) ].
     !! Here Ik is the identity matrix of size k and X**T is the transpose of
     !! X. kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'T', DTGSYL solves the transposed system Z**T*y = scale*b,
     !! which is equivalent to solve for R and L in
     !! A**T * R + D**T * L = scale * C           (3)
     !! R * B**T + L * E**T = scale * -F
     !! This case (TRANS = 'T') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using DLACON.
     !! If IJOB >= 1, DTGSYL computes a Frobenius norm-based estimate
     !! of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z. See [1-2] for more
     !! information.
     !! This is a level 3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp64), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_dcopy by calls to stdlib_I64_dlaset.
        ! sven hammarling, 1/5/02.
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp64) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, ppqq, pq, q
           real(dp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>4_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( notran ) then
                 if( ijob==1_ilp64 .or. ijob==2_ilp64 ) then
                    lwmin = max( 1_ilp64, 2_ilp64*m*n )
                 else
                    lwmin = 1_ilp64
                 end if
              else
                 lwmin = 1_ilp64
              end if
              work( 1_ilp64 ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              scale = 1_ilp64
              if( notran ) then
                 if( ijob/=0_ilp64 ) then
                    dif = 0_ilp64
                 end if
              end if
              return
           end if
           ! determine optimal block sizes mb and nb
           mb = stdlib_I64_ilaenv( 2_ilp64, 'DTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           nb = stdlib_I64_ilaenv( 5_ilp64, 'DTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           isolve = 1_ilp64
           ifunc = 0_ilp64
           if( notran ) then
              if( ijob>=3_ilp64 ) then
                 ifunc = ijob - 2_ilp64
                 call stdlib_I64_dlaset( 'F', m, n, zero, zero, c, ldc )
                 call stdlib_I64_dlaset( 'F', m, n, zero, zero, f, ldf )
              else if( ijob>=1_ilp64 ) then
                 isolve = 2_ilp64
              end if
           end if
           if( ( mb<=1_ilp64 .and. nb<=1_ilp64 ) .or. ( mb>=m .and. nb>=n ) )then
              loop_30: do iround = 1, isolve
                 ! use unblocked level 2 solver
                 dscale = zero
                 dsum = one
                 pq = 0_ilp64
                 call stdlib_I64_dtgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,iwork, pq, info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_dlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_dlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_dlaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_I64_dlaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_dlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_dlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp64
           i = 1_ilp64
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp64
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           if( a( i, i-1 )/=zero )i = i + 1_ilp64
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp64
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp64
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           if( b( j, j-1 )/=zero )j = j + 1_ilp64
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp64
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp64
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j)-subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1,..., 1; j = 1, 2,..., q
                 dscale = zero
                 dsum = one
                 pq = 0_ilp64
                 scale = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp64
                       mb = ie - is + 1_ilp64
                       ppqq = 0_ilp64
                       call stdlib_I64_dtgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,iwork( q+2 ), ppqq, linfo )
                       if( linfo>0_ilp64 )info = linfo
                       pq = pq + ppqq
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          do k = js, je
                             call stdlib_I64_dscal( is-1, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( is-1, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          do k = js, je
                             call stdlib_I64_dscal( m-ie, scaloc, c( ie+1, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m-ie, scaloc, f( ie+1, k ), 1_ilp64 )
                          end do
                          do k = je + 1, n
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_dgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp64, is ), lda, c( is, &
                                    js ), ldc, one,c( 1_ilp64, js ), ldc )
                          call stdlib_I64_dgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp64, is ), ldd, c( is, &
                                    js ), ldc, one,f( 1_ilp64, js ), ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_dgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, b( js, &
                                    je+1 ), ldb,one, c( is, je+1 ), ldc )
                          call stdlib_I64_dgemm( 'N', 'N', mb, n-je, nb, one,f( is, js ), ldf, e( js, &
                                    je+1 ), lde,one, f( is, je+1 ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_dlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_dlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_dlaset( 'F', m, n, zero, zero, c, ldc )
                    call stdlib_I64_dlaset( 'F', m, n, zero, zero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_dlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_dlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                   ! a(i, i)**t * r(i, j)  + d(i, i)**t * l(i, j)  =  c(i, j)
                   ! r(i, j)  * b(j, j)**t + l(i, j)  * e(j, j)**t = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    call stdlib_I64_dtgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,iwork( q+2 ), ppqq, linfo )
                    if( linfo>0_ilp64 )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       do k = js, je
                          call stdlib_I64_dscal( is-1, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_dscal( is-1, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       do k = js, je
                          call stdlib_I64_dscal( m-ie, scaloc, c( ie+1, k ), 1_ilp64 )
                          call stdlib_I64_dscal( m-ie, scaloc, f( ie+1, k ), 1_ilp64 )
                       end do
                       do k = je + 1, n
                          call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                          call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_I64_dgemm( 'N', 'T', mb, js-1, nb, one, c( is, js ),ldc, b( 1_ilp64, js )&
                                 , ldb, one, f( is, 1_ilp64 ),ldf )
                       call stdlib_I64_dgemm( 'N', 'T', mb, js-1, nb, one, f( is, js ),ldf, e( 1_ilp64, js )&
                                 , lde, one, f( is, 1_ilp64 ),ldf )
                    end if
                    if( i<p ) then
                       call stdlib_I64_dgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( is, &
                                 js ), ldc, one,c( ie+1, js ), ldc )
                       call stdlib_I64_dgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( is, &
                                 js ), ldf, one,c( ie+1, js ), ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_dtgsyl


     pure module subroutine stdlib_I64_ctgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! CTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C            (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with complex entries. A, B, D and E are upper
     !! triangular (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
     !! is an output scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
     !! is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]        (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Here Ix is the identity matrix of size x and X**H is the conjugate
     !! transpose of X. Kron(X, Y) is the Kronecker product between the
     !! matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H *y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R + D**H * L = scale * C           (3)
     !! R * B**H + L * E**H = scale * -F
     !! This case (TRANS = 'C') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using CLACON.
     !! If IJOB >= 1, CTGSYL computes a Frobenius norm-based estimate of
     !! Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z.
     !! This is a level-3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp64), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_ccopy by calls to stdlib_I64_claset.
        ! sven hammarling, 1/5/02.
           
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp64) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, pq, q
           real(sp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>4_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( notran ) then
                 if( ijob==1_ilp64 .or. ijob==2_ilp64 ) then
                    lwmin = max( 1_ilp64, 2_ilp64*m*n )
                 else
                    lwmin = 1_ilp64
                 end if
              else
                 lwmin = 1_ilp64
              end if
              work( 1_ilp64 ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              scale = 1_ilp64
              if( notran ) then
                 if( ijob/=0_ilp64 ) then
                    dif = 0_ilp64
                 end if
              end if
              return
           end if
           ! determine  optimal block sizes mb and nb
           mb = stdlib_I64_ilaenv( 2_ilp64, 'CTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           nb = stdlib_I64_ilaenv( 5_ilp64, 'CTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           isolve = 1_ilp64
           ifunc = 0_ilp64
           if( notran ) then
              if( ijob>=3_ilp64 ) then
                 ifunc = ijob - 2_ilp64
                 call stdlib_I64_claset( 'F', m, n, czero, czero, c, ldc )
                 call stdlib_I64_claset( 'F', m, n, czero, czero, f, ldf )
              else if( ijob>=1_ilp64 .and. notran ) then
                 isolve = 2_ilp64
              end if
           end if
           if( ( mb<=1_ilp64 .and. nb<=1_ilp64 ) .or. ( mb>=m .and. nb>=n ) )then
              ! use unblocked level 2 solver
              loop_30: do iround = 1, isolve
                 scale = one
                 dscale = zero
                 dsum = one
                 pq = m*n
                 call stdlib_I64_ctgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_clacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_clacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_claset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_I64_claset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_clacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_clacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp64
           i = 1_ilp64
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp64
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp64
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp64
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp64
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp64
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j) - subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
                 pq = 0_ilp64
                 scale = one
                 dscale = zero
                 dsum = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp64
                       mb = ie - is + 1_ilp64
                       call stdlib_I64_ctgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,linfo )
                       if( linfo>0_ilp64 )info = linfo
                       pq = pq + mb*nb
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                       
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                       
                          end do
                          do k = js, je
                             call stdlib_I64_cscal( is-1, cmplx( scaloc, zero,KIND=sp),c( 1_ilp64, k ), 1_ilp64 )
                                       
                             call stdlib_I64_cscal( is-1, cmplx( scaloc, zero,KIND=sp),f( 1_ilp64, k ), 1_ilp64 )
                                       
                          end do
                          do k = js, je
                             call stdlib_I64_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),c( ie+1, k ), &
                                       1_ilp64 )
                             call stdlib_I64_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),f( ie+1, k ), &
                                       1_ilp64 )
                          end do
                          do k = je + 1, n
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                       
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i,j) and l(i,j) into remaining equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_cgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=sp), a(&
                           1_ilp64, is ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=sp),c( 1_ilp64, js ), &
                                     ldc )
                          call stdlib_I64_cgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=sp), d(&
                           1_ilp64, is ), ldd,c( is, js ), ldc, cmplx( one, zero,KIND=sp),f( 1_ilp64, js ), &
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_cgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=sp), f( &
                          is, js ), ldf,b( js, je+1 ), ldb, cmplx( one, zero,KIND=sp),c( is, je+1 &
                                    ), ldc )
                          call stdlib_I64_cgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=sp), f( &
                          is, js ), ldf,e( js, je+1 ), lde, cmplx( one, zero,KIND=sp),f( is, je+1 &
                                    ), ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=sp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_clacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_clacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_claset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_I64_claset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_clacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_clacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                  ! a(i, i)**h * r(i, j) + d(i, i)**h * l(i, j) = c(i, j)
                  ! r(i, j) * b(j, j)  + l(i, j) * e(j, j) = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    call stdlib_I64_ctgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,linfo )
                    if( linfo>0_ilp64 )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       do k = js, je
                          call stdlib_I64_cscal( is-1, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_cscal( is-1, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       do k = js, je
                          call stdlib_I64_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),c( ie+1, k ), 1_ilp64 )
                                    
                          call stdlib_I64_cscal( m-ie, cmplx( scaloc, zero,KIND=sp),f( ie+1, k ), 1_ilp64 )
                                    
                       end do
                       do k = je + 1, n
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i,j) and l(i,j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_I64_cgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=sp), c( is,&
                        js ), ldc,b( 1_ilp64, js ), ldb, cmplx( one, zero,KIND=sp),f( is, 1_ilp64 ), ldf )
                                  
                       call stdlib_I64_cgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=sp), f( is,&
                        js ), ldf,e( 1_ilp64, js ), lde, cmplx( one, zero,KIND=sp),f( is, 1_ilp64 ), ldf )
                                  
                    end if
                    if( i<p ) then
                       call stdlib_I64_cgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=sp), a( &
                       is, ie+1 ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=sp),c( ie+1, js ), &
                                 ldc )
                       call stdlib_I64_cgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=sp), d( &
                       is, ie+1 ), ldd,f( is, js ), ldf, cmplx( one, zero,KIND=sp),c( ie+1, js ), &
                                 ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ctgsyl

     pure module subroutine stdlib_I64_ztgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! ZTGSYL solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C            (1)
     !! D * R - L * E = scale * F
     !! where R and L are unknown m-by-n matrices, (A, D), (B, E) and
     !! (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
     !! respectively, with complex entries. A, B, D and E are upper
     !! triangular (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
     !! is an output scaling factor chosen to avoid overflow.
     !! In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
     !! is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]        (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Here Ix is the identity matrix of size x and X**H is the conjugate
     !! transpose of X. Kron(X, Y) is the Kronecker product between the
     !! matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H *y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R + D**H * L = scale * C           (3)
     !! R * B**H + L * E**H = scale * -F
     !! This case (TRANS = 'C') is used to compute an one-norm-based estimate
     !! of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
     !! and (B,E), using ZLACON.
     !! If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of
     !! Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
     !! reciprocal of the smallest singular value of Z.
     !! This is a level-3 BLAS algorithm.
               ldf, scale, dif, work, lwork,iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp64), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_ccopy by calls to stdlib_I64_claset.
        ! sven hammarling, 1/5/02.
           
           
           ! Local Scalars 
           logical(lk) :: lquery, notran
           integer(ilp64) :: i, ie, ifunc, iround, is, isolve, j, je, js, k, linfo, lwmin, mb, nb, &
                     p, pq, q
           real(dp) :: dscale, dsum, scale2, scaloc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>4_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( notran ) then
                 if( ijob==1_ilp64 .or. ijob==2_ilp64 ) then
                    lwmin = max( 1_ilp64, 2_ilp64*m*n )
                 else
                    lwmin = 1_ilp64
                 end if
              else
                 lwmin = 1_ilp64
              end if
              work( 1_ilp64 ) = lwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGSYL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              scale = 1_ilp64
              if( notran ) then
                 if( ijob/=0_ilp64 ) then
                    dif = 0_ilp64
                 end if
              end if
              return
           end if
           ! determine  optimal block sizes mb and nb
           mb = stdlib_I64_ilaenv( 2_ilp64, 'ZTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           nb = stdlib_I64_ilaenv( 5_ilp64, 'ZTGSYL', trans, m, n, -1_ilp64, -1_ilp64 )
           isolve = 1_ilp64
           ifunc = 0_ilp64
           if( notran ) then
              if( ijob>=3_ilp64 ) then
                 ifunc = ijob - 2_ilp64
                 call stdlib_I64_zlaset( 'F', m, n, czero, czero, c, ldc )
                 call stdlib_I64_zlaset( 'F', m, n, czero, czero, f, ldf )
              else if( ijob>=1_ilp64 .and. notran ) then
                 isolve = 2_ilp64
              end if
           end if
           if( ( mb<=1_ilp64 .and. nb<=1_ilp64 ) .or. ( mb>=m .and. nb>=n ) )then
              ! use unblocked level 2 solver
              loop_30: do iround = 1, isolve
                 scale = one
                 dscale = zero
                 dsum = one
                 pq = m*n
                 call stdlib_I64_ztgsy2( trans, ifunc, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f,&
                            ldf, scale, dsum, dscale,info )
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_zlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_zlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_zlaset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_I64_zlaset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_zlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_zlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_30
              return
           end if
           ! determine block structure of a
           p = 0_ilp64
           i = 1_ilp64
           40 continue
           if( i>m )go to 50
           p = p + 1_ilp64
           iwork( p ) = i
           i = i + mb
           if( i>=m )go to 50
           go to 40
           50 continue
           iwork( p+1 ) = m + 1_ilp64
           if( iwork( p )==iwork( p+1 ) )p = p - 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           60 continue
           if( j>n )go to 70
           q = q + 1_ilp64
           iwork( q ) = j
           j = j + nb
           if( j>=n )go to 70
           go to 60
           70 continue
           iwork( q+1 ) = n + 1_ilp64
           if( iwork( q )==iwork( q+1 ) )q = q - 1_ilp64
           if( notran ) then
              loop_150: do iround = 1, isolve
                 ! solve (i, j) - subsystem
                     ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                     ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
                 ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
                 pq = 0_ilp64
                 scale = one
                 dscale = zero
                 dsum = one
                 loop_130: do j = p + 2, q
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    loop_120: do i = p, 1, -1
                       is = iwork( i )
                       ie = iwork( i+1 ) - 1_ilp64
                       mb = ie - is + 1_ilp64
                       call stdlib_I64_ztgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), &
                       ldb, c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, &
                                 scaloc, dsum, dscale,linfo )
                       if( linfo>0_ilp64 )info = linfo
                       pq = pq + mb*nb
                       if( scaloc/=one ) then
                          do k = 1, js - 1
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp64, k ), 1_ilp64 )
                                       
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp64, k ), 1_ilp64 )
                                       
                          end do
                          do k = js, je
                             call stdlib_I64_zscal( is-1, cmplx( scaloc, zero,KIND=dp),c( 1_ilp64, k ), 1_ilp64 )
                                       
                             call stdlib_I64_zscal( is-1, cmplx( scaloc, zero,KIND=dp),f( 1_ilp64, k ), 1_ilp64 )
                                       
                          end do
                          do k = js, je
                             call stdlib_I64_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),c( ie+1, k ), &
                                       1_ilp64 )
                             call stdlib_I64_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),f( ie+1, k ), &
                                       1_ilp64 )
                          end do
                          do k = je + 1, n
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp64, k ), 1_ilp64 )
                                       
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp64, k ), 1_ilp64 )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                       ! substitute r(i,j) and l(i,j) into remaining equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_zgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=dp), a(&
                           1_ilp64, is ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=dp),c( 1_ilp64, js ), &
                                     ldc )
                          call stdlib_I64_zgemm( 'N', 'N', is-1, nb, mb,cmplx( -one, zero,KIND=dp), d(&
                           1_ilp64, is ), ldd,c( is, js ), ldc, cmplx( one, zero,KIND=dp),f( 1_ilp64, js ), &
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_zgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=dp), f( &
                          is, js ), ldf,b( js, je+1 ), ldb,cmplx( one, zero,KIND=dp), c( is, je+1 &
                                    ),ldc )
                          call stdlib_I64_zgemm( 'N', 'N', mb, n-je, nb,cmplx( one, zero,KIND=dp), f( &
                          is, js ), ldf,e( js, je+1 ), lde,cmplx( one, zero,KIND=dp), f( is, je+1 &
                                    ),ldf )
                       end if
                    end do loop_120
                 end do loop_130
                 if( dscale/=zero ) then
                    if( ijob==1_ilp64 .or. ijob==3_ilp64 ) then
                       dif = sqrt( real( 2_ilp64*m*n,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    else
                       dif = sqrt( real( pq,KIND=dp) ) / ( dscale*sqrt( dsum ) )
                    end if
                 end if
                 if( isolve==2_ilp64 .and. iround==1_ilp64 ) then
                    if( notran ) then
                       ifunc = ijob
                    end if
                    scale2 = scale
                    call stdlib_I64_zlacpy( 'F', m, n, c, ldc, work, m )
                    call stdlib_I64_zlacpy( 'F', m, n, f, ldf, work( m*n+1 ), m )
                    call stdlib_I64_zlaset( 'F', m, n, czero, czero, c, ldc )
                    call stdlib_I64_zlaset( 'F', m, n, czero, czero, f, ldf )
                 else if( isolve==2_ilp64 .and. iround==2_ilp64 ) then
                    call stdlib_I64_zlacpy( 'F', m, n, work, m, c, ldc )
                    call stdlib_I64_zlacpy( 'F', m, n, work( m*n+1 ), m, f, ldf )
                    scale = scale2
                 end if
              end do loop_150
           else
              ! solve transposed (i, j)-subsystem
                  ! a(i, i)**h * r(i, j) + d(i, i)**h * l(i, j) = c(i, j)
                  ! r(i, j) * b(j, j)  + l(i, j) * e(j, j) = -f(i, j)
              ! for i = 1,2,..., p; j = q, q-1,..., 1
              scale = one
              loop_210: do i = 1, p
                 is = iwork( i )
                 ie = iwork( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_200: do j = q, p + 2, -1
                    js = iwork( j )
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    call stdlib_I64_ztgsy2( trans, ifunc, mb, nb, a( is, is ), lda,b( js, js ), ldb, &
                    c( is, js ), ldc,d( is, is ), ldd, e( js, js ), lde,f( is, js ), ldf, scaloc, &
                              dsum, dscale,linfo )
                    if( linfo>0_ilp64 )info = linfo
                    if( scaloc/=one ) then
                       do k = 1, js - 1
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       do k = js, je
                          call stdlib_I64_zscal( is-1, cmplx( scaloc, zero,KIND=dp),c( 1_ilp64, k ), 1_ilp64 )
                                    
                          call stdlib_I64_zscal( is-1, cmplx( scaloc, zero,KIND=dp),f( 1_ilp64, k ), 1_ilp64 )
                                    
                       end do
                       do k = js, je
                          call stdlib_I64_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),c( ie+1, k ), 1_ilp64 )
                                    
                          call stdlib_I64_zscal( m-ie, cmplx( scaloc, zero,KIND=dp),f( ie+1, k ), 1_ilp64 )
                                    
                       end do
                       do k = je + 1, n
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! substitute r(i,j) and l(i,j) into remaining equation.
                    if( j>p+2 ) then
                       call stdlib_I64_zgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=dp), c( is,&
                        js ), ldc,b( 1_ilp64, js ), ldb, cmplx( one, zero,KIND=dp),f( is, 1_ilp64 ), ldf )
                                  
                       call stdlib_I64_zgemm( 'N', 'C', mb, js-1, nb,cmplx( one, zero,KIND=dp), f( is,&
                        js ), ldf,e( 1_ilp64, js ), lde, cmplx( one, zero,KIND=dp),f( is, 1_ilp64 ), ldf )
                                  
                    end if
                    if( i<p ) then
                       call stdlib_I64_zgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=dp), a( &
                       is, ie+1 ), lda,c( is, js ), ldc, cmplx( one, zero,KIND=dp),c( ie+1, js ), &
                                 ldc )
                       call stdlib_I64_zgemm( 'C', 'N', m-ie, nb, mb,cmplx( -one, zero,KIND=dp), d( &
                       is, ie+1 ), ldd,f( is, js ), ldf, cmplx( one, zero,KIND=dp),c( ie+1, js ), &
                                 ldc )
                    end if
                 end do loop_200
              end do loop_210
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ztgsyl




     pure module subroutine stdlib_I64_stgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! STGSY2 solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                (1)
     !! D * R - L * E = scale * F,
     !! using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)
     !! must be in generalized Schur canonical form, i.e. A, B are upper
     !! quasi triangular and D, E are upper triangular. The solution (R, L)
     !! overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor
     !! chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Z*x = scale*b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**T, Im) ],
     !! Ik is the identity matrix of size k and X**T is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! In the process of solving (1), we solve a number of such systems
     !! where Dim(In), Dim(In) = 1 or 2.
     !! If TRANS = 'T', solve the transposed system Z**T*y = scale*b for y,
     !! which is equivalent to solve for R and L in
     !! A**T * R  + D**T * L   = scale * C           (3)
     !! R  * B**T + L  * E**T  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! sigma_min(Z) using reverse communication with SLACON.
     !! STGSY2 also (IJOB >= 1) contributes to the computation in STGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of the matrix pair in
     !! STGSYL. See STGSYL for details.
               ldf, scale, rdsum, rdscal,iwork, pq, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp64), intent(out) :: info, pq
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_scopy by calls to stdlib_I64_slaset.
        ! sven hammarling, 27/5/02.
           ! Parameters 
           integer(ilp64), parameter :: ldz = 8_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp64) :: i, ie, ierr, ii, is, isp1, j, je, jj, js, jsp1, k, mb, nb, p, q, &
                     zdim
           real(sp) :: alpha, scaloc
           ! Local Arrays 
           integer(ilp64) :: ipiv(ldz), jpiv(ldz)
           real(sp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           ierr = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>2_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGSY2', -info )
              return
           end if
           ! determine block structure of a
           pq = 0_ilp64
           p = 0_ilp64
           i = 1_ilp64
           10 continue
           if( i>m )go to 20
           p = p + 1_ilp64
           iwork( p ) = i
           if( i==m )go to 20
           if( a( i+1, i )/=zero ) then
              i = i + 2_ilp64
           else
              i = i + 1_ilp64
           end if
           go to 10
           20 continue
           iwork( p+1 ) = m + 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           30 continue
           if( j>n )go to 40
           q = q + 1_ilp64
           iwork( q ) = j
           if( j==n )go to 40
           if( b( j+1, j )/=zero ) then
              j = j + 2_ilp64
           else
              j = j + 1_ilp64
           end if
           go to 30
           40 continue
           iwork( q+1 ) = n + 1_ilp64
           pq = p*( q-p-1 )
           if( notran ) then
              ! solve (i, j) - subsystem
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
              scale = one
              scaloc = one
              loop_120: do j = p + 2, q
                 js = iwork( j )
                 jsp1 = js + 1_ilp64
                 je = iwork( j+1 ) - 1_ilp64
                 nb = je - js + 1_ilp64
                 loop_110: do i = p, 1, -1
                    is = iwork( i )
                    isp1 = is + 1_ilp64
                    ie = iwork( i+1 ) - 1_ilp64
                    mb = ie - is + 1_ilp64
                    zdim = mb*nb*2_ilp64
                    if( ( mb==1_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 2-by-2 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = d( is, is )
                       z( 1_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 2_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = f( is, js )
                       ! solve z * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       f( is, js ) = rhs( 2_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          alpha = -rhs( 1_ilp64 )
                          call stdlib_I64_saxpy( is-1, alpha, a( 1_ilp64, is ), 1_ilp64, c( 1_ilp64, js ),1_ilp64 )
                          call stdlib_I64_saxpy( is-1, alpha, d( 1_ilp64, is ), 1_ilp64, f( 1_ilp64, js ),1_ilp64 )
                       end if
                       if( j<q ) then
                          call stdlib_I64_saxpy( n-je, rhs( 2_ilp64 ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_saxpy( n-je, rhs( 2_ilp64 ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==1_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = zero
                       z( 3_ilp64, 1_ilp64 ) = d( is, is )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = zero
                       z( 2_ilp64, 2_ilp64 ) = a( is, is )
                       z( 3_ilp64, 2_ilp64 ) = zero
                       z( 4_ilp64, 2_ilp64 ) = d( is, is )
                       z( 1_ilp64, 3_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 3_ilp64 ) = -b( js, jsp1 )
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = -e( js, jsp1 )
                       z( 1_ilp64, 4_ilp64 ) = -b( jsp1, js )
                       z( 2_ilp64, 4_ilp64 ) = -b( jsp1, jsp1 )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( is, jsp1 )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( is, jsp1 )
                       ! solve z * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( is, jsp1 ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( is, jsp1 ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_sger( is-1, nb, -one, a( 1_ilp64, is ), 1_ilp64, rhs( 1_ilp64 ),1_ilp64, c( 1_ilp64, js&
                              & ),&
                                     ldc )
                          call stdlib_I64_sger( is-1, nb, -one, d( 1_ilp64, is ), 1_ilp64, rhs( 1_ilp64 ),1_ilp64, f( 1_ilp64, js&
                              & ),&
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_saxpy( n-je, rhs( 3_ilp64 ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_saxpy( n-je, rhs( 3_ilp64 ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                          call stdlib_I64_saxpy( n-je, rhs( 4_ilp64 ), b( jsp1, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_saxpy( n-je, rhs( 4_ilp64 ), e( jsp1, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( isp1, is )
                       z( 3_ilp64, 1_ilp64 ) = d( is, is )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = a( is, isp1 )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 3_ilp64, 2_ilp64 ) = d( is, isp1 )
                       z( 4_ilp64, 2_ilp64 ) = d( isp1, isp1 )
                       z( 1_ilp64, 3_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 3_ilp64 ) = zero
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = -b( js, js )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( isp1, js )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( isp1, js )
                       ! solve z * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( isp1, js ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( isp1, js ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_sgemv( 'N', is-1, mb, -one, a( 1_ilp64, is ), lda,rhs( 1_ilp64 ), 1_ilp64, &
                                    one, c( 1_ilp64, js ), 1_ilp64 )
                          call stdlib_I64_sgemv( 'N', is-1, mb, -one, d( 1_ilp64, is ), ldd,rhs( 1_ilp64 ), 1_ilp64, &
                                    one, f( 1_ilp64, js ), 1_ilp64 )
                       end if
                       if( j<q ) then
                          call stdlib_I64_sger( mb, n-je, one, rhs( 3_ilp64 ), 1_ilp64,b( js, je+1 ), ldb, c( is, &
                                    je+1 ), ldc )
                          call stdlib_I64_sger( mb, n-je, one, rhs( 3_ilp64 ), 1_ilp64,e( js, je+1 ), lde, f( is, &
                                    je+1 ), ldf )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build an 8-by-8 system z * x = rhs
                       call stdlib_I64_slaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( isp1, is )
                       z( 5_ilp64, 1_ilp64 ) = d( is, is )
                       z( 1_ilp64, 2_ilp64 ) = a( is, isp1 )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 5_ilp64, 2_ilp64 ) = d( is, isp1 )
                       z( 6_ilp64, 2_ilp64 ) = d( isp1, isp1 )
                       z( 3_ilp64, 3_ilp64 ) = a( is, is )
                       z( 4_ilp64, 3_ilp64 ) = a( isp1, is )
                       z( 7_ilp64, 3_ilp64 ) = d( is, is )
                       z( 3_ilp64, 4_ilp64 ) = a( is, isp1 )
                       z( 4_ilp64, 4_ilp64 ) = a( isp1, isp1 )
                       z( 7_ilp64, 4_ilp64 ) = d( is, isp1 )
                       z( 8_ilp64, 4_ilp64 ) = d( isp1, isp1 )
                       z( 1_ilp64, 5_ilp64 ) = -b( js, js )
                       z( 3_ilp64, 5_ilp64 ) = -b( js, jsp1 )
                       z( 5_ilp64, 5_ilp64 ) = -e( js, js )
                       z( 7_ilp64, 5_ilp64 ) = -e( js, jsp1 )
                       z( 2_ilp64, 6_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 6_ilp64 ) = -b( js, jsp1 )
                       z( 6_ilp64, 6_ilp64 ) = -e( js, js )
                       z( 8_ilp64, 6_ilp64 ) = -e( js, jsp1 )
                       z( 1_ilp64, 7_ilp64 ) = -b( jsp1, js )
                       z( 3_ilp64, 7_ilp64 ) = -b( jsp1, jsp1 )
                       z( 7_ilp64, 7_ilp64 ) = -e( jsp1, jsp1 )
                       z( 2_ilp64, 8_ilp64 ) = -b( jsp1, js )
                       z( 4_ilp64, 8_ilp64 ) = -b( jsp1, jsp1 )
                       z( 8_ilp64, 8_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_scopy( mb, c( is, js+jj ), 1_ilp64, rhs( k ), 1_ilp64 )
                          call stdlib_I64_scopy( mb, f( is, js+jj ), 1_ilp64, rhs( ii ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_slatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_scopy( mb, rhs( k ), 1_ilp64, c( is, js+jj ), 1_ilp64 )
                          call stdlib_I64_scopy( mb, rhs( ii ), 1_ilp64, f( is, js+jj ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_sgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp64, is ), lda, rhs( 1_ilp64 &
                                    ), mb, one,c( 1_ilp64, js ), ldc )
                          call stdlib_I64_sgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp64, is ), ldd, rhs( 1_ilp64 &
                                    ), mb, one,f( 1_ilp64, js ), ldf )
                       end if
                       if( j<q ) then
                          k = mb*nb + 1_ilp64
                          call stdlib_I64_sgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, b( js, je+&
                                    1_ilp64 ), ldb, one,c( is, je+1 ), ldc )
                          call stdlib_I64_sgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, e( js, je+&
                                    1_ilp64 ), lde, one,f( is, je+1 ), ldf )
                       end if
                    end if
                 end do loop_110
              end do loop_120
           else
              ! solve (i, j) - subsystem
                   ! a(i, i)**t * r(i, j) + d(i, i)**t * l(j, j)  =  c(i, j)
                   ! r(i, i)  * b(j, j) + l(i, j)  * e(j, j)  = -f(i, j)
              ! for i = 1, 2, ..., p, j = q, q - 1, ..., 1
              scale = one
              scaloc = one
              loop_200: do i = 1, p
                 is = iwork( i )
                 isp1 = is + 1_ilp64
                 ie = iwork( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_190: do j = q, p + 2, -1
                    js = iwork( j )
                    jsp1 = js + 1_ilp64
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    zdim = mb*nb*2_ilp64
                    if( ( mb==1_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 2-by-2 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 1_ilp64, 2_ilp64 ) = d( is, is )
                       z( 2_ilp64, 2_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = f( is, js )
                       ! solve z**t * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       f( is, js ) = rhs( 2_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          alpha = rhs( 1_ilp64 )
                          call stdlib_I64_saxpy( js-1, alpha, b( 1_ilp64, js ), 1_ilp64, f( is, 1_ilp64 ),ldf )
                          alpha = rhs( 2_ilp64 )
                          call stdlib_I64_saxpy( js-1, alpha, e( 1_ilp64, js ), 1_ilp64, f( is, 1_ilp64 ),ldf )
                       end if
                       if( i<p ) then
                          alpha = -rhs( 1_ilp64 )
                          call stdlib_I64_saxpy( m-ie, alpha, a( is, ie+1 ), lda,c( ie+1, js ), 1_ilp64 )
                                    
                          alpha = -rhs( 2_ilp64 )
                          call stdlib_I64_saxpy( m-ie, alpha, d( is, ie+1 ), ldd,c( ie+1, js ), 1_ilp64 )
                                    
                       end if
                    else if( ( mb==1_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = zero
                       z( 3_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 1_ilp64 ) = -b( jsp1, js )
                       z( 1_ilp64, 2_ilp64 ) = zero
                       z( 2_ilp64, 2_ilp64 ) = a( is, is )
                       z( 3_ilp64, 2_ilp64 ) = -b( js, jsp1 )
                       z( 4_ilp64, 2_ilp64 ) = -b( jsp1, jsp1 )
                       z( 1_ilp64, 3_ilp64 ) = d( is, is )
                       z( 2_ilp64, 3_ilp64 ) = zero
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = d( is, is )
                       z( 3_ilp64, 4_ilp64 ) = -e( js, jsp1 )
                       z( 4_ilp64, 4_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( is, jsp1 )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( is, jsp1 )
                       ! solve z**t * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( is, jsp1 ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( is, jsp1 ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_saxpy( js-1, rhs( 1_ilp64 ), b( 1_ilp64, js ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_saxpy( js-1, rhs( 2_ilp64 ), b( 1_ilp64, jsp1 ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_saxpy( js-1, rhs( 3_ilp64 ), e( 1_ilp64, js ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_saxpy( js-1, rhs( 4_ilp64 ), e( 1_ilp64, jsp1 ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                       end if
                       if( i<p ) then
                          call stdlib_I64_sger( m-ie, nb, -one, a( is, ie+1 ), lda,rhs( 1_ilp64 ), 1_ilp64, c( ie+&
                                    1_ilp64, js ), ldc )
                          call stdlib_I64_sger( m-ie, nb, -one, d( is, ie+1 ), ldd,rhs( 3_ilp64 ), 1_ilp64, c( ie+&
                                    1_ilp64, js ), ldc )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( is, isp1 )
                       z( 3_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = a( isp1, is )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 3_ilp64, 2_ilp64 ) = zero
                       z( 4_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 1_ilp64, 3_ilp64 ) = d( is, is )
                       z( 2_ilp64, 3_ilp64 ) = d( is, isp1 )
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = d( isp1, isp1 )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( isp1, js )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( isp1, js )
                       ! solve z**t * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( isp1, js ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( isp1, js ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_sger( mb, js-1, one, rhs( 1_ilp64 ), 1_ilp64, b( 1_ilp64, js ),1_ilp64, f( is, 1_ilp64&
                              & ), &
                                    ldf )
                          call stdlib_I64_sger( mb, js-1, one, rhs( 3_ilp64 ), 1_ilp64, e( 1_ilp64, js ),1_ilp64, f( is, 1_ilp64&
                              & ), &
                                    ldf )
                       end if
                       if( i<p ) then
                          call stdlib_I64_sgemv( 'T', mb, m-ie, -one, a( is, ie+1 ),lda, rhs( 1_ilp64 ), 1_ilp64, &
                                    one, c( ie+1, js ),1_ilp64 )
                          call stdlib_I64_sgemv( 'T', mb, m-ie, -one, d( is, ie+1 ),ldd, rhs( 3_ilp64 ), 1_ilp64, &
                                    one, c( ie+1, js ),1_ilp64 )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build an 8-by-8 system z**t * x = rhs
                       call stdlib_I64_slaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( is, isp1 )
                       z( 5_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 7_ilp64, 1_ilp64 ) = -b( jsp1, js )
                       z( 1_ilp64, 2_ilp64 ) = a( isp1, is )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 6_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 8_ilp64, 2_ilp64 ) = -b( jsp1, js )
                       z( 3_ilp64, 3_ilp64 ) = a( is, is )
                       z( 4_ilp64, 3_ilp64 ) = a( is, isp1 )
                       z( 5_ilp64, 3_ilp64 ) = -b( js, jsp1 )
                       z( 7_ilp64, 3_ilp64 ) = -b( jsp1, jsp1 )
                       z( 3_ilp64, 4_ilp64 ) = a( isp1, is )
                       z( 4_ilp64, 4_ilp64 ) = a( isp1, isp1 )
                       z( 6_ilp64, 4_ilp64 ) = -b( js, jsp1 )
                       z( 8_ilp64, 4_ilp64 ) = -b( jsp1, jsp1 )
                       z( 1_ilp64, 5_ilp64 ) = d( is, is )
                       z( 2_ilp64, 5_ilp64 ) = d( is, isp1 )
                       z( 5_ilp64, 5_ilp64 ) = -e( js, js )
                       z( 2_ilp64, 6_ilp64 ) = d( isp1, isp1 )
                       z( 6_ilp64, 6_ilp64 ) = -e( js, js )
                       z( 3_ilp64, 7_ilp64 ) = d( is, is )
                       z( 4_ilp64, 7_ilp64 ) = d( is, isp1 )
                       z( 5_ilp64, 7_ilp64 ) = -e( js, jsp1 )
                       z( 7_ilp64, 7_ilp64 ) = -e( jsp1, jsp1 )
                       z( 4_ilp64, 8_ilp64 ) = d( isp1, isp1 )
                       z( 6_ilp64, 8_ilp64 ) = -e( js, jsp1 )
                       z( 8_ilp64, 8_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_scopy( mb, c( is, js+jj ), 1_ilp64, rhs( k ), 1_ilp64 )
                          call stdlib_I64_scopy( mb, f( is, js+jj ), 1_ilp64, rhs( ii ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z**t * x = rhs
                       call stdlib_I64_sgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_sgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_sscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_sscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_scopy( mb, rhs( k ), 1_ilp64, c( is, js+jj ), 1_ilp64 )
                          call stdlib_I64_scopy( mb, rhs( ii ), 1_ilp64, f( is, js+jj ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_sgemm( 'N', 'T', mb, js-1, nb, one,c( is, js ), ldc, b( 1_ilp64, &
                                    js ), ldb, one,f( is, 1_ilp64 ), ldf )
                          call stdlib_I64_sgemm( 'N', 'T', mb, js-1, nb, one,f( is, js ), ldf, e( 1_ilp64, &
                                    js ), lde, one,f( is, 1_ilp64 ), ldf )
                       end if
                       if( i<p ) then
                          call stdlib_I64_sgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( &
                                    is, js ), ldc,one, c( ie+1, js ), ldc )
                          call stdlib_I64_sgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( &
                                    is, js ), ldf,one, c( ie+1, js ), ldc )
                       end if
                    end if
                 end do loop_190
              end do loop_200
           end if
           return
     end subroutine stdlib_I64_stgsy2

     pure module subroutine stdlib_I64_dtgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! DTGSY2 solves the generalized Sylvester equation:
     !! A * R - L * B = scale * C                (1)
     !! D * R - L * E = scale * F,
     !! using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)
     !! must be in generalized Schur canonical form, i.e. A, B are upper
     !! quasi triangular and D, E are upper triangular. The solution (R, L)
     !! overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor
     !! chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Z*x = scale*b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**T, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**T, Im) ],
     !! Ik is the identity matrix of size k and X**T is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! In the process of solving (1), we solve a number of such systems
     !! where Dim(In), Dim(In) = 1 or 2.
     !! If TRANS = 'T', solve the transposed system Z**T*y = scale*b for y,
     !! which is equivalent to solve for R and L in
     !! A**T * R  + D**T * L   = scale * C           (3)
     !! R  * B**T + L  * E**T  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! sigma_min(Z) using reverse communication with DLACON.
     !! DTGSY2 also (IJOB >= 1) contributes to the computation in DTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of the matrix pair in
     !! DTGSYL. See DTGSYL for details.
               ldf, scale, rdsum, rdscal,iwork, pq, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp64), intent(out) :: info, pq
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_dcopy by calls to stdlib_I64_dlaset.
        ! sven hammarling, 27/5/02.
           ! Parameters 
           integer(ilp64), parameter :: ldz = 8_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp64) :: i, ie, ierr, ii, is, isp1, j, je, jj, js, jsp1, k, mb, nb, p, q, &
                     zdim
           real(dp) :: alpha, scaloc
           ! Local Arrays 
           integer(ilp64) :: ipiv(ldz), jpiv(ldz)
           real(dp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           ierr = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>2_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGSY2', -info )
              return
           end if
           ! determine block structure of a
           pq = 0_ilp64
           p = 0_ilp64
           i = 1_ilp64
           10 continue
           if( i>m )go to 20
           p = p + 1_ilp64
           iwork( p ) = i
           if( i==m )go to 20
           if( a( i+1, i )/=zero ) then
              i = i + 2_ilp64
           else
              i = i + 1_ilp64
           end if
           go to 10
           20 continue
           iwork( p+1 ) = m + 1_ilp64
           ! determine block structure of b
           q = p + 1_ilp64
           j = 1_ilp64
           30 continue
           if( j>n )go to 40
           q = q + 1_ilp64
           iwork( q ) = j
           if( j==n )go to 40
           if( b( j+1, j )/=zero ) then
              j = j + 2_ilp64
           else
              j = j + 1_ilp64
           end if
           go to 30
           40 continue
           iwork( q+1 ) = n + 1_ilp64
           pq = p*( q-p-1 )
           if( notran ) then
              ! solve (i, j) - subsystem
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = p, p - 1, ..., 1; j = 1, 2, ..., q
              scale = one
              scaloc = one
              loop_120: do j = p + 2, q
                 js = iwork( j )
                 jsp1 = js + 1_ilp64
                 je = iwork( j+1 ) - 1_ilp64
                 nb = je - js + 1_ilp64
                 loop_110: do i = p, 1, -1
                    is = iwork( i )
                    isp1 = is + 1_ilp64
                    ie = iwork( i+1 ) - 1_ilp64
                    mb = ie - is + 1_ilp64
                    zdim = mb*nb*2_ilp64
                    if( ( mb==1_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 2-by-2 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = d( is, is )
                       z( 1_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 2_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = f( is, js )
                       ! solve z * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       f( is, js ) = rhs( 2_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          alpha = -rhs( 1_ilp64 )
                          call stdlib_I64_daxpy( is-1, alpha, a( 1_ilp64, is ), 1_ilp64, c( 1_ilp64, js ),1_ilp64 )
                          call stdlib_I64_daxpy( is-1, alpha, d( 1_ilp64, is ), 1_ilp64, f( 1_ilp64, js ),1_ilp64 )
                       end if
                       if( j<q ) then
                          call stdlib_I64_daxpy( n-je, rhs( 2_ilp64 ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_daxpy( n-je, rhs( 2_ilp64 ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==1_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = zero
                       z( 3_ilp64, 1_ilp64 ) = d( is, is )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = zero
                       z( 2_ilp64, 2_ilp64 ) = a( is, is )
                       z( 3_ilp64, 2_ilp64 ) = zero
                       z( 4_ilp64, 2_ilp64 ) = d( is, is )
                       z( 1_ilp64, 3_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 3_ilp64 ) = -b( js, jsp1 )
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = -e( js, jsp1 )
                       z( 1_ilp64, 4_ilp64 ) = -b( jsp1, js )
                       z( 2_ilp64, 4_ilp64 ) = -b( jsp1, jsp1 )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( is, jsp1 )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( is, jsp1 )
                       ! solve z * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( is, jsp1 ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( is, jsp1 ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_dger( is-1, nb, -one, a( 1_ilp64, is ), 1_ilp64, rhs( 1_ilp64 ),1_ilp64, c( 1_ilp64, js&
                              & ),&
                                     ldc )
                          call stdlib_I64_dger( is-1, nb, -one, d( 1_ilp64, is ), 1_ilp64, rhs( 1_ilp64 ),1_ilp64, f( 1_ilp64, js&
                              & ),&
                                     ldf )
                       end if
                       if( j<q ) then
                          call stdlib_I64_daxpy( n-je, rhs( 3_ilp64 ), b( js, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_daxpy( n-je, rhs( 3_ilp64 ), e( js, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                          call stdlib_I64_daxpy( n-je, rhs( 4_ilp64 ), b( jsp1, je+1 ), ldb,c( is, je+1 ), &
                                    ldc )
                          call stdlib_I64_daxpy( n-je, rhs( 4_ilp64 ), e( jsp1, je+1 ), lde,f( is, je+1 ), &
                                    ldf )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 4-by-4 system z * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( isp1, is )
                       z( 3_ilp64, 1_ilp64 ) = d( is, is )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = a( is, isp1 )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 3_ilp64, 2_ilp64 ) = d( is, isp1 )
                       z( 4_ilp64, 2_ilp64 ) = d( isp1, isp1 )
                       z( 1_ilp64, 3_ilp64 ) = -b( js, js )
                       z( 2_ilp64, 3_ilp64 ) = zero
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = -b( js, js )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( isp1, js )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( isp1, js )
                       ! solve z * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( isp1, js ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( isp1, js ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_dgemv( 'N', is-1, mb, -one, a( 1_ilp64, is ), lda,rhs( 1_ilp64 ), 1_ilp64, &
                                    one, c( 1_ilp64, js ), 1_ilp64 )
                          call stdlib_I64_dgemv( 'N', is-1, mb, -one, d( 1_ilp64, is ), ldd,rhs( 1_ilp64 ), 1_ilp64, &
                                    one, f( 1_ilp64, js ), 1_ilp64 )
                       end if
                       if( j<q ) then
                          call stdlib_I64_dger( mb, n-je, one, rhs( 3_ilp64 ), 1_ilp64,b( js, je+1 ), ldb, c( is, &
                                    je+1 ), ldc )
                          call stdlib_I64_dger( mb, n-je, one, rhs( 3_ilp64 ), 1_ilp64,e( js, je+1 ), lde, f( is, &
                                    je+1 ), ldf )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build an 8-by-8 system z * x = rhs
                       call stdlib_I64_dlaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( isp1, is )
                       z( 5_ilp64, 1_ilp64 ) = d( is, is )
                       z( 1_ilp64, 2_ilp64 ) = a( is, isp1 )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 5_ilp64, 2_ilp64 ) = d( is, isp1 )
                       z( 6_ilp64, 2_ilp64 ) = d( isp1, isp1 )
                       z( 3_ilp64, 3_ilp64 ) = a( is, is )
                       z( 4_ilp64, 3_ilp64 ) = a( isp1, is )
                       z( 7_ilp64, 3_ilp64 ) = d( is, is )
                       z( 3_ilp64, 4_ilp64 ) = a( is, isp1 )
                       z( 4_ilp64, 4_ilp64 ) = a( isp1, isp1 )
                       z( 7_ilp64, 4_ilp64 ) = d( is, isp1 )
                       z( 8_ilp64, 4_ilp64 ) = d( isp1, isp1 )
                       z( 1_ilp64, 5_ilp64 ) = -b( js, js )
                       z( 3_ilp64, 5_ilp64 ) = -b( js, jsp1 )
                       z( 5_ilp64, 5_ilp64 ) = -e( js, js )
                       z( 7_ilp64, 5_ilp64 ) = -e( js, jsp1 )
                       z( 2_ilp64, 6_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 6_ilp64 ) = -b( js, jsp1 )
                       z( 6_ilp64, 6_ilp64 ) = -e( js, js )
                       z( 8_ilp64, 6_ilp64 ) = -e( js, jsp1 )
                       z( 1_ilp64, 7_ilp64 ) = -b( jsp1, js )
                       z( 3_ilp64, 7_ilp64 ) = -b( jsp1, jsp1 )
                       z( 7_ilp64, 7_ilp64 ) = -e( jsp1, jsp1 )
                       z( 2_ilp64, 8_ilp64 ) = -b( jsp1, js )
                       z( 4_ilp64, 8_ilp64 ) = -b( jsp1, jsp1 )
                       z( 8_ilp64, 8_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_dcopy( mb, c( is, js+jj ), 1_ilp64, rhs( k ), 1_ilp64 )
                          call stdlib_I64_dcopy( mb, f( is, js+jj ), 1_ilp64, rhs( ii ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       if( ijob==0_ilp64 ) then
                          call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv,scaloc )
                          if( scaloc/=one ) then
                             do k = 1, n
                                call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                                call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                             end do
                             scale = scale*scaloc
                          end if
                       else
                          call stdlib_I64_dlatdf( ijob, zdim, z, ldz, rhs, rdsum,rdscal, ipiv, jpiv )
                                    
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_dcopy( mb, rhs( k ), 1_ilp64, c( is, js+jj ), 1_ilp64 )
                          call stdlib_I64_dcopy( mb, rhs( ii ), 1_ilp64, f( is, js+jj ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( i>1_ilp64 ) then
                          call stdlib_I64_dgemm( 'N', 'N', is-1, nb, mb, -one,a( 1_ilp64, is ), lda, rhs( 1_ilp64 &
                                    ), mb, one,c( 1_ilp64, js ), ldc )
                          call stdlib_I64_dgemm( 'N', 'N', is-1, nb, mb, -one,d( 1_ilp64, is ), ldd, rhs( 1_ilp64 &
                                    ), mb, one,f( 1_ilp64, js ), ldf )
                       end if
                       if( j<q ) then
                          k = mb*nb + 1_ilp64
                          call stdlib_I64_dgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, b( js, je+&
                                    1_ilp64 ), ldb, one,c( is, je+1 ), ldc )
                          call stdlib_I64_dgemm( 'N', 'N', mb, n-je, nb, one, rhs( k ),mb, e( js, je+&
                                    1_ilp64 ), lde, one,f( is, je+1 ), ldf )
                       end if
                    end if
                 end do loop_110
              end do loop_120
           else
              ! solve (i, j) - subsystem
                   ! a(i, i)**t * r(i, j) + d(i, i)**t * l(j, j)  =  c(i, j)
                   ! r(i, i)  * b(j, j) + l(i, j)  * e(j, j)  = -f(i, j)
              ! for i = 1, 2, ..., p, j = q, q - 1, ..., 1
              scale = one
              scaloc = one
              loop_200: do i = 1, p
                 is = iwork( i )
                 isp1 = is + 1_ilp64
                 ie = iwork ( i+1 ) - 1_ilp64
                 mb = ie - is + 1_ilp64
                 loop_190: do j = q, p + 2, -1
                    js = iwork( j )
                    jsp1 = js + 1_ilp64
                    je = iwork( j+1 ) - 1_ilp64
                    nb = je - js + 1_ilp64
                    zdim = mb*nb*2_ilp64
                    if( ( mb==1_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 2-by-2 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 1_ilp64, 2_ilp64 ) = d( is, is )
                       z( 2_ilp64, 2_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = f( is, js )
                       ! solve z**t * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       f( is, js ) = rhs( 2_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          alpha = rhs( 1_ilp64 )
                          call stdlib_I64_daxpy( js-1, alpha, b( 1_ilp64, js ), 1_ilp64, f( is, 1_ilp64 ),ldf )
                          alpha = rhs( 2_ilp64 )
                          call stdlib_I64_daxpy( js-1, alpha, e( 1_ilp64, js ), 1_ilp64, f( is, 1_ilp64 ),ldf )
                       end if
                       if( i<p ) then
                          alpha = -rhs( 1_ilp64 )
                          call stdlib_I64_daxpy( m-ie, alpha, a( is, ie+1 ), lda,c( ie+1, js ), 1_ilp64 )
                                    
                          alpha = -rhs( 2_ilp64 )
                          call stdlib_I64_daxpy( m-ie, alpha, d( is, ie+1 ), ldd,c( ie+1, js ), 1_ilp64 )
                                    
                       end if
                    else if( ( mb==1_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = zero
                       z( 3_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 1_ilp64 ) = -b( jsp1, js )
                       z( 1_ilp64, 2_ilp64 ) = zero
                       z( 2_ilp64, 2_ilp64 ) = a( is, is )
                       z( 3_ilp64, 2_ilp64 ) = -b( js, jsp1 )
                       z( 4_ilp64, 2_ilp64 ) = -b( jsp1, jsp1 )
                       z( 1_ilp64, 3_ilp64 ) = d( is, is )
                       z( 2_ilp64, 3_ilp64 ) = zero
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = d( is, is )
                       z( 3_ilp64, 4_ilp64 ) = -e( js, jsp1 )
                       z( 4_ilp64, 4_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( is, jsp1 )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( is, jsp1 )
                       ! solve z**t * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( is, jsp1 ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( is, jsp1 ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_daxpy( js-1, rhs( 1_ilp64 ), b( 1_ilp64, js ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_daxpy( js-1, rhs( 2_ilp64 ), b( 1_ilp64, jsp1 ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_daxpy( js-1, rhs( 3_ilp64 ), e( 1_ilp64, js ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                          call stdlib_I64_daxpy( js-1, rhs( 4_ilp64 ), e( 1_ilp64, jsp1 ), 1_ilp64,f( is, 1_ilp64 ), ldf )
                                    
                       end if
                       if( i<p ) then
                          call stdlib_I64_dger( m-ie, nb, -one, a( is, ie+1 ), lda,rhs( 1_ilp64 ), 1_ilp64, c( ie+&
                                    1_ilp64, js ), ldc )
                          call stdlib_I64_dger( m-ie, nb, -one, d( is, ie+1 ), ldd,rhs( 3_ilp64 ), 1_ilp64, c( ie+&
                                    1_ilp64, js ), ldc )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==1_ilp64 ) ) then
                       ! build a 4-by-4 system z**t * x = rhs
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( is, isp1 )
                       z( 3_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 4_ilp64, 1_ilp64 ) = zero
                       z( 1_ilp64, 2_ilp64 ) = a( isp1, is )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 3_ilp64, 2_ilp64 ) = zero
                       z( 4_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 1_ilp64, 3_ilp64 ) = d( is, is )
                       z( 2_ilp64, 3_ilp64 ) = d( is, isp1 )
                       z( 3_ilp64, 3_ilp64 ) = -e( js, js )
                       z( 4_ilp64, 3_ilp64 ) = zero
                       z( 1_ilp64, 4_ilp64 ) = zero
                       z( 2_ilp64, 4_ilp64 ) = d( isp1, isp1 )
                       z( 3_ilp64, 4_ilp64 ) = zero
                       z( 4_ilp64, 4_ilp64 ) = -e( js, js )
                       ! set up right hand side(s)
                       rhs( 1_ilp64 ) = c( is, js )
                       rhs( 2_ilp64 ) = c( isp1, js )
                       rhs( 3_ilp64 ) = f( is, js )
                       rhs( 4_ilp64 ) = f( isp1, js )
                       ! solve z**t * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       c( is, js ) = rhs( 1_ilp64 )
                       c( isp1, js ) = rhs( 2_ilp64 )
                       f( is, js ) = rhs( 3_ilp64 )
                       f( isp1, js ) = rhs( 4_ilp64 )
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_dger( mb, js-1, one, rhs( 1_ilp64 ), 1_ilp64, b( 1_ilp64, js ),1_ilp64, f( is, 1_ilp64&
                              & ), &
                                    ldf )
                          call stdlib_I64_dger( mb, js-1, one, rhs( 3_ilp64 ), 1_ilp64, e( 1_ilp64, js ),1_ilp64, f( is, 1_ilp64&
                              & ), &
                                    ldf )
                       end if
                       if( i<p ) then
                          call stdlib_I64_dgemv( 'T', mb, m-ie, -one, a( is, ie+1 ),lda, rhs( 1_ilp64 ), 1_ilp64, &
                                    one, c( ie+1, js ),1_ilp64 )
                          call stdlib_I64_dgemv( 'T', mb, m-ie, -one, d( is, ie+1 ),ldd, rhs( 3_ilp64 ), 1_ilp64, &
                                    one, c( ie+1, js ),1_ilp64 )
                       end if
                    else if( ( mb==2_ilp64 ) .and. ( nb==2_ilp64 ) ) then
                       ! build an 8-by-8 system z**t * x = rhs
                       call stdlib_I64_dlaset( 'F', ldz, ldz, zero, zero, z, ldz )
                       z( 1_ilp64, 1_ilp64 ) = a( is, is )
                       z( 2_ilp64, 1_ilp64 ) = a( is, isp1 )
                       z( 5_ilp64, 1_ilp64 ) = -b( js, js )
                       z( 7_ilp64, 1_ilp64 ) = -b( jsp1, js )
                       z( 1_ilp64, 2_ilp64 ) = a( isp1, is )
                       z( 2_ilp64, 2_ilp64 ) = a( isp1, isp1 )
                       z( 6_ilp64, 2_ilp64 ) = -b( js, js )
                       z( 8_ilp64, 2_ilp64 ) = -b( jsp1, js )
                       z( 3_ilp64, 3_ilp64 ) = a( is, is )
                       z( 4_ilp64, 3_ilp64 ) = a( is, isp1 )
                       z( 5_ilp64, 3_ilp64 ) = -b( js, jsp1 )
                       z( 7_ilp64, 3_ilp64 ) = -b( jsp1, jsp1 )
                       z( 3_ilp64, 4_ilp64 ) = a( isp1, is )
                       z( 4_ilp64, 4_ilp64 ) = a( isp1, isp1 )
                       z( 6_ilp64, 4_ilp64 ) = -b( js, jsp1 )
                       z( 8_ilp64, 4_ilp64 ) = -b( jsp1, jsp1 )
                       z( 1_ilp64, 5_ilp64 ) = d( is, is )
                       z( 2_ilp64, 5_ilp64 ) = d( is, isp1 )
                       z( 5_ilp64, 5_ilp64 ) = -e( js, js )
                       z( 2_ilp64, 6_ilp64 ) = d( isp1, isp1 )
                       z( 6_ilp64, 6_ilp64 ) = -e( js, js )
                       z( 3_ilp64, 7_ilp64 ) = d( is, is )
                       z( 4_ilp64, 7_ilp64 ) = d( is, isp1 )
                       z( 5_ilp64, 7_ilp64 ) = -e( js, jsp1 )
                       z( 7_ilp64, 7_ilp64 ) = -e( jsp1, jsp1 )
                       z( 4_ilp64, 8_ilp64 ) = d( isp1, isp1 )
                       z( 6_ilp64, 8_ilp64 ) = -e( js, jsp1 )
                       z( 8_ilp64, 8_ilp64 ) = -e( jsp1, jsp1 )
                       ! set up right hand side(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_dcopy( mb, c( is, js+jj ), 1_ilp64, rhs( k ), 1_ilp64 )
                          call stdlib_I64_dcopy( mb, f( is, js+jj ), 1_ilp64, rhs( ii ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! solve z**t * x = rhs
                       call stdlib_I64_dgetc2( zdim, z, ldz, ipiv, jpiv, ierr )
                       if( ierr>0_ilp64 )info = ierr
                       call stdlib_I64_dgesc2( zdim, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_dscal( m, scaloc, c( 1_ilp64, k ), 1_ilp64 )
                             call stdlib_I64_dscal( m, scaloc, f( 1_ilp64, k ), 1_ilp64 )
                          end do
                          scale = scale*scaloc
                       end if
                       ! unpack solution vector(s)
                       k = 1_ilp64
                       ii = mb*nb + 1_ilp64
                       do jj = 0, nb - 1
                          call stdlib_I64_dcopy( mb, rhs( k ), 1_ilp64, c( is, js+jj ), 1_ilp64 )
                          call stdlib_I64_dcopy( mb, rhs( ii ), 1_ilp64, f( is, js+jj ), 1_ilp64 )
                          k = k + mb
                          ii = ii + mb
                       end do
                       ! substitute r(i, j) and l(i, j) into remaining
                       ! equation.
                       if( j>p+2 ) then
                          call stdlib_I64_dgemm( 'N', 'T', mb, js-1, nb, one,c( is, js ), ldc, b( 1_ilp64, &
                                    js ), ldb, one,f( is, 1_ilp64 ), ldf )
                          call stdlib_I64_dgemm( 'N', 'T', mb, js-1, nb, one,f( is, js ), ldf, e( 1_ilp64, &
                                    js ), lde, one,f( is, 1_ilp64 ), ldf )
                       end if
                       if( i<p ) then
                          call stdlib_I64_dgemm( 'T', 'N', m-ie, nb, mb, -one,a( is, ie+1 ), lda, c( &
                                    is, js ), ldc,one, c( ie+1, js ), ldc )
                          call stdlib_I64_dgemm( 'T', 'N', m-ie, nb, mb, -one,d( is, ie+1 ), ldd, f( &
                                    is, js ), ldf,one, c( ie+1, js ), ldc )
                       end if
                    end if
                 end do loop_190
              end do loop_200
           end if
           return
     end subroutine stdlib_I64_dtgsy2


     pure module subroutine stdlib_I64_ctgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! CTGSY2 solves the generalized Sylvester equation
     !! A * R - L * B = scale *  C               (1)
     !! D * R - L * E = scale * F
     !! using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
     !! (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Zx = scale * b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Ik is the identity matrix of size k and X**H is the transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H*y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R  + D**H * L   = scale * C           (3)
     !! R  * B**H + L  * E**H  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! = sigma_min(Z) using reverse communication with CLACON.
     !! CTGSY2 also (IJOB >= 1) contributes to the computation in CTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of two matrix pairs in
     !! CTGSYL.
               ldf, scale, rdsum, rdscal,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp64), intent(out) :: info
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: ldz = 2_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp64) :: i, ierr, j, k
           real(sp) :: scaloc
           complex(sp) :: alpha
           ! Local Arrays 
           integer(ilp64) :: ipiv(ldz), jpiv(ldz)
           complex(sp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           ierr = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>2_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGSY2', -info )
              return
           end if
           if( notran ) then
              ! solve (i, j) - system
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = m, m - 1, ..., 1; j = 1, 2, ..., n
              scale = one
              scaloc = one
              loop_30: do j = 1, n
                 loop_20: do i = m, 1, -1
                    ! build 2 by 2 system
                    z( 1_ilp64, 1_ilp64 ) = a( i, i )
                    z( 2_ilp64, 1_ilp64 ) = d( i, i )
                    z( 1_ilp64, 2_ilp64 ) = -b( j, j )
                    z( 2_ilp64, 2_ilp64 ) = -e( j, j )
                    ! set up right hand side(s)
                    rhs( 1_ilp64 ) = c( i, j )
                    rhs( 2_ilp64 ) = f( i, j )
                    ! solve z * x = rhs
                    call stdlib_I64_cgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp64 )info = ierr
                    if( ijob==0_ilp64 ) then
                       call stdlib_I64_cgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                       
                             call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                    else
                       call stdlib_I64_clatdf( ijob, ldz, z, ldz, rhs, rdsum, rdscal,ipiv, jpiv )
                                 
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp64 )
                    f( i, j ) = rhs( 2_ilp64 )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( i>1_ilp64 ) then
                       alpha = -rhs( 1_ilp64 )
                       call stdlib_I64_caxpy( i-1, alpha, a( 1_ilp64, i ), 1_ilp64, c( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_caxpy( i-1, alpha, d( 1_ilp64, i ), 1_ilp64, f( 1_ilp64, j ), 1_ilp64 )
                    end if
                    if( j<n ) then
                       call stdlib_I64_caxpy( n-j, rhs( 2_ilp64 ), b( j, j+1 ), ldb,c( i, j+1 ), ldc )
                                 
                       call stdlib_I64_caxpy( n-j, rhs( 2_ilp64 ), e( j, j+1 ), lde,f( i, j+1 ), ldf )
                                 
                    end if
                 end do loop_20
              end do loop_30
           else
              ! solve transposed (i, j) - system:
                 ! a(i, i)**h * r(i, j) + d(i, i)**h * l(j, j) = c(i, j)
                 ! r(i, i) * b(j, j) + l(i, j) * e(j, j)   = -f(i, j)
              ! for i = 1, 2, ..., m, j = n, n - 1, ..., 1
              scale = one
              scaloc = one
              loop_80: do i = 1, m
                 loop_70: do j = n, 1, -1
                    ! build 2 by 2 system z**h
                    z( 1_ilp64, 1_ilp64 ) = conjg( a( i, i ) )
                    z( 2_ilp64, 1_ilp64 ) = -conjg( b( j, j ) )
                    z( 1_ilp64, 2_ilp64 ) = conjg( d( i, i ) )
                    z( 2_ilp64, 2_ilp64 ) = -conjg( e( j, j ) )
                    ! set up right hand side(s)
                    rhs( 1_ilp64 ) = c( i, j )
                    rhs( 2_ilp64 ) = f( i, j )
                    ! solve z**h * x = rhs
                    call stdlib_I64_cgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp64 )info = ierr
                    call stdlib_I64_cgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                    if( scaloc/=one ) then
                       do k = 1, n
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_cscal( m, cmplx( scaloc, zero,KIND=sp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp64 )
                    f( i, j ) = rhs( 2_ilp64 )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    do k = 1, j - 1
                       f( i, k ) = f( i, k ) + rhs( 1_ilp64 )*conjg( b( k, j ) ) +rhs( 2_ilp64 )*conjg( e( k, &
                                 j ) )
                    end do
                    do k = i + 1, m
                       c( k, j ) = c( k, j ) - conjg( a( i, k ) )*rhs( 1_ilp64 ) -conjg( d( i, k ) )&
                                 *rhs( 2_ilp64 )
                    end do
                 end do loop_70
              end do loop_80
           end if
           return
     end subroutine stdlib_I64_ctgsy2

     pure module subroutine stdlib_I64_ztgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
     !! ZTGSY2 solves the generalized Sylvester equation
     !! A * R - L * B = scale * C               (1)
     !! D * R - L * E = scale * F
     !! using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
     !! (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
     !! N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
     !! (i.e., (A,D) and (B,E) in generalized Schur form).
     !! The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
     !! scaling factor chosen to avoid overflow.
     !! In matrix notation solving equation (1) corresponds to solve
     !! Zx = scale * b, where Z is defined as
     !! Z = [ kron(In, A)  -kron(B**H, Im) ]             (2)
     !! [ kron(In, D)  -kron(E**H, Im) ],
     !! Ik is the identity matrix of size k and X**H is the conjuguate transpose of X.
     !! kron(X, Y) is the Kronecker product between the matrices X and Y.
     !! If TRANS = 'C', y in the conjugate transposed system Z**H*y = scale*b
     !! is solved for, which is equivalent to solve for R and L in
     !! A**H * R  + D**H * L   = scale * C           (3)
     !! R  * B**H + L  * E**H  = scale * -F
     !! This case is used to compute an estimate of Dif[(A, D), (B, E)] =
     !! = sigma_min(Z) using reverse communication with ZLACON.
     !! ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL
     !! of an upper bound on the separation between to matrix pairs. Then
     !! the input (A, D), (B, E) are sub-pencils of two matrix pairs in
     !! ZTGSYL.
               ldf, scale, rdsum, rdscal,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp64), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp64), intent(out) :: info
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: ldz = 2_ilp64
           
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp64) :: i, ierr, j, k
           real(dp) :: scaloc
           complex(dp) :: alpha
           ! Local Arrays 
           integer(ilp64) :: ipiv(ldz), jpiv(ldz)
           complex(dp) :: rhs(ldz), z(ldz,ldz)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           info = 0_ilp64
           ierr = 0_ilp64
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -1_ilp64
           else if( notran ) then
              if( ( ijob<0_ilp64 ) .or. ( ijob>2_ilp64 ) ) then
                 info = -2_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              if( m<=0_ilp64 ) then
                 info = -3_ilp64
              else if( n<=0_ilp64 ) then
                 info = -4_ilp64
              else if( lda<max( 1_ilp64, m ) ) then
                 info = -6_ilp64
              else if( ldb<max( 1_ilp64, n ) ) then
                 info = -8_ilp64
              else if( ldc<max( 1_ilp64, m ) ) then
                 info = -10_ilp64
              else if( ldd<max( 1_ilp64, m ) ) then
                 info = -12_ilp64
              else if( lde<max( 1_ilp64, n ) ) then
                 info = -14_ilp64
              else if( ldf<max( 1_ilp64, m ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGSY2', -info )
              return
           end if
           if( notran ) then
              ! solve (i, j) - system
                 ! a(i, i) * r(i, j) - l(i, j) * b(j, j) = c(i, j)
                 ! d(i, i) * r(i, j) - l(i, j) * e(j, j) = f(i, j)
              ! for i = m, m - 1, ..., 1; j = 1, 2, ..., n
              scale = one
              scaloc = one
              loop_30: do j = 1, n
                 loop_20: do i = m, 1, -1
                    ! build 2 by 2 system
                    z( 1_ilp64, 1_ilp64 ) = a( i, i )
                    z( 2_ilp64, 1_ilp64 ) = d( i, i )
                    z( 1_ilp64, 2_ilp64 ) = -b( j, j )
                    z( 2_ilp64, 2_ilp64 ) = -e( j, j )
                    ! set up right hand side(s)
                    rhs( 1_ilp64 ) = c( i, j )
                    rhs( 2_ilp64 ) = f( i, j )
                    ! solve z * x = rhs
                    call stdlib_I64_zgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp64 )info = ierr
                    if( ijob==0_ilp64 ) then
                       call stdlib_I64_zgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                       if( scaloc/=one ) then
                          do k = 1, n
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),c( 1_ilp64, k ), 1_ilp64 )
                                       
                             call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp),f( 1_ilp64, k ), 1_ilp64 )
                                       
                          end do
                          scale = scale*scaloc
                       end if
                    else
                       call stdlib_I64_zlatdf( ijob, ldz, z, ldz, rhs, rdsum, rdscal,ipiv, jpiv )
                                 
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp64 )
                    f( i, j ) = rhs( 2_ilp64 )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    if( i>1_ilp64 ) then
                       alpha = -rhs( 1_ilp64 )
                       call stdlib_I64_zaxpy( i-1, alpha, a( 1_ilp64, i ), 1_ilp64, c( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_zaxpy( i-1, alpha, d( 1_ilp64, i ), 1_ilp64, f( 1_ilp64, j ), 1_ilp64 )
                    end if
                    if( j<n ) then
                       call stdlib_I64_zaxpy( n-j, rhs( 2_ilp64 ), b( j, j+1 ), ldb,c( i, j+1 ), ldc )
                                 
                       call stdlib_I64_zaxpy( n-j, rhs( 2_ilp64 ), e( j, j+1 ), lde,f( i, j+1 ), ldf )
                                 
                    end if
                 end do loop_20
              end do loop_30
           else
              ! solve transposed (i, j) - system:
                 ! a(i, i)**h * r(i, j) + d(i, i)**h * l(j, j) = c(i, j)
                 ! r(i, i) * b(j, j) + l(i, j) * e(j, j)   = -f(i, j)
              ! for i = 1, 2, ..., m, j = n, n - 1, ..., 1
              scale = one
              scaloc = one
              loop_80: do i = 1, m
                 loop_70: do j = n, 1, -1
                    ! build 2 by 2 system z**h
                    z( 1_ilp64, 1_ilp64 ) = conjg( a( i, i ) )
                    z( 2_ilp64, 1_ilp64 ) = -conjg( b( j, j ) )
                    z( 1_ilp64, 2_ilp64 ) = conjg( d( i, i ) )
                    z( 2_ilp64, 2_ilp64 ) = -conjg( e( j, j ) )
                    ! set up right hand side(s)
                    rhs( 1_ilp64 ) = c( i, j )
                    rhs( 2_ilp64 ) = f( i, j )
                    ! solve z**h * x = rhs
                    call stdlib_I64_zgetc2( ldz, z, ldz, ipiv, jpiv, ierr )
                    if( ierr>0_ilp64 )info = ierr
                    call stdlib_I64_zgesc2( ldz, z, ldz, rhs, ipiv, jpiv, scaloc )
                    if( scaloc/=one ) then
                       do k = 1, n
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), c( 1_ilp64, k ),1_ilp64 )
                                    
                          call stdlib_I64_zscal( m, cmplx( scaloc, zero,KIND=dp), f( 1_ilp64, k ),1_ilp64 )
                                    
                       end do
                       scale = scale*scaloc
                    end if
                    ! unpack solution vector(s)
                    c( i, j ) = rhs( 1_ilp64 )
                    f( i, j ) = rhs( 2_ilp64 )
                    ! substitute r(i, j) and l(i, j) into remaining equation.
                    do k = 1, j - 1
                       f( i, k ) = f( i, k ) + rhs( 1_ilp64 )*conjg( b( k, j ) ) +rhs( 2_ilp64 )*conjg( e( k, &
                                 j ) )
                    end do
                    do k = i + 1, m
                       c( k, j ) = c( k, j ) - conjg( a( i, k ) )*rhs( 1_ilp64 ) -conjg( d( i, k ) )&
                                 *rhs( 2_ilp64 )
                    end do
                 end do loop_70
              end do loop_80
           end if
           return
     end subroutine stdlib_I64_ztgsy2




     pure module subroutine stdlib_I64_slagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
     !! SLAGV2 computes the Generalized Schur factorization of a real 2-by-2
     !! matrix pencil (A,B) where B is upper triangular. This routine
     !! computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
     !! SNR such that
     !! 1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
     !! types), then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],
     !! 2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
     !! then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]
     !! where b11 >= b22 > 0.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: lda, ldb
           real(sp), intent(out) :: csl, csr, snl, snr
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(2_ilp64), alphar(2_ilp64), beta(2_ilp64)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: anorm, ascale, bnorm, bscale, h1, h2, h3, qq, r, rr, safmin, scale1, &
                     scale2, t, ulp, wi, wr1, wr2
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_I64_slamch( 'S' )
           ulp = stdlib_I64_slamch( 'P' )
           ! scale a
           anorm = max( abs( a( 1_ilp64, 1_ilp64 ) )+abs( a( 2_ilp64, 1_ilp64 ) ),abs( a( 1_ilp64, 2_ilp64 ) )+abs( a( 2_ilp64,&
               & 2_ilp64 ) ), &
                     safmin )
           ascale = one / anorm
           a( 1_ilp64, 1_ilp64 ) = ascale*a( 1_ilp64, 1_ilp64 )
           a( 1_ilp64, 2_ilp64 ) = ascale*a( 1_ilp64, 2_ilp64 )
           a( 2_ilp64, 1_ilp64 ) = ascale*a( 2_ilp64, 1_ilp64 )
           a( 2_ilp64, 2_ilp64 ) = ascale*a( 2_ilp64, 2_ilp64 )
           ! scale b
           bnorm = max( abs( b( 1_ilp64, 1_ilp64 ) ), abs( b( 1_ilp64, 2_ilp64 ) )+abs( b( 2_ilp64, 2_ilp64 ) ),safmin )
           bscale = one / bnorm
           b( 1_ilp64, 1_ilp64 ) = bscale*b( 1_ilp64, 1_ilp64 )
           b( 1_ilp64, 2_ilp64 ) = bscale*b( 1_ilp64, 2_ilp64 )
           b( 2_ilp64, 2_ilp64 ) = bscale*b( 2_ilp64, 2_ilp64 )
           ! check if a can be deflated
           if( abs( a( 2_ilp64, 1_ilp64 ) )<=ulp ) then
              csl = one
              snl = zero
              csr = one
              snr = zero
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              wi = zero
           ! check if b is singular
           else if( abs( b( 1_ilp64, 1_ilp64 ) )<=ulp ) then
              call stdlib_I64_slartg( a( 1_ilp64, 1_ilp64 ), a( 2_ilp64, 1_ilp64 ), csl, snl, r )
              csr = one
              snr = zero
              call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
              call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 1_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              wi = zero
           else if( abs( b( 2_ilp64, 2_ilp64 ) )<=ulp ) then
              call stdlib_I64_slartg( a( 2_ilp64, 2_ilp64 ), a( 2_ilp64, 1_ilp64 ), csr, snr, t )
              snr = -snr
              call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
              call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
              csl = one
              snl = zero
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 2_ilp64 ) = zero
              wi = zero
           else
              ! b is nonsingular, first compute the eigenvalues of (a,b)
              call stdlib_I64_slag2( a, lda, b, ldb, safmin, scale1, scale2, wr1, wr2,wi )
              if( wi==zero ) then
                 ! two real eigenvalues, compute s*a-w*b
                 h1 = scale1*a( 1_ilp64, 1_ilp64 ) - wr1*b( 1_ilp64, 1_ilp64 )
                 h2 = scale1*a( 1_ilp64, 2_ilp64 ) - wr1*b( 1_ilp64, 2_ilp64 )
                 h3 = scale1*a( 2_ilp64, 2_ilp64 ) - wr1*b( 2_ilp64, 2_ilp64 )
                 rr = stdlib_I64_slapy2( h1, h2 )
                 qq = stdlib_I64_slapy2( scale1*a( 2_ilp64, 1_ilp64 ), h3 )
                 if( rr>qq ) then
                    ! find right rotation matrix to zero 1,1 element of
                    ! (sa - wb)
                    call stdlib_I64_slartg( h2, h1, csr, snr, t )
                 else
                    ! find right rotation matrix to zero 2,1 element of
                    ! (sa - wb)
                    call stdlib_I64_slartg( h3, scale1*a( 2_ilp64, 1_ilp64 ), csr, snr, t )
                 end if
                 snr = -snr
                 call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 ! compute inf norms of a and b
                 h1 = max( abs( a( 1_ilp64, 1_ilp64 ) )+abs( a( 1_ilp64, 2_ilp64 ) ),abs( a( 2_ilp64, 1_ilp64 ) )+abs( a( 2_ilp64,&
                     & 2_ilp64 ) ) )
                           
                 h2 = max( abs( b( 1_ilp64, 1_ilp64 ) )+abs( b( 1_ilp64, 2_ilp64 ) ),abs( b( 2_ilp64, 1_ilp64 ) )+abs( b( 2_ilp64,&
                     & 2_ilp64 ) ) )
                           
                 if( ( scale1*h1 )>=abs( wr1 )*h2 ) then
                    ! find left rotation matrix q to zero out b(2,1)
                    call stdlib_I64_slartg( b( 1_ilp64, 1_ilp64 ), b( 2_ilp64, 1_ilp64 ), csl, snl, r )
                 else
                    ! find left rotation matrix q to zero out a(2,1)
                    call stdlib_I64_slartg( a( 1_ilp64, 1_ilp64 ), a( 2_ilp64, 1_ilp64 ), csl, snl, r )
                 end if
                 call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
                 call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
                 a( 2_ilp64, 1_ilp64 ) = zero
                 b( 2_ilp64, 1_ilp64 ) = zero
              else
                 ! a pair of complex conjugate eigenvalues
                 ! first compute the svd of the matrix b
                 call stdlib_I64_slasv2( b( 1_ilp64, 1_ilp64 ), b( 1_ilp64, 2_ilp64 ), b( 2_ilp64, 2_ilp64 ), r, t, snr,csr, snl,&
                     & csl )
                           
                 ! form (a,b) := q(a,b)z**t where q is left rotation matrix and
                 ! z is right rotation matrix computed from stdlib_I64_slasv2
                 call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
                 call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
                 call stdlib_I64_srot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 call stdlib_I64_srot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 b( 2_ilp64, 1_ilp64 ) = zero
                 b( 1_ilp64, 2_ilp64 ) = zero
              end if
           end if
           ! unscaling
           a( 1_ilp64, 1_ilp64 ) = anorm*a( 1_ilp64, 1_ilp64 )
           a( 2_ilp64, 1_ilp64 ) = anorm*a( 2_ilp64, 1_ilp64 )
           a( 1_ilp64, 2_ilp64 ) = anorm*a( 1_ilp64, 2_ilp64 )
           a( 2_ilp64, 2_ilp64 ) = anorm*a( 2_ilp64, 2_ilp64 )
           b( 1_ilp64, 1_ilp64 ) = bnorm*b( 1_ilp64, 1_ilp64 )
           b( 2_ilp64, 1_ilp64 ) = bnorm*b( 2_ilp64, 1_ilp64 )
           b( 1_ilp64, 2_ilp64 ) = bnorm*b( 1_ilp64, 2_ilp64 )
           b( 2_ilp64, 2_ilp64 ) = bnorm*b( 2_ilp64, 2_ilp64 )
           if( wi==zero ) then
              alphar( 1_ilp64 ) = a( 1_ilp64, 1_ilp64 )
              alphar( 2_ilp64 ) = a( 2_ilp64, 2_ilp64 )
              alphai( 1_ilp64 ) = zero
              alphai( 2_ilp64 ) = zero
              beta( 1_ilp64 ) = b( 1_ilp64, 1_ilp64 )
              beta( 2_ilp64 ) = b( 2_ilp64, 2_ilp64 )
           else
              alphar( 1_ilp64 ) = anorm*wr1 / scale1 / bnorm
              alphai( 1_ilp64 ) = anorm*wi / scale1 / bnorm
              alphar( 2_ilp64 ) = alphar( 1_ilp64 )
              alphai( 2_ilp64 ) = -alphai( 1_ilp64 )
              beta( 1_ilp64 ) = one
              beta( 2_ilp64 ) = one
           end if
           return
     end subroutine stdlib_I64_slagv2

     pure module subroutine stdlib_I64_dlagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
     !! DLAGV2 computes the Generalized Schur factorization of a real 2-by-2
     !! matrix pencil (A,B) where B is upper triangular. This routine
     !! computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
     !! SNR such that
     !! 1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
     !! types), then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],
     !! 2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
     !! then
     !! [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
     !! [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
     !! [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
     !! [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]
     !! where b11 >= b22 > 0.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: lda, ldb
           real(dp), intent(out) :: csl, csr, snl, snr
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(2_ilp64), alphar(2_ilp64), beta(2_ilp64)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: anorm, ascale, bnorm, bscale, h1, h2, h3, qq, r, rr, safmin, scale1, &
                     scale2, t, ulp, wi, wr1, wr2
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_I64_dlamch( 'S' )
           ulp = stdlib_I64_dlamch( 'P' )
           ! scale a
           anorm = max( abs( a( 1_ilp64, 1_ilp64 ) )+abs( a( 2_ilp64, 1_ilp64 ) ),abs( a( 1_ilp64, 2_ilp64 ) )+abs( a( 2_ilp64,&
               & 2_ilp64 ) ), &
                     safmin )
           ascale = one / anorm
           a( 1_ilp64, 1_ilp64 ) = ascale*a( 1_ilp64, 1_ilp64 )
           a( 1_ilp64, 2_ilp64 ) = ascale*a( 1_ilp64, 2_ilp64 )
           a( 2_ilp64, 1_ilp64 ) = ascale*a( 2_ilp64, 1_ilp64 )
           a( 2_ilp64, 2_ilp64 ) = ascale*a( 2_ilp64, 2_ilp64 )
           ! scale b
           bnorm = max( abs( b( 1_ilp64, 1_ilp64 ) ), abs( b( 1_ilp64, 2_ilp64 ) )+abs( b( 2_ilp64, 2_ilp64 ) ),safmin )
           bscale = one / bnorm
           b( 1_ilp64, 1_ilp64 ) = bscale*b( 1_ilp64, 1_ilp64 )
           b( 1_ilp64, 2_ilp64 ) = bscale*b( 1_ilp64, 2_ilp64 )
           b( 2_ilp64, 2_ilp64 ) = bscale*b( 2_ilp64, 2_ilp64 )
           ! check if a can be deflated
           if( abs( a( 2_ilp64, 1_ilp64 ) )<=ulp ) then
              csl = one
              snl = zero
              csr = one
              snr = zero
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              wi = zero
           ! check if b is singular
           else if( abs( b( 1_ilp64, 1_ilp64 ) )<=ulp ) then
              call stdlib_I64_dlartg( a( 1_ilp64, 1_ilp64 ), a( 2_ilp64, 1_ilp64 ), csl, snl, r )
              csr = one
              snr = zero
              call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
              call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 1_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              wi = zero
           else if( abs( b( 2_ilp64, 2_ilp64 ) )<=ulp ) then
              call stdlib_I64_dlartg( a( 2_ilp64, 2_ilp64 ), a( 2_ilp64, 1_ilp64 ), csr, snr, t )
              snr = -snr
              call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
              call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
              csl = one
              snl = zero
              a( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 1_ilp64 ) = zero
              b( 2_ilp64, 2_ilp64 ) = zero
              wi = zero
           else
              ! b is nonsingular, first compute the eigenvalues of (a,b)
              call stdlib_I64_dlag2( a, lda, b, ldb, safmin, scale1, scale2, wr1, wr2,wi )
              if( wi==zero ) then
                 ! two real eigenvalues, compute s*a-w*b
                 h1 = scale1*a( 1_ilp64, 1_ilp64 ) - wr1*b( 1_ilp64, 1_ilp64 )
                 h2 = scale1*a( 1_ilp64, 2_ilp64 ) - wr1*b( 1_ilp64, 2_ilp64 )
                 h3 = scale1*a( 2_ilp64, 2_ilp64 ) - wr1*b( 2_ilp64, 2_ilp64 )
                 rr = stdlib_I64_dlapy2( h1, h2 )
                 qq = stdlib_I64_dlapy2( scale1*a( 2_ilp64, 1_ilp64 ), h3 )
                 if( rr>qq ) then
                    ! find right rotation matrix to zero 1,1 element of
                    ! (sa - wb)
                    call stdlib_I64_dlartg( h2, h1, csr, snr, t )
                 else
                    ! find right rotation matrix to zero 2,1 element of
                    ! (sa - wb)
                    call stdlib_I64_dlartg( h3, scale1*a( 2_ilp64, 1_ilp64 ), csr, snr, t )
                 end if
                 snr = -snr
                 call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 ! compute inf norms of a and b
                 h1 = max( abs( a( 1_ilp64, 1_ilp64 ) )+abs( a( 1_ilp64, 2_ilp64 ) ),abs( a( 2_ilp64, 1_ilp64 ) )+abs( a( 2_ilp64,&
                     & 2_ilp64 ) ) )
                           
                 h2 = max( abs( b( 1_ilp64, 1_ilp64 ) )+abs( b( 1_ilp64, 2_ilp64 ) ),abs( b( 2_ilp64, 1_ilp64 ) )+abs( b( 2_ilp64,&
                     & 2_ilp64 ) ) )
                           
                 if( ( scale1*h1 )>=abs( wr1 )*h2 ) then
                    ! find left rotation matrix q to zero out b(2,1)
                    call stdlib_I64_dlartg( b( 1_ilp64, 1_ilp64 ), b( 2_ilp64, 1_ilp64 ), csl, snl, r )
                 else
                    ! find left rotation matrix q to zero out a(2,1)
                    call stdlib_I64_dlartg( a( 1_ilp64, 1_ilp64 ), a( 2_ilp64, 1_ilp64 ), csl, snl, r )
                 end if
                 call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
                 call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
                 a( 2_ilp64, 1_ilp64 ) = zero
                 b( 2_ilp64, 1_ilp64 ) = zero
              else
                 ! a pair of complex conjugate eigenvalues
                 ! first compute the svd of the matrix b
                 call stdlib_I64_dlasv2( b( 1_ilp64, 1_ilp64 ), b( 1_ilp64, 2_ilp64 ), b( 2_ilp64, 2_ilp64 ), r, t, snr,csr, snl,&
                     & csl )
                           
                 ! form (a,b) := q(a,b)z**t where q is left rotation matrix and
                 ! z is right rotation matrix computed from stdlib_I64_dlasv2
                 call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), lda, a( 2_ilp64, 1_ilp64 ), lda, csl, snl )
                 call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), ldb, b( 2_ilp64, 1_ilp64 ), ldb, csl, snl )
                 call stdlib_I64_drot( 2_ilp64, a( 1_ilp64, 1_ilp64 ), 1_ilp64, a( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 call stdlib_I64_drot( 2_ilp64, b( 1_ilp64, 1_ilp64 ), 1_ilp64, b( 1_ilp64, 2_ilp64 ), 1_ilp64, csr, snr )
                 b( 2_ilp64, 1_ilp64 ) = zero
                 b( 1_ilp64, 2_ilp64 ) = zero
              end if
           end if
           ! unscaling
           a( 1_ilp64, 1_ilp64 ) = anorm*a( 1_ilp64, 1_ilp64 )
           a( 2_ilp64, 1_ilp64 ) = anorm*a( 2_ilp64, 1_ilp64 )
           a( 1_ilp64, 2_ilp64 ) = anorm*a( 1_ilp64, 2_ilp64 )
           a( 2_ilp64, 2_ilp64 ) = anorm*a( 2_ilp64, 2_ilp64 )
           b( 1_ilp64, 1_ilp64 ) = bnorm*b( 1_ilp64, 1_ilp64 )
           b( 2_ilp64, 1_ilp64 ) = bnorm*b( 2_ilp64, 1_ilp64 )
           b( 1_ilp64, 2_ilp64 ) = bnorm*b( 1_ilp64, 2_ilp64 )
           b( 2_ilp64, 2_ilp64 ) = bnorm*b( 2_ilp64, 2_ilp64 )
           if( wi==zero ) then
              alphar( 1_ilp64 ) = a( 1_ilp64, 1_ilp64 )
              alphar( 2_ilp64 ) = a( 2_ilp64, 2_ilp64 )
              alphai( 1_ilp64 ) = zero
              alphai( 2_ilp64 ) = zero
              beta( 1_ilp64 ) = b( 1_ilp64, 1_ilp64 )
              beta( 2_ilp64 ) = b( 2_ilp64, 2_ilp64 )
           else
              alphar( 1_ilp64 ) = anorm*wr1 / scale1 / bnorm
              alphai( 1_ilp64 ) = anorm*wi / scale1 / bnorm
              alphar( 2_ilp64 ) = alphar( 1_ilp64 )
              alphai( 2_ilp64 ) = -alphai( 1_ilp64 )
              beta( 1_ilp64 ) = one
              beta( 2_ilp64 ) = one
           end if
           return
     end subroutine stdlib_I64_dlagv2




     pure module subroutine stdlib_I64_stgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! STGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of real matrices (S,P), where S is a quasi-triangular matrix
     !! and P is upper triangular.  Matrix pairs of this type are produced by
     !! the generalized Schur factorization of a matrix pair (A,B):
     !! A = Q*S*Z**T,  B = Q*P*Z**T
     !! as computed by SGGHRD + SHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal blocks of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the orthogonal factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(in) :: p(ldp,*), s(lds,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: safety = 1.0e+2_sp
           
           ! Local Scalars 
           logical(lk) :: compl, compr, il2by2, ilabad, ilall, ilback, ilbbad, ilcomp, ilcplx, &
                     lsa, lsb
           integer(ilp64) :: i, ibeg, ieig, iend, ihwmny, iinfo, im, iside, j, ja, jc, je, jr, jw, &
                     na, nw
           real(sp) :: acoef, acoefa, anorm, ascale, bcoefa, bcoefi, bcoefr, big, bignum, bnorm, &
           bscale, cim2a, cim2b, cimaga, cimagb, cre2a, cre2b, creala, crealb, dmin, safmin, &
                     salfar, sbeta, scale, small, temp, temp2, temp2i, temp2r, ulp, xmax, xscale
           ! Local Arrays 
           real(sp) :: bdiag(2_ilp64), sum(2_ilp64,2_ilp64), sums(2_ilp64,2_ilp64), sump(2_ilp64,2_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp64
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp64
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp64
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp64
              ilall = .true.
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp64
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp64
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp64
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp64
           end if
           info = 0_ilp64
           if( iside<0_ilp64 ) then
              info = -1_ilp64
           else if( ihwmny<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lds<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldp<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGEVC', -info )
              return
           end if
           ! count the number of eigenvectors to be computed
           if( .not.ilall ) then
              im = 0_ilp64
              ilcplx = .false.
              loop_10: do j = 1, n
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_10
                 end if
                 if( j<n ) then
                    if( s( j+1, j )/=zero )ilcplx = .true.
                 end if
                 if( ilcplx ) then
                    if( select( j ) .or. select( j+1 ) )im = im + 2_ilp64
                 else
                    if( select( j ) )im = im + 1_ilp64
                 end if
              end do loop_10
           else
              im = n
           end if
           ! check 2-by-2 diagonal blocks of a, b
           ilabad = .false.
           ilbbad = .false.
           do j = 1, n - 1
              if( s( j+1, j )/=zero ) then
                 if( p( j, j )==zero .or. p( j+1, j+1 )==zero .or.p( j, j+1 )/=zero )ilbbad = &
                           .true.
                 if( j<n-1 ) then
                    if( s( j+2, j+1 )/=zero )ilabad = .true.
                 end if
              end if
           end do
           if( ilabad ) then
              info = -5_ilp64
           else if( ilbbad ) then
              info = -7_ilp64
           else if( compl .and. ldvl<n .or. ldvl<1_ilp64 ) then
              info = -10_ilp64
           else if( compr .and. ldvr<n .or. ldvr<1_ilp64 ) then
              info = -12_ilp64
           else if( mm<im ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_I64_slabad( safmin, big )
           ulp = stdlib_I64_slamch( 'EPSILON' )*stdlib_I64_slamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part (i.e., excluding all elements belonging to the diagonal
           ! blocks) of a and b to check for possible overflow in the
           ! triangular solver.
           anorm = abs( s( 1_ilp64, 1_ilp64 ) )
           if( n>1_ilp64 )anorm = anorm + abs( s( 2_ilp64, 1_ilp64 ) )
           bnorm = abs( p( 1_ilp64, 1_ilp64 ) )
           work( 1_ilp64 ) = zero
           work( n+1 ) = zero
           do j = 2, n
              temp = zero
              temp2 = zero
              if( s( j, j-1 )==zero ) then
                 iend = j - 1_ilp64
              else
                 iend = j - 2_ilp64
              end if
              do i = 1, iend
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              work( j ) = temp
              work( n+j ) = temp2
              do i = iend + 1, min( j+1, n )
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              anorm = max( anorm, temp )
              bnorm = max( bnorm, temp2 )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp64
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_220: do je = 1, n
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at.
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_220
                 end if
                 nw = 1_ilp64
                 if( je<n ) then
                    if( s( je+1, je )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp64
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je+1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_220
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       ieig = ieig + 1_ilp64
                       do jr = 1, n
                          vl( jr, ieig ) = zero
                       end do
                       vl( ieig, ieig ) = one
                       cycle loop_220
                    end if
                 end if
                 ! clear vector
                 do jr = 1, nw*n
                    work( 2_ilp64*n+jr ) = zero
                 end do
                                                       ! t
                 ! compute coefficients in  ( a a - b b )  y = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp64*n+je ) = one
                    xmax = one
                 else
                    ! complex eigenvalue
                    call stdlib_I64_slag2( s( je, je ), lds, p( je, je ), ldp,safmin*safety, acoef, &
                              temp, bcoefr, temp2,bcoefi )
                    bcoefi = -bcoefi
                    if( bcoefi==zero ) then
                       info = je
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    temp = acoef*s( je+1, je )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp64*n+je ) = one
                       work( 3_ilp64*n+je ) = zero
                       work( 2_ilp64*n+je+1 ) = -temp2r / temp
                       work( 3_ilp64*n+je+1 ) = -temp2i / temp
                    else
                       work( 2_ilp64*n+je+1 ) = one
                       work( 3_ilp64*n+je+1 ) = zero
                       temp = acoef*s( je, je+1 )
                       work( 2_ilp64*n+je ) = ( bcoefr*p( je+1, je+1 )-acoef*s( je+1, je+1 ) ) / &
                                 temp
                       work( 3_ilp64*n+je ) = bcoefi*p( je+1, je+1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp64*n+je ) )+abs( work( 3_ilp64*n+je ) ),abs( work( 2_ilp64*n+je+1 ) &
                              )+abs( work( 3_ilp64*n+je+1 ) ) )
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                 ! t
                 ! triangular solve of  (a a - b b)  y = 0
                                         ! t
                 ! (rowwise in  (a a - b b) , or columnwise in (a a - b b) )
                 il2by2 = .false.
                 loop_160: do j = je + nw, n
                    if( il2by2 ) then
                       il2by2 = .false.
                       cycle loop_160
                    end if
                    na = 1_ilp64
                    bdiag( 1_ilp64 ) = p( j, j )
                    if( j<n ) then
                       if( s( j+1, j )/=zero ) then
                          il2by2 = .true.
                          bdiag( 2_ilp64 ) = p( j+1, j+1 )
                          na = 2_ilp64
                       end if
                    end if
                    ! check whether scaling is necessary for dot products
                    xscale = one / max( one, xmax )
                    temp = max( work( j ), work( n+j ),acoefa*work( j )+bcoefa*work( n+j ) )
                              
                    if( il2by2 )temp = max( temp, work( j+1 ), work( n+j+1 ),acoefa*work( j+1 )+&
                              bcoefa*work( n+j+1 ) )
                    if( temp>bignum*xscale ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = xmax*xscale
                    end if
                    ! compute dot products
                          ! j-1
                    ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                          ! k=je
                    ! to reduce the op count, this is done as
                    ! _        j-1                  _        j-1
                    ! a*conjg( sum  s(k,j)*x(k) ) - b*conjg( sum  p(k,j)*x(k) )
                             ! k=je                          k=je
                    ! which may cause underflow problems if a or b are close
                    ! to underflow.  (e.g., less than small.)
                    do jw = 1, nw
                       do ja = 1, na
                          sums( ja, jw ) = zero
                          sump( ja, jw ) = zero
                          do jr = je, j - 1
                             sums( ja, jw ) = sums( ja, jw ) +s( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                             sump( ja, jw ) = sump( ja, jw ) +p( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                          end do
                       end do
                    end do
                    do ja = 1, na
                       if( ilcplx ) then
                          sum( ja, 1_ilp64 ) = -acoef*sums( ja, 1_ilp64 ) +bcoefr*sump( ja, 1_ilp64 ) -bcoefi*sump( &
                                    ja, 2_ilp64 )
                          sum( ja, 2_ilp64 ) = -acoef*sums( ja, 2_ilp64 ) +bcoefr*sump( ja, 2_ilp64 ) +bcoefi*sump( &
                                    ja, 1_ilp64 )
                       else
                          sum( ja, 1_ilp64 ) = -acoef*sums( ja, 1_ilp64 ) +bcoefr*sump( ja, 1_ilp64 )
                       end if
                    end do
                                        ! t
                    ! solve  ( a a - b b )  y = sum(,)
                    ! with scaling and perturbation of the denominator
                    call stdlib_I64_slaln2( .true., na, nw, dmin, acoef, s( j, j ), lds,bdiag( 1_ilp64 ), &
                    bdiag( 2_ilp64 ), sum, 2_ilp64, bcoefr,bcoefi, work( 2_ilp64*n+j ), n, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = scale*xmax
                    end if
                    xmax = max( xmax, temp )
                 end do loop_160
                 ! copy eigenvector to vl, back transforming if
                 ! howmny='b'.
                 ieig = ieig + 1_ilp64
                 if( ilback ) then
                    do jw = 0, nw - 1
                       call stdlib_I64_sgemv( 'N', n, n+1-je, one, vl( 1_ilp64, je ), ldvl,work( ( jw+2 )*n+&
                                 je ), 1_ilp64, zero,work( ( jw+4 )*n+1 ), 1_ilp64 )
                    end do
                    call stdlib_I64_slacpy( ' ', n, nw, work( 4_ilp64*n+1 ), n, vl( 1_ilp64, je ),ldvl )
                    ibeg = 1_ilp64
                 else
                    call stdlib_I64_slacpy( ' ', n, nw, work( 2_ilp64*n+1 ), n, vl( 1_ilp64, ieig ),ldvl )
                    ibeg = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) )+abs( vl( j, ieig+1 ) ) )
                    end do
                 else
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = ibeg, n
                          vl( jr, ieig+jw ) = xscale*vl( jr, ieig+jw )
                       end do
                    end do
                 end if
                 ieig = ieig + nw - 1_ilp64
              end do loop_220
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp64
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_500: do je = n, 1, -1
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at -- if complex, select(je)
                 ! or select(je-1).
                 ! if this is a complex pair, the 2-by-2 diagonal block
                 ! corresponding to the eigenvalue is in rows/columns je-1:je
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_500
                 end if
                 nw = 1_ilp64
                 if( je>1_ilp64 ) then
                    if( s( je, je-1 )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp64
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je-1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_500
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- unit eigenvector
                       ieig = ieig - 1_ilp64
                       do jr = 1, n
                          vr( jr, ieig ) = zero
                       end do
                       vr( ieig, ieig ) = one
                       cycle loop_500
                    end if
                 end if
                 ! clear vector
                 do jw = 0, nw - 1
                    do jr = 1, n
                       work( ( jw+2 )*n+jr ) = zero
                    end do
                 end do
                 ! compute coefficients in  ( a a - b b ) x = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp64*n+je ) = one
                    xmax = one
                    ! compute contribution from column je of a and b to sum
                    ! (see "further details", above.)
                    do jr = 1, je - 1
                       work( 2_ilp64*n+jr ) = bcoefr*p( jr, je ) -acoef*s( jr, je )
                    end do
                 else
                    ! complex eigenvalue
                    call stdlib_I64_slag2( s( je-1, je-1 ), lds, p( je-1, je-1 ), ldp,safmin*safety, &
                              acoef, temp, bcoefr, temp2,bcoefi )
                    if( bcoefi==zero ) then
                       info = je - 1_ilp64
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    ! and contribution to sums
                    temp = acoef*s( je, je-1 )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>=abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp64*n+je ) = one
                       work( 3_ilp64*n+je ) = zero
                       work( 2_ilp64*n+je-1 ) = -temp2r / temp
                       work( 3_ilp64*n+je-1 ) = -temp2i / temp
                    else
                       work( 2_ilp64*n+je-1 ) = one
                       work( 3_ilp64*n+je-1 ) = zero
                       temp = acoef*s( je-1, je )
                       work( 2_ilp64*n+je ) = ( bcoefr*p( je-1, je-1 )-acoef*s( je-1, je-1 ) ) / &
                                 temp
                       work( 3_ilp64*n+je ) = bcoefi*p( je-1, je-1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp64*n+je ) )+abs( work( 3_ilp64*n+je ) ),abs( work( 2_ilp64*n+je-1 ) &
                              )+abs( work( 3_ilp64*n+je-1 ) ) )
                    ! compute contribution from columns je and je-1
                    ! of a and b to the sums.
                    creala = acoef*work( 2_ilp64*n+je-1 )
                    cimaga = acoef*work( 3_ilp64*n+je-1 )
                    crealb = bcoefr*work( 2_ilp64*n+je-1 ) -bcoefi*work( 3_ilp64*n+je-1 )
                    cimagb = bcoefi*work( 2_ilp64*n+je-1 ) +bcoefr*work( 3_ilp64*n+je-1 )
                    cre2a = acoef*work( 2_ilp64*n+je )
                    cim2a = acoef*work( 3_ilp64*n+je )
                    cre2b = bcoefr*work( 2_ilp64*n+je ) - bcoefi*work( 3_ilp64*n+je )
                    cim2b = bcoefi*work( 2_ilp64*n+je ) + bcoefr*work( 3_ilp64*n+je )
                    do jr = 1, je - 2
                       work( 2_ilp64*n+jr ) = -creala*s( jr, je-1 ) +crealb*p( jr, je-1 ) -cre2a*s( jr, &
                                 je ) + cre2b*p( jr, je )
                       work( 3_ilp64*n+jr ) = -cimaga*s( jr, je-1 ) +cimagb*p( jr, je-1 ) -cim2a*s( jr, &
                                 je ) + cim2b*p( jr, je )
                    end do
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                 ! columnwise triangular solve of  (a a - b b)  x = 0
                 il2by2 = .false.
                 loop_370: do j = je - nw, 1, -1
                    ! if a 2-by-2 block, is in position j-1:j, wait until
                    ! next iteration to process it (when it will be j:j+1)
                    if( .not.il2by2 .and. j>1_ilp64 ) then
                       if( s( j, j-1 )/=zero ) then
                          il2by2 = .true.
                          cycle loop_370
                       end if
                    end if
                    bdiag( 1_ilp64 ) = p( j, j )
                    if( il2by2 ) then
                       na = 2_ilp64
                       bdiag( 2_ilp64 ) = p( j+1, j+1 )
                    else
                       na = 1_ilp64
                    end if
                    ! compute x(j) (and x(j+1), if 2-by-2 block)
                    call stdlib_I64_slaln2( .false., na, nw, dmin, acoef, s( j, j ),lds, bdiag( 1_ilp64 ), &
                    bdiag( 2_ilp64 ), work( 2_ilp64*n+j ),n, bcoefr, bcoefi, sum, 2_ilp64, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = 1, je
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                    end if
                    xmax = max( scale*xmax, temp )
                    do jw = 1, nw
                       do ja = 1, na
                          work( ( jw+1 )*n+j+ja-1 ) = sum( ja, jw )
                       end do
                    end do
                    ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                    if( j>1_ilp64 ) then
                       ! check whether scaling is necessary for sum.
                       xscale = one / max( one, xmax )
                       temp = acoefa*work( j ) + bcoefa*work( n+j )
                       if( il2by2 )temp = max( temp, acoefa*work( j+1 )+bcoefa*work( n+j+1 ) )
                                 
                       temp = max( temp, acoefa, bcoefa )
                       if( temp>bignum*xscale ) then
                          do jw = 0, nw - 1
                             do jr = 1, je
                                work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                             end do
                          end do
                          xmax = xmax*xscale
                       end if
                       ! compute the contributions of the off-diagonals of
                       ! column j (and j+1, if 2-by-2 block) of a and b to the
                       ! sums.
                       do ja = 1, na
                          if( ilcplx ) then
                             creala = acoef*work( 2_ilp64*n+j+ja-1 )
                             cimaga = acoef*work( 3_ilp64*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp64*n+j+ja-1 ) -bcoefi*work( 3_ilp64*n+j+ja-1 )
                             cimagb = bcoefi*work( 2_ilp64*n+j+ja-1 ) +bcoefr*work( 3_ilp64*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp64*n+jr ) = work( 2_ilp64*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                                work( 3_ilp64*n+jr ) = work( 3_ilp64*n+jr ) -cimaga*s( jr, j+ja-1 ) +cimagb*p(&
                                           jr, j+ja-1 )
                             end do
                          else
                             creala = acoef*work( 2_ilp64*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp64*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp64*n+jr ) = work( 2_ilp64*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                             end do
                          end if
                       end do
                    end if
                    il2by2 = .false.
                 end do loop_370
                 ! copy eigenvector to vr, back transforming if
                 ! howmny='b'.
                 ieig = ieig - nw
                 if( ilback ) then
                    do jw = 0, nw - 1
                       do jr = 1, n
                          work( ( jw+4 )*n+jr ) = work( ( jw+2 )*n+1 )*vr( jr, 1_ilp64 )
                       end do
                       ! a series of compiler directives to defeat
                       ! vectorization for the next loop
                       do jc = 2, je
                          do jr = 1, n
                             work( ( jw+4 )*n+jr ) = work( ( jw+4 )*n+jr ) +work( ( jw+2 )*n+jc )&
                                       *vr( jr, jc )
                          end do
                       end do
                    end do
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+4 )*n+jr )
                       end do
                    end do
                    iend = n
                 else
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+2 )*n+jr )
                       end do
                    end do
                    iend = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) )+abs( vr( j, ieig+1 ) ) )
                    end do
                 else
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = 1, iend
                          vr( jr, ieig+jw ) = xscale*vr( jr, ieig+jw )
                       end do
                    end do
                 end if
              end do loop_500
           end if
           return
     end subroutine stdlib_I64_stgevc

     pure module subroutine stdlib_I64_dtgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! DTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of real matrices (S,P), where S is a quasi-triangular matrix
     !! and P is upper triangular.  Matrix pairs of this type are produced by
     !! the generalized Schur factorization of a matrix pair (A,B):
     !! A = Q*S*Z**T,  B = Q*P*Z**T
     !! as computed by DGGHRD + DHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal blocks of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the orthogonal factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(in) :: p(ldp,*), s(lds,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: safety = 1.0e+2_dp
           
           ! Local Scalars 
           logical(lk) :: compl, compr, il2by2, ilabad, ilall, ilback, ilbbad, ilcomp, ilcplx, &
                     lsa, lsb
           integer(ilp64) :: i, ibeg, ieig, iend, ihwmny, iinfo, im, iside, j, ja, jc, je, jr, jw, &
                     na, nw
           real(dp) :: acoef, acoefa, anorm, ascale, bcoefa, bcoefi, bcoefr, big, bignum, bnorm, &
           bscale, cim2a, cim2b, cimaga, cimagb, cre2a, cre2b, creala, crealb, dmin, safmin, &
                     salfar, sbeta, scale, small, temp, temp2, temp2i, temp2r, ulp, xmax, xscale
           ! Local Arrays 
           real(dp) :: bdiag(2_ilp64), sum(2_ilp64,2_ilp64), sums(2_ilp64,2_ilp64), sump(2_ilp64,2_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp64
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp64
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp64
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp64
              ilall = .true.
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp64
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp64
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp64
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp64
           end if
           info = 0_ilp64
           if( iside<0_ilp64 ) then
              info = -1_ilp64
           else if( ihwmny<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lds<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldp<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors to be computed
           if( .not.ilall ) then
              im = 0_ilp64
              ilcplx = .false.
              loop_10: do j = 1, n
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_10
                 end if
                 if( j<n ) then
                    if( s( j+1, j )/=zero )ilcplx = .true.
                 end if
                 if( ilcplx ) then
                    if( select( j ) .or. select( j+1 ) )im = im + 2_ilp64
                 else
                    if( select( j ) )im = im + 1_ilp64
                 end if
              end do loop_10
           else
              im = n
           end if
           ! check 2-by-2 diagonal blocks of a, b
           ilabad = .false.
           ilbbad = .false.
           do j = 1, n - 1
              if( s( j+1, j )/=zero ) then
                 if( p( j, j )==zero .or. p( j+1, j+1 )==zero .or.p( j, j+1 )/=zero )ilbbad = &
                           .true.
                 if( j<n-1 ) then
                    if( s( j+2, j+1 )/=zero )ilabad = .true.
                 end if
              end if
           end do
           if( ilabad ) then
              info = -5_ilp64
           else if( ilbbad ) then
              info = -7_ilp64
           else if( compl .and. ldvl<n .or. ldvl<1_ilp64 ) then
              info = -10_ilp64
           else if( compr .and. ldvr<n .or. ldvr<1_ilp64 ) then
              info = -12_ilp64
           else if( mm<im ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_I64_dlabad( safmin, big )
           ulp = stdlib_I64_dlamch( 'EPSILON' )*stdlib_I64_dlamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part (i.e., excluding all elements belonging to the diagonal
           ! blocks) of a and b to check for possible overflow in the
           ! triangular solver.
           anorm = abs( s( 1_ilp64, 1_ilp64 ) )
           if( n>1_ilp64 )anorm = anorm + abs( s( 2_ilp64, 1_ilp64 ) )
           bnorm = abs( p( 1_ilp64, 1_ilp64 ) )
           work( 1_ilp64 ) = zero
           work( n+1 ) = zero
           do j = 2, n
              temp = zero
              temp2 = zero
              if( s( j, j-1 )==zero ) then
                 iend = j - 1_ilp64
              else
                 iend = j - 2_ilp64
              end if
              do i = 1, iend
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              work( j ) = temp
              work( n+j ) = temp2
              do i = iend + 1, min( j+1, n )
                 temp = temp + abs( s( i, j ) )
                 temp2 = temp2 + abs( p( i, j ) )
              end do
              anorm = max( anorm, temp )
              bnorm = max( bnorm, temp2 )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp64
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_220: do je = 1, n
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at.
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_220
                 end if
                 nw = 1_ilp64
                 if( je<n ) then
                    if( s( je+1, je )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp64
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je+1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_220
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       ieig = ieig + 1_ilp64
                       do jr = 1, n
                          vl( jr, ieig ) = zero
                       end do
                       vl( ieig, ieig ) = one
                       cycle loop_220
                    end if
                 end if
                 ! clear vector
                 do jr = 1, nw*n
                    work( 2_ilp64*n+jr ) = zero
                 end do
                                                       ! t
                 ! compute coefficients in  ( a a - b b )  y = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp64*n+je ) = one
                    xmax = one
                 else
                    ! complex eigenvalue
                    call stdlib_I64_dlag2( s( je, je ), lds, p( je, je ), ldp,safmin*safety, acoef, &
                              temp, bcoefr, temp2,bcoefi )
                    bcoefi = -bcoefi
                    if( bcoefi==zero ) then
                       info = je
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    temp = acoef*s( je+1, je )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp64*n+je ) = one
                       work( 3_ilp64*n+je ) = zero
                       work( 2_ilp64*n+je+1 ) = -temp2r / temp
                       work( 3_ilp64*n+je+1 ) = -temp2i / temp
                    else
                       work( 2_ilp64*n+je+1 ) = one
                       work( 3_ilp64*n+je+1 ) = zero
                       temp = acoef*s( je, je+1 )
                       work( 2_ilp64*n+je ) = ( bcoefr*p( je+1, je+1 )-acoef*s( je+1, je+1 ) ) / &
                                 temp
                       work( 3_ilp64*n+je ) = bcoefi*p( je+1, je+1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp64*n+je ) )+abs( work( 3_ilp64*n+je ) ),abs( work( 2_ilp64*n+je+1 ) &
                              )+abs( work( 3_ilp64*n+je+1 ) ) )
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                 ! t
                 ! triangular solve of  (a a - b b)  y = 0
                                         ! t
                 ! (rowwise in  (a a - b b) , or columnwise in (a a - b b) )
                 il2by2 = .false.
                 loop_160: do j = je + nw, n
                    if( il2by2 ) then
                       il2by2 = .false.
                       cycle loop_160
                    end if
                    na = 1_ilp64
                    bdiag( 1_ilp64 ) = p( j, j )
                    if( j<n ) then
                       if( s( j+1, j )/=zero ) then
                          il2by2 = .true.
                          bdiag( 2_ilp64 ) = p( j+1, j+1 )
                          na = 2_ilp64
                       end if
                    end if
                    ! check whether scaling is necessary for dot products
                    xscale = one / max( one, xmax )
                    temp = max( work( j ), work( n+j ),acoefa*work( j )+bcoefa*work( n+j ) )
                              
                    if( il2by2 )temp = max( temp, work( j+1 ), work( n+j+1 ),acoefa*work( j+1 )+&
                              bcoefa*work( n+j+1 ) )
                    if( temp>bignum*xscale ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = xmax*xscale
                    end if
                    ! compute dot products
                          ! j-1
                    ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                          ! k=je
                    ! to reduce the op count, this is done as
                    ! _        j-1                  _        j-1
                    ! a*conjg( sum  s(k,j)*x(k) ) - b*conjg( sum  p(k,j)*x(k) )
                             ! k=je                          k=je
                    ! which may cause underflow problems if a or b are close
                    ! to underflow.  (e.g., less than small.)
                    do jw = 1, nw
                       do ja = 1, na
                          sums( ja, jw ) = zero
                          sump( ja, jw ) = zero
                          do jr = je, j - 1
                             sums( ja, jw ) = sums( ja, jw ) +s( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                             sump( ja, jw ) = sump( ja, jw ) +p( jr, j+ja-1 )*work( ( jw+1 )*n+jr &
                                       )
                          end do
                       end do
                    end do
                    do ja = 1, na
                       if( ilcplx ) then
                          sum( ja, 1_ilp64 ) = -acoef*sums( ja, 1_ilp64 ) +bcoefr*sump( ja, 1_ilp64 ) -bcoefi*sump( &
                                    ja, 2_ilp64 )
                          sum( ja, 2_ilp64 ) = -acoef*sums( ja, 2_ilp64 ) +bcoefr*sump( ja, 2_ilp64 ) +bcoefi*sump( &
                                    ja, 1_ilp64 )
                       else
                          sum( ja, 1_ilp64 ) = -acoef*sums( ja, 1_ilp64 ) +bcoefr*sump( ja, 1_ilp64 )
                       end if
                    end do
                                        ! t
                    ! solve  ( a a - b b )  y = sum(,)
                    ! with scaling and perturbation of the denominator
                    call stdlib_I64_dlaln2( .true., na, nw, dmin, acoef, s( j, j ), lds,bdiag( 1_ilp64 ), &
                    bdiag( 2_ilp64 ), sum, 2_ilp64, bcoefr,bcoefi, work( 2_ilp64*n+j ), n, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = je, j - 1
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                       xmax = scale*xmax
                    end if
                    xmax = max( xmax, temp )
                 end do loop_160
                 ! copy eigenvector to vl, back transforming if
                 ! howmny='b'.
                 ieig = ieig + 1_ilp64
                 if( ilback ) then
                    do jw = 0, nw - 1
                       call stdlib_I64_dgemv( 'N', n, n+1-je, one, vl( 1_ilp64, je ), ldvl,work( ( jw+2 )*n+&
                                 je ), 1_ilp64, zero,work( ( jw+4 )*n+1 ), 1_ilp64 )
                    end do
                    call stdlib_I64_dlacpy( ' ', n, nw, work( 4_ilp64*n+1 ), n, vl( 1_ilp64, je ),ldvl )
                    ibeg = 1_ilp64
                 else
                    call stdlib_I64_dlacpy( ' ', n, nw, work( 2_ilp64*n+1 ), n, vl( 1_ilp64, ieig ),ldvl )
                    ibeg = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) )+abs( vl( j, ieig+1 ) ) )
                    end do
                 else
                    do j = ibeg, n
                       xmax = max( xmax, abs( vl( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = ibeg, n
                          vl( jr, ieig+jw ) = xscale*vl( jr, ieig+jw )
                       end do
                    end do
                 end if
                 ieig = ieig + nw - 1_ilp64
              end do loop_220
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp64
              ! main loop over eigenvalues
              ilcplx = .false.
              loop_500: do je = n, 1, -1
                 ! skip this iteration if (a) howmny='s' and select=.false., or
                 ! (b) this would be the second of a complex pair.
                 ! check for complex eigenvalue, so as to be sure of which
                 ! entry(-ies) of select to look at -- if complex, select(je)
                 ! or select(je-1).
                 ! if this is a complex pair, the 2-by-2 diagonal block
                 ! corresponding to the eigenvalue is in rows/columns je-1:je
                 if( ilcplx ) then
                    ilcplx = .false.
                    cycle loop_500
                 end if
                 nw = 1_ilp64
                 if( je>1_ilp64 ) then
                    if( s( je, je-1 )/=zero ) then
                       ilcplx = .true.
                       nw = 2_ilp64
                    end if
                 end if
                 if( ilall ) then
                    ilcomp = .true.
                 else if( ilcplx ) then
                    ilcomp = select( je ) .or. select( je-1 )
                 else
                    ilcomp = select( je )
                 end if
                 if( .not.ilcomp )cycle loop_500
                 ! decide if (a) singular pencil, (b) real eigenvalue, or
                 ! (c) complex eigenvalue.
                 if( .not.ilcplx ) then
                    if( abs( s( je, je ) )<=safmin .and.abs( p( je, je ) )<=safmin ) then
                       ! singular matrix pencil -- unit eigenvector
                       ieig = ieig - 1_ilp64
                       do jr = 1, n
                          vr( jr, ieig ) = zero
                       end do
                       vr( ieig, ieig ) = one
                       cycle loop_500
                    end if
                 end if
                 ! clear vector
                 do jw = 0, nw - 1
                    do jr = 1, n
                       work( ( jw+2 )*n+jr ) = zero
                    end do
                 end do
                 ! compute coefficients in  ( a a - b b ) x = 0
                    ! a  is  acoef
                    ! b  is  bcoefr + i*bcoefi
                 if( .not.ilcplx ) then
                    ! real eigenvalue
                    temp = one / max( abs( s( je, je ) )*ascale,abs( p( je, je ) )*bscale, safmin &
                              )
                    salfar = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*p( je, je ) )*bscale
                    acoef = sbeta*ascale
                    bcoefr = salfar*bscale
                    bcoefi = zero
                    ! scale to avoid underflow
                    scale = one
                    lsa = abs( sbeta )>=safmin .and. abs( acoef )<small
                    lsb = abs( salfar )>=safmin .and. abs( bcoefr )<small
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs( salfar ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoef ),abs( bcoefr ) ) ) &
                                 )
                       if( lsa ) then
                          acoef = ascale*( scale*sbeta )
                       else
                          acoef = scale*acoef
                       end if
                       if( lsb ) then
                          bcoefr = bscale*( scale*salfar )
                       else
                          bcoefr = scale*bcoefr
                       end if
                    end if
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr )
                    ! first component is 1
                    work( 2_ilp64*n+je ) = one
                    xmax = one
                    ! compute contribution from column je of a and b to sum
                    ! (see "further details", above.)
                    do jr = 1, je - 1
                       work( 2_ilp64*n+jr ) = bcoefr*p( jr, je ) -acoef*s( jr, je )
                    end do
                 else
                    ! complex eigenvalue
                    call stdlib_I64_dlag2( s( je-1, je-1 ), lds, p( je-1, je-1 ), ldp,safmin*safety, &
                              acoef, temp, bcoefr, temp2,bcoefi )
                    if( bcoefi==zero ) then
                       info = je - 1_ilp64
                       return
                    end if
                    ! scale to avoid over/underflow
                    acoefa = abs( acoef )
                    bcoefa = abs( bcoefr ) + abs( bcoefi )
                    scale = one
                    if( acoefa*ulp<safmin .and. acoefa>=safmin )scale = ( safmin / ulp ) / &
                              acoefa
                    if( bcoefa*ulp<safmin .and. bcoefa>=safmin )scale = max( scale, ( safmin / &
                              ulp ) / bcoefa )
                    if( safmin*acoefa>ascale )scale = ascale / ( safmin*acoefa )
                    if( safmin*bcoefa>bscale )scale = min( scale, bscale / ( safmin*bcoefa ) )
                              
                    if( scale/=one ) then
                       acoef = scale*acoef
                       acoefa = abs( acoef )
                       bcoefr = scale*bcoefr
                       bcoefi = scale*bcoefi
                       bcoefa = abs( bcoefr ) + abs( bcoefi )
                    end if
                    ! compute first two components of eigenvector
                    ! and contribution to sums
                    temp = acoef*s( je, je-1 )
                    temp2r = acoef*s( je, je ) - bcoefr*p( je, je )
                    temp2i = -bcoefi*p( je, je )
                    if( abs( temp )>=abs( temp2r )+abs( temp2i ) ) then
                       work( 2_ilp64*n+je ) = one
                       work( 3_ilp64*n+je ) = zero
                       work( 2_ilp64*n+je-1 ) = -temp2r / temp
                       work( 3_ilp64*n+je-1 ) = -temp2i / temp
                    else
                       work( 2_ilp64*n+je-1 ) = one
                       work( 3_ilp64*n+je-1 ) = zero
                       temp = acoef*s( je-1, je )
                       work( 2_ilp64*n+je ) = ( bcoefr*p( je-1, je-1 )-acoef*s( je-1, je-1 ) ) / &
                                 temp
                       work( 3_ilp64*n+je ) = bcoefi*p( je-1, je-1 ) / temp
                    end if
                    xmax = max( abs( work( 2_ilp64*n+je ) )+abs( work( 3_ilp64*n+je ) ),abs( work( 2_ilp64*n+je-1 ) &
                              )+abs( work( 3_ilp64*n+je-1 ) ) )
                    ! compute contribution from columns je and je-1
                    ! of a and b to the sums.
                    creala = acoef*work( 2_ilp64*n+je-1 )
                    cimaga = acoef*work( 3_ilp64*n+je-1 )
                    crealb = bcoefr*work( 2_ilp64*n+je-1 ) -bcoefi*work( 3_ilp64*n+je-1 )
                    cimagb = bcoefi*work( 2_ilp64*n+je-1 ) +bcoefr*work( 3_ilp64*n+je-1 )
                    cre2a = acoef*work( 2_ilp64*n+je )
                    cim2a = acoef*work( 3_ilp64*n+je )
                    cre2b = bcoefr*work( 2_ilp64*n+je ) - bcoefi*work( 3_ilp64*n+je )
                    cim2b = bcoefi*work( 2_ilp64*n+je ) + bcoefr*work( 3_ilp64*n+je )
                    do jr = 1, je - 2
                       work( 2_ilp64*n+jr ) = -creala*s( jr, je-1 ) +crealb*p( jr, je-1 ) -cre2a*s( jr, &
                                 je ) + cre2b*p( jr, je )
                       work( 3_ilp64*n+jr ) = -cimaga*s( jr, je-1 ) +cimagb*p( jr, je-1 ) -cim2a*s( jr, &
                                 je ) + cim2b*p( jr, je )
                    end do
                 end if
                 dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                 ! columnwise triangular solve of  (a a - b b)  x = 0
                 il2by2 = .false.
                 loop_370: do j = je - nw, 1, -1
                    ! if a 2-by-2 block, is in position j-1:j, wait until
                    ! next iteration to process it (when it will be j:j+1)
                    if( .not.il2by2 .and. j>1_ilp64 ) then
                       if( s( j, j-1 )/=zero ) then
                          il2by2 = .true.
                          cycle loop_370
                       end if
                    end if
                    bdiag( 1_ilp64 ) = p( j, j )
                    if( il2by2 ) then
                       na = 2_ilp64
                       bdiag( 2_ilp64 ) = p( j+1, j+1 )
                    else
                       na = 1_ilp64
                    end if
                    ! compute x(j) (and x(j+1), if 2-by-2 block)
                    call stdlib_I64_dlaln2( .false., na, nw, dmin, acoef, s( j, j ),lds, bdiag( 1_ilp64 ), &
                    bdiag( 2_ilp64 ), work( 2_ilp64*n+j ),n, bcoefr, bcoefi, sum, 2_ilp64, scale, temp,iinfo )
                              
                    if( scale<one ) then
                       do jw = 0, nw - 1
                          do jr = 1, je
                             work( ( jw+2 )*n+jr ) = scale*work( ( jw+2 )*n+jr )
                          end do
                       end do
                    end if
                    xmax = max( scale*xmax, temp )
                    do jw = 1, nw
                       do ja = 1, na
                          work( ( jw+1 )*n+j+ja-1 ) = sum( ja, jw )
                       end do
                    end do
                    ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                    if( j>1_ilp64 ) then
                       ! check whether scaling is necessary for sum.
                       xscale = one / max( one, xmax )
                       temp = acoefa*work( j ) + bcoefa*work( n+j )
                       if( il2by2 )temp = max( temp, acoefa*work( j+1 )+bcoefa*work( n+j+1 ) )
                                 
                       temp = max( temp, acoefa, bcoefa )
                       if( temp>bignum*xscale ) then
                          do jw = 0, nw - 1
                             do jr = 1, je
                                work( ( jw+2 )*n+jr ) = xscale*work( ( jw+2 )*n+jr )
                             end do
                          end do
                          xmax = xmax*xscale
                       end if
                       ! compute the contributions of the off-diagonals of
                       ! column j (and j+1, if 2-by-2 block) of a and b to the
                       ! sums.
                       do ja = 1, na
                          if( ilcplx ) then
                             creala = acoef*work( 2_ilp64*n+j+ja-1 )
                             cimaga = acoef*work( 3_ilp64*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp64*n+j+ja-1 ) -bcoefi*work( 3_ilp64*n+j+ja-1 )
                             cimagb = bcoefi*work( 2_ilp64*n+j+ja-1 ) +bcoefr*work( 3_ilp64*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp64*n+jr ) = work( 2_ilp64*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                                work( 3_ilp64*n+jr ) = work( 3_ilp64*n+jr ) -cimaga*s( jr, j+ja-1 ) +cimagb*p(&
                                           jr, j+ja-1 )
                             end do
                          else
                             creala = acoef*work( 2_ilp64*n+j+ja-1 )
                             crealb = bcoefr*work( 2_ilp64*n+j+ja-1 )
                             do jr = 1, j - 1
                                work( 2_ilp64*n+jr ) = work( 2_ilp64*n+jr ) -creala*s( jr, j+ja-1 ) +crealb*p(&
                                           jr, j+ja-1 )
                             end do
                          end if
                       end do
                    end if
                    il2by2 = .false.
                 end do loop_370
                 ! copy eigenvector to vr, back transforming if
                 ! howmny='b'.
                 ieig = ieig - nw
                 if( ilback ) then
                    do jw = 0, nw - 1
                       do jr = 1, n
                          work( ( jw+4 )*n+jr ) = work( ( jw+2 )*n+1 )*vr( jr, 1_ilp64 )
                       end do
                       ! a series of compiler directives to defeat
                       ! vectorization for the next loop
                       do jc = 2, je
                          do jr = 1, n
                             work( ( jw+4 )*n+jr ) = work( ( jw+4 )*n+jr ) +work( ( jw+2 )*n+jc )&
                                       *vr( jr, jc )
                          end do
                       end do
                    end do
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+4 )*n+jr )
                       end do
                    end do
                    iend = n
                 else
                    do jw = 0, nw - 1
                       do jr = 1, n
                          vr( jr, ieig+jw ) = work( ( jw+2 )*n+jr )
                       end do
                    end do
                    iend = je
                 end if
                 ! scale eigenvector
                 xmax = zero
                 if( ilcplx ) then
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) )+abs( vr( j, ieig+1 ) ) )
                    end do
                 else
                    do j = 1, iend
                       xmax = max( xmax, abs( vr( j, ieig ) ) )
                    end do
                 end if
                 if( xmax>safmin ) then
                    xscale = one / xmax
                    do jw = 0, nw - 1
                       do jr = 1, iend
                          vr( jr, ieig+jw ) = xscale*vr( jr, ieig+jw )
                       end do
                    end do
                 end if
              end do loop_500
           end if
           return
     end subroutine stdlib_I64_dtgevc


     pure module subroutine stdlib_I64_ctgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! CTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of complex matrices (S,P), where S and P are upper triangular.
     !! Matrix pairs of this type are produced by the generalized Schur
     !! factorization of a complex matrix pair (A,B):
     !! A = Q*S*Z**H,  B = Q*P*Z**H
     !! as computed by CGGHRD + CHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal elements of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the unitary factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: p(ldp,*), s(lds,*)
           complex(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: compl, compr, ilall, ilback, ilbbad, ilcomp, lsa, lsb
           integer(ilp64) :: i, ibeg, ieig, iend, ihwmny, im, iside, isrc, j, je, jr
           real(sp) :: acoefa, acoeff, anorm, ascale, bcoefa, big, bignum, bnorm, bscale, dmin, &
                     safmin, sbeta, scale, small, temp, ulp, xmax
           complex(sp) :: bcoeff, ca, cb, d, salpha, sum, suma, sumb, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp64
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp64
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp64
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp64
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp64
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp64
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp64
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp64
           end if
           info = 0_ilp64
           if( iside<0_ilp64 ) then
              info = -1_ilp64
           else if( ihwmny<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lds<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldp<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors
           if( .not.ilall ) then
              im = 0_ilp64
              do j = 1, n
                 if( select( j ) )im = im + 1_ilp64
              end do
           else
              im = n
           end if
           ! check diagonal of b
           ilbbad = .false.
           do j = 1, n
              if( aimag( p( j, j ) )/=zero )ilbbad = .true.
           end do
           if( ilbbad ) then
              info = -7_ilp64
           else if( compl .and. ldvl<n .or. ldvl<1_ilp64 ) then
              info = -10_ilp64
           else if( compr .and. ldvr<n .or. ldvr<1_ilp64 ) then
              info = -12_ilp64
           else if( mm<im ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_I64_slabad( safmin, big )
           ulp = stdlib_I64_slamch( 'EPSILON' )*stdlib_I64_slamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part of a and b to check for possible overflow in the triangular
           ! solver.
           anorm = abs1( s( 1_ilp64, 1_ilp64 ) )
           bnorm = abs1( p( 1_ilp64, 1_ilp64 ) )
           rwork( 1_ilp64 ) = zero
           rwork( n+1 ) = zero
           do j = 2, n
              rwork( j ) = zero
              rwork( n+j ) = zero
              do i = 1, j - 1
                 rwork( j ) = rwork( j ) + abs1( s( i, j ) )
                 rwork( n+j ) = rwork( n+j ) + abs1( p( i, j ) )
              end do
              anorm = max( anorm, rwork( j )+abs1( s( j, j ) ) )
              bnorm = max( bnorm, rwork( n+j )+abs1( p( j, j ) ) )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp64
              ! main loop over eigenvalues
              loop_140: do je = 1, n
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig + 1_ilp64
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=sp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vl( jr, ieig ) = czero
                       end do
                       vl( ieig, ieig ) = cone
                       cycle loop_140
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                         ! h
                       ! y  ( a a - b b ) = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=sp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=sp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                    ! h
                    ! triangular solve of  (a a - b b)  y = 0
                                            ! h
                    ! (rowwise in  (a a - b b) , or columnwise in a a - b b)
                    loop_100: do j = je + 1, n
                       ! compute
                             ! j-1
                       ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                             ! k=je
                       ! (scale if necessary)
                       temp = one / xmax
                       if( acoefa*rwork( j )+bcoefa*rwork( n+j )>bignum*temp ) then
                          do jr = je, j - 1
                             work( jr ) = temp*work( jr )
                          end do
                          xmax = one
                       end if
                       suma = czero
                       sumb = czero
                       do jr = je, j - 1
                          suma = suma + conjg( s( jr, j ) )*work( jr )
                          sumb = sumb + conjg( p( jr, j ) )*work( jr )
                       end do
                       sum = acoeff*suma - conjg( bcoeff )*sumb
                       ! form x(j) = - sum / conjg( a*s(j,j) - b*p(j,j) )
                       ! with scaling and perturbation of the denominator
                       d = conjg( acoeff*s( j, j )-bcoeff*p( j, j ) )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=sp)
                       if( abs1( d )<one ) then
                          if( abs1( sum )>=bignum*abs1( d ) ) then
                             temp = one / abs1( sum )
                             do jr = je, j - 1
                                work( jr ) = temp*work( jr )
                             end do
                             xmax = temp*xmax
                             sum = temp*sum
                          end if
                       end if
                       work( j ) = stdlib_I64_cladiv( -sum, d )
                       xmax = max( xmax, abs1( work( j ) ) )
                    end do loop_100
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_I64_cgemv( 'N', n, n+1-je, cone, vl( 1_ilp64, je ), ldvl,work( je ), 1_ilp64, &
                                 czero, work( n+1 ), 1_ilp64 )
                       isrc = 2_ilp64
                       ibeg = 1_ilp64
                    else
                       isrc = 1_ilp64
                       ibeg = je
                    end if
                    ! copy and scale eigenvector into column of vl
                    xmax = zero
                    do jr = ibeg, n
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = ibeg, n
                          vl( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       ibeg = n + 1_ilp64
                    end if
                    do jr = 1, ibeg - 1
                       vl( jr, ieig ) = czero
                    end do
                 end if
              end do loop_140
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp64
              ! main loop over eigenvalues
              loop_250: do je = n, 1, -1
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig - 1_ilp64
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=sp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vr( jr, ieig ) = czero
                       end do
                       vr( ieig, ieig ) = cone
                       cycle loop_250
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                    ! ( a a - b b ) x  = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=sp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=sp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                    ! triangular solve of  (a a - b b) x = 0  (columnwise)
                    ! work(1:j-1) contains sums w,
                    ! work(j+1:je) contains x
                    do jr = 1, je - 1
                       work( jr ) = acoeff*s( jr, je ) - bcoeff*p( jr, je )
                    end do
                    work( je ) = cone
                    loop_210: do j = je - 1, 1, -1
                       ! form x(j) := - w(j) / d
                       ! with scaling and perturbation of the denominator
                       d = acoeff*s( j, j ) - bcoeff*p( j, j )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=sp)
                       if( abs1( d )<one ) then
                          if( abs1( work( j ) )>=bignum*abs1( d ) ) then
                             temp = one / abs1( work( j ) )
                             do jr = 1, je
                                work( jr ) = temp*work( jr )
                             end do
                          end if
                       end if
                       work( j ) = stdlib_I64_cladiv( -work( j ), d )
                       if( j>1_ilp64 ) then
                          ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                          if( abs1( work( j ) )>one ) then
                             temp = one / abs1( work( j ) )
                             if( acoefa*rwork( j )+bcoefa*rwork( n+j )>=bignum*temp ) then
                                do jr = 1, je
                                   work( jr ) = temp*work( jr )
                                end do
                             end if
                          end if
                          ca = acoeff*work( j )
                          cb = bcoeff*work( j )
                          do jr = 1, j - 1
                             work( jr ) = work( jr ) + ca*s( jr, j ) -cb*p( jr, j )
                          end do
                       end if
                    end do loop_210
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_I64_cgemv( 'N', n, je, cone, vr, ldvr, work, 1_ilp64,czero, work( n+1 ), &
                                 1_ilp64 )
                       isrc = 2_ilp64
                       iend = n
                    else
                       isrc = 1_ilp64
                       iend = je
                    end if
                    ! copy and scale eigenvector into column of vr
                    xmax = zero
                    do jr = 1, iend
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = 1, iend
                          vr( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       iend = 0_ilp64
                    end if
                    do jr = iend + 1, n
                       vr( jr, ieig ) = czero
                    end do
                 end if
              end do loop_250
           end if
           return
     end subroutine stdlib_I64_ctgevc

     pure module subroutine stdlib_I64_ztgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
     !! ZTGEVC computes some or all of the right and/or left eigenvectors of
     !! a pair of complex matrices (S,P), where S and P are upper triangular.
     !! Matrix pairs of this type are produced by the generalized Schur
     !! factorization of a complex matrix pair (A,B):
     !! A = Q*S*Z**H,  B = Q*P*Z**H
     !! as computed by ZGGHRD + ZHGEQZ.
     !! The right eigenvector x and the left eigenvector y of (S,P)
     !! corresponding to an eigenvalue w are defined by:
     !! S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
     !! where y**H denotes the conjugate tranpose of y.
     !! The eigenvalues are not input to this routine, but are computed
     !! directly from the diagonal elements of S and P.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of (S,P), or the products Z*X and/or Q*Y,
     !! where Z and Q are input matrices.
     !! If Q and Z are the unitary factors from the generalized Schur
     !! factorization of a matrix pair (A,B), then Z*X and Q*Y
     !! are the matrices of right and left eigenvectors of (A,B).
               mm, m, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp64), intent(out) :: info, m
           integer(ilp64), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: p(ldp,*), s(lds,*)
           complex(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: compl, compr, ilall, ilback, ilbbad, ilcomp, lsa, lsb
           integer(ilp64) :: i, ibeg, ieig, iend, ihwmny, im, iside, isrc, j, je, jr
           real(dp) :: acoefa, acoeff, anorm, ascale, bcoefa, big, bignum, bnorm, bscale, dmin, &
                     safmin, sbeta, scale, small, temp, ulp, xmax
           complex(dp) :: bcoeff, ca, cb, d, salpha, sum, suma, sumb, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode and test the input parameters
           if( stdlib_lsame( howmny, 'A' ) ) then
              ihwmny = 1_ilp64
              ilall = .true.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'S' ) ) then
              ihwmny = 2_ilp64
              ilall = .false.
              ilback = .false.
           else if( stdlib_lsame( howmny, 'B' ) ) then
              ihwmny = 3_ilp64
              ilall = .true.
              ilback = .true.
           else
              ihwmny = -1_ilp64
           end if
           if( stdlib_lsame( side, 'R' ) ) then
              iside = 1_ilp64
              compl = .false.
              compr = .true.
           else if( stdlib_lsame( side, 'L' ) ) then
              iside = 2_ilp64
              compl = .true.
              compr = .false.
           else if( stdlib_lsame( side, 'B' ) ) then
              iside = 3_ilp64
              compl = .true.
              compr = .true.
           else
              iside = -1_ilp64
           end if
           info = 0_ilp64
           if( iside<0_ilp64 ) then
              info = -1_ilp64
           else if( ihwmny<0_ilp64 ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lds<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldp<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGEVC', -info )
              return
           end if
           ! count the number of eigenvectors
           if( .not.ilall ) then
              im = 0_ilp64
              do j = 1, n
                 if( select( j ) )im = im + 1_ilp64
              end do
           else
              im = n
           end if
           ! check diagonal of b
           ilbbad = .false.
           do j = 1, n
              if( aimag( p( j, j ) )/=zero )ilbbad = .true.
           end do
           if( ilbbad ) then
              info = -7_ilp64
           else if( compl .and. ldvl<n .or. ldvl<1_ilp64 ) then
              info = -10_ilp64
           else if( compr .and. ldvr<n .or. ldvr<1_ilp64 ) then
              info = -12_ilp64
           else if( mm<im ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGEVC', -info )
              return
           end if
           ! quick return if possible
           m = im
           if( n==0 )return
           ! machine constants
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           big = one / safmin
           call stdlib_I64_dlabad( safmin, big )
           ulp = stdlib_I64_dlamch( 'EPSILON' )*stdlib_I64_dlamch( 'BASE' )
           small = safmin*n / ulp
           big = one / small
           bignum = one / ( safmin*n )
           ! compute the 1-norm of each column of the strictly upper triangular
           ! part of a and b to check for possible overflow in the triangular
           ! solver.
           anorm = abs1( s( 1_ilp64, 1_ilp64 ) )
           bnorm = abs1( p( 1_ilp64, 1_ilp64 ) )
           rwork( 1_ilp64 ) = zero
           rwork( n+1 ) = zero
           do j = 2, n
              rwork( j ) = zero
              rwork( n+j ) = zero
              do i = 1, j - 1
                 rwork( j ) = rwork( j ) + abs1( s( i, j ) )
                 rwork( n+j ) = rwork( n+j ) + abs1( p( i, j ) )
              end do
              anorm = max( anorm, rwork( j )+abs1( s( j, j ) ) )
              bnorm = max( bnorm, rwork( n+j )+abs1( p( j, j ) ) )
           end do
           ascale = one / max( anorm, safmin )
           bscale = one / max( bnorm, safmin )
           ! left eigenvectors
           if( compl ) then
              ieig = 0_ilp64
              ! main loop over eigenvalues
              loop_140: do je = 1, n
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig + 1_ilp64
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=dp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vl( jr, ieig ) = czero
                       end do
                       vl( ieig, ieig ) = cone
                       cycle loop_140
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                         ! h
                       ! y  ( a a - b b ) = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=dp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=dp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                                                    ! h
                    ! triangular solve of  (a a - b b)  y = 0
                                            ! h
                    ! (rowwise in  (a a - b b) , or columnwise in a a - b b)
                    loop_100: do j = je + 1, n
                       ! compute
                             ! j-1
                       ! sum = sum  conjg( a*s(k,j) - b*p(k,j) )*x(k)
                             ! k=je
                       ! (scale if necessary)
                       temp = one / xmax
                       if( acoefa*rwork( j )+bcoefa*rwork( n+j )>bignum*temp ) then
                          do jr = je, j - 1
                             work( jr ) = temp*work( jr )
                          end do
                          xmax = one
                       end if
                       suma = czero
                       sumb = czero
                       do jr = je, j - 1
                          suma = suma + conjg( s( jr, j ) )*work( jr )
                          sumb = sumb + conjg( p( jr, j ) )*work( jr )
                       end do
                       sum = acoeff*suma - conjg( bcoeff )*sumb
                       ! form x(j) = - sum / conjg( a*s(j,j) - b*p(j,j) )
                       ! with scaling and perturbation of the denominator
                       d = conjg( acoeff*s( j, j )-bcoeff*p( j, j ) )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=dp)
                       if( abs1( d )<one ) then
                          if( abs1( sum )>=bignum*abs1( d ) ) then
                             temp = one / abs1( sum )
                             do jr = je, j - 1
                                work( jr ) = temp*work( jr )
                             end do
                             xmax = temp*xmax
                             sum = temp*sum
                          end if
                       end if
                       work( j ) = stdlib_I64_zladiv( -sum, d )
                       xmax = max( xmax, abs1( work( j ) ) )
                    end do loop_100
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_I64_zgemv( 'N', n, n+1-je, cone, vl( 1_ilp64, je ), ldvl,work( je ), 1_ilp64, &
                                 czero, work( n+1 ), 1_ilp64 )
                       isrc = 2_ilp64
                       ibeg = 1_ilp64
                    else
                       isrc = 1_ilp64
                       ibeg = je
                    end if
                    ! copy and scale eigenvector into column of vl
                    xmax = zero
                    do jr = ibeg, n
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = ibeg, n
                          vl( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       ibeg = n + 1_ilp64
                    end if
                    do jr = 1, ibeg - 1
                       vl( jr, ieig ) = czero
                    end do
                 end if
              end do loop_140
           end if
           ! right eigenvectors
           if( compr ) then
              ieig = im + 1_ilp64
              ! main loop over eigenvalues
              loop_250: do je = n, 1, -1
                 if( ilall ) then
                    ilcomp = .true.
                 else
                    ilcomp = select( je )
                 end if
                 if( ilcomp ) then
                    ieig = ieig - 1_ilp64
                    if( abs1( s( je, je ) )<=safmin .and.abs( real( p( je, je ),KIND=dp) )&
                              <=safmin ) then
                       ! singular matrix pencil -- return unit eigenvector
                       do jr = 1, n
                          vr( jr, ieig ) = czero
                       end do
                       vr( ieig, ieig ) = cone
                       cycle loop_250
                    end if
                    ! non-singular eigenvalue:
                    ! compute coefficients  a  and  b  in
                    ! ( a a - b b ) x  = 0
                    temp = one / max( abs1( s( je, je ) )*ascale,abs( real( p( je, je ),KIND=dp) )&
                              *bscale, safmin )
                    salpha = ( temp*s( je, je ) )*ascale
                    sbeta = ( temp*real( p( je, je ),KIND=dp) )*bscale
                    acoeff = sbeta*ascale
                    bcoeff = salpha*bscale
                    ! scale to avoid underflow
                    lsa = abs( sbeta )>=safmin .and. abs( acoeff )<small
                    lsb = abs1( salpha )>=safmin .and. abs1( bcoeff )<small
                    scale = one
                    if( lsa )scale = ( small / abs( sbeta ) )*min( anorm, big )
                    if( lsb )scale = max( scale, ( small / abs1( salpha ) )*min( bnorm, big ) )
                              
                    if( lsa .or. lsb ) then
                       scale = min( scale, one /( safmin*max( one, abs( acoeff ),abs1( bcoeff ) ) &
                                 ) )
                       if( lsa ) then
                          acoeff = ascale*( scale*sbeta )
                       else
                          acoeff = scale*acoeff
                       end if
                       if( lsb ) then
                          bcoeff = bscale*( scale*salpha )
                       else
                          bcoeff = scale*bcoeff
                       end if
                    end if
                    acoefa = abs( acoeff )
                    bcoefa = abs1( bcoeff )
                    xmax = one
                    do jr = 1, n
                       work( jr ) = czero
                    end do
                    work( je ) = cone
                    dmin = max( ulp*acoefa*anorm, ulp*bcoefa*bnorm, safmin )
                    ! triangular solve of  (a a - b b) x = 0  (columnwise)
                    ! work(1:j-1) contains sums w,
                    ! work(j+1:je) contains x
                    do jr = 1, je - 1
                       work( jr ) = acoeff*s( jr, je ) - bcoeff*p( jr, je )
                    end do
                    work( je ) = cone
                    loop_210: do j = je - 1, 1, -1
                       ! form x(j) := - w(j) / d
                       ! with scaling and perturbation of the denominator
                       d = acoeff*s( j, j ) - bcoeff*p( j, j )
                       if( abs1( d )<=dmin )d = cmplx( dmin,KIND=dp)
                       if( abs1( d )<one ) then
                          if( abs1( work( j ) )>=bignum*abs1( d ) ) then
                             temp = one / abs1( work( j ) )
                             do jr = 1, je
                                work( jr ) = temp*work( jr )
                             end do
                          end if
                       end if
                       work( j ) = stdlib_I64_zladiv( -work( j ), d )
                       if( j>1_ilp64 ) then
                          ! w = w + x(j)*(a s(*,j) - b p(*,j) ) with scaling
                          if( abs1( work( j ) )>one ) then
                             temp = one / abs1( work( j ) )
                             if( acoefa*rwork( j )+bcoefa*rwork( n+j )>=bignum*temp ) then
                                do jr = 1, je
                                   work( jr ) = temp*work( jr )
                                end do
                             end if
                          end if
                          ca = acoeff*work( j )
                          cb = bcoeff*work( j )
                          do jr = 1, j - 1
                             work( jr ) = work( jr ) + ca*s( jr, j ) -cb*p( jr, j )
                          end do
                       end if
                    end do loop_210
                    ! back transform eigenvector if howmny='b'.
                    if( ilback ) then
                       call stdlib_I64_zgemv( 'N', n, je, cone, vr, ldvr, work, 1_ilp64,czero, work( n+1 ), &
                                 1_ilp64 )
                       isrc = 2_ilp64
                       iend = n
                    else
                       isrc = 1_ilp64
                       iend = je
                    end if
                    ! copy and scale eigenvector into column of vr
                    xmax = zero
                    do jr = 1, iend
                       xmax = max( xmax, abs1( work( ( isrc-1 )*n+jr ) ) )
                    end do
                    if( xmax>safmin ) then
                       temp = one / xmax
                       do jr = 1, iend
                          vr( jr, ieig ) = temp*work( ( isrc-1 )*n+jr )
                       end do
                    else
                       iend = 0_ilp64
                    end if
                    do jr = iend + 1, n
                       vr( jr, ieig ) = czero
                    end do
                 end if
              end do loop_250
           end if
           return
     end subroutine stdlib_I64_ztgevc




     pure module subroutine stdlib_I64_stgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! STGEXC reorders the generalized real Schur decomposition of a real
     !! matrix pair (A,B) using an orthogonal equivalence transformation
     !! (A, B) = Q * (A, B) * Z**T,
     !! so that the diagonal block of (A, B) with row index IFST is moved
     !! to row ILST.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(inout) :: ifst, ilst
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: here, lwmin, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldq<1_ilp64 .or. wantq .and. ( ldq<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. wantz .and. ( ldz<max( 1_ilp64, n ) ) ) then
              info = -11_ilp64
           else if( ifst<1_ilp64 .or. ifst>n ) then
              info = -12_ilp64
           else if( ilst<1_ilp64 .or. ilst>n ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 lwmin = 1_ilp64
              else
                 lwmin = 4_ilp64*n + 16_ilp64
              end if
              work(1_ilp64) = lwmin
              if (lwork<lwmin .and. .not.lquery) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STGEXC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of the specified block and find out
           ! if it is 1-by-1 or 2-by-2.
           if( ifst>1_ilp64 ) then
              if( a( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp64
           end if
           nbf = 1_ilp64
           if( ifst<n ) then
              if( a( ifst+1, ifst )/=zero )nbf = 2_ilp64
           end if
           ! determine the first row of the final block
           ! and find out if it is 1-by-1 or 2-by-2.
           if( ilst>1_ilp64 ) then
              if( a( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp64
           end if
           nbl = 1_ilp64
           if( ilst<n ) then
              if( a( ilst+1, ilst )/=zero )nbl = 2_ilp64
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst.
              if( nbf==2_ilp64 .and. nbl==1_ilp64 )ilst = ilst - 1_ilp64
              if( nbf==1_ilp64 .and. nbl==2_ilp64 )ilst = ilst + 1_ilp64
              here = ifst
              10 continue
              ! swap with next one below.
              if( nbf==1_ilp64 .or. nbf==2_ilp64 ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp64
                 if( here+nbf+1<=n ) then
                    if( a( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, nbf, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp64 ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp64
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp64
                 if( here+3<=n ) then
                    if( a( here+3, here+2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here+1, 1_ilp64, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp64 ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, 1_ilp64, &
                              1_ilp64, work, lwork, info )
                    if( info/=0_ilp64 ) then
                       ilst = here
                       return
                    end if
                    here = here + 1_ilp64
                 else
                    ! recompute nbnext in case of 2-by-2 split.
                    if( a( here+2, here+1 )==zero )nbnext = 1_ilp64
                    if( nbnext==2_ilp64 ) then
                       ! 2-by-2 block did not split.
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, nbnext, work, lwork,info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp64
                    else
                       ! 2-by-2 block did split.
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp64
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp64
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap with next one below.
              if( nbf==1_ilp64 .or. nbf==2_ilp64 ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp64
                 if( here>=3_ilp64 ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, nbf, work, lwork,info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp64 ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp64
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp64
                 if( here>=3_ilp64 ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, 1_ilp64, work, lwork,info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp64 ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, &
                              nbnext, 1_ilp64, work, lwork, info )
                    if( info/=0_ilp64 ) then
                       ilst = here
                       return
                    end if
                    here = here - 1_ilp64
                 else
                   ! recompute nbnext in case of 2-by-2 split.
                    if( a( here, here-1 )==zero )nbnext = 1_ilp64
                    if( nbnext==2_ilp64 ) then
                       ! 2-by-2 block did not split.
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here-1,&
                                  2_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp64
                    else
                       ! 2-by-2 block did split.
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp64
                       call stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp64
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_stgexc

     pure module subroutine stdlib_I64_dtgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! DTGEXC reorders the generalized real Schur decomposition of a real
     !! matrix pair (A,B) using an orthogonal equivalence transformation
     !! (A, B) = Q * (A, B) * Z**T,
     !! so that the diagonal block of (A, B) with row index IFST is moved
     !! to row ILST.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(inout) :: ifst, ilst
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: here, lwmin, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldq<1_ilp64 .or. wantq .and. ( ldq<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. wantz .and. ( ldz<max( 1_ilp64, n ) ) ) then
              info = -11_ilp64
           else if( ifst<1_ilp64 .or. ifst>n ) then
              info = -12_ilp64
           else if( ilst<1_ilp64 .or. ilst>n ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 lwmin = 1_ilp64
              else
                 lwmin = 4_ilp64*n + 16_ilp64
              end if
              work(1_ilp64) = lwmin
              if (lwork<lwmin .and. .not.lquery) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTGEXC', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of the specified block and find out
           ! if it is 1-by-1 or 2-by-2.
           if( ifst>1_ilp64 ) then
              if( a( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp64
           end if
           nbf = 1_ilp64
           if( ifst<n ) then
              if( a( ifst+1, ifst )/=zero )nbf = 2_ilp64
           end if
           ! determine the first row of the final block
           ! and find out if it is 1-by-1 or 2-by-2.
           if( ilst>1_ilp64 ) then
              if( a( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp64
           end if
           nbl = 1_ilp64
           if( ilst<n ) then
              if( a( ilst+1, ilst )/=zero )nbl = 2_ilp64
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst.
              if( nbf==2_ilp64 .and. nbl==1_ilp64 )ilst = ilst - 1_ilp64
              if( nbf==1_ilp64 .and. nbl==2_ilp64 )ilst = ilst + 1_ilp64
              here = ifst
              10 continue
              ! swap with next one below.
              if( nbf==1_ilp64 .or. nbf==2_ilp64 ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp64
                 if( here+nbf+1<=n ) then
                    if( a( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, nbf, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp64 ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp64
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp64
                 if( here+3<=n ) then
                    if( a( here+3, here+2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here+1, 1_ilp64, &
                           nbnext, work, lwork, info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp64 ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, 1_ilp64, &
                              1_ilp64, work, lwork, info )
                    if( info/=0_ilp64 ) then
                       ilst = here
                       return
                    end if
                    here = here + 1_ilp64
                 else
                    ! recompute nbnext in case of 2-by-2 split.
                    if( a( here+2, here+1 )==zero )nbnext = 1_ilp64
                    if( nbnext==2_ilp64 ) then
                       ! 2-by-2 block did not split.
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, nbnext, work, lwork,info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp64
                    else
                       ! 2-by-2 block did split.
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp64
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here + 1_ilp64
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap with next one below.
              if( nbf==1_ilp64 .or. nbf==2_ilp64 ) then
                 ! current block either 1-by-1 or 2-by-2.
                 nbnext = 1_ilp64
                 if( here>=3_ilp64 ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, nbf, work, lwork,info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2-by-2 block breaks into two 1-by-1 blocks.
                 if( nbf==2_ilp64 ) then
                    if( a( here+1, here )==zero )nbf = 3_ilp64
                 end if
              else
                 ! current block consists of two 1-by-1 blocks, each of which
                 ! must be swapped individually.
                 nbnext = 1_ilp64
                 if( here>=3_ilp64 ) then
                    if( a( here-1, here-2 )/=zero )nbnext = 2_ilp64
                 end if
                 call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here-nbnext, &
                           nbnext, 1_ilp64, work, lwork,info )
                 if( info/=0_ilp64 ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp64 ) then
                    ! swap two 1-by-1 blocks.
                    call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, here, &
                              nbnext, 1_ilp64, work, lwork, info )
                    if( info/=0_ilp64 ) then
                       ilst = here
                       return
                    end if
                    here = here - 1_ilp64
                 else
                   ! recompute nbnext in case of 2-by-2 split.
                    if( a( here, here-1 )==zero )nbnext = 1_ilp64
                    if( nbnext==2_ilp64 ) then
                       ! 2-by-2 block did not split.
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here-1,&
                                  2_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp64
                    else
                       ! 2-by-2 block did split.
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp64
                       call stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq,z, ldz, here, &
                                 1_ilp64, 1_ilp64, work, lwork, info )
                       if( info/=0_ilp64 ) then
                          ilst = here
                          return
                       end if
                       here = here - 1_ilp64
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_dtgexc


     pure module subroutine stdlib_I64_ctgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! CTGEXC reorders the generalized Schur decomposition of a complex
     !! matrix pair (A,B), using an unitary equivalence transformation
     !! (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
     !! row index IFST is moved to row ILST.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp64), intent(inout) :: ilst
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: here
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldq<1_ilp64 .or. wantq .and. ( ldq<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. wantz .and. ( ldz<max( 1_ilp64, n ) ) ) then
              info = -11_ilp64
           else if( ifst<1_ilp64 .or. ifst>n ) then
              info = -12_ilp64
           else if( ilst<1_ilp64 .or. ilst>n ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTGEXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           if( ifst==ilst )return
           if( ifst<ilst ) then
              here = ifst
              10 continue
              ! swap with next one below
              call stdlib_I64_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp64 ) then
                 ilst = here
                 return
              end if
              here = here + 1_ilp64
              if( here<ilst )go to 10
              here = here - 1_ilp64
           else
              here = ifst - 1_ilp64
              20 continue
              ! swap with next one above
              call stdlib_I64_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp64 ) then
                 ilst = here
                 return
              end if
              here = here - 1_ilp64
              if( here>=ilst )go to 20
              here = here + 1_ilp64
           end if
           ilst = here
           return
     end subroutine stdlib_I64_ctgexc

     pure module subroutine stdlib_I64_ztgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
     !! ZTGEXC reorders the generalized Schur decomposition of a complex
     !! matrix pair (A,B), using an unitary equivalence transformation
     !! (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
     !! row index IFST is moved to row ILST.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp64), intent(inout) :: ilst
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: here
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input arguments.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldq<1_ilp64 .or. wantq .and. ( ldq<max( 1_ilp64, n ) ) ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. wantz .and. ( ldz<max( 1_ilp64, n ) ) ) then
              info = -11_ilp64
           else if( ifst<1_ilp64 .or. ifst>n ) then
              info = -12_ilp64
           else if( ilst<1_ilp64 .or. ilst>n ) then
              info = -13_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTGEXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           if( ifst==ilst )return
           if( ifst<ilst ) then
              here = ifst
              10 continue
              ! swap with next one below
              call stdlib_I64_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp64 ) then
                 ilst = here
                 return
              end if
              here = here + 1_ilp64
              if( here<ilst )go to 10
              here = here - 1_ilp64
           else
              here = ifst - 1_ilp64
              20 continue
              ! swap with next one above
              call stdlib_I64_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz,here, info )
                        
              if( info/=0_ilp64 ) then
                 ilst = here
                 return
              end if
              here = here - 1_ilp64
              if( here>=ilst )go to 20
              here = here + 1_ilp64
           end if
           ilst = here
           return
     end subroutine stdlib_I64_ztgexc




     pure module subroutine stdlib_I64_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
     !! STGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
     !! of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
     !! (A, B) by an orthogonal equivalence transformation.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_scopy by calls to stdlib_I64_slaset, or by do
        ! loops. sven hammarling, 1/5/02.
           ! Parameters 
           real(sp), parameter :: twenty = 2.0e+01_sp
           integer(ilp64), parameter :: ldst = 4_ilp64
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp64) :: i, idum, linfo, m
           real(sp) :: bqra21, brqa21, ddum, dnorma, dnormb, dscale, dsum, eps, f, g, sa, sb, &
                     scale, smlnum, thresha, threshb
           ! Local Arrays 
           integer(ilp64) :: iwork(ldst)
           real(sp) :: ai(2_ilp64), ar(2_ilp64), be(2_ilp64), ir(ldst,ldst), ircop(ldst,ldst), li(ldst,ldst), licop(&
           ldst,ldst), s(ldst,ldst), scpy(ldst,ldst), t(ldst,ldst), taul(ldst), taur(ldst), tcpy(&
                     ldst,ldst)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           ! quick return if possible
           if( n<=1 .or. n1<=0 .or. n2<=0 )return
           if( n1>n .or. ( j1+n1 )>n )return
           m = n1 + n2
           if( lwork<max( n*m, m*m*2_ilp64 ) ) then
              info = -16_ilp64
              work( 1_ilp64 ) = max( n*m, m*m*2_ilp64 )
              return
           end if
           weak = .false.
           strong = .false.
           ! make a local copy of selected block
           call stdlib_I64_slaset( 'FULL', ldst, ldst, zero, zero, li, ldst )
           call stdlib_I64_slaset( 'FULL', ldst, ldst, zero, zero, ir, ldst )
           call stdlib_I64_slacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_I64_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute threshold for testing acceptance of swapping.
           eps = stdlib_I64_slamch( 'P' )
           smlnum = stdlib_I64_slamch( 'S' ) / eps
           dscale = zero
           dsum = one
           call stdlib_I64_slacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_I64_slassq( m*m, work, 1_ilp64, dscale, dsum )
           dnorma = dscale*sqrt( dsum )
           dscale = zero
           dsum = one
           call stdlib_I64_slacpy( 'FULL', m, m, t, ldst, work, m )
           call stdlib_I64_slassq( m*m, work, 1_ilp64, dscale, dsum )
           dnormb = dscale*sqrt( dsum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*dnorma, smlnum )
           threshb = max( twenty*eps*dnormb, smlnum )
           if( m==2_ilp64 ) then
              ! case 1: swap 1-by-1 and 1-by-1 blocks.
              ! compute orthogonal ql and rq that swap 1-by-1 and 1-by-1 blocks
              ! using givens rotations and perform the swap tentatively.
              f = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 1_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 1_ilp64 )
              g = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 2_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 2_ilp64 )
              sa = abs( s( 2_ilp64, 2_ilp64 ) ) * abs( t( 1_ilp64, 1_ilp64 ) )
              sb = abs( s( 1_ilp64, 1_ilp64 ) ) * abs( t( 2_ilp64, 2_ilp64 ) )
              call stdlib_I64_slartg( f, g, ir( 1_ilp64, 2_ilp64 ), ir( 1_ilp64, 1_ilp64 ), ddum )
              ir( 2_ilp64, 1_ilp64 ) = -ir( 1_ilp64, 2_ilp64 )
              ir( 2_ilp64, 2_ilp64 ) = ir( 1_ilp64, 1_ilp64 )
              call stdlib_I64_srot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), 1_ilp64, s( 1_ilp64, 2_ilp64 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 ) )
              call stdlib_I64_srot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), 1_ilp64, t( 1_ilp64, 2_ilp64 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 ) )
              if( sa>=sb ) then
                 call stdlib_I64_slartg( s( 1_ilp64, 1_ilp64 ), s( 2_ilp64, 1_ilp64 ), li( 1_ilp64, 1_ilp64 ), li( 2_ilp64,&
                     & 1_ilp64 ),ddum )
              else
                 call stdlib_I64_slartg( t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ), li( 1_ilp64, 1_ilp64 ), li( 2_ilp64,&
                     & 1_ilp64 ),ddum )
              end if
              call stdlib_I64_srot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), ldst, s( 2_ilp64, 1_ilp64 ), ldst, li( 1_ilp64, 1_ilp64 ),li(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_srot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), ldst, t( 2_ilp64, 1_ilp64 ), ldst, li( 1_ilp64, 1_ilp64 ),li(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              li( 2_ilp64, 2_ilp64 ) = li( 1_ilp64, 1_ilp64 )
              li( 1_ilp64, 2_ilp64 ) = -li( 2_ilp64, 1_ilp64 )
              ! weak stability test: |s21| <= o(eps f-norm((a)))
                                 ! and  |t21| <= o(eps f-norm((b)))
              weak = abs( s( 2_ilp64, 1_ilp64 ) ) <= thresha .and.abs( t( 2_ilp64, 1_ilp64 ) ) <= threshb
              if( .not.weak )go to 70
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_I64_slacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_I64_sgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_slassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_I64_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_I64_sgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_slassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                     ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              call stdlib_I64_srot( j1+1, a( 1_ilp64, j1 ), 1_ilp64, a( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64 ),ir(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_srot( j1+1, b( 1_ilp64, j1 ), 1_ilp64, b( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64 ),ir(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_srot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda,li( 1_ilp64, 1_ilp64 ), li( 2_ilp64, 1_ilp64 &
                        ) )
              call stdlib_I64_srot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb,li( 1_ilp64, 1_ilp64 ), li( 2_ilp64, 1_ilp64 &
                        ) )
              ! set  n1-by-n2 (2,1) - blocks to zero.
              a( j1+1, j1 ) = zero
              b( j1+1, j1 ) = zero
              ! accumulate transformations into q and z if requested.
              if( wantz )call stdlib_I64_srot( n, z( 1_ilp64, j1 ), 1_ilp64, z( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 &
                        ) )
              if( wantq )call stdlib_I64_srot( n, q( 1_ilp64, j1 ), 1_ilp64, q( 1_ilp64, j1+1 ), 1_ilp64, li( 1_ilp64, 1_ilp64&
                  & ),li( 2_ilp64, 1_ilp64 &
                        ) )
              ! exit with info = 0 if swap was successfully performed.
              return
           else
              ! case 2: swap 1-by-1 and 2-by-2 blocks, or 2-by-2
                      ! and 2-by-2 blocks.
              ! solve the generalized sylvester equation
                       ! s11 * r - l * s22 = scale * s12
                       ! t11 * r - l * t22 = scale * t12
              ! for r and l. solutions in li and ir.
              call stdlib_I64_slacpy( 'FULL', n1, n2, t( 1_ilp64, n1+1 ), ldst, li, ldst )
              call stdlib_I64_slacpy( 'FULL', n1, n2, s( 1_ilp64, n1+1 ), ldst,ir( n2+1, n1+1 ), ldst )
                        
              call stdlib_I64_stgsy2( 'N', 0_ilp64, n1, n2, s, ldst, s( n1+1, n1+1 ), ldst,ir( n2+1, n1+1 ),&
               ldst, t, ldst, t( n1+1, n1+1 ),ldst, li, ldst, scale, dsum, dscale, iwork, idum,&
                         linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix ql:
                          ! ql**t * li = [ tl ]
                                       ! [ 0  ]
              ! where
                          ! li =  [      -l              ]
                                ! [ scale * identity(n2) ]
              do i = 1, n2
                 call stdlib_I64_sscal( n1, -one, li( 1_ilp64, i ), 1_ilp64 )
                 li( n1+i, i ) = scale
              end do
              call stdlib_I64_sgeqr2( m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_sorg2r( m, m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix rq:
                          ! ir * rq**t =   [ 0  tr],
               ! where ir = [ scale * identity(n1), r ]
              do i = 1, n1
                 ir( n2+i, i ) = scale
              end do
              call stdlib_I64_sgerq2( n1, m, ir( n2+1, 1_ilp64 ), ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_sorgr2( m, m, n1, ir, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              ! perform the swapping tentatively:
              call stdlib_I64_sgemm( 'T', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
              call stdlib_I64_sgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, s,ldst )
              call stdlib_I64_sgemm( 'T', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
              call stdlib_I64_sgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, t,ldst )
              call stdlib_I64_slacpy( 'F', m, m, s, ldst, scpy, ldst )
              call stdlib_I64_slacpy( 'F', m, m, t, ldst, tcpy, ldst )
              call stdlib_I64_slacpy( 'F', m, m, ir, ldst, ircop, ldst )
              call stdlib_I64_slacpy( 'F', m, m, li, ldst, licop, ldst )
              ! triangularize the b-part by an rq factorization.
              ! apply transformation (from left) to a-part, giving s.
              call stdlib_I64_sgerq2( m, m, t, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_sormr2( 'R', 'T', m, m, m, t, ldst, taur, s, ldst, work,linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_sormr2( 'L', 'N', m, m, m, t, ldst, taur, ir, ldst, work,linfo )
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in brqa21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_I64_slassq( n1, s( n2+1, i ), 1_ilp64, dscale, dsum )
              end do
              brqa21 = dscale*sqrt( dsum )
              ! triangularize the b-part by a qr factorization.
              ! apply transformation (from right) to a-part, giving s.
              call stdlib_I64_sgeqr2( m, m, tcpy, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_sorm2r( 'L', 'T', m, m, m, tcpy, ldst, taul, scpy, ldst,work, info )
                        
              call stdlib_I64_sorm2r( 'R', 'N', m, m, m, tcpy, ldst, taul, licop, ldst,work, info )
                        
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in bqra21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_I64_slassq( n1, scpy( n2+1, i ), 1_ilp64, dscale, dsum )
              end do
              bqra21 = dscale*sqrt( dsum )
              ! decide which method to use.
                ! weak stability test:
                   ! f-norm(s21) <= o(eps * f-norm((s)))
              if( bqra21<=brqa21 .and. bqra21<=thresha ) then
                 call stdlib_I64_slacpy( 'F', m, m, scpy, ldst, s, ldst )
                 call stdlib_I64_slacpy( 'F', m, m, tcpy, ldst, t, ldst )
                 call stdlib_I64_slacpy( 'F', m, m, ircop, ldst, ir, ldst )
                 call stdlib_I64_slacpy( 'F', m, m, licop, ldst, li, ldst )
              else if( brqa21>=thresha ) then
                 go to 70
              end if
              ! set lower triangle of b-part to zero
              if (m>1_ilp64) call stdlib_I64_slaset( 'LOWER', m-1, m-1, zero, zero, t(2_ilp64,1_ilp64), ldst )
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_I64_slacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_slassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_I64_slacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_I64_sgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_slassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! if the swap is accepted ("weakly" and "strongly"), apply the
              ! transformations and set n1-by-n2 (2,1)-block to zero.
              call stdlib_I64_slaset( 'FULL', n1, n2, zero, zero, s(n2+1,1_ilp64), ldst )
              ! copy back m-by-m diagonal block starting at index j1 of (a, b)
              call stdlib_I64_slacpy( 'F', m, m, s, ldst, a( j1, j1 ), lda )
              call stdlib_I64_slacpy( 'F', m, m, t, ldst, b( j1, j1 ), ldb )
              call stdlib_I64_slaset( 'FULL', ldst, ldst, zero, zero, t, ldst )
              ! standardize existing 2-by-2 blocks.
              call stdlib_I64_slaset( 'FULL', m, m, zero, zero, work, m )
              work( 1_ilp64 ) = one
              t( 1_ilp64, 1_ilp64 ) = one
              idum = lwork - m*m - 2_ilp64
              if( n2>1_ilp64 ) then
                 call stdlib_I64_slagv2( a( j1, j1 ), lda, b( j1, j1 ), ldb, ar, ai, be,work( 1_ilp64 ), &
                           work( 2_ilp64 ), t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ) )
                 work( m+1 ) = -work( 2_ilp64 )
                 work( m+2 ) = work( 1_ilp64 )
                 t( n2, n2 ) = t( 1_ilp64, 1_ilp64 )
                 t( 1_ilp64, 2_ilp64 ) = -t( 2_ilp64, 1_ilp64 )
              end if
              work( m*m ) = one
              t( m, m ) = one
              if( n1>1_ilp64 ) then
                 call stdlib_I64_slagv2( a( j1+n2, j1+n2 ), lda, b( j1+n2, j1+n2 ), ldb,taur, taul, &
                 work( m*m+1 ), work( n2*m+n2+1 ),work( n2*m+n2+2 ), t( n2+1, n2+1 ),t( m, m-1 ) )
                           
                 work( m*m ) = work( n2*m+n2+1 )
                 work( m*m-1 ) = -work( n2*m+n2+2 )
                 t( m, m ) = t( n2+1, n2+1 )
                 t( m-1, m ) = -t( m, m-1 )
              end if
              call stdlib_I64_sgemm( 'T', 'N', n2, n1, n2, one, work, m, a( j1, j1+n2 ),lda, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_I64_slacpy( 'FULL', n2, n1, work( m*m+1 ), n2, a( j1, j1+n2 ),lda )
              call stdlib_I64_sgemm( 'T', 'N', n2, n1, n2, one, work, m, b( j1, j1+n2 ),ldb, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_I64_slacpy( 'FULL', n2, n1, work( m*m+1 ), n2, b( j1, j1+n2 ),ldb )
              call stdlib_I64_sgemm( 'N', 'N', m, m, m, one, li, ldst, work, m, zero,work( m*m+1 ), m &
                        )
              call stdlib_I64_slacpy( 'FULL', m, m, work( m*m+1 ), m, li, ldst )
              call stdlib_I64_sgemm( 'N', 'N', n2, n1, n1, one, a( j1, j1+n2 ), lda,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_I64_slacpy( 'FULL', n2, n1, work, n2, a( j1, j1+n2 ), lda )
              call stdlib_I64_sgemm( 'N', 'N', n2, n1, n1, one, b( j1, j1+n2 ), ldb,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_I64_slacpy( 'FULL', n2, n1, work, n2, b( j1, j1+n2 ), ldb )
              call stdlib_I64_sgemm( 'T', 'N', m, m, m, one, ir, ldst, t, ldst, zero,work, m )
              call stdlib_I64_slacpy( 'FULL', m, m, work, m, ir, ldst )
              ! accumulate transformations into q and z if requested.
              if( wantq ) then
                 call stdlib_I64_sgemm( 'N', 'N', n, m, m, one, q( 1_ilp64, j1 ), ldq, li,ldst, zero, work, &
                           n )
                 call stdlib_I64_slacpy( 'FULL', n, m, work, n, q( 1_ilp64, j1 ), ldq )
              end if
              if( wantz ) then
                 call stdlib_I64_sgemm( 'N', 'N', n, m, m, one, z( 1_ilp64, j1 ), ldz, ir,ldst, zero, work, &
                           n )
                 call stdlib_I64_slacpy( 'FULL', n, m, work, n, z( 1_ilp64, j1 ), ldz )
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                      ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              i = j1 + m
              if( i<=n ) then
                 call stdlib_I64_sgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,a( j1, i ), lda, zero, &
                           work, m )
                 call stdlib_I64_slacpy( 'FULL', m, n-i+1, work, m, a( j1, i ), lda )
                 call stdlib_I64_sgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,b( j1, i ), ldb, zero, &
                           work, m )
                 call stdlib_I64_slacpy( 'FULL', m, n-i+1, work, m, b( j1, i ), ldb )
              end if
              i = j1 - 1_ilp64
              if( i>0_ilp64 ) then
                 call stdlib_I64_sgemm( 'N', 'N', i, m, m, one, a( 1_ilp64, j1 ), lda, ir,ldst, zero, work, &
                           i )
                 call stdlib_I64_slacpy( 'FULL', i, m, work, i, a( 1_ilp64, j1 ), lda )
                 call stdlib_I64_sgemm( 'N', 'N', i, m, m, one, b( 1_ilp64, j1 ), ldb, ir,ldst, zero, work, &
                           i )
                 call stdlib_I64_slacpy( 'FULL', i, m, work, i, b( 1_ilp64, j1 ), ldb )
              end if
              ! exit with info = 0 if swap was successfully performed.
              return
           end if
           ! exit with info = 1 if swap was rejected.
           70 continue
           info = 1_ilp64
           return
     end subroutine stdlib_I64_stgex2

     pure module subroutine stdlib_I64_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
     !! DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
     !! of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
     !! (A, B) by an orthogonal equivalence transformation.
     !! (A, B) must be in generalized real Schur canonical form (as returned
     !! by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
     !! diagonal blocks. B is upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
     !! Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
               work, lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
        ! replaced various illegal calls to stdlib_I64_dcopy by calls to stdlib_I64_dlaset, or by do
        ! loops. sven hammarling, 1/5/02.
           ! Parameters 
           real(dp), parameter :: twenty = 2.0e+01_dp
           integer(ilp64), parameter :: ldst = 4_ilp64
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp64) :: i, idum, linfo, m
           real(dp) :: bqra21, brqa21, ddum, dnorma, dnormb, dscale, dsum, eps, f, g, sa, sb, &
                     scale, smlnum, thresha, threshb
           ! Local Arrays 
           integer(ilp64) :: iwork(ldst)
           real(dp) :: ai(2_ilp64), ar(2_ilp64), be(2_ilp64), ir(ldst,ldst), ircop(ldst,ldst), li(ldst,ldst), licop(&
           ldst,ldst), s(ldst,ldst), scpy(ldst,ldst), t(ldst,ldst), taul(ldst), taur(ldst), tcpy(&
                     ldst,ldst)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           ! quick return if possible
           if( n<=1 .or. n1<=0 .or. n2<=0 )return
           if( n1>n .or. ( j1+n1 )>n )return
           m = n1 + n2
           if( lwork<max( 1_ilp64, n*m, m*m*2_ilp64 ) ) then
              info = -16_ilp64
              work( 1_ilp64 ) = max( 1_ilp64, n*m, m*m*2_ilp64 )
              return
           end if
           weak = .false.
           strong = .false.
           ! make a local copy of selected block
           call stdlib_I64_dlaset( 'FULL', ldst, ldst, zero, zero, li, ldst )
           call stdlib_I64_dlaset( 'FULL', ldst, ldst, zero, zero, ir, ldst )
           call stdlib_I64_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_I64_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute threshold for testing acceptance of swapping.
           eps = stdlib_I64_dlamch( 'P' )
           smlnum = stdlib_I64_dlamch( 'S' ) / eps
           dscale = zero
           dsum = one
           call stdlib_I64_dlacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_I64_dlassq( m*m, work, 1_ilp64, dscale, dsum )
           dnorma = dscale*sqrt( dsum )
           dscale = zero
           dsum = one
           call stdlib_I64_dlacpy( 'FULL', m, m, t, ldst, work, m )
           call stdlib_I64_dlassq( m*m, work, 1_ilp64, dscale, dsum )
           dnormb = dscale*sqrt( dsum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*dnorma, smlnum )
           threshb = max( twenty*eps*dnormb, smlnum )
           if( m==2_ilp64 ) then
              ! case 1: swap 1-by-1 and 1-by-1 blocks.
              ! compute orthogonal ql and rq that swap 1-by-1 and 1-by-1 blocks
              ! using givens rotations and perform the swap tentatively.
              f = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 1_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 1_ilp64 )
              g = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 2_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 2_ilp64 )
              sa = abs( s( 2_ilp64, 2_ilp64 ) ) * abs( t( 1_ilp64, 1_ilp64 ) )
              sb = abs( s( 1_ilp64, 1_ilp64 ) ) * abs( t( 2_ilp64, 2_ilp64 ) )
              call stdlib_I64_dlartg( f, g, ir( 1_ilp64, 2_ilp64 ), ir( 1_ilp64, 1_ilp64 ), ddum )
              ir( 2_ilp64, 1_ilp64 ) = -ir( 1_ilp64, 2_ilp64 )
              ir( 2_ilp64, 2_ilp64 ) = ir( 1_ilp64, 1_ilp64 )
              call stdlib_I64_drot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), 1_ilp64, s( 1_ilp64, 2_ilp64 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 ) )
              call stdlib_I64_drot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), 1_ilp64, t( 1_ilp64, 2_ilp64 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 ) )
              if( sa>=sb ) then
                 call stdlib_I64_dlartg( s( 1_ilp64, 1_ilp64 ), s( 2_ilp64, 1_ilp64 ), li( 1_ilp64, 1_ilp64 ), li( 2_ilp64,&
                     & 1_ilp64 ),ddum )
              else
                 call stdlib_I64_dlartg( t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ), li( 1_ilp64, 1_ilp64 ), li( 2_ilp64,&
                     & 1_ilp64 ),ddum )
              end if
              call stdlib_I64_drot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), ldst, s( 2_ilp64, 1_ilp64 ), ldst, li( 1_ilp64, 1_ilp64 ),li(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_drot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), ldst, t( 2_ilp64, 1_ilp64 ), ldst, li( 1_ilp64, 1_ilp64 ),li(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              li( 2_ilp64, 2_ilp64 ) = li( 1_ilp64, 1_ilp64 )
              li( 1_ilp64, 2_ilp64 ) = -li( 2_ilp64, 1_ilp64 )
              ! weak stability test: |s21| <= o(eps f-norm((a)))
                                 ! and  |t21| <= o(eps f-norm((b)))
              weak = abs( s( 2_ilp64, 1_ilp64 ) ) <= thresha .and.abs( t( 2_ilp64, 1_ilp64 ) ) <= threshb
              if( .not.weak )go to 70
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_I64_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_I64_dgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_dlassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_I64_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_I64_dgemm( 'N', 'T', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_dlassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                     ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              call stdlib_I64_drot( j1+1, a( 1_ilp64, j1 ), 1_ilp64, a( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64 ),ir(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_drot( j1+1, b( 1_ilp64, j1 ), 1_ilp64, b( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64 ),ir(&
                  & 2_ilp64, 1_ilp64 ) )
                        
              call stdlib_I64_drot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda,li( 1_ilp64, 1_ilp64 ), li( 2_ilp64, 1_ilp64 &
                        ) )
              call stdlib_I64_drot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb,li( 1_ilp64, 1_ilp64 ), li( 2_ilp64, 1_ilp64 &
                        ) )
              ! set  n1-by-n2 (2,1) - blocks to zero.
              a( j1+1, j1 ) = zero
              b( j1+1, j1 ) = zero
              ! accumulate transformations into q and z if requested.
              if( wantz )call stdlib_I64_drot( n, z( 1_ilp64, j1 ), 1_ilp64, z( 1_ilp64, j1+1 ), 1_ilp64, ir( 1_ilp64, 1_ilp64&
                  & ),ir( 2_ilp64, 1_ilp64 &
                        ) )
              if( wantq )call stdlib_I64_drot( n, q( 1_ilp64, j1 ), 1_ilp64, q( 1_ilp64, j1+1 ), 1_ilp64, li( 1_ilp64, 1_ilp64&
                  & ),li( 2_ilp64, 1_ilp64 &
                        ) )
              ! exit with info = 0 if swap was successfully performed.
              return
           else
              ! case 2: swap 1-by-1 and 2-by-2 blocks, or 2-by-2
                      ! and 2-by-2 blocks.
              ! solve the generalized sylvester equation
                       ! s11 * r - l * s22 = scale * s12
                       ! t11 * r - l * t22 = scale * t12
              ! for r and l. solutions in li and ir.
              call stdlib_I64_dlacpy( 'FULL', n1, n2, t( 1_ilp64, n1+1 ), ldst, li, ldst )
              call stdlib_I64_dlacpy( 'FULL', n1, n2, s( 1_ilp64, n1+1 ), ldst,ir( n2+1, n1+1 ), ldst )
                        
              call stdlib_I64_dtgsy2( 'N', 0_ilp64, n1, n2, s, ldst, s( n1+1, n1+1 ), ldst,ir( n2+1, n1+1 ),&
               ldst, t, ldst, t( n1+1, n1+1 ),ldst, li, ldst, scale, dsum, dscale, iwork, idum,&
                         linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix ql:
                          ! ql**t * li = [ tl ]
                                       ! [ 0  ]
              ! where
                          ! li =  [      -l              ]
                                ! [ scale * identity(n2) ]
              do i = 1, n2
                 call stdlib_I64_dscal( n1, -one, li( 1_ilp64, i ), 1_ilp64 )
                 li( n1+i, i ) = scale
              end do
              call stdlib_I64_dgeqr2( m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_dorg2r( m, m, n2, li, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              ! compute orthogonal matrix rq:
                          ! ir * rq**t =   [ 0  tr],
               ! where ir = [ scale * identity(n1), r ]
              do i = 1, n1
                 ir( n2+i, i ) = scale
              end do
              call stdlib_I64_dgerq2( n1, m, ir( n2+1, 1_ilp64 ), ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_dorgr2( m, m, n1, ir, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              ! perform the swapping tentatively:
              call stdlib_I64_dgemm( 'T', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
              call stdlib_I64_dgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, s,ldst )
              call stdlib_I64_dgemm( 'T', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
              call stdlib_I64_dgemm( 'N', 'T', m, m, m, one, work, m, ir, ldst, zero, t,ldst )
              call stdlib_I64_dlacpy( 'F', m, m, s, ldst, scpy, ldst )
              call stdlib_I64_dlacpy( 'F', m, m, t, ldst, tcpy, ldst )
              call stdlib_I64_dlacpy( 'F', m, m, ir, ldst, ircop, ldst )
              call stdlib_I64_dlacpy( 'F', m, m, li, ldst, licop, ldst )
              ! triangularize the b-part by an rq factorization.
              ! apply transformation (from left) to a-part, giving s.
              call stdlib_I64_dgerq2( m, m, t, ldst, taur, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_dormr2( 'R', 'T', m, m, m, t, ldst, taur, s, ldst, work,linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_dormr2( 'L', 'N', m, m, m, t, ldst, taur, ir, ldst, work,linfo )
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in brqa21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_I64_dlassq( n1, s( n2+1, i ), 1_ilp64, dscale, dsum )
              end do
              brqa21 = dscale*sqrt( dsum )
              ! triangularize the b-part by a qr factorization.
              ! apply transformation (from right) to a-part, giving s.
              call stdlib_I64_dgeqr2( m, m, tcpy, ldst, taul, work, linfo )
              if( linfo/=0 )go to 70
              call stdlib_I64_dorm2r( 'L', 'T', m, m, m, tcpy, ldst, taul, scpy, ldst,work, info )
                        
              call stdlib_I64_dorm2r( 'R', 'N', m, m, m, tcpy, ldst, taul, licop, ldst,work, info )
                        
              if( linfo/=0 )go to 70
              ! compute f-norm(s21) in bqra21. (t21 is 0.)
              dscale = zero
              dsum = one
              do i = 1, n2
                 call stdlib_I64_dlassq( n1, scpy( n2+1, i ), 1_ilp64, dscale, dsum )
              end do
              bqra21 = dscale*sqrt( dsum )
              ! decide which method to use.
                ! weak stability test:
                   ! f-norm(s21) <= o(eps * f-norm((s)))
              if( bqra21<=brqa21 .and. bqra21<=thresha ) then
                 call stdlib_I64_dlacpy( 'F', m, m, scpy, ldst, s, ldst )
                 call stdlib_I64_dlacpy( 'F', m, m, tcpy, ldst, t, ldst )
                 call stdlib_I64_dlacpy( 'F', m, m, ircop, ldst, ir, ldst )
                 call stdlib_I64_dlacpy( 'F', m, m, licop, ldst, li, ldst )
              else if( brqa21>=thresha ) then
                 go to 70
              end if
              ! set lower triangle of b-part to zero
              call stdlib_I64_dlaset( 'LOWER', m-1, m-1, zero, zero, t(2_ilp64,1_ilp64), ldst )
              if( wands ) then
                 ! strong stability test:
                     ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                     ! and
                     ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
                 call stdlib_I64_dlacpy( 'FULL', m, m, a( j1, j1 ), lda, work( m*m+1 ),m )
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, one, li, ldst, s, ldst, zero,work, m )
                           
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_dlassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sa = dscale*sqrt( dsum )
                 call stdlib_I64_dlacpy( 'FULL', m, m, b( j1, j1 ), ldb, work( m*m+1 ),m )
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, one, li, ldst, t, ldst, zero,work, m )
                           
                 call stdlib_I64_dgemm( 'N', 'N', m, m, m, -one, work, m, ir, ldst, one,work( m*m+1 ),&
                            m )
                 dscale = zero
                 dsum = one
                 call stdlib_I64_dlassq( m*m, work( m*m+1 ), 1_ilp64, dscale, dsum )
                 sb = dscale*sqrt( dsum )
                 strong = sa<=thresha .and. sb<=threshb
                 if( .not.strong )go to 70
              end if
              ! if the swap is accepted ("weakly" and "strongly"), apply the
              ! transformations and set n1-by-n2 (2,1)-block to zero.
              call stdlib_I64_dlaset( 'FULL', n1, n2, zero, zero, s(n2+1,1_ilp64), ldst )
              ! copy back m-by-m diagonal block starting at index j1 of (a, b)
              call stdlib_I64_dlacpy( 'F', m, m, s, ldst, a( j1, j1 ), lda )
              call stdlib_I64_dlacpy( 'F', m, m, t, ldst, b( j1, j1 ), ldb )
              call stdlib_I64_dlaset( 'FULL', ldst, ldst, zero, zero, t, ldst )
              ! standardize existing 2-by-2 blocks.
              call stdlib_I64_dlaset( 'FULL', m, m, zero, zero, work, m )
              work( 1_ilp64 ) = one
              t( 1_ilp64, 1_ilp64 ) = one
              idum = lwork - m*m - 2_ilp64
              if( n2>1_ilp64 ) then
                 call stdlib_I64_dlagv2( a( j1, j1 ), lda, b( j1, j1 ), ldb, ar, ai, be,work( 1_ilp64 ), &
                           work( 2_ilp64 ), t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ) )
                 work( m+1 ) = -work( 2_ilp64 )
                 work( m+2 ) = work( 1_ilp64 )
                 t( n2, n2 ) = t( 1_ilp64, 1_ilp64 )
                 t( 1_ilp64, 2_ilp64 ) = -t( 2_ilp64, 1_ilp64 )
              end if
              work( m*m ) = one
              t( m, m ) = one
              if( n1>1_ilp64 ) then
                 call stdlib_I64_dlagv2( a( j1+n2, j1+n2 ), lda, b( j1+n2, j1+n2 ), ldb,taur, taul, &
                 work( m*m+1 ), work( n2*m+n2+1 ),work( n2*m+n2+2 ), t( n2+1, n2+1 ),t( m, m-1 ) )
                           
                 work( m*m ) = work( n2*m+n2+1 )
                 work( m*m-1 ) = -work( n2*m+n2+2 )
                 t( m, m ) = t( n2+1, n2+1 )
                 t( m-1, m ) = -t( m, m-1 )
              end if
              call stdlib_I64_dgemm( 'T', 'N', n2, n1, n2, one, work, m, a( j1, j1+n2 ),lda, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_I64_dlacpy( 'FULL', n2, n1, work( m*m+1 ), n2, a( j1, j1+n2 ),lda )
              call stdlib_I64_dgemm( 'T', 'N', n2, n1, n2, one, work, m, b( j1, j1+n2 ),ldb, zero, &
                        work( m*m+1 ), n2 )
              call stdlib_I64_dlacpy( 'FULL', n2, n1, work( m*m+1 ), n2, b( j1, j1+n2 ),ldb )
              call stdlib_I64_dgemm( 'N', 'N', m, m, m, one, li, ldst, work, m, zero,work( m*m+1 ), m &
                        )
              call stdlib_I64_dlacpy( 'FULL', m, m, work( m*m+1 ), m, li, ldst )
              call stdlib_I64_dgemm( 'N', 'N', n2, n1, n1, one, a( j1, j1+n2 ), lda,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_I64_dlacpy( 'FULL', n2, n1, work, n2, a( j1, j1+n2 ), lda )
              call stdlib_I64_dgemm( 'N', 'N', n2, n1, n1, one, b( j1, j1+n2 ), ldb,t( n2+1, n2+1 ), &
                        ldst, zero, work, n2 )
              call stdlib_I64_dlacpy( 'FULL', n2, n1, work, n2, b( j1, j1+n2 ), ldb )
              call stdlib_I64_dgemm( 'T', 'N', m, m, m, one, ir, ldst, t, ldst, zero,work, m )
              call stdlib_I64_dlacpy( 'FULL', m, m, work, m, ir, ldst )
              ! accumulate transformations into q and z if requested.
              if( wantq ) then
                 call stdlib_I64_dgemm( 'N', 'N', n, m, m, one, q( 1_ilp64, j1 ), ldq, li,ldst, zero, work, &
                           n )
                 call stdlib_I64_dlacpy( 'FULL', n, m, work, n, q( 1_ilp64, j1 ), ldq )
              end if
              if( wantz ) then
                 call stdlib_I64_dgemm( 'N', 'N', n, m, m, one, z( 1_ilp64, j1 ), ldz, ir,ldst, zero, work, &
                           n )
                 call stdlib_I64_dlacpy( 'FULL', n, m, work, n, z( 1_ilp64, j1 ), ldz )
              end if
              ! update (a(j1:j1+m-1, m+j1:n), b(j1:j1+m-1, m+j1:n)) and
                      ! (a(1:j1-1, j1:j1+m), b(1:j1-1, j1:j1+m)).
              i = j1 + m
              if( i<=n ) then
                 call stdlib_I64_dgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,a( j1, i ), lda, zero, &
                           work, m )
                 call stdlib_I64_dlacpy( 'FULL', m, n-i+1, work, m, a( j1, i ), lda )
                 call stdlib_I64_dgemm( 'T', 'N', m, n-i+1, m, one, li, ldst,b( j1, i ), ldb, zero, &
                           work, m )
                 call stdlib_I64_dlacpy( 'FULL', m, n-i+1, work, m, b( j1, i ), ldb )
              end if
              i = j1 - 1_ilp64
              if( i>0_ilp64 ) then
                 call stdlib_I64_dgemm( 'N', 'N', i, m, m, one, a( 1_ilp64, j1 ), lda, ir,ldst, zero, work, &
                           i )
                 call stdlib_I64_dlacpy( 'FULL', i, m, work, i, a( 1_ilp64, j1 ), lda )
                 call stdlib_I64_dgemm( 'N', 'N', i, m, m, one, b( 1_ilp64, j1 ), ldb, ir,ldst, zero, work, &
                           i )
                 call stdlib_I64_dlacpy( 'FULL', i, m, work, i, b( 1_ilp64, j1 ), ldb )
              end if
              ! exit with info = 0 if swap was successfully performed.
              return
           end if
           ! exit with info = 1 if swap was rejected.
           70 continue
           info = 1_ilp64
           return
     end subroutine stdlib_I64_dtgex2


     pure module subroutine stdlib_I64_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
     !! CTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
     !! in an upper triangular matrix pair (A, B) by an unitary equivalence
     !! transformation.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: j1, lda, ldb, ldq, ldz, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: twenty = 2.0e+1_sp
           integer(ilp64), parameter :: ldst = 2_ilp64
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp64) :: i, m
           real(sp) :: cq, cz, eps, sa, sb, scale, smlnum, sum, thresha, threshb
           complex(sp) :: cdum, f, g, sq, sz
           ! Local Arrays 
           complex(sp) :: s(ldst,ldst), t(ldst,ldst), work(8_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           ! quick return if possible
           if( n<=1 )return
           m = ldst
           weak = .false.
           strong = .false.
           ! make a local copy of selected block in (a, b)
           call stdlib_I64_clacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_I64_clacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute the threshold for testing the acceptance of swapping.
           eps = stdlib_I64_slamch( 'P' )
           smlnum = stdlib_I64_slamch( 'S' ) / eps
           scale = real( czero,KIND=sp)
           sum = real( cone,KIND=sp)
           call stdlib_I64_clacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_I64_clacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
           call stdlib_I64_classq( m*m, work, 1_ilp64, scale, sum )
           sa = scale*sqrt( sum )
           scale = real( czero,KIND=sp)
           sum = real( cone,KIND=sp)
           call stdlib_I64_classq( m*m, work(m*m+1), 1_ilp64, scale, sum )
           sb = scale*sqrt( sum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*sa, smlnum )
           threshb = max( twenty*eps*sb, smlnum )
           ! compute unitary ql and rq that swap 1-by-1 and 1-by-1 blocks
           ! using givens rotations and perform the swap tentatively.
           f = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 1_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 1_ilp64 )
           g = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 2_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 2_ilp64 )
           sa = abs( s( 2_ilp64, 2_ilp64 ) ) * abs( t( 1_ilp64, 1_ilp64 ) )
           sb = abs( s( 1_ilp64, 1_ilp64 ) ) * abs( t( 2_ilp64, 2_ilp64 ) )
           call stdlib_I64_clartg( g, f, cz, sz, cdum )
           sz = -sz
           call stdlib_I64_crot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), 1_ilp64, s( 1_ilp64, 2_ilp64 ), 1_ilp64, cz, conjg( sz ) )
           call stdlib_I64_crot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), 1_ilp64, t( 1_ilp64, 2_ilp64 ), 1_ilp64, cz, conjg( sz ) )
           if( sa>=sb ) then
              call stdlib_I64_clartg( s( 1_ilp64, 1_ilp64 ), s( 2_ilp64, 1_ilp64 ), cq, sq, cdum )
           else
              call stdlib_I64_clartg( t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ), cq, sq, cdum )
           end if
           call stdlib_I64_crot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), ldst, s( 2_ilp64, 1_ilp64 ), ldst, cq, sq )
           call stdlib_I64_crot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), ldst, t( 2_ilp64, 1_ilp64 ), ldst, cq, sq )
           ! weak stability test: |s21| <= o(eps f-norm((a)))
                                ! and  |t21| <= o(eps f-norm((b)))
           weak = abs( s( 2_ilp64, 1_ilp64 ) )<=thresha .and.abs( t( 2_ilp64, 1_ilp64 ) )<=threshb
           if( .not.weak )go to 20
           if( wands ) then
              ! strong stability test:
                 ! f-norm((a-ql**h*s*qr, b-ql**h*t*qr)) <= o(eps*f-norm((a, b)))
              call stdlib_I64_clacpy( 'FULL', m, m, s, ldst, work, m )
              call stdlib_I64_clacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
              call stdlib_I64_crot( 2_ilp64, work, 1_ilp64, work( 3_ilp64 ), 1_ilp64, cz, -conjg( sz ) )
              call stdlib_I64_crot( 2_ilp64, work( 5_ilp64 ), 1_ilp64, work( 7_ilp64 ), 1_ilp64, cz, -conjg( sz ) )
              call stdlib_I64_crot( 2_ilp64, work, 2_ilp64, work( 2_ilp64 ), 2_ilp64, cq, -sq )
              call stdlib_I64_crot( 2_ilp64, work( 5_ilp64 ), 2_ilp64, work( 6_ilp64 ), 2_ilp64, cq, -sq )
              do i = 1, 2
                 work( i ) = work( i ) - a( j1+i-1, j1 )
                 work( i+2 ) = work( i+2 ) - a( j1+i-1, j1+1 )
                 work( i+4 ) = work( i+4 ) - b( j1+i-1, j1 )
                 work( i+6 ) = work( i+6 ) - b( j1+i-1, j1+1 )
              end do
              scale = real( czero,KIND=sp)
              sum = real( cone,KIND=sp)
              call stdlib_I64_classq( m*m, work, 1_ilp64, scale, sum )
              sa = scale*sqrt( sum )
              scale = real( czero,KIND=sp)
              sum = real( cone,KIND=sp)
              call stdlib_I64_classq( m*m, work(m*m+1), 1_ilp64, scale, sum )
              sb = scale*sqrt( sum )
              strong = sa<=thresha .and. sb<=threshb
              if( .not.strong )go to 20
           end if
           ! if the swap is accepted ("weakly" and "strongly"), apply the
           ! equivalence transformations to the original matrix pair (a,b)
           call stdlib_I64_crot( j1+1, a( 1_ilp64, j1 ), 1_ilp64, a( 1_ilp64, j1+1 ), 1_ilp64, cz, conjg( sz ) )
           call stdlib_I64_crot( j1+1, b( 1_ilp64, j1 ), 1_ilp64, b( 1_ilp64, j1+1 ), 1_ilp64, cz, conjg( sz ) )
           call stdlib_I64_crot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda, cq, sq )
           call stdlib_I64_crot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb, cq, sq )
           ! set  n1 by n2 (2,1) blocks to 0
           a( j1+1, j1 ) = czero
           b( j1+1, j1 ) = czero
           ! accumulate transformations into q and z if requested.
           if( wantz )call stdlib_I64_crot( n, z( 1_ilp64, j1 ), 1_ilp64, z( 1_ilp64, j1+1 ), 1_ilp64, cz, conjg( sz ) )
                     
           if( wantq )call stdlib_I64_crot( n, q( 1_ilp64, j1 ), 1_ilp64, q( 1_ilp64, j1+1 ), 1_ilp64, cq, conjg( sq ) )
                     
           ! exit with info = 0 if swap was successfully performed.
           return
           ! exit with info = 1 if swap was rejected.
           20 continue
           info = 1_ilp64
           return
     end subroutine stdlib_I64_ctgex2

     pure module subroutine stdlib_I64_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
     !! ZTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
     !! in an upper triangular matrix pair (A, B) by an unitary equivalence
     !! transformation.
     !! (A, B) must be in generalized Schur canonical form, that is, A and
     !! B are both upper triangular.
     !! Optionally, the matrices Q and Z of generalized Schur vectors are
     !! updated.
     !! Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
     !! Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: j1, lda, ldb, ldq, ldz, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: twenty = 2.0e+1_dp
           integer(ilp64), parameter :: ldst = 2_ilp64
           logical(lk), parameter :: wands = .true.
           
           
           
           
           ! Local Scalars 
           logical(lk) :: strong, weak
           integer(ilp64) :: i, m
           real(dp) :: cq, cz, eps, sa, sb, scale, smlnum, sum, thresha, threshb
           complex(dp) :: cdum, f, g, sq, sz
           ! Local Arrays 
           complex(dp) :: s(ldst,ldst), t(ldst,ldst), work(8_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           ! quick return if possible
           if( n<=1 )return
           m = ldst
           weak = .false.
           strong = .false.
           ! make a local copy of selected block in (a, b)
           call stdlib_I64_zlacpy( 'FULL', m, m, a( j1, j1 ), lda, s, ldst )
           call stdlib_I64_zlacpy( 'FULL', m, m, b( j1, j1 ), ldb, t, ldst )
           ! compute the threshold for testing the acceptance of swapping.
           eps = stdlib_I64_dlamch( 'P' )
           smlnum = stdlib_I64_dlamch( 'S' ) / eps
           scale = real( czero,KIND=dp)
           sum = real( cone,KIND=dp)
           call stdlib_I64_zlacpy( 'FULL', m, m, s, ldst, work, m )
           call stdlib_I64_zlacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
           call stdlib_I64_zlassq( m*m, work, 1_ilp64, scale, sum )
           sa = scale*sqrt( sum )
           scale = real( czero,KIND=dp)
           sum = real( cone,KIND=dp)
           call stdlib_I64_zlassq( m*m, work(m*m+1), 1_ilp64, scale, sum )
           sb = scale*sqrt( sum )
           ! thres has been changed from
              ! thresh = max( ten*eps*sa, smlnum )
           ! to
              ! thresh = max( twenty*eps*sa, smlnum )
           ! on 04/01/10.
           ! "bug" reported by ondra kamenik, confirmed by julie langou, fixed by
           ! jim demmel and guillaume revy. see forum post 1783.
           thresha = max( twenty*eps*sa, smlnum )
           threshb = max( twenty*eps*sb, smlnum )
           ! compute unitary ql and rq that swap 1-by-1 and 1-by-1 blocks
           ! using givens rotations and perform the swap tentatively.
           f = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 1_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 1_ilp64 )
           g = s( 2_ilp64, 2_ilp64 )*t( 1_ilp64, 2_ilp64 ) - t( 2_ilp64, 2_ilp64 )*s( 1_ilp64, 2_ilp64 )
           sa = abs( s( 2_ilp64, 2_ilp64 ) ) * abs( t( 1_ilp64, 1_ilp64 ) )
           sb = abs( s( 1_ilp64, 1_ilp64 ) ) * abs( t( 2_ilp64, 2_ilp64 ) )
           call stdlib_I64_zlartg( g, f, cz, sz, cdum )
           sz = -sz
           call stdlib_I64_zrot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), 1_ilp64, s( 1_ilp64, 2_ilp64 ), 1_ilp64, cz, conjg( sz ) )
           call stdlib_I64_zrot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), 1_ilp64, t( 1_ilp64, 2_ilp64 ), 1_ilp64, cz, conjg( sz ) )
           if( sa>=sb ) then
              call stdlib_I64_zlartg( s( 1_ilp64, 1_ilp64 ), s( 2_ilp64, 1_ilp64 ), cq, sq, cdum )
           else
              call stdlib_I64_zlartg( t( 1_ilp64, 1_ilp64 ), t( 2_ilp64, 1_ilp64 ), cq, sq, cdum )
           end if
           call stdlib_I64_zrot( 2_ilp64, s( 1_ilp64, 1_ilp64 ), ldst, s( 2_ilp64, 1_ilp64 ), ldst, cq, sq )
           call stdlib_I64_zrot( 2_ilp64, t( 1_ilp64, 1_ilp64 ), ldst, t( 2_ilp64, 1_ilp64 ), ldst, cq, sq )
           ! weak stability test: |s21| <= o(eps f-norm((a)))
                                ! and  |t21| <= o(eps f-norm((b)))
           weak = abs( s( 2_ilp64, 1_ilp64 ) )<=thresha .and.abs( t( 2_ilp64, 1_ilp64 ) )<=threshb
           if( .not.weak )go to 20
           if( wands ) then
              ! strong stability test:
                 ! f-norm((a-ql**h*s*qr)) <= o(eps*f-norm((a)))
                 ! and
                 ! f-norm((b-ql**h*t*qr)) <= o(eps*f-norm((b)))
              call stdlib_I64_zlacpy( 'FULL', m, m, s, ldst, work, m )
              call stdlib_I64_zlacpy( 'FULL', m, m, t, ldst, work( m*m+1 ), m )
              call stdlib_I64_zrot( 2_ilp64, work, 1_ilp64, work( 3_ilp64 ), 1_ilp64, cz, -conjg( sz ) )
              call stdlib_I64_zrot( 2_ilp64, work( 5_ilp64 ), 1_ilp64, work( 7_ilp64 ), 1_ilp64, cz, -conjg( sz ) )
              call stdlib_I64_zrot( 2_ilp64, work, 2_ilp64, work( 2_ilp64 ), 2_ilp64, cq, -sq )
              call stdlib_I64_zrot( 2_ilp64, work( 5_ilp64 ), 2_ilp64, work( 6_ilp64 ), 2_ilp64, cq, -sq )
              do i = 1, 2
                 work( i ) = work( i ) - a( j1+i-1, j1 )
                 work( i+2 ) = work( i+2 ) - a( j1+i-1, j1+1 )
                 work( i+4 ) = work( i+4 ) - b( j1+i-1, j1 )
                 work( i+6 ) = work( i+6 ) - b( j1+i-1, j1+1 )
              end do
              scale = real( czero,KIND=dp)
              sum = real( cone,KIND=dp)
              call stdlib_I64_zlassq( m*m, work, 1_ilp64, scale, sum )
              sa = scale*sqrt( sum )
              scale = real( czero,KIND=dp)
              sum = real( cone,KIND=dp)
              call stdlib_I64_zlassq( m*m, work(m*m+1), 1_ilp64, scale, sum )
              sb = scale*sqrt( sum )
              strong = sa<=thresha .and. sb<=threshb
              if( .not.strong )go to 20
           end if
           ! if the swap is accepted ("weakly" and "strongly"), apply the
           ! equivalence transformations to the original matrix pair (a,b)
           call stdlib_I64_zrot( j1+1, a( 1_ilp64, j1 ), 1_ilp64, a( 1_ilp64, j1+1 ), 1_ilp64, cz,conjg( sz ) )
           call stdlib_I64_zrot( j1+1, b( 1_ilp64, j1 ), 1_ilp64, b( 1_ilp64, j1+1 ), 1_ilp64, cz,conjg( sz ) )
           call stdlib_I64_zrot( n-j1+1, a( j1, j1 ), lda, a( j1+1, j1 ), lda, cq, sq )
           call stdlib_I64_zrot( n-j1+1, b( j1, j1 ), ldb, b( j1+1, j1 ), ldb, cq, sq )
           ! set  n1 by n2 (2,1) blocks to 0
           a( j1+1, j1 ) = czero
           b( j1+1, j1 ) = czero
           ! accumulate transformations into q and z if requested.
           if( wantz )call stdlib_I64_zrot( n, z( 1_ilp64, j1 ), 1_ilp64, z( 1_ilp64, j1+1 ), 1_ilp64, cz,conjg( sz ) )
                     
           if( wantq )call stdlib_I64_zrot( n, q( 1_ilp64, j1 ), 1_ilp64, q( 1_ilp64, j1+1 ), 1_ilp64, cq,conjg( sq ) )
                     
           ! exit with info = 0 if swap was successfully performed.
           return
           ! exit with info = 1 if swap was rejected.
           20 continue
           info = 1_ilp64
           return
     end subroutine stdlib_I64_ztgex2



end submodule stdlib_lapack_eigv_comp2
