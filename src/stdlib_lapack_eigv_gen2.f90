submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_gen2
  implicit none


  contains

     module subroutine stdlib_shseqr( job, compz, n, ilo, ihi, h, ldh, wr, wi, z,ldz, work, lwork, info )
     !! SHSEQR computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: nl = 49_ilp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_slahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== nl allocates some local workspace to help small matrices
           ! .    through a rare stdlib_slahqr failure.  nl > ntiny = 15 is
           ! .    required and nl <= nmin = stdlib_ilaenv(ispec=12,...) is recom-
           ! .    mended.  (the default value of nmin is 75.)  using nl = 49
           ! .    allows up to six simultaneous shifts and a 16-by-16
           ! .    deflation window.  ====
           
           
           ! Local Arrays 
           real(sp) :: hl(nl,nl), workl(nl)
           ! Local Scalars 
           integer(ilp) :: i, kbot, nmin
           logical(lk) :: initz, lquery, wantt, wantz
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== decode and check the input parameters. ====
           wantt = stdlib_lsame( job, 'S' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           work( 1_ilp ) = real( max( 1_ilp, n ),KIND=sp)
           lquery = lwork==-1_ilp
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'E' ) .and. .not.wantt ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              ! ==== quick return in case of invalid argument. ====
              call stdlib_xerbla( 'SHSEQR', -info )
              return
           else if( n==0_ilp ) then
              ! ==== quick return in case n = 0; nothing to do. ====
              return
           else if( lquery ) then
              ! ==== quick return in case of a workspace query ====
              call stdlib_slaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                        work, lwork, info )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = max( real( max( 1_ilp, n ),KIND=sp), work( 1_ilp ) )
              return
           else
              ! ==== copy eigenvalues isolated by stdlib_sgebal ====
              do i = 1, ilo - 1
                 wr( i ) = h( i, i )
                 wi( i ) = zero
              end do
              do i = ihi + 1, n
                 wr( i ) = h( i, i )
                 wi( i ) = zero
              end do
              ! ==== initialize z, if requested ====
              if( initz )call stdlib_slaset( 'A', n, n, zero, one, z, ldz )
              ! ==== quick return if possible ====
              if( ilo==ihi ) then
                 wr( ilo ) = h( ilo, ilo )
                 wi( ilo ) = zero
                 return
              end if
              ! ==== stdlib_slahqr/stdlib_slaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'SHSEQR', job( : 1_ilp ) // compz( : 1_ilp ), n,ilo, ihi, lwork )
                        
              nmin = max( ntiny, nmin )
              ! ==== stdlib_slaqr0 for big matrices; stdlib_slahqr for small ones ====
              if( n>nmin ) then
                 call stdlib_slaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                           work, lwork, info )
              else
                 ! ==== small matrix ====
                 call stdlib_slahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                           info )
                 if( info>0_ilp ) then
                    ! ==== a rare stdlib_slahqr failure!  stdlib_slaqr0 sometimes succeeds
                    ! .    when stdlib_slahqr fails. ====
                    kbot = info
                    if( n>=nl ) then
                       ! ==== larger matrices have enough subdiagonal scratch
                       ! .    space to call stdlib_slaqr0 directly. ====
                       call stdlib_slaqr0( wantt, wantz, n, ilo, kbot, h, ldh, wr,wi, ilo, ihi, z,&
                                  ldz, work, lwork, info )
                    else
                       ! ==== tiny matrices don't have enough subdiagonal
                       ! .    scratch space to benefit from stdlib_slaqr0.  hence,
                       ! .    tiny matrices must be copied into a larger
                       ! .    array before calling stdlib_slaqr0. ====
                       call stdlib_slacpy( 'A', n, n, h, ldh, hl, nl )
                       hl( n+1, n ) = zero
                       call stdlib_slaset( 'A', nl, nl-n, zero, zero, hl( 1_ilp, n+1 ),nl )
                       call stdlib_slaqr0( wantt, wantz, nl, ilo, kbot, hl, nl, wr,wi, ilo, ihi, &
                                 z, ldz, workl, nl, info )
                       if( wantt .or. info/=0_ilp )call stdlib_slacpy( 'A', n, n, hl, nl, h, ldh )
                                 
                    end if
                 end if
              end if
              ! ==== clear out the trash, if necessary. ====
              if( ( wantt .or. info/=0_ilp ) .and. n>2_ilp )call stdlib_slaset( 'L', n-2, n-2, zero, zero,&
                         h( 3_ilp, 1_ilp ), ldh )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = max( real( max( 1_ilp, n ),KIND=sp), work( 1_ilp ) )
           end if
     end subroutine stdlib_shseqr

     module subroutine stdlib_dhseqr( job, compz, n, ilo, ihi, h, ldh, wr, wi, z,ldz, work, lwork, info )
     !! DHSEQR computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: nl = 49_ilp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_dlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== nl allocates some local workspace to help small matrices
           ! .    through a rare stdlib_dlahqr failure.  nl > ntiny = 15 is
           ! .    required and nl <= nmin = stdlib_ilaenv(ispec=12,...) is recom-
           ! .    mended.  (the default value of nmin is 75.)  using nl = 49
           ! .    allows up to six simultaneous shifts and a 16-by-16
           ! .    deflation window.  ====
           
           
           ! Local Arrays 
           real(dp) :: hl(nl,nl), workl(nl)
           ! Local Scalars 
           integer(ilp) :: i, kbot, nmin
           logical(lk) :: initz, lquery, wantt, wantz
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== decode and check the input parameters. ====
           wantt = stdlib_lsame( job, 'S' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           work( 1_ilp ) = real( max( 1_ilp, n ),KIND=dp)
           lquery = lwork==-1_ilp
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'E' ) .and. .not.wantt ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<max( 1_ilp, n ) ) ) then
              info = -11_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              ! ==== quick return in case of invalid argument. ====
              call stdlib_xerbla( 'DHSEQR', -info )
              return
           else if( n==0_ilp ) then
              ! ==== quick return in case n = 0; nothing to do. ====
              return
           else if( lquery ) then
              ! ==== quick return in case of a workspace query ====
              call stdlib_dlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                        work, lwork, info )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = max( real( max( 1_ilp, n ),KIND=dp), work( 1_ilp ) )
              return
           else
              ! ==== copy eigenvalues isolated by stdlib_dgebal ====
              do i = 1, ilo - 1
                 wr( i ) = h( i, i )
                 wi( i ) = zero
              end do
              do i = ihi + 1, n
                 wr( i ) = h( i, i )
                 wi( i ) = zero
              end do
              ! ==== initialize z, if requested ====
              if( initz )call stdlib_dlaset( 'A', n, n, zero, one, z, ldz )
              ! ==== quick return if possible ====
              if( ilo==ihi ) then
                 wr( ilo ) = h( ilo, ilo )
                 wi( ilo ) = zero
                 return
              end if
              ! ==== stdlib_dlahqr/stdlib_dlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'DHSEQR', job( : 1_ilp ) // compz( : 1_ilp ), n,ilo, ihi, lwork )
                        
              nmin = max( ntiny, nmin )
              ! ==== stdlib_dlaqr0 for big matrices; stdlib_dlahqr for small ones ====
              if( n>nmin ) then
                 call stdlib_dlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                           work, lwork, info )
              else
                 ! ==== small matrix ====
                 call stdlib_dlahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi, ilo,ihi, z, ldz, &
                           info )
                 if( info>0_ilp ) then
                    ! ==== a rare stdlib_dlahqr failure!  stdlib_dlaqr0 sometimes succeeds
                    ! .    when stdlib_dlahqr fails. ====
                    kbot = info
                    if( n>=nl ) then
                       ! ==== larger matrices have enough subdiagonal scratch
                       ! .    space to call stdlib_dlaqr0 directly. ====
                       call stdlib_dlaqr0( wantt, wantz, n, ilo, kbot, h, ldh, wr,wi, ilo, ihi, z,&
                                  ldz, work, lwork, info )
                    else
                       ! ==== tiny matrices don't have enough subdiagonal
                       ! .    scratch space to benefit from stdlib_dlaqr0.  hence,
                       ! .    tiny matrices must be copied into a larger
                       ! .    array before calling stdlib_dlaqr0. ====
                       call stdlib_dlacpy( 'A', n, n, h, ldh, hl, nl )
                       hl( n+1, n ) = zero
                       call stdlib_dlaset( 'A', nl, nl-n, zero, zero, hl( 1_ilp, n+1 ),nl )
                       call stdlib_dlaqr0( wantt, wantz, nl, ilo, kbot, hl, nl, wr,wi, ilo, ihi, &
                                 z, ldz, workl, nl, info )
                       if( wantt .or. info/=0_ilp )call stdlib_dlacpy( 'A', n, n, hl, nl, h, ldh )
                                 
                    end if
                 end if
              end if
              ! ==== clear out the trash, if necessary. ====
              if( ( wantt .or. info/=0_ilp ) .and. n>2_ilp )call stdlib_dlaset( 'L', n-2, n-2, zero, zero,&
                         h( 3_ilp, 1_ilp ), ldh )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = max( real( max( 1_ilp, n ),KIND=dp), work( 1_ilp ) )
           end if
     end subroutine stdlib_dhseqr


     pure module subroutine stdlib_chseqr( job, compz, n, ilo, ihi, h, ldh, w, z, ldz,work, lwork, info )
     !! CHSEQR computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: nl = 49_ilp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_clahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== nl allocates some local workspace to help small matrices
           ! .    through a rare stdlib_clahqr failure.  nl > ntiny = 15 is
           ! .    required and nl <= nmin = stdlib_ilaenv(ispec=12,...) is recom-
           ! .    mended.  (the default value of nmin is 75.)  using nl = 49
           ! .    allows up to six simultaneous shifts and a 16-by-16
           ! .    deflation window.  ====
           
           
           
           ! Local Arrays 
           complex(sp) :: hl(nl,nl), workl(nl)
           ! Local Scalars 
           integer(ilp) :: kbot, nmin
           logical(lk) :: initz, lquery, wantt, wantz
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== decode and check the input parameters. ====
           wantt = stdlib_lsame( job, 'S' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           work( 1_ilp ) = cmplx( real( max( 1_ilp, n ),KIND=sp), zero,KIND=sp)
           lquery = lwork==-1_ilp
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'E' ) .and. .not.wantt ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<max( 1_ilp, n ) ) ) then
              info = -10_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              ! ==== quick return in case of invalid argument. ====
              call stdlib_xerbla( 'CHSEQR', -info )
              return
           else if( n==0_ilp ) then
              ! ==== quick return in case n = 0; nothing to do. ====
              return
           else if( lquery ) then
              ! ==== quick return in case of a workspace query ====
              call stdlib_claqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi, z,ldz, work, &
                        lwork, info )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = cmplx( max( real( work( 1_ilp ),KIND=sp), real( max( 1_ilp,n ),KIND=sp) ), &
                        zero,KIND=sp)
              return
           else
              ! ==== copy eigenvalues isolated by stdlib_cgebal ====
              if( ilo>1_ilp )call stdlib_ccopy( ilo-1, h, ldh+1, w, 1_ilp )
              if( ihi<n )call stdlib_ccopy( n-ihi, h( ihi+1, ihi+1 ), ldh+1, w( ihi+1 ), 1_ilp )
                        
              ! ==== initialize z, if requested ====
              if( initz )call stdlib_claset( 'A', n, n, czero, cone, z, ldz )
              ! ==== quick return if possible ====
              if( ilo==ihi ) then
                 w( ilo ) = h( ilo, ilo )
                 return
              end if
              ! ==== stdlib_clahqr/stdlib_claqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'CHSEQR', job( : 1_ilp ) // compz( : 1_ilp ), n,ilo, ihi, lwork )
                        
              nmin = max( ntiny, nmin )
              ! ==== stdlib_claqr0 for big matrices; stdlib_clahqr for small ones ====
              if( n>nmin ) then
                 call stdlib_claqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi,z, ldz, work, &
                           lwork, info )
              else
                 ! ==== small matrix ====
                 call stdlib_clahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi,z, ldz, info )
                           
                 if( info>0_ilp ) then
                    ! ==== a rare stdlib_clahqr failure!  stdlib_claqr0 sometimes succeeds
                    ! .    when stdlib_clahqr fails. ====
                    kbot = info
                    if( n>=nl ) then
                       ! ==== larger matrices have enough subdiagonal scratch
                       ! .    space to call stdlib_claqr0 directly. ====
                       call stdlib_claqr0( wantt, wantz, n, ilo, kbot, h, ldh, w,ilo, ihi, z, ldz,&
                                  work, lwork, info )
                    else
                       ! ==== tiny matrices don't have enough subdiagonal
                       ! .    scratch space to benefit from stdlib_claqr0.  hence,
                       ! .    tiny matrices must be copied into a larger
                       ! .    array before calling stdlib_claqr0. ====
                       call stdlib_clacpy( 'A', n, n, h, ldh, hl, nl )
                       hl( n+1, n ) = czero
                       call stdlib_claset( 'A', nl, nl-n, czero, czero, hl( 1_ilp, n+1 ),nl )
                       call stdlib_claqr0( wantt, wantz, nl, ilo, kbot, hl, nl, w,ilo, ihi, z, &
                                 ldz, workl, nl, info )
                       if( wantt .or. info/=0_ilp )call stdlib_clacpy( 'A', n, n, hl, nl, h, ldh )
                                 
                    end if
                 end if
              end if
              ! ==== clear out the trash, if necessary. ====
              if( ( wantt .or. info/=0_ilp ) .and. n>2_ilp )call stdlib_claset( 'L', n-2, n-2, czero, &
                        czero, h( 3_ilp, 1_ilp ), ldh )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = cmplx( max( real( max( 1_ilp, n ),KIND=sp),real( work( 1_ilp ),KIND=sp) ), &
                        zero,KIND=sp)
           end if
     end subroutine stdlib_chseqr

     pure module subroutine stdlib_zhseqr( job, compz, n, ilo, ihi, h, ldh, w, z, ldz,work, lwork, info )
     !! ZHSEQR computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: nl = 49_ilp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_zlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== nl allocates some local workspace to help small matrices
           ! .    through a rare stdlib_zlahqr failure.  nl > ntiny = 15 is
           ! .    required and nl <= nmin = stdlib_ilaenv(ispec=12,...) is recom-
           ! .    mended.  (the default value of nmin is 75.)  using nl = 49
           ! .    allows up to six simultaneous shifts and a 16-by-16
           ! .    deflation window.  ====
           
           
           
           ! Local Arrays 
           complex(dp) :: hl(nl,nl), workl(nl)
           ! Local Scalars 
           integer(ilp) :: kbot, nmin
           logical(lk) :: initz, lquery, wantt, wantz
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== decode and check the input parameters. ====
           wantt = stdlib_lsame( job, 'S' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           work( 1_ilp ) = cmplx( real( max( 1_ilp, n ),KIND=dp), zero,KIND=dp)
           lquery = lwork==-1_ilp
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'E' ) .and. .not.wantt ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp .or. ilo>max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ihi<min( ilo, n ) .or. ihi>n ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<max( 1_ilp, n ) ) ) then
              info = -10_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              ! ==== quick return in case of invalid argument. ====
              call stdlib_xerbla( 'ZHSEQR', -info )
              return
           else if( n==0_ilp ) then
              ! ==== quick return in case n = 0; nothing to do. ====
              return
           else if( lquery ) then
              ! ==== quick return in case of a workspace query ====
              call stdlib_zlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi, z,ldz, work, &
                        lwork, info )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = cmplx( max( real( work( 1_ilp ),KIND=dp), real( max( 1_ilp,n ),KIND=dp) ), &
                        zero,KIND=dp)
              return
           else
              ! ==== copy eigenvalues isolated by stdlib_zgebal ====
              if( ilo>1_ilp )call stdlib_zcopy( ilo-1, h, ldh+1, w, 1_ilp )
              if( ihi<n )call stdlib_zcopy( n-ihi, h( ihi+1, ihi+1 ), ldh+1, w( ihi+1 ), 1_ilp )
                        
              ! ==== initialize z, if requested ====
              if( initz )call stdlib_zlaset( 'A', n, n, czero, cone, z, ldz )
              ! ==== quick return if possible ====
              if( ilo==ihi ) then
                 w( ilo ) = h( ilo, ilo )
                 return
              end if
              ! ==== stdlib_zlahqr/stdlib_zlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'ZHSEQR', job( : 1_ilp ) // compz( : 1_ilp ), n,ilo, ihi, lwork )
                        
              nmin = max( ntiny, nmin )
              ! ==== stdlib_zlaqr0 for big matrices; stdlib_zlahqr for small ones ====
              if( n>nmin ) then
                 call stdlib_zlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi,z, ldz, work, &
                           lwork, info )
              else
                 ! ==== small matrix ====
                 call stdlib_zlahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, ilo, ihi,z, ldz, info )
                           
                 if( info>0_ilp ) then
                    ! ==== a rare stdlib_zlahqr failure!  stdlib_zlaqr0 sometimes succeeds
                    ! .    when stdlib_zlahqr fails. ====
                    kbot = info
                    if( n>=nl ) then
                       ! ==== larger matrices have enough subdiagonal scratch
                       ! .    space to call stdlib_zlaqr0 directly. ====
                       call stdlib_zlaqr0( wantt, wantz, n, ilo, kbot, h, ldh, w,ilo, ihi, z, ldz,&
                                  work, lwork, info )
                    else
                       ! ==== tiny matrices don't have enough subdiagonal
                       ! .    scratch space to benefit from stdlib_zlaqr0.  hence,
                       ! .    tiny matrices must be copied into a larger
                       ! .    array before calling stdlib_zlaqr0. ====
                       call stdlib_zlacpy( 'A', n, n, h, ldh, hl, nl )
                       hl( n+1, n ) = czero
                       call stdlib_zlaset( 'A', nl, nl-n, czero, czero, hl( 1_ilp, n+1 ),nl )
                       call stdlib_zlaqr0( wantt, wantz, nl, ilo, kbot, hl, nl, w,ilo, ihi, z, &
                                 ldz, workl, nl, info )
                       if( wantt .or. info/=0_ilp )call stdlib_zlacpy( 'A', n, n, hl, nl, h, ldh )
                                 
                    end if
                 end if
              end if
              ! ==== clear out the trash, if necessary. ====
              if( ( wantt .or. info/=0_ilp ) .and. n>2_ilp )call stdlib_zlaset( 'L', n-2, n-2, czero, &
                        czero, h( 3_ilp, 1_ilp ), ldh )
              ! ==== ensure reported workspace size is backward-compatible with
              ! .    previous lapack versions. ====
              work( 1_ilp ) = cmplx( max( real( max( 1_ilp, n ),KIND=dp),real( work( 1_ilp ),KIND=dp) ), &
                        zero,KIND=dp)
           end if
     end subroutine stdlib_zhseqr




     module subroutine stdlib_shsein( side, eigsrc, initv, select, n, h, ldh, wr, wi,vl, ldvl, vr, ldvr, &
     !! SHSEIN uses inverse iteration to find specified right and/or left
     !! eigenvectors of a real upper Hessenberg matrix H.
     !! The right eigenvector x and the left eigenvector y of the matrix H
     !! corresponding to an eigenvalue w are defined by:
     !! H * x = w * x,     y**h * H = w * y**h
     !! where y**h denotes the conjugate transpose of the vector y.
               mm, m, work, ifaill,ifailr, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(sp), intent(in) :: h(ldh,*), wi(*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), wr(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: bothv, fromqr, leftv, noinit, pair, rightv
           integer(ilp) :: i, iinfo, k, kl, kln, kr, ksi, ksr, ldwork
           real(sp) :: bignum, eps3, hnorm, smlnum, ulp, unfl, wki, wkr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           fromqr = stdlib_lsame( eigsrc, 'Q' )
           noinit = stdlib_lsame( initv, 'N' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors, and standardize the array select.
           m = 0_ilp
           pair = .false.
           do k = 1, n
              if( pair ) then
                 pair = .false.
                 select( k ) = .false.
              else
                 if( wi( k )==zero ) then
                    if( select( k ) )m = m + 1_ilp
                 else
                    pair = .true.
                    if( select( k ) .or. select( k+1 ) ) then
                       select( k ) = .true.
                       m = m + 2_ilp
                    end if
                 end if
              end if
           end do
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.fromqr .and. .not.stdlib_lsame( eigsrc, 'N' ) ) then
              info = -2_ilp
           else if( .not.noinit .and. .not.stdlib_lsame( initv, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -13_ilp
           else if( mm<m ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SHSEIN', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set machine-dependent constants.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ldwork = n + 1_ilp
           kl = 1_ilp
           kln = 0_ilp
           if( fromqr ) then
              kr = 0_ilp
           else
              kr = n
           end if
           ksr = 1_ilp
           loop_120: do k = 1, n
              if( select( k ) ) then
                 ! compute eigenvector(s) corresponding to w(k).
                 if( fromqr ) then
                    ! if affiliation of eigenvalues is known, check whether
                    ! the matrix splits.
                    ! determine kl and kr such that 1 <= kl <= k <= kr <= n
                    ! and h(kl,kl-1) and h(kr+1,kr) are zero (or kl = 1 or
                    ! kr = n).
                    ! then inverse iteration can be performed with the
                    ! submatrix h(kl:n,kl:n) for a left eigenvector, and with
                    ! the submatrix h(1:kr,1:kr) for a right eigenvector.
                    do i = k, kl + 1, -1
                       if( h( i, i-1 )==zero )go to 30
                    end do
                    30 continue
                    kl = i
                    if( k>kr ) then
                       do i = k, n - 1
                          if( h( i+1, i )==zero )go to 50
                       end do
                       50 continue
                       kr = i
                    end if
                 end if
                 if( kl/=kln ) then
                    kln = kl
                    ! compute infinity-norm of submatrix h(kl:kr,kl:kr) if it
                    ! has not ben computed before.
                    hnorm = stdlib_slanhs( 'I', kr-kl+1, h( kl, kl ), ldh, work )
                    if( stdlib_sisnan( hnorm ) ) then
                       info = -6_ilp
                       return
                    else if( hnorm>zero ) then
                       eps3 = hnorm*ulp
                    else
                       eps3 = smlnum
                    end if
                 end if
                 ! perturb eigenvalue if it is close to any previous
                 ! selected eigenvalues affiliated to the submatrix
                 ! h(kl:kr,kl:kr). close roots are modified by eps3.
                 wkr = wr( k )
                 wki = wi( k )
                 60 continue
                 do i = k - 1, kl, -1
                    if( select( i ) .and. abs( wr( i )-wkr )+abs( wi( i )-wki )<eps3 ) &
                              then
                       wkr = wkr + eps3
                       go to 60
                    end if
                 end do
                 wr( k ) = wkr
                 pair = wki/=zero
                 if( pair ) then
                    ksi = ksr + 1_ilp
                 else
                    ksi = ksr
                 end if
                 if( leftv ) then
                    ! compute left eigenvector.
                    call stdlib_slaein( .false., noinit, n-kl+1, h( kl, kl ), ldh,wkr, wki, vl( &
                    kl, ksr ), vl( kl, ksi ),work, ldwork, work( n*n+n+1 ), eps3, smlnum,bignum, &
                              iinfo )
                    if( iinfo>0_ilp ) then
                       if( pair ) then
                          info = info + 2_ilp
                       else
                          info = info + 1_ilp
                       end if
                       ifaill( ksr ) = k
                       ifaill( ksi ) = k
                    else
                       ifaill( ksr ) = 0_ilp
                       ifaill( ksi ) = 0_ilp
                    end if
                    do i = 1, kl - 1
                       vl( i, ksr ) = zero
                    end do
                    if( pair ) then
                       do i = 1, kl - 1
                          vl( i, ksi ) = zero
                       end do
                    end if
                 end if
                 if( rightv ) then
                    ! compute right eigenvector.
                    call stdlib_slaein( .true., noinit, kr, h, ldh, wkr, wki,vr( 1_ilp, ksr ), vr( 1_ilp, &
                              ksi ), work, ldwork,work( n*n+n+1 ), eps3, smlnum, bignum,iinfo )
                    if( iinfo>0_ilp ) then
                       if( pair ) then
                          info = info + 2_ilp
                       else
                          info = info + 1_ilp
                       end if
                       ifailr( ksr ) = k
                       ifailr( ksi ) = k
                    else
                       ifailr( ksr ) = 0_ilp
                       ifailr( ksi ) = 0_ilp
                    end if
                    do i = kr + 1, n
                       vr( i, ksr ) = zero
                    end do
                    if( pair ) then
                       do i = kr + 1, n
                          vr( i, ksi ) = zero
                       end do
                    end if
                 end if
                 if( pair ) then
                    ksr = ksr + 2_ilp
                 else
                    ksr = ksr + 1_ilp
                 end if
              end if
           end do loop_120
           return
     end subroutine stdlib_shsein

     module subroutine stdlib_dhsein( side, eigsrc, initv, select, n, h, ldh, wr, wi,vl, ldvl, vr, ldvr, &
     !! DHSEIN uses inverse iteration to find specified right and/or left
     !! eigenvectors of a real upper Hessenberg matrix H.
     !! The right eigenvector x and the left eigenvector y of the matrix H
     !! corresponding to an eigenvalue w are defined by:
     !! H * x = w * x,     y**h * H = w * y**h
     !! where y**h denotes the conjugate transpose of the vector y.
               mm, m, work, ifaill,ifailr, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(dp), intent(in) :: h(ldh,*), wi(*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), wr(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: bothv, fromqr, leftv, noinit, pair, rightv
           integer(ilp) :: i, iinfo, k, kl, kln, kr, ksi, ksr, ldwork
           real(dp) :: bignum, eps3, hnorm, smlnum, ulp, unfl, wki, wkr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           fromqr = stdlib_lsame( eigsrc, 'Q' )
           noinit = stdlib_lsame( initv, 'N' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors, and standardize the array select.
           m = 0_ilp
           pair = .false.
           do k = 1, n
              if( pair ) then
                 pair = .false.
                 select( k ) = .false.
              else
                 if( wi( k )==zero ) then
                    if( select( k ) )m = m + 1_ilp
                 else
                    pair = .true.
                    if( select( k ) .or. select( k+1 ) ) then
                       select( k ) = .true.
                       m = m + 2_ilp
                    end if
                 end if
              end if
           end do
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.fromqr .and. .not.stdlib_lsame( eigsrc, 'N' ) ) then
              info = -2_ilp
           else if( .not.noinit .and. .not.stdlib_lsame( initv, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -11_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -13_ilp
           else if( mm<m ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DHSEIN', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set machine-dependent constants.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ldwork = n + 1_ilp
           kl = 1_ilp
           kln = 0_ilp
           if( fromqr ) then
              kr = 0_ilp
           else
              kr = n
           end if
           ksr = 1_ilp
           loop_120: do k = 1, n
              if( select( k ) ) then
                 ! compute eigenvector(s) corresponding to w(k).
                 if( fromqr ) then
                    ! if affiliation of eigenvalues is known, check whether
                    ! the matrix splits.
                    ! determine kl and kr such that 1 <= kl <= k <= kr <= n
                    ! and h(kl,kl-1) and h(kr+1,kr) are zero (or kl = 1 or
                    ! kr = n).
                    ! then inverse iteration can be performed with the
                    ! submatrix h(kl:n,kl:n) for a left eigenvector, and with
                    ! the submatrix h(1:kr,1:kr) for a right eigenvector.
                    do i = k, kl + 1, -1
                       if( h( i, i-1 )==zero )go to 30
                    end do
                    30 continue
                    kl = i
                    if( k>kr ) then
                       do i = k, n - 1
                          if( h( i+1, i )==zero )go to 50
                       end do
                       50 continue
                       kr = i
                    end if
                 end if
                 if( kl/=kln ) then
                    kln = kl
                    ! compute infinity-norm of submatrix h(kl:kr,kl:kr) if it
                    ! has not ben computed before.
                    hnorm = stdlib_dlanhs( 'I', kr-kl+1, h( kl, kl ), ldh, work )
                    if( stdlib_disnan( hnorm ) ) then
                       info = -6_ilp
                       return
                    else if( hnorm>zero ) then
                       eps3 = hnorm*ulp
                    else
                       eps3 = smlnum
                    end if
                 end if
                 ! perturb eigenvalue if it is close to any previous
                 ! selected eigenvalues affiliated to the submatrix
                 ! h(kl:kr,kl:kr). close roots are modified by eps3.
                 wkr = wr( k )
                 wki = wi( k )
                 60 continue
                 do i = k - 1, kl, -1
                    if( select( i ) .and. abs( wr( i )-wkr )+abs( wi( i )-wki )<eps3 ) &
                              then
                       wkr = wkr + eps3
                       go to 60
                    end if
                 end do
                 wr( k ) = wkr
                 pair = wki/=zero
                 if( pair ) then
                    ksi = ksr + 1_ilp
                 else
                    ksi = ksr
                 end if
                 if( leftv ) then
                    ! compute left eigenvector.
                    call stdlib_dlaein( .false., noinit, n-kl+1, h( kl, kl ), ldh,wkr, wki, vl( &
                    kl, ksr ), vl( kl, ksi ),work, ldwork, work( n*n+n+1 ), eps3, smlnum,bignum, &
                              iinfo )
                    if( iinfo>0_ilp ) then
                       if( pair ) then
                          info = info + 2_ilp
                       else
                          info = info + 1_ilp
                       end if
                       ifaill( ksr ) = k
                       ifaill( ksi ) = k
                    else
                       ifaill( ksr ) = 0_ilp
                       ifaill( ksi ) = 0_ilp
                    end if
                    do i = 1, kl - 1
                       vl( i, ksr ) = zero
                    end do
                    if( pair ) then
                       do i = 1, kl - 1
                          vl( i, ksi ) = zero
                       end do
                    end if
                 end if
                 if( rightv ) then
                    ! compute right eigenvector.
                    call stdlib_dlaein( .true., noinit, kr, h, ldh, wkr, wki,vr( 1_ilp, ksr ), vr( 1_ilp, &
                              ksi ), work, ldwork,work( n*n+n+1 ), eps3, smlnum, bignum,iinfo )
                    if( iinfo>0_ilp ) then
                       if( pair ) then
                          info = info + 2_ilp
                       else
                          info = info + 1_ilp
                       end if
                       ifailr( ksr ) = k
                       ifailr( ksi ) = k
                    else
                       ifailr( ksr ) = 0_ilp
                       ifailr( ksi ) = 0_ilp
                    end if
                    do i = kr + 1, n
                       vr( i, ksr ) = zero
                    end do
                    if( pair ) then
                       do i = kr + 1, n
                          vr( i, ksi ) = zero
                       end do
                    end if
                 end if
                 if( pair ) then
                    ksr = ksr + 2_ilp
                 else
                    ksr = ksr + 1_ilp
                 end if
              end if
           end do loop_120
           return
     end subroutine stdlib_dhsein


     module subroutine stdlib_chsein( side, eigsrc, initv, select, n, h, ldh, w, vl,ldvl, vr, ldvr, mm, &
     !! CHSEIN uses inverse iteration to find specified right and/or left
     !! eigenvectors of a complex upper Hessenberg matrix H.
     !! The right eigenvector x and the left eigenvector y of the matrix H
     !! corresponding to an eigenvalue w are defined by:
     !! H * x = w * x,     y**h * H = w * y**h
     !! where y**h denotes the conjugate transpose of the vector y.
               m, work, rwork, ifaill,ifailr, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), w(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: bothv, fromqr, leftv, noinit, rightv
           integer(ilp) :: i, iinfo, k, kl, kln, kr, ks, ldwork
           real(sp) :: eps3, hnorm, smlnum, ulp, unfl
           complex(sp) :: cdum, wk
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters.
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           fromqr = stdlib_lsame( eigsrc, 'Q' )
           noinit = stdlib_lsame( initv, 'N' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           m = 0_ilp
           do k = 1, n
              if( select( k ) )m = m + 1_ilp
           end do
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.fromqr .and. .not.stdlib_lsame( eigsrc, 'N' ) ) then
              info = -2_ilp
           else if( .not.noinit .and. .not.stdlib_lsame( initv, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -10_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -12_ilp
           else if( mm<m ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHSEIN', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set machine-dependent constants.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ldwork = n
           kl = 1_ilp
           kln = 0_ilp
           if( fromqr ) then
              kr = 0_ilp
           else
              kr = n
           end if
           ks = 1_ilp
           loop_100: do k = 1, n
              if( select( k ) ) then
                 ! compute eigenvector(s) corresponding to w(k).
                 if( fromqr ) then
                    ! if affiliation of eigenvalues is known, check whether
                    ! the matrix splits.
                    ! determine kl and kr such that 1 <= kl <= k <= kr <= n
                    ! and h(kl,kl-1) and h(kr+1,kr) are czero (or kl = 1 or
                    ! kr = n).
                    ! then inverse iteration can be performed with the
                    ! submatrix h(kl:n,kl:n) for a left eigenvector, and with
                    ! the submatrix h(1:kr,1:kr) for a right eigenvector.
                    do i = k, kl + 1, -1
                       if( h( i, i-1 )==czero )go to 30
                    end do
                    30 continue
                    kl = i
                    if( k>kr ) then
                       do i = k, n - 1
                          if( h( i+1, i )==czero )go to 50
                       end do
                       50 continue
                       kr = i
                    end if
                 end if
                 if( kl/=kln ) then
                    kln = kl
                    ! compute infinity-norm of submatrix h(kl:kr,kl:kr) if it
                    ! has not ben computed before.
                    hnorm = stdlib_clanhs( 'I', kr-kl+1, h( kl, kl ), ldh, rwork )
                    if( stdlib_sisnan( hnorm ) ) then
                       info = -6_ilp
                       return
                    else if( (hnorm>zero) ) then
                       eps3 = hnorm*ulp
                    else
                       eps3 = smlnum
                    end if
                 end if
                 ! perturb eigenvalue if it is close to any previous
                 ! selected eigenvalues affiliated to the submatrix
                 ! h(kl:kr,kl:kr). close roots are modified by eps3.
                 wk = w( k )
                 60 continue
                 do i = k - 1, kl, -1
                    if( select( i ) .and. cabs1( w( i )-wk )<eps3 ) then
                       wk = wk + eps3
                       go to 60
                    end if
                 end do
                 w( k ) = wk
                 if( leftv ) then
                    ! compute left eigenvector.
                    call stdlib_claein( .false., noinit, n-kl+1, h( kl, kl ), ldh,wk, vl( kl, ks )&
                              , work, ldwork, rwork, eps3,smlnum, iinfo )
                    if( iinfo>0_ilp ) then
                       info = info + 1_ilp
                       ifaill( ks ) = k
                    else
                       ifaill( ks ) = 0_ilp
                    end if
                    do i = 1, kl - 1
                       vl( i, ks ) = czero
                    end do
                 end if
                 if( rightv ) then
                    ! compute right eigenvector.
                    call stdlib_claein( .true., noinit, kr, h, ldh, wk, vr( 1_ilp, ks ),work, ldwork, &
                              rwork, eps3, smlnum, iinfo )
                    if( iinfo>0_ilp ) then
                       info = info + 1_ilp
                       ifailr( ks ) = k
                    else
                       ifailr( ks ) = 0_ilp
                    end if
                    do i = kr + 1, n
                       vr( i, ks ) = czero
                    end do
                 end if
                 ks = ks + 1_ilp
              end if
           end do loop_100
           return
     end subroutine stdlib_chsein

     module subroutine stdlib_zhsein( side, eigsrc, initv, select, n, h, ldh, w, vl,ldvl, vr, ldvr, mm, &
     !! ZHSEIN uses inverse iteration to find specified right and/or left
     !! eigenvectors of a complex upper Hessenberg matrix H.
     !! The right eigenvector x and the left eigenvector y of the matrix H
     !! corresponding to an eigenvalue w are defined by:
     !! H * x = w * x,     y**h * H = w * y**h
     !! where y**h denotes the conjugate transpose of the vector y.
               m, work, rwork, ifaill,ifailr, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), w(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: bothv, fromqr, leftv, noinit, rightv
           integer(ilp) :: i, iinfo, k, kl, kln, kr, ks, ldwork
           real(dp) :: eps3, hnorm, smlnum, ulp, unfl
           complex(dp) :: cdum, wk
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters.
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           fromqr = stdlib_lsame( eigsrc, 'Q' )
           noinit = stdlib_lsame( initv, 'N' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           m = 0_ilp
           do k = 1, n
              if( select( k ) )m = m + 1_ilp
           end do
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.fromqr .and. .not.stdlib_lsame( eigsrc, 'N' ) ) then
              info = -2_ilp
           else if( .not.noinit .and. .not.stdlib_lsame( initv, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldh<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -10_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -12_ilp
           else if( mm<m ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHSEIN', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set machine-dependent constants.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ldwork = n
           kl = 1_ilp
           kln = 0_ilp
           if( fromqr ) then
              kr = 0_ilp
           else
              kr = n
           end if
           ks = 1_ilp
           loop_100: do k = 1, n
              if( select( k ) ) then
                 ! compute eigenvector(s) corresponding to w(k).
                 if( fromqr ) then
                    ! if affiliation of eigenvalues is known, check whether
                    ! the matrix splits.
                    ! determine kl and kr such that 1 <= kl <= k <= kr <= n
                    ! and h(kl,kl-1) and h(kr+1,kr) are czero (or kl = 1 or
                    ! kr = n).
                    ! then inverse iteration can be performed with the
                    ! submatrix h(kl:n,kl:n) for a left eigenvector, and with
                    ! the submatrix h(1:kr,1:kr) for a right eigenvector.
                    do i = k, kl + 1, -1
                       if( h( i, i-1 )==czero )go to 30
                    end do
                    30 continue
                    kl = i
                    if( k>kr ) then
                       do i = k, n - 1
                          if( h( i+1, i )==czero )go to 50
                       end do
                       50 continue
                       kr = i
                    end if
                 end if
                 if( kl/=kln ) then
                    kln = kl
                    ! compute infinity-norm of submatrix h(kl:kr,kl:kr) if it
                    ! has not ben computed before.
                    hnorm = stdlib_zlanhs( 'I', kr-kl+1, h( kl, kl ), ldh, rwork )
                    if( stdlib_disnan( hnorm ) ) then
                       info = -6_ilp
                       return
                    else if( hnorm>zero ) then
                       eps3 = hnorm*ulp
                    else
                       eps3 = smlnum
                    end if
                 end if
                 ! perturb eigenvalue if it is close to any previous
                 ! selected eigenvalues affiliated to the submatrix
                 ! h(kl:kr,kl:kr). close roots are modified by eps3.
                 wk = w( k )
                 60 continue
                 do i = k - 1, kl, -1
                    if( select( i ) .and. cabs1( w( i )-wk )<eps3 ) then
                       wk = wk + eps3
                       go to 60
                    end if
                 end do
                 w( k ) = wk
                 if( leftv ) then
                    ! compute left eigenvector.
                    call stdlib_zlaein( .false., noinit, n-kl+1, h( kl, kl ), ldh,wk, vl( kl, ks )&
                              , work, ldwork, rwork, eps3,smlnum, iinfo )
                    if( iinfo>0_ilp ) then
                       info = info + 1_ilp
                       ifaill( ks ) = k
                    else
                       ifaill( ks ) = 0_ilp
                    end if
                    do i = 1, kl - 1
                       vl( i, ks ) = czero
                    end do
                 end if
                 if( rightv ) then
                    ! compute right eigenvector.
                    call stdlib_zlaein( .true., noinit, kr, h, ldh, wk, vr( 1_ilp, ks ),work, ldwork, &
                              rwork, eps3, smlnum, iinfo )
                    if( iinfo>0_ilp ) then
                       info = info + 1_ilp
                       ifailr( ks ) = k
                    else
                       ifailr( ks ) = 0_ilp
                    end if
                    do i = kr + 1, n
                       vr( i, ks ) = czero
                    end do
                 end if
                 ks = ks + 1_ilp
              end if
           end do loop_100
           return
     end subroutine stdlib_zhsein




     pure module subroutine stdlib_strevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! STREVC computes some or all of the right and/or left eigenvectors of
     !! a real upper quasi-triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a real general matrix:  A = Q*T*Q**T, as computed by SHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal blocks of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix.  If Q is the orthogonal factor that reduces a matrix
     !! A to Schur form T, then Q*X and Q*Y are the matrices of right and
     !! left eigenvectors of A.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, over, pair, rightv, somev
           integer(ilp) :: i, ierr, ii, ip, is, j, j1, j2, jnxt, k, ki, n2
           real(sp) :: beta, bignum, emax, ovfl, rec, remax, scale, smin, smlnum, ulp, unfl, &
                     vcrit, vmax, wi, wr, xnorm
           ! Intrinsic Functions 
           ! Local Arrays 
           real(sp) :: x(2_ilp,2_ilp)
           ! Executable Statements 
           ! decode and test the input parameters
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           allv = stdlib_lsame( howmny, 'A' )
           over = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else
              ! set m to the number of columns required to store the selected
              ! eigenvectors, standardize the array select if necessary, and
              ! test mm.
              if( somev ) then
                 m = 0_ilp
                 pair = .false.
                 do j = 1, n
                    if( pair ) then
                       pair = .false.
                       select( j ) = .false.
                    else
                       if( j<n ) then
                          if( t( j+1, j )==zero ) then
                             if( select( j ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( j ) .or. select( j+1 ) ) then
                                select( j ) = .true.
                                m = m + 2_ilp
                             end if
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( mm<m ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STREVC', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set the constants to control overflow.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_slabad( unfl, ovfl )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = zero
              do i = 1, j - 1
                 work( j ) = work( j ) + abs( t( i, j ) )
              end do
           end do
           ! index ip is used to specify the real or complex eigenvalue:
             ! ip = 0, real eigenvalue,
                  ! 1, first of conjugate complex pair: (wr,wi)
                 ! -1, second of conjugate complex pair: (wr,wi)
           n2 = 2_ilp*n
           if( rightv ) then
              ! compute right eigenvectors.
              ip = 0_ilp
              is = m
              loop_140: do ki = n, 1, -1
                 if( ip==1 )go to 130
                 if( ki==1 )go to 40
                 if( t( ki, ki-1 )==zero )go to 40
                 ip = -1_ilp
                 40 continue
                 if( somev ) then
                    if( ip==0_ilp ) then
                       if( .not.select( ki ) )go to 130
                    else
                       if( .not.select( ki-1 ) )go to 130
                    end if
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki-1 ) ) )*sqrt( abs( t( ki-1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! real right eigenvector
                    work( ki+n ) = one
                    ! form right-hand side
                    do k = 1, ki - 1
                       work( k+n ) = -t( k, ki )
                    end do
                    ! solve the upper quasi-triangular system:
                       ! (t(1:ki-1,1:ki-1) - wr)*x = scale*work.
                    jnxt = ki - 1_ilp
                    loop_60: do j = ki - 1, 1, -1
                       if( j>jnxt )cycle loop_60
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1 = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_slaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_saxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+n ), n, wr, zero, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x(1,1) and x(2,1) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                          work( j-1+n ) = x( 1_ilp, 1_ilp )
                          work( j+n ) = x( 2_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_saxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                       end if
                    end do loop_60
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       call stdlib_scopy( ki, work( 1_ilp+n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       ii = stdlib_isamax( ki, vr( 1_ilp, is ), 1_ilp )
                       remax = one / abs( vr( ii, is ) )
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is ) = zero
                       end do
                    else
                       if( ki>1_ilp )call stdlib_sgemv( 'N', n, ki-1, one, vr, ldvr,work( 1_ilp+n ), 1_ilp, &
                                 work( ki+n ),vr( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_isamax( n, vr( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vr( ii, ki ) )
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    end if
                 else
                    ! complex right eigenvector.
                    ! initial solve
                      ! [ (t(ki-1,ki-1) t(ki-1,ki) ) - (wr + i* wi)]*x = 0.
                      ! [ (t(ki,ki-1)   t(ki,ki)   )               ]
                    if( abs( t( ki-1, ki ) )>=abs( t( ki, ki-1 ) ) ) then
                       work( ki-1+n ) = one
                       work( ki+n2 ) = wi / t( ki-1, ki )
                    else
                       work( ki-1+n ) = -wi / t( ki, ki-1 )
                       work( ki+n2 ) = one
                    end if
                    work( ki+n ) = zero
                    work( ki-1+n2 ) = zero
                    ! form right-hand side
                    do k = 1, ki - 2
                       work( k+n ) = -work( ki-1+n )*t( k, ki-1 )
                       work( k+n2 ) = -work( ki+n2 )*t( k, ki )
                    end do
                    ! solve upper quasi-triangular system:
                    ! (t(1:ki-2,1:ki-2) - (wr+i*wi))*x = scale*(work+i*work2)
                    jnxt = ki - 2_ilp
                    loop_90: do j = ki - 2, 1, -1
                       if( j>jnxt )cycle loop_90
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1 = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_slaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr, wi,x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) and x(1,2) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                             call stdlib_sscal( ki, scale, work( 1_ilp+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_saxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-1, -x( 1_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_slaln2( .false., 2_ilp, 2_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+n ), n, wr, wi, x, 2_ilp, scale,xnorm, ierr )
                          ! scale x to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                rec = one / xnorm
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp )*rec
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp )*rec
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp )*rec
                                x( 2_ilp, 2_ilp ) = x( 2_ilp, 2_ilp )*rec
                                scale = scale*rec
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                             call stdlib_sscal( ki, scale, work( 1_ilp+n2 ), 1_ilp )
                          end if
                          work( j-1+n ) = x( 1_ilp, 1_ilp )
                          work( j+n ) = x( 2_ilp, 1_ilp )
                          work( j-1+n2 ) = x( 1_ilp, 2_ilp )
                          work( j+n2 ) = x( 2_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_saxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-2, -x( 1_ilp, 2_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                          call stdlib_saxpy( j-2, -x( 2_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                       end if
                    end do loop_90
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       call stdlib_scopy( ki, work( 1_ilp+n ), 1_ilp, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_scopy( ki, work( 1_ilp+n2 ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       emax = zero
                       do k = 1, ki
                          emax = max( emax, abs( vr( k, is-1 ) )+abs( vr( k, is ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is-1 ) = zero
                          vr( k, is ) = zero
                       end do
                    else
                       if( ki>2_ilp ) then
                          call stdlib_sgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp+n ), 1_ilp, work( ki-&
                                    1_ilp+n ),vr( 1_ilp, ki-1 ), 1_ilp )
                          call stdlib_sgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp+n2 ), 1_ilp, work( &
                                    ki+n2 ),vr( 1_ilp, ki ), 1_ilp )
                       else
                          call stdlib_sscal( n, work( ki-1+n ), vr( 1_ilp, ki-1 ), 1_ilp )
                          call stdlib_sscal( n, work( ki+n2 ), vr( 1_ilp, ki ), 1_ilp )
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vr( k, ki-1 ) )+abs( vr( k, ki ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki-1 ), 1_ilp )
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    end if
                 end if
                 is = is - 1_ilp
                 if( ip/=0_ilp )is = is - 1_ilp
                 130 continue
                 if( ip==1_ilp )ip = 0_ilp
                 if( ip==-1_ilp )ip = 1_ilp
              end do loop_140
           end if
           if( leftv ) then
              ! compute left eigenvectors.
              ip = 0_ilp
              is = 1_ilp
              loop_260: do ki = 1, n
                 if( ip==-1 )go to 250
                 if( ki==n )go to 150
                 if( t( ki+1, ki )==zero )go to 150
                 ip = 1_ilp
                 150 continue
                 if( somev ) then
                    if( .not.select( ki ) )go to 250
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki+1 ) ) )*sqrt( abs( t( ki+1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! real left eigenvector.
                    work( ki+n ) = one
                    ! form right-hand side
                    do k = ki + 1, n
                       work( k+n ) = -t( ki, k )
                    end do
                    ! solve the quasi-triangular system:
                       ! (t(ki+1:n,ki+1:n) - wr)**t*x = scale*work
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 1_ilp
                    loop_170: do j = ki + 1, n
                       if( j<jnxt )cycle loop_170
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_sdot( j-ki-1, t( ki+1, j ), 1_ilp,work( &
                                    ki+1+n ), 1_ilp )
                          ! solve (t(j,j)-wr)**t*x = work
                          call stdlib_slaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                                    
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          vmax = max( abs( work( j+n ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_sdot( j-ki-1, t( ki+1, j ), 1_ilp,work( &
                                    ki+1+n ), 1_ilp )
                          work( j+1+n ) = work( j+1+n ) -stdlib_sdot( j-ki-1, t( ki+1, j+1 ), 1_ilp,&
                                    work( ki+1+n ), 1_ilp )
                          ! solve
                            ! [t(j,j)-wr   t(j,j+1)     ]**t* x = scale*( work1 )
                            ! [t(j+1,j)    t(j+1,j+1)-wr]               ( work2 )
                          call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                                    
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+1+n ) = x( 2_ilp, 1_ilp )
                          vmax = max( abs( work( j+n ) ),abs( work( j+1+n ) ), vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_170
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       call stdlib_scopy( n-ki+1, work( ki+n ), 1_ilp, vl( ki, is ), 1_ilp )
                       ii = stdlib_isamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                       remax = one / abs( vl( ii, is ) )
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                       end do
                    else
                       if( ki<n )call stdlib_sgemv( 'N', n, n-ki, one, vl( 1_ilp, ki+1 ), ldvl,work( &
                                 ki+1+n ), 1_ilp, work( ki+n ),vl( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_isamax( n, vl( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vl( ii, ki ) )
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                    end if
                 else
                    ! complex left eigenvector.
                     ! initial solve:
                       ! ((t(ki,ki)    t(ki,ki+1) )**t - (wr - i* wi))*x = 0.
                       ! ((t(ki+1,ki) t(ki+1,ki+1))                )
                    if( abs( t( ki, ki+1 ) )>=abs( t( ki+1, ki ) ) ) then
                       work( ki+n ) = wi / t( ki, ki+1 )
                       work( ki+1+n2 ) = one
                    else
                       work( ki+n ) = one
                       work( ki+1+n2 ) = -wi / t( ki+1, ki )
                    end if
                    work( ki+1+n ) = zero
                    work( ki+n2 ) = zero
                    ! form right-hand side
                    do k = ki + 2, n
                       work( k+n ) = -work( ki+n )*t( ki, k )
                       work( k+n2 ) = -work( ki+1+n2 )*t( ki+1, k )
                    end do
                    ! solve complex quasi-triangular system:
                    ! ( t(ki+2,n:ki+2,n) - (wr-i*wi) )*x = work1+i*work2
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 2_ilp
                    loop_200: do j = ki + 2, n
                       if( j<jnxt )cycle loop_200
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when
                          ! forming the right-hand side elements.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             call stdlib_sscal( n-ki+1, rec, work( ki+n2 ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_sdot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n ), 1_ilp )
                          work( j+n2 ) = work( j+n2 ) -stdlib_sdot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n2 ), 1_ilp )
                          ! solve (t(j,j)-(wr-i*wi))*(x11+i*x12)= wk+i*wk2
                          call stdlib_slaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                             call stdlib_sscal( n-ki+1, scale, work( ki+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          vmax = max( abs( work( j+n ) ),abs( work( j+n2 ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side elements.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             call stdlib_sscal( n-ki+1, rec, work( ki+n2 ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_sdot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n ), 1_ilp )
                          work( j+n2 ) = work( j+n2 ) -stdlib_sdot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n2 ), 1_ilp )
                          work( j+1+n ) = work( j+1+n ) -stdlib_sdot( j-ki-2, t( ki+2, j+1 ), 1_ilp,&
                                    work( ki+2+n ), 1_ilp )
                          work( j+1+n2 ) = work( j+1+n2 ) -stdlib_sdot( j-ki-2, t( ki+2, j+1 ), 1_ilp,&
                                    work( ki+2+n2 ), 1_ilp )
                          ! solve 2-by-2 complex linear equation
                            ! ([t(j,j)   t(j,j+1)  ]**t-(wr-i*wi)*i)*x = scale*b
                            ! ([t(j+1,j) t(j+1,j+1)]               )
                          call stdlib_slaln2( .true., 2_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                             call stdlib_sscal( n-ki+1, scale, work( ki+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          work( j+1+n ) = x( 2_ilp, 1_ilp )
                          work( j+1+n2 ) = x( 2_ilp, 2_ilp )
                          vmax = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 1_ilp, 2_ilp ) ),abs( x( 2_ilp, 1_ilp ) ), abs( x(&
                                     2_ilp, 2_ilp ) ), vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_200
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       call stdlib_scopy( n-ki+1, work( ki+n ), 1_ilp, vl( ki, is ), 1_ilp )
                       call stdlib_scopy( n-ki+1, work( ki+n2 ), 1_ilp, vl( ki, is+1 ),1_ilp )
                       emax = zero
                       do k = ki, n
                          emax = max( emax, abs( vl( k, is ) )+abs( vl( k, is+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is+1 ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                          vl( k, is+1 ) = zero
                       end do
                    else
                       if( ki<n-1 ) then
                          call stdlib_sgemv( 'N', n, n-ki-1, one, vl( 1_ilp, ki+2 ),ldvl, work( ki+2+&
                                    n ), 1_ilp, work( ki+n ),vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_sgemv( 'N', n, n-ki-1, one, vl( 1_ilp, ki+2 ),ldvl, work( ki+2+&
                                    n2 ), 1_ilp,work( ki+1+n2 ), vl( 1_ilp, ki+1 ), 1_ilp )
                       else
                          call stdlib_sscal( n, work( ki+n ), vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_sscal( n, work( ki+1+n2 ), vl( 1_ilp, ki+1 ), 1_ilp )
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vl( k, ki ) )+abs( vl( k, ki+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki+1 ), 1_ilp )
                    end if
                 end if
                 is = is + 1_ilp
                 if( ip/=0_ilp )is = is + 1_ilp
                 250 continue
                 if( ip==-1_ilp )ip = 0_ilp
                 if( ip==1_ilp )ip = -1_ilp
              end do loop_260
           end if
           return
     end subroutine stdlib_strevc

     pure module subroutine stdlib_dtrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! DTREVC computes some or all of the right and/or left eigenvectors of
     !! a real upper quasi-triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a real general matrix:  A = Q*T*Q**T, as computed by DHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal blocks of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix.  If Q is the orthogonal factor that reduces a matrix
     !! A to Schur form T, then Q*X and Q*Y are the matrices of right and
     !! left eigenvectors of A.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, over, pair, rightv, somev
           integer(ilp) :: i, ierr, ii, ip, is, j, j1, j2, jnxt, k, ki, n2
           real(dp) :: beta, bignum, emax, ovfl, rec, remax, scale, smin, smlnum, ulp, unfl, &
                     vcrit, vmax, wi, wr, xnorm
           ! Intrinsic Functions 
           ! Local Arrays 
           real(dp) :: x(2_ilp,2_ilp)
           ! Executable Statements 
           ! decode and test the input parameters
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           allv = stdlib_lsame( howmny, 'A' )
           over = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else
              ! set m to the number of columns required to store the selected
              ! eigenvectors, standardize the array select if necessary, and
              ! test mm.
              if( somev ) then
                 m = 0_ilp
                 pair = .false.
                 do j = 1, n
                    if( pair ) then
                       pair = .false.
                       select( j ) = .false.
                    else
                       if( j<n ) then
                          if( t( j+1, j )==zero ) then
                             if( select( j ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( j ) .or. select( j+1 ) ) then
                                select( j ) = .true.
                                m = m + 2_ilp
                             end if
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( mm<m ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTREVC', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set the constants to control overflow.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_dlabad( unfl, ovfl )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = zero
              do i = 1, j - 1
                 work( j ) = work( j ) + abs( t( i, j ) )
              end do
           end do
           ! index ip is used to specify the real or complex eigenvalue:
             ! ip = 0, real eigenvalue,
                  ! 1, first of conjugate complex pair: (wr,wi)
                 ! -1, second of conjugate complex pair: (wr,wi)
           n2 = 2_ilp*n
           if( rightv ) then
              ! compute right eigenvectors.
              ip = 0_ilp
              is = m
              loop_140: do ki = n, 1, -1
                 if( ip==1 )go to 130
                 if( ki==1 )go to 40
                 if( t( ki, ki-1 )==zero )go to 40
                 ip = -1_ilp
                 40 continue
                 if( somev ) then
                    if( ip==0_ilp ) then
                       if( .not.select( ki ) )go to 130
                    else
                       if( .not.select( ki-1 ) )go to 130
                    end if
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki-1 ) ) )*sqrt( abs( t( ki-1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! real right eigenvector
                    work( ki+n ) = one
                    ! form right-hand side
                    do k = 1, ki - 1
                       work( k+n ) = -t( k, ki )
                    end do
                    ! solve the upper quasi-triangular system:
                       ! (t(1:ki-1,1:ki-1) - wr)*x = scale*work.
                    jnxt = ki - 1_ilp
                    loop_60: do j = ki - 1, 1, -1
                       if( j>jnxt )cycle loop_60
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1 = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_dlaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_daxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+n ), n, wr, zero, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x(1,1) and x(2,1) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                          work( j-1+n ) = x( 1_ilp, 1_ilp )
                          work( j+n ) = x( 2_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_daxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                       end if
                    end do loop_60
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       call stdlib_dcopy( ki, work( 1_ilp+n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       ii = stdlib_idamax( ki, vr( 1_ilp, is ), 1_ilp )
                       remax = one / abs( vr( ii, is ) )
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is ) = zero
                       end do
                    else
                       if( ki>1_ilp )call stdlib_dgemv( 'N', n, ki-1, one, vr, ldvr,work( 1_ilp+n ), 1_ilp, &
                                 work( ki+n ),vr( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_idamax( n, vr( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vr( ii, ki ) )
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    end if
                 else
                    ! complex right eigenvector.
                    ! initial solve
                      ! [ (t(ki-1,ki-1) t(ki-1,ki) ) - (wr + i* wi)]*x = 0.
                      ! [ (t(ki,ki-1)   t(ki,ki)   )               ]
                    if( abs( t( ki-1, ki ) )>=abs( t( ki, ki-1 ) ) ) then
                       work( ki-1+n ) = one
                       work( ki+n2 ) = wi / t( ki-1, ki )
                    else
                       work( ki-1+n ) = -wi / t( ki, ki-1 )
                       work( ki+n2 ) = one
                    end if
                    work( ki+n ) = zero
                    work( ki-1+n2 ) = zero
                    ! form right-hand side
                    do k = 1, ki - 2
                       work( k+n ) = -work( ki-1+n )*t( k, ki-1 )
                       work( k+n2 ) = -work( ki+n2 )*t( k, ki )
                    end do
                    ! solve upper quasi-triangular system:
                    ! (t(1:ki-2,1:ki-2) - (wr+i*wi))*x = scale*(work+i*work2)
                    jnxt = ki - 2_ilp
                    loop_90: do j = ki - 2, 1, -1
                       if( j>jnxt )cycle loop_90
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1 = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_dlaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr, wi,x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) and x(1,2) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                             call stdlib_dscal( ki, scale, work( 1_ilp+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_daxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-1, -x( 1_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_dlaln2( .false., 2_ilp, 2_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+n ), n, wr, wi, x, 2_ilp, scale,xnorm, ierr )
                          ! scale x to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                rec = one / xnorm
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp )*rec
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp )*rec
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp )*rec
                                x( 2_ilp, 2_ilp ) = x( 2_ilp, 2_ilp )*rec
                                scale = scale*rec
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( ki, scale, work( 1_ilp+n ), 1_ilp )
                             call stdlib_dscal( ki, scale, work( 1_ilp+n2 ), 1_ilp )
                          end if
                          work( j-1+n ) = x( 1_ilp, 1_ilp )
                          work( j+n ) = x( 2_ilp, 1_ilp )
                          work( j-1+n2 ) = x( 1_ilp, 2_ilp )
                          work( j+n2 ) = x( 2_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_daxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-2, -x( 1_ilp, 2_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                          call stdlib_daxpy( j-2, -x( 2_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+n2 ), 1_ilp )
                                    
                       end if
                    end do loop_90
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       call stdlib_dcopy( ki, work( 1_ilp+n ), 1_ilp, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_dcopy( ki, work( 1_ilp+n2 ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       emax = zero
                       do k = 1, ki
                          emax = max( emax, abs( vr( k, is-1 ) )+abs( vr( k, is ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is-1 ) = zero
                          vr( k, is ) = zero
                       end do
                    else
                       if( ki>2_ilp ) then
                          call stdlib_dgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp+n ), 1_ilp, work( ki-&
                                    1_ilp+n ),vr( 1_ilp, ki-1 ), 1_ilp )
                          call stdlib_dgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp+n2 ), 1_ilp, work( &
                                    ki+n2 ),vr( 1_ilp, ki ), 1_ilp )
                       else
                          call stdlib_dscal( n, work( ki-1+n ), vr( 1_ilp, ki-1 ), 1_ilp )
                          call stdlib_dscal( n, work( ki+n2 ), vr( 1_ilp, ki ), 1_ilp )
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vr( k, ki-1 ) )+abs( vr( k, ki ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki-1 ), 1_ilp )
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    end if
                 end if
                 is = is - 1_ilp
                 if( ip/=0_ilp )is = is - 1_ilp
                 130 continue
                 if( ip==1_ilp )ip = 0_ilp
                 if( ip==-1_ilp )ip = 1_ilp
              end do loop_140
           end if
           if( leftv ) then
              ! compute left eigenvectors.
              ip = 0_ilp
              is = 1_ilp
              loop_260: do ki = 1, n
                 if( ip==-1 )go to 250
                 if( ki==n )go to 150
                 if( t( ki+1, ki )==zero )go to 150
                 ip = 1_ilp
                 150 continue
                 if( somev ) then
                    if( .not.select( ki ) )go to 250
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki+1 ) ) )*sqrt( abs( t( ki+1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! real left eigenvector.
                    work( ki+n ) = one
                    ! form right-hand side
                    do k = ki + 1, n
                       work( k+n ) = -t( ki, k )
                    end do
                    ! solve the quasi-triangular system:
                       ! (t(ki+1:n,ki+1:n) - wr)**t*x = scale*work
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 1_ilp
                    loop_170: do j = ki + 1, n
                       if( j<jnxt )cycle loop_170
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_ddot( j-ki-1, t( ki+1, j ), 1_ilp,work( &
                                    ki+1+n ), 1_ilp )
                          ! solve (t(j,j)-wr)**t*x = work
                          call stdlib_dlaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                                    
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          vmax = max( abs( work( j+n ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_ddot( j-ki-1, t( ki+1, j ), 1_ilp,work( &
                                    ki+1+n ), 1_ilp )
                          work( j+1+n ) = work( j+1+n ) -stdlib_ddot( j-ki-1, t( ki+1, j+1 ), 1_ilp,&
                                    work( ki+1+n ), 1_ilp )
                          ! solve
                            ! [t(j,j)-wr   t(j,j+1)     ]**t * x = scale*( work1 )
                            ! [t(j+1,j)    t(j+1,j+1)-wr]                ( work2 )
                          call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                                    
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+1+n ) = x( 2_ilp, 1_ilp )
                          vmax = max( abs( work( j+n ) ),abs( work( j+1+n ) ), vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_170
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       call stdlib_dcopy( n-ki+1, work( ki+n ), 1_ilp, vl( ki, is ), 1_ilp )
                       ii = stdlib_idamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                       remax = one / abs( vl( ii, is ) )
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                       end do
                    else
                       if( ki<n )call stdlib_dgemv( 'N', n, n-ki, one, vl( 1_ilp, ki+1 ), ldvl,work( &
                                 ki+1+n ), 1_ilp, work( ki+n ),vl( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_idamax( n, vl( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vl( ii, ki ) )
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                    end if
                 else
                    ! complex left eigenvector.
                     ! initial solve:
                       ! ((t(ki,ki)    t(ki,ki+1) )**t - (wr - i* wi))*x = 0.
                       ! ((t(ki+1,ki) t(ki+1,ki+1))                )
                    if( abs( t( ki, ki+1 ) )>=abs( t( ki+1, ki ) ) ) then
                       work( ki+n ) = wi / t( ki, ki+1 )
                       work( ki+1+n2 ) = one
                    else
                       work( ki+n ) = one
                       work( ki+1+n2 ) = -wi / t( ki+1, ki )
                    end if
                    work( ki+1+n ) = zero
                    work( ki+n2 ) = zero
                    ! form right-hand side
                    do k = ki + 2, n
                       work( k+n ) = -work( ki+n )*t( ki, k )
                       work( k+n2 ) = -work( ki+1+n2 )*t( ki+1, k )
                    end do
                    ! solve complex quasi-triangular system:
                    ! ( t(ki+2,n:ki+2,n) - (wr-i*wi) )*x = work1+i*work2
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 2_ilp
                    loop_200: do j = ki + 2, n
                       if( j<jnxt )cycle loop_200
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when
                          ! forming the right-hand side elements.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             call stdlib_dscal( n-ki+1, rec, work( ki+n2 ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_ddot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n ), 1_ilp )
                          work( j+n2 ) = work( j+n2 ) -stdlib_ddot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n2 ), 1_ilp )
                          ! solve (t(j,j)-(wr-i*wi))*(x11+i*x12)= wk+i*wk2
                          call stdlib_dlaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                             call stdlib_dscal( n-ki+1, scale, work( ki+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          vmax = max( abs( work( j+n ) ),abs( work( j+n2 ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side elements.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+n ), 1_ilp )
                             call stdlib_dscal( n-ki+1, rec, work( ki+n2 ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+n ) = work( j+n ) -stdlib_ddot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n ), 1_ilp )
                          work( j+n2 ) = work( j+n2 ) -stdlib_ddot( j-ki-2, t( ki+2, j ), 1_ilp,work( &
                                    ki+2+n2 ), 1_ilp )
                          work( j+1+n ) = work( j+1+n ) -stdlib_ddot( j-ki-2, t( ki+2, j+1 ), 1_ilp,&
                                    work( ki+2+n ), 1_ilp )
                          work( j+1+n2 ) = work( j+1+n2 ) -stdlib_ddot( j-ki-2, t( ki+2, j+1 ), 1_ilp,&
                                    work( ki+2+n2 ), 1_ilp )
                          ! solve 2-by-2 complex linear equation
                            ! ([t(j,j)   t(j,j+1)  ]**t-(wr-i*wi)*i)*x = scale*b
                            ! ([t(j+1,j) t(j+1,j+1)]               )
                          call stdlib_dlaln2( .true., 2_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( n-ki+1, scale, work( ki+n ), 1_ilp )
                             call stdlib_dscal( n-ki+1, scale, work( ki+n2 ), 1_ilp )
                          end if
                          work( j+n ) = x( 1_ilp, 1_ilp )
                          work( j+n2 ) = x( 1_ilp, 2_ilp )
                          work( j+1+n ) = x( 2_ilp, 1_ilp )
                          work( j+1+n2 ) = x( 2_ilp, 2_ilp )
                          vmax = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 1_ilp, 2_ilp ) ),abs( x( 2_ilp, 1_ilp ) ), abs( x(&
                                     2_ilp, 2_ilp ) ), vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_200
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       call stdlib_dcopy( n-ki+1, work( ki+n ), 1_ilp, vl( ki, is ), 1_ilp )
                       call stdlib_dcopy( n-ki+1, work( ki+n2 ), 1_ilp, vl( ki, is+1 ),1_ilp )
                       emax = zero
                       do k = ki, n
                          emax = max( emax, abs( vl( k, is ) )+abs( vl( k, is+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is+1 ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                          vl( k, is+1 ) = zero
                       end do
                    else
                       if( ki<n-1 ) then
                          call stdlib_dgemv( 'N', n, n-ki-1, one, vl( 1_ilp, ki+2 ),ldvl, work( ki+2+&
                                    n ), 1_ilp, work( ki+n ),vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_dgemv( 'N', n, n-ki-1, one, vl( 1_ilp, ki+2 ),ldvl, work( ki+2+&
                                    n2 ), 1_ilp,work( ki+1+n2 ), vl( 1_ilp, ki+1 ), 1_ilp )
                       else
                          call stdlib_dscal( n, work( ki+n ), vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_dscal( n, work( ki+1+n2 ), vl( 1_ilp, ki+1 ), 1_ilp )
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vl( k, ki ) )+abs( vl( k, ki+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki+1 ), 1_ilp )
                    end if
                 end if
                 is = is + 1_ilp
                 if( ip/=0_ilp )is = is + 1_ilp
                 250 continue
                 if( ip==-1_ilp )ip = 0_ilp
                 if( ip==1_ilp )ip = -1_ilp
              end do loop_260
           end if
           return
     end subroutine stdlib_dtrevc


     pure module subroutine stdlib_ctrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! CTREVC computes some or all of the right and/or left eigenvectors of
     !! a complex upper triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a complex general matrix:  A = Q*T*Q**H, as computed by CHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix.  If Q is the unitary factor that reduces a matrix A to
     !! Schur form T, then Q*X and Q*Y are the matrices of right and left
     !! eigenvectors of A.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           complex(sp), parameter :: cmzero = (0.0e+0_sp,0.0e+0_sp)
           complex(sp), parameter :: cmone = (1.0e+0_sp,0.0e+0_sp)
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, over, rightv, somev
           integer(ilp) :: i, ii, is, j, k, ki
           real(sp) :: ovfl, remax, scale, smin, smlnum, ulp, unfl
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           allv = stdlib_lsame( howmny, 'A' )
           over = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           if( somev ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTREVC', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set the constants to control overflow.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_slabad( unfl, ovfl )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ! store the diagonal elements of t in working array work.
           do i = 1, n
              work( i+n ) = t( i, i )
           end do
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           rwork( 1_ilp ) = zero
           do j = 2, n
              rwork( j ) = stdlib_scasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( rightv ) then
              ! compute right eigenvectors.
              is = m
              loop_80: do ki = n, 1, -1
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_80
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 work( 1_ilp ) = cmone
                 ! form right-hand side.
                 do k = 1, ki - 1
                    work( k ) = -t( k, ki )
                 end do
                 ! solve the triangular system:
                    ! (t(1:ki-1,1:ki-1) - t(ki,ki))*x = scale*work.
                 do k = 1, ki - 1
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki>1_ilp ) then
                    call stdlib_clatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', 'Y',ki-1, t, ldt, &
                              work( 1_ilp ), scale, rwork,info )
                    work( ki ) = scale
                 end if
                 ! copy the vector x or q*x to vr and normalize.
                 if( .not.over ) then
                    call stdlib_ccopy( ki, work( 1_ilp ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                    ii = stdlib_icamax( ki, vr( 1_ilp, is ), 1_ilp )
                    remax = one / cabs1( vr( ii, is ) )
                    call stdlib_csscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                    do k = ki + 1, n
                       vr( k, is ) = cmzero
                    end do
                 else
                    if( ki>1_ilp )call stdlib_cgemv( 'N', n, ki-1, cmone, vr, ldvr, work( 1_ilp ),1_ilp, &
                              cmplx( scale,KIND=sp), vr( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_icamax( n, vr( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vr( ii, ki ) )
                    call stdlib_csscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                 end if
                 ! set back the original diagonal elements of t.
                 do k = 1, ki - 1
                    t( k, k ) = work( k+n )
                 end do
                 is = is - 1_ilp
              end do loop_80
           end if
           if( leftv ) then
              ! compute left eigenvectors.
              is = 1_ilp
              loop_130: do ki = 1, n
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_130
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 work( n ) = cmone
                 ! form right-hand side.
                 do k = ki + 1, n
                    work( k ) = -conjg( t( ki, k ) )
                 end do
                 ! solve the triangular system:
                    ! (t(ki+1:n,ki+1:n) - t(ki,ki))**h*x = scale*work.
                 do k = ki + 1, n
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki<n ) then
                    call stdlib_clatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT','Y', n-ki, t( &
                              ki+1, ki+1 ), ldt,work( ki+1 ), scale, rwork, info )
                    work( ki ) = scale
                 end if
                 ! copy the vector x or q*x to vl and normalize.
                 if( .not.over ) then
                    call stdlib_ccopy( n-ki+1, work( ki ), 1_ilp, vl( ki, is ), 1_ilp )
                    ii = stdlib_icamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                    remax = one / cabs1( vl( ii, is ) )
                    call stdlib_csscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                    do k = 1, ki - 1
                       vl( k, is ) = cmzero
                    end do
                 else
                    if( ki<n )call stdlib_cgemv( 'N', n, n-ki, cmone, vl( 1_ilp, ki+1 ), ldvl,work( &
                              ki+1 ), 1_ilp, cmplx( scale,KIND=sp),vl( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_icamax( n, vl( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vl( ii, ki ) )
                    call stdlib_csscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                 end if
                 ! set back the original diagonal elements of t.
                 do k = ki + 1, n
                    t( k, k ) = work( k+n )
                 end do
                 is = is + 1_ilp
              end do loop_130
           end if
           return
     end subroutine stdlib_ctrevc

     pure module subroutine stdlib_ztrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! ZTREVC computes some or all of the right and/or left eigenvectors of
     !! a complex upper triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a complex general matrix:  A = Q*T*Q**H, as computed by ZHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix.  If Q is the unitary factor that reduces a matrix A to
     !! Schur form T, then Q*X and Q*Y are the matrices of right and left
     !! eigenvectors of A.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           complex(dp), parameter :: cmzero = (0.0e+0_dp,0.0e+0_dp)
           complex(dp), parameter :: cmone = (1.0e+0_dp,0.0e+0_dp)
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, over, rightv, somev
           integer(ilp) :: i, ii, is, j, k, ki
           real(dp) :: ovfl, remax, scale, smin, smlnum, ulp, unfl
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           bothv = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv = stdlib_lsame( side, 'L' ) .or. bothv
           allv = stdlib_lsame( howmny, 'A' )
           over = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           if( somev ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTREVC', -info )
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! set the constants to control overflow.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_dlabad( unfl, ovfl )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ! store the diagonal elements of t in working array work.
           do i = 1, n
              work( i+n ) = t( i, i )
           end do
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           rwork( 1_ilp ) = zero
           do j = 2, n
              rwork( j ) = stdlib_dzasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( rightv ) then
              ! compute right eigenvectors.
              is = m
              loop_80: do ki = n, 1, -1
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_80
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 work( 1_ilp ) = cmone
                 ! form right-hand side.
                 do k = 1, ki - 1
                    work( k ) = -t( k, ki )
                 end do
                 ! solve the triangular system:
                    ! (t(1:ki-1,1:ki-1) - t(ki,ki))*x = scale*work.
                 do k = 1, ki - 1
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki>1_ilp ) then
                    call stdlib_zlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', 'Y',ki-1, t, ldt, &
                              work( 1_ilp ), scale, rwork,info )
                    work( ki ) = scale
                 end if
                 ! copy the vector x or q*x to vr and normalize.
                 if( .not.over ) then
                    call stdlib_zcopy( ki, work( 1_ilp ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                    ii = stdlib_izamax( ki, vr( 1_ilp, is ), 1_ilp )
                    remax = one / cabs1( vr( ii, is ) )
                    call stdlib_zdscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                    do k = ki + 1, n
                       vr( k, is ) = cmzero
                    end do
                 else
                    if( ki>1_ilp )call stdlib_zgemv( 'N', n, ki-1, cmone, vr, ldvr, work( 1_ilp ),1_ilp, &
                              cmplx( scale,KIND=dp), vr( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_izamax( n, vr( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vr( ii, ki ) )
                    call stdlib_zdscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                 end if
                 ! set back the original diagonal elements of t.
                 do k = 1, ki - 1
                    t( k, k ) = work( k+n )
                 end do
                 is = is - 1_ilp
              end do loop_80
           end if
           if( leftv ) then
              ! compute left eigenvectors.
              is = 1_ilp
              loop_130: do ki = 1, n
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_130
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 work( n ) = cmone
                 ! form right-hand side.
                 do k = ki + 1, n
                    work( k ) = -conjg( t( ki, k ) )
                 end do
                 ! solve the triangular system:
                    ! (t(ki+1:n,ki+1:n) - t(ki,ki))**h * x = scale*work.
                 do k = ki + 1, n
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki<n ) then
                    call stdlib_zlatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT','Y', n-ki, t( &
                              ki+1, ki+1 ), ldt,work( ki+1 ), scale, rwork, info )
                    work( ki ) = scale
                 end if
                 ! copy the vector x or q*x to vl and normalize.
                 if( .not.over ) then
                    call stdlib_zcopy( n-ki+1, work( ki ), 1_ilp, vl( ki, is ), 1_ilp )
                    ii = stdlib_izamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                    remax = one / cabs1( vl( ii, is ) )
                    call stdlib_zdscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                    do k = 1, ki - 1
                       vl( k, is ) = cmzero
                    end do
                 else
                    if( ki<n )call stdlib_zgemv( 'N', n, n-ki, cmone, vl( 1_ilp, ki+1 ), ldvl,work( &
                              ki+1 ), 1_ilp, cmplx( scale,KIND=dp),vl( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_izamax( n, vl( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vl( ii, ki ) )
                    call stdlib_zdscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                 end if
                 ! set back the original diagonal elements of t.
                 do k = ki + 1, n
                    t( k, k ) = work( k+n )
                 end do
                 is = is + 1_ilp
              end do loop_130
           end if
           return
     end subroutine stdlib_ztrevc




     pure module subroutine stdlib_strevc3( side, howmny, select, n, t, ldt, vl, ldvl,vr, ldvr, mm, m, &
     !! STREVC3 computes some or all of the right and/or left eigenvectors of
     !! a real upper quasi-triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a real general matrix:  A = Q*T*Q**T, as computed by SHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**T)*T = w*(y**T)
     !! where y**T denotes the transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal blocks of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix. If Q is the orthogonal factor that reduces a matrix
     !! A to Schur form T, then Q*X and Q*Y are the matrices of right and
     !! left eigenvectors of A.
     !! This uses a Level 3 BLAS version of the back transformation.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmin = 8_ilp
           integer(ilp), parameter :: nbmax = 128_ilp
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, lquery, over, pair, rightv, somev
           integer(ilp) :: i, ierr, ii, ip, is, j, j1, j2, jnxt, k, ki, iv, maxwrk, nb, &
                     ki2
           real(sp) :: beta, bignum, emax, ovfl, rec, remax, scale, smin, smlnum, ulp, unfl, &
                     vcrit, vmax, wi, wr, xnorm
           ! Intrinsic Functions 
           ! Local Arrays 
           real(sp) :: x(2_ilp,2_ilp)
           integer(ilp) :: iscomplex(nbmax)
           ! Executable Statements 
           ! decode and test the input parameters
           bothv  = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv  = stdlib_lsame( side, 'L' ) .or. bothv
           allv  = stdlib_lsame( howmny, 'A' )
           over  = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'STREVC', side // howmny, n, -1_ilp, -1_ilp, -1_ilp )
           maxwrk = n + 2_ilp*n*nb
           work(1_ilp) = maxwrk
           lquery = ( lwork==-1_ilp )
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n ) .and. .not.lquery ) then
              info = -14_ilp
           else
              ! set m to the number of columns required to store the selected
              ! eigenvectors, standardize the array select if necessary, and
              ! test mm.
              if( somev ) then
                 m = 0_ilp
                 pair = .false.
                 do j = 1, n
                    if( pair ) then
                       pair = .false.
                       select( j ) = .false.
                    else
                       if( j<n ) then
                          if( t( j+1, j )==zero ) then
                             if( select( j ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( j ) .or. select( j+1 ) ) then
                                select( j ) = .true.
                                m = m + 2_ilp
                             end if
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( mm<m ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STREVC3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! use blocked version of back-transformation if sufficient workspace.
           ! zero-out the workspace to avoid potential nan propagation.
           if( over .and. lwork >= n + 2_ilp*n*nbmin ) then
              nb = (lwork - n) / (2_ilp*n)
              nb = min( nb, nbmax )
              call stdlib_slaset( 'F', n, 1_ilp+2*nb, zero, zero, work, n )
           else
              nb = 1_ilp
           end if
           ! set the constants to control overflow.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_slabad( unfl, ovfl )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = zero
              do i = 1, j - 1
                 work( j ) = work( j ) + abs( t( i, j ) )
              end do
           end do
           ! index ip is used to specify the real or complex eigenvalue:
             ! ip = 0, real eigenvalue,
                  ! 1, first  of conjugate complex pair: (wr,wi)
                 ! -1, second of conjugate complex pair: (wr,wi)
             ! iscomplex array stores ip for each column in current block.
           if( rightv ) then
              ! ============================================================
              ! compute right eigenvectors.
              ! iv is index of column in current block.
              ! for complex right vector, uses iv-1 for real part and iv for complex part.
              ! non-blocked version always uses iv=2;
              ! blocked     version starts with iv=nb, goes down to 1 or 2.
              ! (note the "0-th" column is used for 1-norms computed above.)
              iv = 2_ilp
              if( nb>2_ilp ) then
                 iv = nb
              end if
              ip = 0_ilp
              is = m
              loop_140: do ki = n, 1, -1
                 if( ip==-1_ilp ) then
                    ! previous iteration (ki+1) was second of conjugate pair,
                    ! so this ki is first of conjugate pair; skip to end of loop
                    ip = 1_ilp
                    cycle loop_140
                 else if( ki==1_ilp ) then
                    ! last column, so this ki must be real eigenvalue
                    ip = 0_ilp
                 else if( t( ki, ki-1 )==zero ) then
                    ! zero on sub-diagonal, so this ki is real eigenvalue
                    ip = 0_ilp
                 else
                    ! non-zero on sub-diagonal, so this ki is second of conjugate pair
                    ip = -1_ilp
                 end if
                 if( somev ) then
                    if( ip==0_ilp ) then
                       if( .not.select( ki ) )cycle loop_140
                    else
                       if( .not.select( ki-1 ) )cycle loop_140
                    end if
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki-1 ) ) )*sqrt( abs( t( ki-1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! --------------------------------------------------------
                    ! real right eigenvector
                    work( ki + iv*n ) = one
                    ! form right-hand side.
                    do k = 1, ki - 1
                       work( k + iv*n ) = -t( k, ki )
                    end do
                    ! solve upper quasi-triangular system:
                    ! [ t(1:ki-1,1:ki-1) - wr ]*x = scale*work.
                    jnxt = ki - 1_ilp
                    loop_60: do j = ki - 1, 1, -1
                       if( j>jnxt )cycle loop_60
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1   = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_slaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( ki, scale, work( 1_ilp+iv*n ), 1_ilp )
                                    
                          work( j+iv*n ) = x( 1_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_saxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+iv*n ), n, wr, zero, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x(1,1) and x(2,1) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( ki, scale, work( 1_ilp+iv*n ), 1_ilp )
                                    
                          work( j-1+iv*n ) = x( 1_ilp, 1_ilp )
                          work( j  +iv*n ) = x( 2_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_saxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                       end if
                    end do loop_60
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vr and normalize.
                       call stdlib_scopy( ki, work( 1_ilp + iv*n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       ii = stdlib_isamax( ki, vr( 1_ilp, is ), 1_ilp )
                       remax = one / abs( vr( ii, is ) )
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki>1_ilp )call stdlib_sgemv( 'N', n, ki-1, one, vr, ldvr,work( 1_ilp + iv*n ), &
                                 1_ilp, work( ki + iv*n ),vr( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_isamax( n, vr( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vr( ii, ki ) )
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out below vector
                       do k = ki + 1, n
                          work( k + iv*n ) = zero
                       end do
                       iscomplex( iv ) = ip
                       ! back-transform and normalization is done below
                    end if
                 else
                    ! --------------------------------------------------------
                    ! complex right eigenvector.
                    ! initial solve
                    ! [ ( t(ki-1,ki-1) t(ki-1,ki) ) - (wr + i*wi) ]*x = 0.
                    ! [ ( t(ki,  ki-1) t(ki,  ki) )               ]
                    if( abs( t( ki-1, ki ) )>=abs( t( ki, ki-1 ) ) ) then
                       work( ki-1 + (iv-1)*n ) = one
                       work( ki   + (iv  )*n ) = wi / t( ki-1, ki )
                    else
                       work( ki-1 + (iv-1)*n ) = -wi / t( ki, ki-1 )
                       work( ki   + (iv  )*n ) = one
                    end if
                    work( ki   + (iv-1)*n ) = zero
                    work( ki-1 + (iv  )*n ) = zero
                    ! form right-hand side.
                    do k = 1, ki - 2
                       work( k+(iv-1)*n ) = -work( ki-1+(iv-1)*n )*t(k,ki-1)
                       work( k+(iv  )*n ) = -work( ki  +(iv  )*n )*t(k,ki  )
                    end do
                    ! solve upper quasi-triangular system:
                    ! [ t(1:ki-2,1:ki-2) - (wr+i*wi) ]*x = scale*(work+i*work2)
                    jnxt = ki - 2_ilp
                    loop_90: do j = ki - 2, 1, -1
                       if( j>jnxt )cycle loop_90
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1   = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_slaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+(iv-1)*n ), n,wr, wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) and x(1,2) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( ki, scale, work( 1_ilp+(iv-1)*n ), 1_ilp )
                             call stdlib_sscal( ki, scale, work( 1_ilp+(iv  )*n ), 1_ilp )
                          end if
                          work( j+(iv-1)*n ) = x( 1_ilp, 1_ilp )
                          work( j+(iv  )*n ) = x( 1_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_saxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv-1)*n ), 1_ilp )
                                    
                          call stdlib_saxpy( j-1, -x( 1_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv  )*n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_slaln2( .false., 2_ilp, 2_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+(iv-1)*n ), n, wr, wi, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                rec = one / xnorm
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp )*rec
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp )*rec
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp )*rec
                                x( 2_ilp, 2_ilp ) = x( 2_ilp, 2_ilp )*rec
                                scale = scale*rec
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( ki, scale, work( 1_ilp+(iv-1)*n ), 1_ilp )
                             call stdlib_sscal( ki, scale, work( 1_ilp+(iv  )*n ), 1_ilp )
                          end if
                          work( j-1+(iv-1)*n ) = x( 1_ilp, 1_ilp )
                          work( j  +(iv-1)*n ) = x( 2_ilp, 1_ilp )
                          work( j-1+(iv  )*n ) = x( 1_ilp, 2_ilp )
                          work( j  +(iv  )*n ) = x( 2_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_saxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+(iv-1)*n   ),&
                                     1_ilp )
                          call stdlib_saxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv-1)*n   ), &
                                    1_ilp )
                          call stdlib_saxpy( j-2, -x( 1_ilp, 2_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+(iv  )*n ), &
                                    1_ilp )
                          call stdlib_saxpy( j-2, -x( 2_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv  )*n ), 1_ilp )
                                    
                       end if
                    end do loop_90
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vr and normalize.
                       call stdlib_scopy( ki, work( 1_ilp+(iv-1)*n ), 1_ilp, vr(1_ilp,is-1), 1_ilp )
                       call stdlib_scopy( ki, work( 1_ilp+(iv  )*n ), 1_ilp, vr(1_ilp,is  ), 1_ilp )
                       emax = zero
                       do k = 1, ki
                          emax = max( emax, abs( vr( k, is-1 ) )+abs( vr( k, is   ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_sscal( ki, remax, vr( 1_ilp, is   ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is-1 ) = zero
                          vr( k, is   ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki>2_ilp ) then
                          call stdlib_sgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp    + (iv-1)*n ), &
                                    1_ilp,work( ki-1 + (iv-1)*n ), vr(1_ilp,ki-1), 1_ilp)
                          call stdlib_sgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp  + (iv)*n ), 1_ilp,&
                                    work( ki + (iv)*n ), vr( 1_ilp, ki ), 1_ilp )
                       else
                          call stdlib_sscal( n, work(ki-1+(iv-1)*n), vr(1_ilp,ki-1), 1_ilp)
                          call stdlib_sscal( n, work(ki  +(iv  )*n), vr(1_ilp,ki  ), 1_ilp)
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vr( k, ki-1 ) )+abs( vr( k, ki   ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki-1 ), 1_ilp )
                       call stdlib_sscal( n, remax, vr( 1_ilp, ki   ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out below vector
                       do k = ki + 1, n
                          work( k + (iv-1)*n ) = zero
                          work( k + (iv  )*n ) = zero
                       end do
                       iscomplex( iv-1 ) = -ip
                       iscomplex( iv   ) =  ip
                       iv = iv - 1_ilp
                       ! back-transform and normalization is done below
                    end if
                 end if
                 if( nb>1_ilp ) then
                    ! --------------------------------------------------------
                    ! blocked version of back-transform
                    ! for complex case, ki2 includes both vectors (ki-1 and ki)
                    if( ip==0_ilp ) then
                       ki2 = ki
                    else
                       ki2 = ki - 1_ilp
                    end if
                    ! columns iv:nb of work are valid vectors.
                    ! when the number of vectors stored reaches nb-1 or nb,
                    ! or if this was last vector, do the gemm
                    if( (iv<=2_ilp) .or. (ki2==1_ilp) ) then
                       call stdlib_sgemm( 'N', 'N', n, nb-iv+1, ki2+nb-iv, one,vr, ldvr,work( 1_ilp + &
                                 (iv)*n    ), n,zero,work( 1_ilp + (nb+iv)*n ), n )
                       ! normalize vectors
                       do k = iv, nb
                          if( iscomplex(k)==0_ilp ) then
                             ! real eigenvector
                             ii = stdlib_isamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                             remax = one / abs( work( ii + (nb+k)*n ) )
                          else if( iscomplex(k)==1_ilp ) then
                             ! first eigenvector of conjugate pair
                             emax = zero
                             do ii = 1, n
                                emax = max( emax,abs( work( ii + (nb+k  )*n ) )+abs( work( ii + (&
                                          nb+k+1)*n ) ) )
                             end do
                             remax = one / emax
                          ! else if iscomplex(k)==-1
                             ! second eigenvector of conjugate pair
                             ! reuse same remax as previous k
                          end if
                          call stdlib_sscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_slacpy( 'F', n, nb-iv+1,work( 1_ilp + (nb+iv)*n ), n,vr( 1_ilp, ki2 ), &
                                 ldvr )
                       iv = nb
                    else
                       iv = iv - 1_ilp
                    end if
                 end if ! blocked back-transform
                 is = is - 1_ilp
                 if( ip/=0_ilp )is = is - 1_ilp
              end do loop_140
           end if
           if( leftv ) then
              ! ============================================================
              ! compute left eigenvectors.
              ! iv is index of column in current block.
              ! for complex left vector, uses iv for real part and iv+1 for complex part.
              ! non-blocked version always uses iv=1;
              ! blocked     version starts with iv=1, goes up to nb-1 or nb.
              ! (note the "0-th" column is used for 1-norms computed above.)
              iv = 1_ilp
              ip = 0_ilp
              is = 1_ilp
              loop_260: do ki = 1, n
                 if( ip==1_ilp ) then
                    ! previous iteration (ki-1) was first of conjugate pair,
                    ! so this ki is second of conjugate pair; skip to end of loop
                    ip = -1_ilp
                    cycle loop_260
                 else if( ki==n ) then
                    ! last column, so this ki must be real eigenvalue
                    ip = 0_ilp
                 else if( t( ki+1, ki )==zero ) then
                    ! zero on sub-diagonal, so this ki is real eigenvalue
                    ip = 0_ilp
                 else
                    ! non-zero on sub-diagonal, so this ki is first of conjugate pair
                    ip = 1_ilp
                 end if
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_260
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki+1 ) ) )*sqrt( abs( t( ki+1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! --------------------------------------------------------
                    ! real left eigenvector
                    work( ki + iv*n ) = one
                    ! form right-hand side.
                    do k = ki + 1, n
                       work( k + iv*n ) = -t( ki, k )
                    end do
                    ! solve transposed quasi-triangular system:
                    ! [ t(ki+1:n,ki+1:n) - wr ]**t * x = scale*work
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 1_ilp
                    loop_170: do j = ki + 1, n
                       if( j<jnxt )cycle loop_170
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+iv*n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+iv*n ) = work( j+iv*n ) -stdlib_sdot( j-ki-1, t( ki+1, j ), 1_ilp,&
                                    work( ki+1+iv*n ), 1_ilp )
                          ! solve [ t(j,j) - wr ]**t * x = work
                          call stdlib_slaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( n-ki+1, scale, work( ki+iv*n ), 1_ilp )
                                    
                          work( j+iv*n ) = x( 1_ilp, 1_ilp )
                          vmax = max( abs( work( j+iv*n ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work( ki+iv*n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+iv*n ) = work( j+iv*n ) -stdlib_sdot( j-ki-1, t( ki+1, j ), 1_ilp,&
                                    work( ki+1+iv*n ), 1_ilp )
                          work( j+1+iv*n ) = work( j+1+iv*n ) -stdlib_sdot( j-ki-1, t( ki+1, j+1 )&
                                    , 1_ilp,work( ki+1+iv*n ), 1_ilp )
                          ! solve
                          ! [ t(j,j)-wr   t(j,j+1)      ]**t * x = scale*( work1 )
                          ! [ t(j+1,j)    t(j+1,j+1)-wr ]                ( work2 )
                          call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_sscal( n-ki+1, scale, work( ki+iv*n ), 1_ilp )
                                    
                          work( j  +iv*n ) = x( 1_ilp, 1_ilp )
                          work( j+1+iv*n ) = x( 2_ilp, 1_ilp )
                          vmax = max( abs( work( j  +iv*n ) ),abs( work( j+1+iv*n ) ), vmax )
                                    
                          vcrit = bignum / vmax
                       end if
                    end do loop_170
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vl and normalize.
                       call stdlib_scopy( n-ki+1, work( ki + iv*n ), 1_ilp,vl( ki, is ), 1_ilp )
                       ii = stdlib_isamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                       remax = one / abs( vl( ii, is ) )
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki<n )call stdlib_sgemv( 'N', n, n-ki, one,vl( 1_ilp, ki+1 ), ldvl,work( &
                                 ki+1 + iv*n ), 1_ilp,work( ki   + iv*n ), vl( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_isamax( n, vl( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vl( ii, ki ) )
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out above vector
                       ! could go from ki-nv+1 to ki-1
                       do k = 1, ki - 1
                          work( k + iv*n ) = zero
                       end do
                       iscomplex( iv ) = ip
                       ! back-transform and normalization is done below
                    end if
                 else
                    ! --------------------------------------------------------
                    ! complex left eigenvector.
                    ! initial solve:
                    ! [ ( t(ki,ki)    t(ki,ki+1)  )**t - (wr - i* wi) ]*x = 0.
                    ! [ ( t(ki+1,ki) t(ki+1,ki+1) )                   ]
                    if( abs( t( ki, ki+1 ) )>=abs( t( ki+1, ki ) ) ) then
                       work( ki   + (iv  )*n ) = wi / t( ki, ki+1 )
                       work( ki+1 + (iv+1)*n ) = one
                    else
                       work( ki   + (iv  )*n ) = one
                       work( ki+1 + (iv+1)*n ) = -wi / t( ki+1, ki )
                    end if
                    work( ki+1 + (iv  )*n ) = zero
                    work( ki   + (iv+1)*n ) = zero
                    ! form right-hand side.
                    do k = ki + 2, n
                       work( k+(iv  )*n ) = -work( ki  +(iv  )*n )*t(ki,  k)
                       work( k+(iv+1)*n ) = -work( ki+1+(iv+1)*n )*t(ki+1,k)
                    end do
                    ! solve transposed quasi-triangular system:
                    ! [ t(ki+2:n,ki+2:n)**t - (wr-i*wi) ]*x = work1+i*work2
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 2_ilp
                    loop_200: do j = ki + 2, n
                       if( j<jnxt )cycle loop_200
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when
                          ! forming the right-hand side elements.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work(ki+(iv  )*n), 1_ilp )
                             call stdlib_sscal( n-ki+1, rec, work(ki+(iv+1)*n), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+(iv  )*n ) = work( j+(iv)*n ) -stdlib_sdot( j-ki-2, t( ki+2, j )&
                                    , 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j+(iv+1)*n ) = work( j+(iv+1)*n ) -stdlib_sdot( j-ki-2, t( ki+2, &
                                    j ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          ! solve [ t(j,j)-(wr-i*wi) ]*(x11+i*x12)= wk+i*wk2
                          call stdlib_slaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( n-ki+1, scale, work(ki+(iv  )*n), 1_ilp)
                             call stdlib_sscal( n-ki+1, scale, work(ki+(iv+1)*n), 1_ilp)
                          end if
                          work( j+(iv  )*n ) = x( 1_ilp, 1_ilp )
                          work( j+(iv+1)*n ) = x( 1_ilp, 2_ilp )
                          vmax = max( abs( work( j+(iv  )*n ) ),abs( work( j+(iv+1)*n ) ), vmax )
                                    
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side elements.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_sscal( n-ki+1, rec, work(ki+(iv  )*n), 1_ilp )
                             call stdlib_sscal( n-ki+1, rec, work(ki+(iv+1)*n), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j  +(iv  )*n ) = work( j+(iv)*n ) -stdlib_sdot( j-ki-2, t( ki+2, &
                                    j ), 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j  +(iv+1)*n ) = work( j+(iv+1)*n ) -stdlib_sdot( j-ki-2, t( ki+2,&
                                     j ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          work( j+1+(iv  )*n ) = work( j+1+(iv)*n ) -stdlib_sdot( j-ki-2, t( ki+2,&
                                     j+1 ), 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j+1+(iv+1)*n ) = work( j+1+(iv+1)*n ) -stdlib_sdot( j-ki-2, t( ki+&
                                    2_ilp, j+1 ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          ! solve 2-by-2 complex linear equation
                          ! [ (t(j,j)   t(j,j+1)  )**t - (wr-i*wi)*i ]*x = scale*b
                          ! [ (t(j+1,j) t(j+1,j+1))                  ]
                          call stdlib_slaln2( .true., 2_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_sscal( n-ki+1, scale, work(ki+(iv  )*n), 1_ilp)
                             call stdlib_sscal( n-ki+1, scale, work(ki+(iv+1)*n), 1_ilp)
                          end if
                          work( j  +(iv  )*n ) = x( 1_ilp, 1_ilp )
                          work( j  +(iv+1)*n ) = x( 1_ilp, 2_ilp )
                          work( j+1+(iv  )*n ) = x( 2_ilp, 1_ilp )
                          work( j+1+(iv+1)*n ) = x( 2_ilp, 2_ilp )
                          vmax = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 1_ilp, 2_ilp ) ),abs( x( 2_ilp, 1_ilp ) ), abs( x(&
                                     2_ilp, 2_ilp ) ),vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_200
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vl and normalize.
                       call stdlib_scopy( n-ki+1, work( ki + (iv  )*n ), 1_ilp,vl( ki, is   ), 1_ilp )
                                 
                       call stdlib_scopy( n-ki+1, work( ki + (iv+1)*n ), 1_ilp,vl( ki, is+1 ), 1_ilp )
                                 
                       emax = zero
                       do k = ki, n
                          emax = max( emax, abs( vl( k, is   ) )+abs( vl( k, is+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is   ), 1_ilp )
                       call stdlib_sscal( n-ki+1, remax, vl( ki, is+1 ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is   ) = zero
                          vl( k, is+1 ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki<n-1 ) then
                          call stdlib_sgemv( 'N', n, n-ki-1, one,vl( 1_ilp, ki+2 ), ldvl,work( ki+2 + &
                                    (iv)*n ), 1_ilp,work( ki   + (iv)*n ),vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_sgemv( 'N', n, n-ki-1, one,vl( 1_ilp, ki+2 ), ldvl,work( ki+2 + &
                                    (iv+1)*n ), 1_ilp,work( ki+1 + (iv+1)*n ),vl( 1_ilp, ki+1 ), 1_ilp )
                       else
                          call stdlib_sscal( n, work(ki+  (iv  )*n), vl(1_ilp, ki  ), 1_ilp)
                          call stdlib_sscal( n, work(ki+1+(iv+1)*n), vl(1_ilp, ki+1), 1_ilp)
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vl( k, ki   ) )+abs( vl( k, ki+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki   ), 1_ilp )
                       call stdlib_sscal( n, remax, vl( 1_ilp, ki+1 ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out above vector
                       ! could go from ki-nv+1 to ki-1
                       do k = 1, ki - 1
                          work( k + (iv  )*n ) = zero
                          work( k + (iv+1)*n ) = zero
                       end do
                       iscomplex( iv   ) =  ip
                       iscomplex( iv+1 ) = -ip
                       iv = iv + 1_ilp
                       ! back-transform and normalization is done below
                    end if
                 end if
                 if( nb>1_ilp ) then
                    ! --------------------------------------------------------
                    ! blocked version of back-transform
                    ! for complex case, ki2 includes both vectors (ki and ki+1)
                    if( ip==0_ilp ) then
                       ki2 = ki
                    else
                       ki2 = ki + 1_ilp
                    end if
                    ! columns 1:iv of work are valid vectors.
                    ! when the number of vectors stored reaches nb-1 or nb,
                    ! or if this was last vector, do the gemm
                    if( (iv>=nb-1) .or. (ki2==n) ) then
                       call stdlib_sgemm( 'N', 'N', n, iv, n-ki2+iv, one,vl( 1_ilp, ki2-iv+1 ), ldvl,&
                                 work( ki2-iv+1 + (1_ilp)*n ), n,zero,work( 1_ilp + (nb+1)*n ), n )
                       ! normalize vectors
                       do k = 1, iv
                          if( iscomplex(k)==0_ilp) then
                             ! real eigenvector
                             ii = stdlib_isamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                             remax = one / abs( work( ii + (nb+k)*n ) )
                          else if( iscomplex(k)==1_ilp) then
                             ! first eigenvector of conjugate pair
                             emax = zero
                             do ii = 1, n
                                emax = max( emax,abs( work( ii + (nb+k  )*n ) )+abs( work( ii + (&
                                          nb+k+1)*n ) ) )
                             end do
                             remax = one / emax
                          ! else if iscomplex(k)==-1
                             ! second eigenvector of conjugate pair
                             ! reuse same remax as previous k
                          end if
                          call stdlib_sscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_slacpy( 'F', n, iv,work( 1_ilp + (nb+1)*n ), n,vl( 1_ilp, ki2-iv+1 ), &
                                 ldvl )
                       iv = 1_ilp
                    else
                       iv = iv + 1_ilp
                    end if
                 end if ! blocked back-transform
                 is = is + 1_ilp
                 if( ip/=0_ilp )is = is + 1_ilp
              end do loop_260
           end if
           return
     end subroutine stdlib_strevc3

     pure module subroutine stdlib_dtrevc3( side, howmny, select, n, t, ldt, vl, ldvl,vr, ldvr, mm, m, &
     !! DTREVC3 computes some or all of the right and/or left eigenvectors of
     !! a real upper quasi-triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a real general matrix:  A = Q*T*Q**T, as computed by DHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**T)*T = w*(y**T)
     !! where y**T denotes the transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal blocks of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix. If Q is the orthogonal factor that reduces a matrix
     !! A to Schur form T, then Q*X and Q*Y are the matrices of right and
     !! left eigenvectors of A.
     !! This uses a Level 3 BLAS version of the back transformation.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, mm, n
           ! Array Arguments 
           logical(lk), intent(inout) :: select(*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmin = 8_ilp
           integer(ilp), parameter :: nbmax = 128_ilp
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, lquery, over, pair, rightv, somev
           integer(ilp) :: i, ierr, ii, ip, is, j, j1, j2, jnxt, k, ki, iv, maxwrk, nb, &
                     ki2
           real(dp) :: beta, bignum, emax, ovfl, rec, remax, scale, smin, smlnum, ulp, unfl, &
                     vcrit, vmax, wi, wr, xnorm
           ! Intrinsic Functions 
           ! Local Arrays 
           real(dp) :: x(2_ilp,2_ilp)
           integer(ilp) :: iscomplex(nbmax)
           ! Executable Statements 
           ! decode and test the input parameters
           bothv  = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv  = stdlib_lsame( side, 'L' ) .or. bothv
           allv  = stdlib_lsame( howmny, 'A' )
           over  = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DTREVC', side // howmny, n, -1_ilp, -1_ilp, -1_ilp )
           maxwrk = n + 2_ilp*n*nb
           work(1_ilp) = maxwrk
           lquery = ( lwork==-1_ilp )
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n ) .and. .not.lquery ) then
              info = -14_ilp
           else
              ! set m to the number of columns required to store the selected
              ! eigenvectors, standardize the array select if necessary, and
              ! test mm.
              if( somev ) then
                 m = 0_ilp
                 pair = .false.
                 do j = 1, n
                    if( pair ) then
                       pair = .false.
                       select( j ) = .false.
                    else
                       if( j<n ) then
                          if( t( j+1, j )==zero ) then
                             if( select( j ) )m = m + 1_ilp
                          else
                             pair = .true.
                             if( select( j ) .or. select( j+1 ) ) then
                                select( j ) = .true.
                                m = m + 2_ilp
                             end if
                          end if
                       else
                          if( select( n ) )m = m + 1_ilp
                       end if
                    end if
                 end do
              else
                 m = n
              end if
              if( mm<m ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTREVC3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! use blocked version of back-transformation if sufficient workspace.
           ! zero-out the workspace to avoid potential nan propagation.
           if( over .and. lwork >= n + 2_ilp*n*nbmin ) then
              nb = (lwork - n) / (2_ilp*n)
              nb = min( nb, nbmax )
              call stdlib_dlaset( 'F', n, 1_ilp+2*nb, zero, zero, work, n )
           else
              nb = 1_ilp
           end if
           ! set the constants to control overflow.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_dlabad( unfl, ovfl )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           bignum = ( one-ulp ) / smlnum
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = zero
              do i = 1, j - 1
                 work( j ) = work( j ) + abs( t( i, j ) )
              end do
           end do
           ! index ip is used to specify the real or complex eigenvalue:
             ! ip = 0, real eigenvalue,
                  ! 1, first  of conjugate complex pair: (wr,wi)
                 ! -1, second of conjugate complex pair: (wr,wi)
             ! iscomplex array stores ip for each column in current block.
           if( rightv ) then
              ! ============================================================
              ! compute right eigenvectors.
              ! iv is index of column in current block.
              ! for complex right vector, uses iv-1 for real part and iv for complex part.
              ! non-blocked version always uses iv=2;
              ! blocked     version starts with iv=nb, goes down to 1 or 2.
              ! (note the "0-th" column is used for 1-norms computed above.)
              iv = 2_ilp
              if( nb>2_ilp ) then
                 iv = nb
              end if
              ip = 0_ilp
              is = m
              loop_140: do ki = n, 1, -1
                 if( ip==-1_ilp ) then
                    ! previous iteration (ki+1) was second of conjugate pair,
                    ! so this ki is first of conjugate pair; skip to end of loop
                    ip = 1_ilp
                    cycle loop_140
                 else if( ki==1_ilp ) then
                    ! last column, so this ki must be real eigenvalue
                    ip = 0_ilp
                 else if( t( ki, ki-1 )==zero ) then
                    ! zero on sub-diagonal, so this ki is real eigenvalue
                    ip = 0_ilp
                 else
                    ! non-zero on sub-diagonal, so this ki is second of conjugate pair
                    ip = -1_ilp
                 end if
                 if( somev ) then
                    if( ip==0_ilp ) then
                       if( .not.select( ki ) )cycle loop_140
                    else
                       if( .not.select( ki-1 ) )cycle loop_140
                    end if
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki-1 ) ) )*sqrt( abs( t( ki-1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! --------------------------------------------------------
                    ! real right eigenvector
                    work( ki + iv*n ) = one
                    ! form right-hand side.
                    do k = 1, ki - 1
                       work( k + iv*n ) = -t( k, ki )
                    end do
                    ! solve upper quasi-triangular system:
                    ! [ t(1:ki-1,1:ki-1) - wr ]*x = scale*work.
                    jnxt = ki - 1_ilp
                    loop_60: do j = ki - 1, 1, -1
                       if( j>jnxt )cycle loop_60
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1   = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_dlaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( ki, scale, work( 1_ilp+iv*n ), 1_ilp )
                                    
                          work( j+iv*n ) = x( 1_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_daxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+iv*n ), n, wr, zero, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x(1,1) and x(2,1) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( ki, scale, work( 1_ilp+iv*n ), 1_ilp )
                                    
                          work( j-1+iv*n ) = x( 1_ilp, 1_ilp )
                          work( j  +iv*n ) = x( 2_ilp, 1_ilp )
                          ! update right-hand side
                          call stdlib_daxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+iv*n ), 1_ilp )
                                    
                       end if
                    end do loop_60
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vr and normalize.
                       call stdlib_dcopy( ki, work( 1_ilp + iv*n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                       ii = stdlib_idamax( ki, vr( 1_ilp, is ), 1_ilp )
                       remax = one / abs( vr( ii, is ) )
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki>1_ilp )call stdlib_dgemv( 'N', n, ki-1, one, vr, ldvr,work( 1_ilp + iv*n ), &
                                 1_ilp, work( ki + iv*n ),vr( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_idamax( n, vr( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vr( ii, ki ) )
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out below vector
                       do k = ki + 1, n
                          work( k + iv*n ) = zero
                       end do
                       iscomplex( iv ) = ip
                       ! back-transform and normalization is done below
                    end if
                 else
                    ! --------------------------------------------------------
                    ! complex right eigenvector.
                    ! initial solve
                    ! [ ( t(ki-1,ki-1) t(ki-1,ki) ) - (wr + i*wi) ]*x = 0.
                    ! [ ( t(ki,  ki-1) t(ki,  ki) )               ]
                    if( abs( t( ki-1, ki ) )>=abs( t( ki, ki-1 ) ) ) then
                       work( ki-1 + (iv-1)*n ) = one
                       work( ki   + (iv  )*n ) = wi / t( ki-1, ki )
                    else
                       work( ki-1 + (iv-1)*n ) = -wi / t( ki, ki-1 )
                       work( ki   + (iv  )*n ) = one
                    end if
                    work( ki   + (iv-1)*n ) = zero
                    work( ki-1 + (iv  )*n ) = zero
                    ! form right-hand side.
                    do k = 1, ki - 2
                       work( k+(iv-1)*n ) = -work( ki-1+(iv-1)*n )*t(k,ki-1)
                       work( k+(iv  )*n ) = -work( ki  +(iv  )*n )*t(k,ki  )
                    end do
                    ! solve upper quasi-triangular system:
                    ! [ t(1:ki-2,1:ki-2) - (wr+i*wi) ]*x = scale*(work+i*work2)
                    jnxt = ki - 2_ilp
                    loop_90: do j = ki - 2, 1, -1
                       if( j>jnxt )cycle loop_90
                       j1 = j
                       j2 = j
                       jnxt = j - 1_ilp
                       if( j>1_ilp ) then
                          if( t( j, j-1 )/=zero ) then
                             j1   = j - 1_ilp
                             jnxt = j - 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          call stdlib_dlaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+(iv-1)*n ), n,wr, wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale x(1,1) and x(1,2) to avoid overflow when
                          ! updating the right-hand side.
                          if( xnorm>one ) then
                             if( work( j )>bignum / xnorm ) then
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp ) / xnorm
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp ) / xnorm
                                scale = scale / xnorm
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( ki, scale, work( 1_ilp+(iv-1)*n ), 1_ilp )
                             call stdlib_dscal( ki, scale, work( 1_ilp+(iv  )*n ), 1_ilp )
                          end if
                          work( j+(iv-1)*n ) = x( 1_ilp, 1_ilp )
                          work( j+(iv  )*n ) = x( 1_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_daxpy( j-1, -x( 1_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv-1)*n ), 1_ilp )
                                    
                          call stdlib_daxpy( j-1, -x( 1_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv  )*n ), 1_ilp )
                                    
                       else
                          ! 2-by-2 diagonal block
                          call stdlib_dlaln2( .false., 2_ilp, 2_ilp, smin, one,t( j-1, j-1 ), ldt, one, &
                                    one,work( j-1+(iv-1)*n ), n, wr, wi, x, 2_ilp,scale, xnorm, ierr )
                          ! scale x to avoid overflow when updating
                          ! the right-hand side.
                          if( xnorm>one ) then
                             beta = max( work( j-1 ), work( j ) )
                             if( beta>bignum / xnorm ) then
                                rec = one / xnorm
                                x( 1_ilp, 1_ilp ) = x( 1_ilp, 1_ilp )*rec
                                x( 1_ilp, 2_ilp ) = x( 1_ilp, 2_ilp )*rec
                                x( 2_ilp, 1_ilp ) = x( 2_ilp, 1_ilp )*rec
                                x( 2_ilp, 2_ilp ) = x( 2_ilp, 2_ilp )*rec
                                scale = scale*rec
                             end if
                          end if
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( ki, scale, work( 1_ilp+(iv-1)*n ), 1_ilp )
                             call stdlib_dscal( ki, scale, work( 1_ilp+(iv  )*n ), 1_ilp )
                          end if
                          work( j-1+(iv-1)*n ) = x( 1_ilp, 1_ilp )
                          work( j  +(iv-1)*n ) = x( 2_ilp, 1_ilp )
                          work( j-1+(iv  )*n ) = x( 1_ilp, 2_ilp )
                          work( j  +(iv  )*n ) = x( 2_ilp, 2_ilp )
                          ! update the right-hand side
                          call stdlib_daxpy( j-2, -x( 1_ilp, 1_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+(iv-1)*n   ),&
                                     1_ilp )
                          call stdlib_daxpy( j-2, -x( 2_ilp, 1_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv-1)*n   ), &
                                    1_ilp )
                          call stdlib_daxpy( j-2, -x( 1_ilp, 2_ilp ), t( 1_ilp, j-1 ), 1_ilp,work( 1_ilp+(iv  )*n ), &
                                    1_ilp )
                          call stdlib_daxpy( j-2, -x( 2_ilp, 2_ilp ), t( 1_ilp, j ), 1_ilp,work( 1_ilp+(iv  )*n ), 1_ilp )
                                    
                       end if
                    end do loop_90
                    ! copy the vector x or q*x to vr and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vr and normalize.
                       call stdlib_dcopy( ki, work( 1_ilp+(iv-1)*n ), 1_ilp, vr(1_ilp,is-1), 1_ilp )
                       call stdlib_dcopy( ki, work( 1_ilp+(iv  )*n ), 1_ilp, vr(1_ilp,is  ), 1_ilp )
                       emax = zero
                       do k = 1, ki
                          emax = max( emax, abs( vr( k, is-1 ) )+abs( vr( k, is   ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is-1 ), 1_ilp )
                       call stdlib_dscal( ki, remax, vr( 1_ilp, is   ), 1_ilp )
                       do k = ki + 1, n
                          vr( k, is-1 ) = zero
                          vr( k, is   ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki>2_ilp ) then
                          call stdlib_dgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp    + (iv-1)*n ), &
                                    1_ilp,work( ki-1 + (iv-1)*n ), vr(1_ilp,ki-1), 1_ilp)
                          call stdlib_dgemv( 'N', n, ki-2, one, vr, ldvr,work( 1_ilp  + (iv)*n ), 1_ilp,&
                                    work( ki + (iv)*n ), vr( 1_ilp, ki ), 1_ilp )
                       else
                          call stdlib_dscal( n, work(ki-1+(iv-1)*n), vr(1_ilp,ki-1), 1_ilp)
                          call stdlib_dscal( n, work(ki  +(iv  )*n), vr(1_ilp,ki  ), 1_ilp)
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vr( k, ki-1 ) )+abs( vr( k, ki   ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki-1 ), 1_ilp )
                       call stdlib_dscal( n, remax, vr( 1_ilp, ki   ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out below vector
                       do k = ki + 1, n
                          work( k + (iv-1)*n ) = zero
                          work( k + (iv  )*n ) = zero
                       end do
                       iscomplex( iv-1 ) = -ip
                       iscomplex( iv   ) =  ip
                       iv = iv - 1_ilp
                       ! back-transform and normalization is done below
                    end if
                 end if
                 if( nb>1_ilp ) then
                    ! --------------------------------------------------------
                    ! blocked version of back-transform
                    ! for complex case, ki2 includes both vectors (ki-1 and ki)
                    if( ip==0_ilp ) then
                       ki2 = ki
                    else
                       ki2 = ki - 1_ilp
                    end if
                    ! columns iv:nb of work are valid vectors.
                    ! when the number of vectors stored reaches nb-1 or nb,
                    ! or if this was last vector, do the gemm
                    if( (iv<=2_ilp) .or. (ki2==1_ilp) ) then
                       call stdlib_dgemm( 'N', 'N', n, nb-iv+1, ki2+nb-iv, one,vr, ldvr,work( 1_ilp + &
                                 (iv)*n    ), n,zero,work( 1_ilp + (nb+iv)*n ), n )
                       ! normalize vectors
                       do k = iv, nb
                          if( iscomplex(k)==0_ilp ) then
                             ! real eigenvector
                             ii = stdlib_idamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                             remax = one / abs( work( ii + (nb+k)*n ) )
                          else if( iscomplex(k)==1_ilp ) then
                             ! first eigenvector of conjugate pair
                             emax = zero
                             do ii = 1, n
                                emax = max( emax,abs( work( ii + (nb+k  )*n ) )+abs( work( ii + (&
                                          nb+k+1)*n ) ) )
                             end do
                             remax = one / emax
                          ! else if iscomplex(k)==-1
                             ! second eigenvector of conjugate pair
                             ! reuse same remax as previous k
                          end if
                          call stdlib_dscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_dlacpy( 'F', n, nb-iv+1,work( 1_ilp + (nb+iv)*n ), n,vr( 1_ilp, ki2 ), &
                                 ldvr )
                       iv = nb
                    else
                       iv = iv - 1_ilp
                    end if
                 end if ! blocked back-transform
                 is = is - 1_ilp
                 if( ip/=0_ilp )is = is - 1_ilp
              end do loop_140
           end if
           if( leftv ) then
              ! ============================================================
              ! compute left eigenvectors.
              ! iv is index of column in current block.
              ! for complex left vector, uses iv for real part and iv+1 for complex part.
              ! non-blocked version always uses iv=1;
              ! blocked     version starts with iv=1, goes up to nb-1 or nb.
              ! (note the "0-th" column is used for 1-norms computed above.)
              iv = 1_ilp
              ip = 0_ilp
              is = 1_ilp
              loop_260: do ki = 1, n
                 if( ip==1_ilp ) then
                    ! previous iteration (ki-1) was first of conjugate pair,
                    ! so this ki is second of conjugate pair; skip to end of loop
                    ip = -1_ilp
                    cycle loop_260
                 else if( ki==n ) then
                    ! last column, so this ki must be real eigenvalue
                    ip = 0_ilp
                 else if( t( ki+1, ki )==zero ) then
                    ! zero on sub-diagonal, so this ki is real eigenvalue
                    ip = 0_ilp
                 else
                    ! non-zero on sub-diagonal, so this ki is first of conjugate pair
                    ip = 1_ilp
                 end if
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_260
                 end if
                 ! compute the ki-th eigenvalue (wr,wi).
                 wr = t( ki, ki )
                 wi = zero
                 if( ip/=0_ilp )wi = sqrt( abs( t( ki, ki+1 ) ) )*sqrt( abs( t( ki+1, ki ) ) )
                 smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
                 if( ip==0_ilp ) then
                    ! --------------------------------------------------------
                    ! real left eigenvector
                    work( ki + iv*n ) = one
                    ! form right-hand side.
                    do k = ki + 1, n
                       work( k + iv*n ) = -t( ki, k )
                    end do
                    ! solve transposed quasi-triangular system:
                    ! [ t(ki+1:n,ki+1:n) - wr ]**t * x = scale*work
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 1_ilp
                    loop_170: do j = ki + 1, n
                       if( j<jnxt )cycle loop_170
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+iv*n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+iv*n ) = work( j+iv*n ) -stdlib_ddot( j-ki-1, t( ki+1, j ), 1_ilp,&
                                    work( ki+1+iv*n ), 1_ilp )
                          ! solve [ t(j,j) - wr ]**t * x = work
                          call stdlib_dlaln2( .false., 1_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( n-ki+1, scale, work( ki+iv*n ), 1_ilp )
                                    
                          work( j+iv*n ) = x( 1_ilp, 1_ilp )
                          vmax = max( abs( work( j+iv*n ) ), vmax )
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work( ki+iv*n ), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+iv*n ) = work( j+iv*n ) -stdlib_ddot( j-ki-1, t( ki+1, j ), 1_ilp,&
                                    work( ki+1+iv*n ), 1_ilp )
                          work( j+1+iv*n ) = work( j+1+iv*n ) -stdlib_ddot( j-ki-1, t( ki+1, j+1 )&
                                    , 1_ilp,work( ki+1+iv*n ), 1_ilp )
                          ! solve
                          ! [ t(j,j)-wr   t(j,j+1)      ]**t * x = scale*( work1 )
                          ! [ t(j+1,j)    t(j+1,j+1)-wr ]                ( work2 )
                          call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,zero, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one )call stdlib_dscal( n-ki+1, scale, work( ki+iv*n ), 1_ilp )
                                    
                          work( j  +iv*n ) = x( 1_ilp, 1_ilp )
                          work( j+1+iv*n ) = x( 2_ilp, 1_ilp )
                          vmax = max( abs( work( j  +iv*n ) ),abs( work( j+1+iv*n ) ), vmax )
                                    
                          vcrit = bignum / vmax
                       end if
                    end do loop_170
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vl and normalize.
                       call stdlib_dcopy( n-ki+1, work( ki + iv*n ), 1_ilp,vl( ki, is ), 1_ilp )
                       ii = stdlib_idamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                       remax = one / abs( vl( ii, is ) )
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki<n )call stdlib_dgemv( 'N', n, n-ki, one,vl( 1_ilp, ki+1 ), ldvl,work( &
                                 ki+1 + iv*n ), 1_ilp,work( ki   + iv*n ), vl( 1_ilp, ki ), 1_ilp )
                       ii = stdlib_idamax( n, vl( 1_ilp, ki ), 1_ilp )
                       remax = one / abs( vl( ii, ki ) )
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out above vector
                       ! could go from ki-nv+1 to ki-1
                       do k = 1, ki - 1
                          work( k + iv*n ) = zero
                       end do
                       iscomplex( iv ) = ip
                       ! back-transform and normalization is done below
                    end if
                 else
                    ! --------------------------------------------------------
                    ! complex left eigenvector.
                    ! initial solve:
                    ! [ ( t(ki,ki)    t(ki,ki+1)  )**t - (wr - i* wi) ]*x = 0.
                    ! [ ( t(ki+1,ki) t(ki+1,ki+1) )                   ]
                    if( abs( t( ki, ki+1 ) )>=abs( t( ki+1, ki ) ) ) then
                       work( ki   + (iv  )*n ) = wi / t( ki, ki+1 )
                       work( ki+1 + (iv+1)*n ) = one
                    else
                       work( ki   + (iv  )*n ) = one
                       work( ki+1 + (iv+1)*n ) = -wi / t( ki+1, ki )
                    end if
                    work( ki+1 + (iv  )*n ) = zero
                    work( ki   + (iv+1)*n ) = zero
                    ! form right-hand side.
                    do k = ki + 2, n
                       work( k+(iv  )*n ) = -work( ki  +(iv  )*n )*t(ki,  k)
                       work( k+(iv+1)*n ) = -work( ki+1+(iv+1)*n )*t(ki+1,k)
                    end do
                    ! solve transposed quasi-triangular system:
                    ! [ t(ki+2:n,ki+2:n)**t - (wr-i*wi) ]*x = work1+i*work2
                    vmax = one
                    vcrit = bignum
                    jnxt = ki + 2_ilp
                    loop_200: do j = ki + 2, n
                       if( j<jnxt )cycle loop_200
                       j1 = j
                       j2 = j
                       jnxt = j + 1_ilp
                       if( j<n ) then
                          if( t( j+1, j )/=zero ) then
                             j2 = j + 1_ilp
                             jnxt = j + 2_ilp
                          end if
                       end if
                       if( j1==j2 ) then
                          ! 1-by-1 diagonal block
                          ! scale if necessary to avoid overflow when
                          ! forming the right-hand side elements.
                          if( work( j )>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work(ki+(iv  )*n), 1_ilp )
                             call stdlib_dscal( n-ki+1, rec, work(ki+(iv+1)*n), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j+(iv  )*n ) = work( j+(iv)*n ) -stdlib_ddot( j-ki-2, t( ki+2, j )&
                                    , 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j+(iv+1)*n ) = work( j+(iv+1)*n ) -stdlib_ddot( j-ki-2, t( ki+2, &
                                    j ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          ! solve [ t(j,j)-(wr-i*wi) ]*(x11+i*x12)= wk+i*wk2
                          call stdlib_dlaln2( .false., 1_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( n-ki+1, scale, work(ki+(iv  )*n), 1_ilp)
                             call stdlib_dscal( n-ki+1, scale, work(ki+(iv+1)*n), 1_ilp)
                          end if
                          work( j+(iv  )*n ) = x( 1_ilp, 1_ilp )
                          work( j+(iv+1)*n ) = x( 1_ilp, 2_ilp )
                          vmax = max( abs( work( j+(iv  )*n ) ),abs( work( j+(iv+1)*n ) ), vmax )
                                    
                          vcrit = bignum / vmax
                       else
                          ! 2-by-2 diagonal block
                          ! scale if necessary to avoid overflow when forming
                          ! the right-hand side elements.
                          beta = max( work( j ), work( j+1 ) )
                          if( beta>vcrit ) then
                             rec = one / vmax
                             call stdlib_dscal( n-ki+1, rec, work(ki+(iv  )*n), 1_ilp )
                             call stdlib_dscal( n-ki+1, rec, work(ki+(iv+1)*n), 1_ilp )
                             vmax = one
                             vcrit = bignum
                          end if
                          work( j  +(iv  )*n ) = work( j+(iv)*n ) -stdlib_ddot( j-ki-2, t( ki+2, &
                                    j ), 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j  +(iv+1)*n ) = work( j+(iv+1)*n ) -stdlib_ddot( j-ki-2, t( ki+2,&
                                     j ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          work( j+1+(iv  )*n ) = work( j+1+(iv)*n ) -stdlib_ddot( j-ki-2, t( ki+2,&
                                     j+1 ), 1_ilp,work( ki+2+(iv)*n ), 1_ilp )
                          work( j+1+(iv+1)*n ) = work( j+1+(iv+1)*n ) -stdlib_ddot( j-ki-2, t( ki+&
                                    2_ilp, j+1 ), 1_ilp,work( ki+2+(iv+1)*n ), 1_ilp )
                          ! solve 2-by-2 complex linear equation
                          ! [ (t(j,j)   t(j,j+1)  )**t - (wr-i*wi)*i ]*x = scale*b
                          ! [ (t(j+1,j) t(j+1,j+1))                  ]
                          call stdlib_dlaln2( .true., 2_ilp, 2_ilp, smin, one, t( j, j ),ldt, one, one, &
                                    work( j+iv*n ), n, wr,-wi, x, 2_ilp, scale, xnorm, ierr )
                          ! scale if necessary
                          if( scale/=one ) then
                             call stdlib_dscal( n-ki+1, scale, work(ki+(iv  )*n), 1_ilp)
                             call stdlib_dscal( n-ki+1, scale, work(ki+(iv+1)*n), 1_ilp)
                          end if
                          work( j  +(iv  )*n ) = x( 1_ilp, 1_ilp )
                          work( j  +(iv+1)*n ) = x( 1_ilp, 2_ilp )
                          work( j+1+(iv  )*n ) = x( 2_ilp, 1_ilp )
                          work( j+1+(iv+1)*n ) = x( 2_ilp, 2_ilp )
                          vmax = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 1_ilp, 2_ilp ) ),abs( x( 2_ilp, 1_ilp ) ), abs( x(&
                                     2_ilp, 2_ilp ) ),vmax )
                          vcrit = bignum / vmax
                       end if
                    end do loop_200
                    ! copy the vector x or q*x to vl and normalize.
                    if( .not.over ) then
                       ! ------------------------------
                       ! no back-transform: copy x to vl and normalize.
                       call stdlib_dcopy( n-ki+1, work( ki + (iv  )*n ), 1_ilp,vl( ki, is   ), 1_ilp )
                                 
                       call stdlib_dcopy( n-ki+1, work( ki + (iv+1)*n ), 1_ilp,vl( ki, is+1 ), 1_ilp )
                                 
                       emax = zero
                       do k = ki, n
                          emax = max( emax, abs( vl( k, is   ) )+abs( vl( k, is+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is   ), 1_ilp )
                       call stdlib_dscal( n-ki+1, remax, vl( ki, is+1 ), 1_ilp )
                       do k = 1, ki - 1
                          vl( k, is   ) = zero
                          vl( k, is+1 ) = zero
                       end do
                    else if( nb==1_ilp ) then
                       ! ------------------------------
                       ! version 1: back-transform each vector with gemv, q*x.
                       if( ki<n-1 ) then
                          call stdlib_dgemv( 'N', n, n-ki-1, one,vl( 1_ilp, ki+2 ), ldvl,work( ki+2 + &
                                    (iv)*n ), 1_ilp,work( ki   + (iv)*n ),vl( 1_ilp, ki ), 1_ilp )
                          call stdlib_dgemv( 'N', n, n-ki-1, one,vl( 1_ilp, ki+2 ), ldvl,work( ki+2 + &
                                    (iv+1)*n ), 1_ilp,work( ki+1 + (iv+1)*n ),vl( 1_ilp, ki+1 ), 1_ilp )
                       else
                          call stdlib_dscal( n, work(ki+  (iv  )*n), vl(1_ilp, ki  ), 1_ilp)
                          call stdlib_dscal( n, work(ki+1+(iv+1)*n), vl(1_ilp, ki+1), 1_ilp)
                       end if
                       emax = zero
                       do k = 1, n
                          emax = max( emax, abs( vl( k, ki   ) )+abs( vl( k, ki+1 ) ) )
                       end do
                       remax = one / emax
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki   ), 1_ilp )
                       call stdlib_dscal( n, remax, vl( 1_ilp, ki+1 ), 1_ilp )
                    else
                       ! ------------------------------
                       ! version 2: back-transform block of vectors with gemm
                       ! zero out above vector
                       ! could go from ki-nv+1 to ki-1
                       do k = 1, ki - 1
                          work( k + (iv  )*n ) = zero
                          work( k + (iv+1)*n ) = zero
                       end do
                       iscomplex( iv   ) =  ip
                       iscomplex( iv+1 ) = -ip
                       iv = iv + 1_ilp
                       ! back-transform and normalization is done below
                    end if
                 end if
                 if( nb>1_ilp ) then
                    ! --------------------------------------------------------
                    ! blocked version of back-transform
                    ! for complex case, ki2 includes both vectors (ki and ki+1)
                    if( ip==0_ilp ) then
                       ki2 = ki
                    else
                       ki2 = ki + 1_ilp
                    end if
                    ! columns 1:iv of work are valid vectors.
                    ! when the number of vectors stored reaches nb-1 or nb,
                    ! or if this was last vector, do the gemm
                    if( (iv>=nb-1) .or. (ki2==n) ) then
                       call stdlib_dgemm( 'N', 'N', n, iv, n-ki2+iv, one,vl( 1_ilp, ki2-iv+1 ), ldvl,&
                                 work( ki2-iv+1 + (1_ilp)*n ), n,zero,work( 1_ilp + (nb+1)*n ), n )
                       ! normalize vectors
                       do k = 1, iv
                          if( iscomplex(k)==0_ilp) then
                             ! real eigenvector
                             ii = stdlib_idamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                             remax = one / abs( work( ii + (nb+k)*n ) )
                          else if( iscomplex(k)==1_ilp) then
                             ! first eigenvector of conjugate pair
                             emax = zero
                             do ii = 1, n
                                emax = max( emax,abs( work( ii + (nb+k  )*n ) )+abs( work( ii + (&
                                          nb+k+1)*n ) ) )
                             end do
                             remax = one / emax
                          ! else if iscomplex(k)==-1
                             ! second eigenvector of conjugate pair
                             ! reuse same remax as previous k
                          end if
                          call stdlib_dscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_dlacpy( 'F', n, iv,work( 1_ilp + (nb+1)*n ), n,vl( 1_ilp, ki2-iv+1 ), &
                                 ldvl )
                       iv = 1_ilp
                    else
                       iv = iv + 1_ilp
                    end if
                 end if ! blocked back-transform
                 is = is + 1_ilp
                 if( ip/=0_ilp )is = is + 1_ilp
              end do loop_260
           end if
           return
     end subroutine stdlib_dtrevc3


     pure module subroutine stdlib_ctrevc3( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! CTREVC3 computes some or all of the right and/or left eigenvectors of
     !! a complex upper triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a complex general matrix:  A = Q*T*Q**H, as computed by CHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix. If Q is the unitary factor that reduces a matrix A to
     !! Schur form T, then Q*X and Q*Y are the matrices of right and left
     !! eigenvectors of A.
     !! This uses a Level 3 BLAS version of the back transformation.
               work, lwork, rwork, lrwork, info)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, lrwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmin = 8_ilp
           integer(ilp), parameter :: nbmax = 128_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, lquery, over, rightv, somev
           integer(ilp) :: i, ii, is, j, k, ki, iv, maxwrk, nb
           real(sp) :: ovfl, remax, scale, smin, smlnum, ulp, unfl
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           bothv  = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv  = stdlib_lsame( side, 'L' ) .or. bothv
           allv  = stdlib_lsame( howmny, 'A' )
           over  = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           if( somev ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CTREVC', side // howmny, n, -1_ilp, -1_ilp, -1_ilp )
           maxwrk = n + 2_ilp*n*nb
           work(1_ilp) = maxwrk
           rwork(1_ilp) = n
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp )
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -11_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -14_ilp
           else if ( lrwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTREVC3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! use blocked version of back-transformation if sufficient workspace.
           ! zero-out the workspace to avoid potential nan propagation.
           if( over .and. lwork >= n + 2_ilp*n*nbmin ) then
              nb = (lwork - n) / (2_ilp*n)
              nb = min( nb, nbmax )
              call stdlib_claset( 'F', n, 1_ilp+2*nb, czero, czero, work, n )
           else
              nb = 1_ilp
           end if
           ! set the constants to control overflow.
           unfl = stdlib_slamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_slabad( unfl, ovfl )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ! store the diagonal elements of t in working array work.
           do i = 1, n
              work( i ) = t( i, i )
           end do
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           rwork( 1_ilp ) = zero
           do j = 2, n
              rwork( j ) = stdlib_scasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( rightv ) then
              ! ============================================================
              ! compute right eigenvectors.
              ! iv is index of column in current block.
              ! non-blocked version always uses iv=nb=1;
              ! blocked     version starts with iv=nb, goes down to 1.
              ! (note the "0-th" column is used to store the original diagonal.)
              iv = nb
              is = m
              loop_80: do ki = n, 1, -1
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_80
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 ! --------------------------------------------------------
                 ! complex right eigenvector
                 work( ki + iv*n ) = cone
                 ! form right-hand side.
                 do k = 1, ki - 1
                    work( k + iv*n ) = -t( k, ki )
                 end do
                 ! solve upper triangular system:
                 ! [ t(1:ki-1,1:ki-1) - t(ki,ki) ]*x = scale*work.
                 do k = 1, ki - 1
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki>1_ilp ) then
                    call stdlib_clatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', 'Y',ki-1, t, ldt, &
                              work( 1_ilp + iv*n ), scale,rwork, info )
                    work( ki + iv*n ) = scale
                 end if
                 ! copy the vector x or q*x to vr and normalize.
                 if( .not.over ) then
                    ! ------------------------------
                    ! no back-transform: copy x to vr and normalize.
                    call stdlib_ccopy( ki, work( 1_ilp + iv*n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                    ii = stdlib_icamax( ki, vr( 1_ilp, is ), 1_ilp )
                    remax = one / cabs1( vr( ii, is ) )
                    call stdlib_csscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                    do k = ki + 1, n
                       vr( k, is ) = czero
                    end do
                 else if( nb==1_ilp ) then
                    ! ------------------------------
                    ! version 1: back-transform each vector with gemv, q*x.
                    if( ki>1_ilp )call stdlib_cgemv( 'N', n, ki-1, cone, vr, ldvr,work( 1_ilp + iv*n ), 1_ilp,&
                               cmplx( scale,KIND=sp),vr( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_icamax( n, vr( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vr( ii, ki ) )
                    call stdlib_csscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                 else
                    ! ------------------------------
                    ! version 2: back-transform block of vectors with gemm
                    ! zero out below vector
                    do k = ki + 1, n
                       work( k + iv*n ) = czero
                    end do
                    ! columns iv:nb of work are valid vectors.
                    ! when the number of vectors stored reaches nb,
                    ! or if this was last vector, do the gemm
                    if( (iv==1_ilp) .or. (ki==1_ilp) ) then
                       call stdlib_cgemm( 'N', 'N', n, nb-iv+1, ki+nb-iv, cone,vr, ldvr,work( 1_ilp + &
                                 (iv)*n    ), n,czero,work( 1_ilp + (nb+iv)*n ), n )
                       ! normalize vectors
                       do k = iv, nb
                          ii = stdlib_icamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                          remax = one / cabs1( work( ii + (nb+k)*n ) )
                          call stdlib_csscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_clacpy( 'F', n, nb-iv+1,work( 1_ilp + (nb+iv)*n ), n,vr( 1_ilp, ki ), &
                                 ldvr )
                       iv = nb
                    else
                       iv = iv - 1_ilp
                    end if
                 end if
                 ! restore the original diagonal elements of t.
                 do k = 1, ki - 1
                    t( k, k ) = work( k )
                 end do
                 is = is - 1_ilp
              end do loop_80
           end if
           if( leftv ) then
              ! ============================================================
              ! compute left eigenvectors.
              ! iv is index of column in current block.
              ! non-blocked version always uses iv=1;
              ! blocked     version starts with iv=1, goes up to nb.
              ! (note the "0-th" column is used to store the original diagonal.)
              iv = 1_ilp
              is = 1_ilp
              loop_130: do ki = 1, n
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_130
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 ! --------------------------------------------------------
                 ! complex left eigenvector
                 work( ki + iv*n ) = cone
                 ! form right-hand side.
                 do k = ki + 1, n
                    work( k + iv*n ) = -conjg( t( ki, k ) )
                 end do
                 ! solve conjugate-transposed triangular system:
                 ! [ t(ki+1:n,ki+1:n) - t(ki,ki) ]**h * x = scale*work.
                 do k = ki + 1, n
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki<n ) then
                    call stdlib_clatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT','Y', n-ki, t( &
                              ki+1, ki+1 ), ldt,work( ki+1 + iv*n ), scale, rwork, info )
                    work( ki + iv*n ) = scale
                 end if
                 ! copy the vector x or q*x to vl and normalize.
                 if( .not.over ) then
                    ! ------------------------------
                    ! no back-transform: copy x to vl and normalize.
                    call stdlib_ccopy( n-ki+1, work( ki + iv*n ), 1_ilp, vl(ki,is), 1_ilp )
                    ii = stdlib_icamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                    remax = one / cabs1( vl( ii, is ) )
                    call stdlib_csscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                    do k = 1, ki - 1
                       vl( k, is ) = czero
                    end do
                 else if( nb==1_ilp ) then
                    ! ------------------------------
                    ! version 1: back-transform each vector with gemv, q*x.
                    if( ki<n )call stdlib_cgemv( 'N', n, n-ki, cone, vl( 1_ilp, ki+1 ), ldvl,work( ki+&
                              1_ilp + iv*n ), 1_ilp, cmplx( scale,KIND=sp),vl( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_icamax( n, vl( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vl( ii, ki ) )
                    call stdlib_csscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                 else
                    ! ------------------------------
                    ! version 2: back-transform block of vectors with gemm
                    ! zero out above vector
                    ! could go from ki-nv+1 to ki-1
                    do k = 1, ki - 1
                       work( k + iv*n ) = czero
                    end do
                    ! columns 1:iv of work are valid vectors.
                    ! when the number of vectors stored reaches nb,
                    ! or if this was last vector, do the gemm
                    if( (iv==nb) .or. (ki==n) ) then
                       call stdlib_cgemm( 'N', 'N', n, iv, n-ki+iv, cone,vl( 1_ilp, ki-iv+1 ), ldvl,&
                                 work( ki-iv+1 + (1_ilp)*n ), n,czero,work( 1_ilp + (nb+1)*n ), n )
                       ! normalize vectors
                       do k = 1, iv
                          ii = stdlib_icamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                          remax = one / cabs1( work( ii + (nb+k)*n ) )
                          call stdlib_csscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_clacpy( 'F', n, iv,work( 1_ilp + (nb+1)*n ), n,vl( 1_ilp, ki-iv+1 ), &
                                 ldvl )
                       iv = 1_ilp
                    else
                       iv = iv + 1_ilp
                    end if
                 end if
                 ! restore the original diagonal elements of t.
                 do k = ki + 1, n
                    t( k, k ) = work( k )
                 end do
                 is = is + 1_ilp
              end do loop_130
           end if
           return
     end subroutine stdlib_ctrevc3

     pure module subroutine stdlib_ztrevc3( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
     !! ZTREVC3 computes some or all of the right and/or left eigenvectors of
     !! a complex upper triangular matrix T.
     !! Matrices of this type are produced by the Schur factorization of
     !! a complex general matrix:  A = Q*T*Q**H, as computed by ZHSEQR.
     !! The right eigenvector x and the left eigenvector y of T corresponding
     !! to an eigenvalue w are defined by:
     !! T*x = w*x,     (y**H)*T = w*(y**H)
     !! where y**H denotes the conjugate transpose of the vector y.
     !! The eigenvalues are not input to this routine, but are read directly
     !! from the diagonal of T.
     !! This routine returns the matrices X and/or Y of right and left
     !! eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
     !! input matrix. If Q is the unitary factor that reduces a matrix A to
     !! Schur form T, then Q*X and Q*Y are the matrices of right and left
     !! eigenvectors of A.
     !! This uses a Level 3 BLAS version of the back transformation.
               work, lwork, rwork, lrwork, info)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, lrwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmin = 8_ilp
           integer(ilp), parameter :: nbmax = 128_ilp
           
           
           
           ! Local Scalars 
           logical(lk) :: allv, bothv, leftv, lquery, over, rightv, somev
           integer(ilp) :: i, ii, is, j, k, ki, iv, maxwrk, nb
           real(dp) :: ovfl, remax, scale, smin, smlnum, ulp, unfl
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           bothv  = stdlib_lsame( side, 'B' )
           rightv = stdlib_lsame( side, 'R' ) .or. bothv
           leftv  = stdlib_lsame( side, 'L' ) .or. bothv
           allv  = stdlib_lsame( howmny, 'A' )
           over  = stdlib_lsame( howmny, 'B' )
           somev = stdlib_lsame( howmny, 'S' )
           ! set m to the number of columns required to store the selected
           ! eigenvectors.
           if( somev ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZTREVC', side // howmny, n, -1_ilp, -1_ilp, -1_ilp )
           maxwrk = n + 2_ilp*n*nb
           work(1_ilp) = maxwrk
           rwork(1_ilp) = n
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp )
           if( .not.rightv .and. .not.leftv ) then
              info = -1_ilp
           else if( .not.allv .and. .not.over .and. .not.somev ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( leftv .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( rightv .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -11_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -14_ilp
           else if ( lrwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTREVC3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( n==0 )return
           ! use blocked version of back-transformation if sufficient workspace.
           ! zero-out the workspace to avoid potential nan propagation.
           if( over .and. lwork >= n + 2_ilp*n*nbmin ) then
              nb = (lwork - n) / (2_ilp*n)
              nb = min( nb, nbmax )
              call stdlib_zlaset( 'F', n, 1_ilp+2*nb, czero, czero, work, n )
           else
              nb = 1_ilp
           end if
           ! set the constants to control overflow.
           unfl = stdlib_dlamch( 'SAFE MINIMUM' )
           ovfl = one / unfl
           call stdlib_dlabad( unfl, ovfl )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = unfl*( n / ulp )
           ! store the diagonal elements of t in working array work.
           do i = 1, n
              work( i ) = t( i, i )
           end do
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           rwork( 1_ilp ) = zero
           do j = 2, n
              rwork( j ) = stdlib_dzasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( rightv ) then
              ! ============================================================
              ! compute right eigenvectors.
              ! iv is index of column in current block.
              ! non-blocked version always uses iv=nb=1;
              ! blocked     version starts with iv=nb, goes down to 1.
              ! (note the "0-th" column is used to store the original diagonal.)
              iv = nb
              is = m
              loop_80: do ki = n, 1, -1
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_80
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 ! --------------------------------------------------------
                 ! complex right eigenvector
                 work( ki + iv*n ) = cone
                 ! form right-hand side.
                 do k = 1, ki - 1
                    work( k + iv*n ) = -t( k, ki )
                 end do
                 ! solve upper triangular system:
                 ! [ t(1:ki-1,1:ki-1) - t(ki,ki) ]*x = scale*work.
                 do k = 1, ki - 1
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki>1_ilp ) then
                    call stdlib_zlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', 'Y',ki-1, t, ldt, &
                              work( 1_ilp + iv*n ), scale,rwork, info )
                    work( ki + iv*n ) = scale
                 end if
                 ! copy the vector x or q*x to vr and normalize.
                 if( .not.over ) then
                    ! ------------------------------
                    ! no back-transform: copy x to vr and normalize.
                    call stdlib_zcopy( ki, work( 1_ilp + iv*n ), 1_ilp, vr( 1_ilp, is ), 1_ilp )
                    ii = stdlib_izamax( ki, vr( 1_ilp, is ), 1_ilp )
                    remax = one / cabs1( vr( ii, is ) )
                    call stdlib_zdscal( ki, remax, vr( 1_ilp, is ), 1_ilp )
                    do k = ki + 1, n
                       vr( k, is ) = czero
                    end do
                 else if( nb==1_ilp ) then
                    ! ------------------------------
                    ! version 1: back-transform each vector with gemv, q*x.
                    if( ki>1_ilp )call stdlib_zgemv( 'N', n, ki-1, cone, vr, ldvr,work( 1_ilp + iv*n ), 1_ilp,&
                               cmplx( scale,KIND=dp),vr( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_izamax( n, vr( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vr( ii, ki ) )
                    call stdlib_zdscal( n, remax, vr( 1_ilp, ki ), 1_ilp )
                 else
                    ! ------------------------------
                    ! version 2: back-transform block of vectors with gemm
                    ! zero out below vector
                    do k = ki + 1, n
                       work( k + iv*n ) = czero
                    end do
                    ! columns iv:nb of work are valid vectors.
                    ! when the number of vectors stored reaches nb,
                    ! or if this was last vector, do the gemm
                    if( (iv==1_ilp) .or. (ki==1_ilp) ) then
                       call stdlib_zgemm( 'N', 'N', n, nb-iv+1, ki+nb-iv, cone,vr, ldvr,work( 1_ilp + &
                                 (iv)*n    ), n,czero,work( 1_ilp + (nb+iv)*n ), n )
                       ! normalize vectors
                       do k = iv, nb
                          ii = stdlib_izamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                          remax = one / cabs1( work( ii + (nb+k)*n ) )
                          call stdlib_zdscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_zlacpy( 'F', n, nb-iv+1,work( 1_ilp + (nb+iv)*n ), n,vr( 1_ilp, ki ), &
                                 ldvr )
                       iv = nb
                    else
                       iv = iv - 1_ilp
                    end if
                 end if
                 ! restore the original diagonal elements of t.
                 do k = 1, ki - 1
                    t( k, k ) = work( k )
                 end do
                 is = is - 1_ilp
              end do loop_80
           end if
           if( leftv ) then
              ! ============================================================
              ! compute left eigenvectors.
              ! iv is index of column in current block.
              ! non-blocked version always uses iv=1;
              ! blocked     version starts with iv=1, goes up to nb.
              ! (note the "0-th" column is used to store the original diagonal.)
              iv = 1_ilp
              is = 1_ilp
              loop_130: do ki = 1, n
                 if( somev ) then
                    if( .not.select( ki ) )cycle loop_130
                 end if
                 smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
                 ! --------------------------------------------------------
                 ! complex left eigenvector
                 work( ki + iv*n ) = cone
                 ! form right-hand side.
                 do k = ki + 1, n
                    work( k + iv*n ) = -conjg( t( ki, k ) )
                 end do
                 ! solve conjugate-transposed triangular system:
                 ! [ t(ki+1:n,ki+1:n) - t(ki,ki) ]**h * x = scale*work.
                 do k = ki + 1, n
                    t( k, k ) = t( k, k ) - t( ki, ki )
                    if( cabs1( t( k, k ) )<smin )t( k, k ) = smin
                 end do
                 if( ki<n ) then
                    call stdlib_zlatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT','Y', n-ki, t( &
                              ki+1, ki+1 ), ldt,work( ki+1 + iv*n ), scale, rwork, info )
                    work( ki + iv*n ) = scale
                 end if
                 ! copy the vector x or q*x to vl and normalize.
                 if( .not.over ) then
                    ! ------------------------------
                    ! no back-transform: copy x to vl and normalize.
                    call stdlib_zcopy( n-ki+1, work( ki + iv*n ), 1_ilp, vl(ki,is), 1_ilp )
                    ii = stdlib_izamax( n-ki+1, vl( ki, is ), 1_ilp ) + ki - 1_ilp
                    remax = one / cabs1( vl( ii, is ) )
                    call stdlib_zdscal( n-ki+1, remax, vl( ki, is ), 1_ilp )
                    do k = 1, ki - 1
                       vl( k, is ) = czero
                    end do
                 else if( nb==1_ilp ) then
                    ! ------------------------------
                    ! version 1: back-transform each vector with gemv, q*x.
                    if( ki<n )call stdlib_zgemv( 'N', n, n-ki, cone, vl( 1_ilp, ki+1 ), ldvl,work( ki+&
                              1_ilp + iv*n ), 1_ilp, cmplx( scale,KIND=dp),vl( 1_ilp, ki ), 1_ilp )
                    ii = stdlib_izamax( n, vl( 1_ilp, ki ), 1_ilp )
                    remax = one / cabs1( vl( ii, ki ) )
                    call stdlib_zdscal( n, remax, vl( 1_ilp, ki ), 1_ilp )
                 else
                    ! ------------------------------
                    ! version 2: back-transform block of vectors with gemm
                    ! zero out above vector
                    ! could go from ki-nv+1 to ki-1
                    do k = 1, ki - 1
                       work( k + iv*n ) = czero
                    end do
                    ! columns 1:iv of work are valid vectors.
                    ! when the number of vectors stored reaches nb,
                    ! or if this was last vector, do the gemm
                    if( (iv==nb) .or. (ki==n) ) then
                       call stdlib_zgemm( 'N', 'N', n, iv, n-ki+iv, cone,vl( 1_ilp, ki-iv+1 ), ldvl,&
                                 work( ki-iv+1 + (1_ilp)*n ), n,czero,work( 1_ilp + (nb+1)*n ), n )
                       ! normalize vectors
                       do k = 1, iv
                          ii = stdlib_izamax( n, work( 1_ilp + (nb+k)*n ), 1_ilp )
                          remax = one / cabs1( work( ii + (nb+k)*n ) )
                          call stdlib_zdscal( n, remax, work( 1_ilp + (nb+k)*n ), 1_ilp )
                       end do
                       call stdlib_zlacpy( 'F', n, iv,work( 1_ilp + (nb+1)*n ), n,vl( 1_ilp, ki-iv+1 ), &
                                 ldvl )
                       iv = 1_ilp
                    else
                       iv = iv + 1_ilp
                    end if
                 end if
                 ! restore the original diagonal elements of t.
                 do k = ki + 1, n
                    t( k, k ) = work( k )
                 end do
                 is = is + 1_ilp
              end do loop_130
           end if
           return
     end subroutine stdlib_ztrevc3




     pure module subroutine stdlib_slaln2( ltrans, na, nw, smin, ca, a, lda, d1, d2, b,ldb, wr, wi, x, &
     !! SLALN2 solves a system of the form  (ca A - w D ) X = s B
     !! or (ca A**T - w D) X = s B   with possible scaling ("s") and
     !! perturbation of A.  (A**T means A-transpose.)
     !! A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
     !! real diagonal matrix, w is a real or complex value, and X and B are
     !! NA x 1 matrices -- real if w is real, complex if w is complex.  NA
     !! may be 1 or 2.
     !! If w is complex, X and B are represented as NA x 2 matrices,
     !! the first column of each being the real part and the second
     !! being the imaginary part.
     !! "s" is a scaling factor (<= 1), computed by SLALN2, which is
     !! so chosen that X can be computed without overflow.  X is further
     !! scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
     !! than overflow.
     !! If both singular values of (ca A - w D) are less than SMIN,
     !! SMIN*identity will be used instead of (ca A - w D).  If only one
     !! singular value is less than SMIN, one element of (ca A - w D) will be
     !! perturbed enough to make the smallest singular value roughly SMIN.
     !! If both singular values are at least SMIN, (ca A - w D) will not be
     !! perturbed.  In any case, the perturbation will be at most some small
     !! multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
     !! are computed by infinity-norm approximations, and thus will only be
     !! correct to a factor of 2 or so.
     !! Note: all input quantities are assumed to be smaller than overflow
     !! by a reasonable factor.  (See BIGNUM.)
               ldx, scale, xnorm, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ltrans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, na, nw
           real(sp), intent(in) :: ca, d1, d2, smin, wi, wr
           real(sp), intent(out) :: scale, xnorm
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: x(ldx,*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: icmax, j
           real(sp) :: bbnd, bi1, bi2, bignum, bnorm, br1, br2, ci21, ci22, cmax, cnorm, cr21, &
           cr22, csi, csr, li21, lr21, smini, smlnum, temp, u22abs, ui11, ui11r, ui12, ui12s, &
                     ui22, ur11, ur11r, ur12, ur12s, ur22, xi1, xi2, xr1, xr2
           ! Local Arrays 
           logical(lk) :: cswap(4_ilp), rswap(4_ilp)
           integer(ilp) :: ipivot(4_ilp,4_ilp)
           real(sp) :: ci(2_ilp,2_ilp), civ(4_ilp), cr(2_ilp,2_ilp), crv(4_ilp)
           ! Intrinsic Functions 
           ! Equivalences 
           equivalence        ( ci( 1_ilp, 1_ilp ), civ( 1_ilp ) ),( cr( 1_ilp, 1_ilp ), crv( 1_ilp ) )
           ! Data Statements 
           cswap = [.false.,.false.,.true.,.true.]
           rswap = [.false.,.true.,.false.,.true.]
           ipivot = reshape([1_ilp,2_ilp,3_ilp,4_ilp,2_ilp,1_ilp,4_ilp,3_ilp,3_ilp,4_ilp,1_ilp,2_ilp,4_ilp,3_ilp,2_ilp,1_ilp],[4_il&
               &p,4_ilp])
           ! Executable Statements 
           ! compute bignum
           smlnum = two*stdlib_slamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           smini = max( smin, smlnum )
           ! don't check for input errors
           info = 0_ilp
           ! standard initializations
           scale = one
           if( na==1_ilp ) then
              ! 1 x 1  (i.e., scalar) system   c x = b
              if( nw==1_ilp ) then
                 ! real 1x1 system.
                 ! c = ca a - w d
                 csr = ca*a( 1_ilp, 1_ilp ) - wr*d1
                 cnorm = abs( csr )
                 ! if | c | < smini, use c = smini
                 if( cnorm<smini ) then
                    csr = smini
                    cnorm = smini
                    info = 1_ilp
                 end if
                 ! check scaling for  x = b / c
                 bnorm = abs( b( 1_ilp, 1_ilp ) )
                 if( cnorm<one .and. bnorm>one ) then
                    if( bnorm>bignum*cnorm )scale = one / bnorm
                 end if
                 ! compute x
                 x( 1_ilp, 1_ilp ) = ( b( 1_ilp, 1_ilp )*scale ) / csr
                 xnorm = abs( x( 1_ilp, 1_ilp ) )
              else
                 ! complex 1x1 system (w is complex)
                 ! c = ca a - w d
                 csr = ca*a( 1_ilp, 1_ilp ) - wr*d1
                 csi = -wi*d1
                 cnorm = abs( csr ) + abs( csi )
                 ! if | c | < smini, use c = smini
                 if( cnorm<smini ) then
                    csr = smini
                    csi = zero
                    cnorm = smini
                    info = 1_ilp
                 end if
                 ! check scaling for  x = b / c
                 bnorm = abs( b( 1_ilp, 1_ilp ) ) + abs( b( 1_ilp, 2_ilp ) )
                 if( cnorm<one .and. bnorm>one ) then
                    if( bnorm>bignum*cnorm )scale = one / bnorm
                 end if
                 ! compute x
                 call stdlib_sladiv( scale*b( 1_ilp, 1_ilp ), scale*b( 1_ilp, 2_ilp ), csr, csi,x( 1_ilp, 1_ilp ), x( 1_ilp, &
                           2_ilp ) )
                 xnorm = abs( x( 1_ilp, 1_ilp ) ) + abs( x( 1_ilp, 2_ilp ) )
              end if
           else
              ! 2x2 system
              ! compute the realpart of  c = ca a - w d  (or  ca a**t - w d,KIND=sp)
              cr( 1_ilp, 1_ilp ) = ca*a( 1_ilp, 1_ilp ) - wr*d1
              cr( 2_ilp, 2_ilp ) = ca*a( 2_ilp, 2_ilp ) - wr*d2
              if( ltrans ) then
                 cr( 1_ilp, 2_ilp ) = ca*a( 2_ilp, 1_ilp )
                 cr( 2_ilp, 1_ilp ) = ca*a( 1_ilp, 2_ilp )
              else
                 cr( 2_ilp, 1_ilp ) = ca*a( 2_ilp, 1_ilp )
                 cr( 1_ilp, 2_ilp ) = ca*a( 1_ilp, 2_ilp )
              end if
              if( nw==1_ilp ) then
                 ! real2x2 system  (w is real,KIND=sp)
                 ! find the largest element in c
                 cmax = zero
                 icmax = 0_ilp
                 do j = 1, 4
                    if( abs( crv( j ) )>cmax ) then
                       cmax = abs( crv( j ) )
                       icmax = j
                    end if
                 end do
                 ! if norm(c) < smini, use smini*identity.
                 if( cmax<smini ) then
                    bnorm = max( abs( b( 1_ilp, 1_ilp ) ), abs( b( 2_ilp, 1_ilp ) ) )
                    if( smini<one .and. bnorm>one ) then
                       if( bnorm>bignum*smini )scale = one / bnorm
                    end if
                    temp = scale / smini
                    x( 1_ilp, 1_ilp ) = temp*b( 1_ilp, 1_ilp )
                    x( 2_ilp, 1_ilp ) = temp*b( 2_ilp, 1_ilp )
                    xnorm = temp*bnorm
                    info = 1_ilp
                    return
                 end if
                 ! gaussian elimination with complete pivoting.
                 ur11 = crv( icmax )
                 cr21 = crv( ipivot( 2_ilp, icmax ) )
                 ur12 = crv( ipivot( 3_ilp, icmax ) )
                 cr22 = crv( ipivot( 4_ilp, icmax ) )
                 ur11r = one / ur11
                 lr21 = ur11r*cr21
                 ur22 = cr22 - ur12*lr21
                 ! if smaller pivot < smini, use smini
                 if( abs( ur22 )<smini ) then
                    ur22 = smini
                    info = 1_ilp
                 end if
                 if( rswap( icmax ) ) then
                    br1 = b( 2_ilp, 1_ilp )
                    br2 = b( 1_ilp, 1_ilp )
                 else
                    br1 = b( 1_ilp, 1_ilp )
                    br2 = b( 2_ilp, 1_ilp )
                 end if
                 br2 = br2 - lr21*br1
                 bbnd = max( abs( br1*( ur22*ur11r ) ), abs( br2 ) )
                 if( bbnd>one .and. abs( ur22 )<one ) then
                    if( bbnd>=bignum*abs( ur22 ) )scale = one / bbnd
                 end if
                 xr2 = ( br2*scale ) / ur22
                 xr1 = ( scale*br1 )*ur11r - xr2*( ur11r*ur12 )
                 if( cswap( icmax ) ) then
                    x( 1_ilp, 1_ilp ) = xr2
                    x( 2_ilp, 1_ilp ) = xr1
                 else
                    x( 1_ilp, 1_ilp ) = xr1
                    x( 2_ilp, 1_ilp ) = xr2
                 end if
                 xnorm = max( abs( xr1 ), abs( xr2 ) )
                 ! further scaling if  norm(a) norm(x) > overflow
                 if( xnorm>one .and. cmax>one ) then
                    if( xnorm>bignum / cmax ) then
                       temp = cmax / bignum
                       x( 1_ilp, 1_ilp ) = temp*x( 1_ilp, 1_ilp )
                       x( 2_ilp, 1_ilp ) = temp*x( 2_ilp, 1_ilp )
                       xnorm = temp*xnorm
                       scale = temp*scale
                    end if
                 end if
              else
                 ! complex 2x2 system  (w is complex)
                 ! find the largest element in c
                 ci( 1_ilp, 1_ilp ) = -wi*d1
                 ci( 2_ilp, 1_ilp ) = zero
                 ci( 1_ilp, 2_ilp ) = zero
                 ci( 2_ilp, 2_ilp ) = -wi*d2
                 cmax = zero
                 icmax = 0_ilp
                 do j = 1, 4
                    if( abs( crv( j ) )+abs( civ( j ) )>cmax ) then
                       cmax = abs( crv( j ) ) + abs( civ( j ) )
                       icmax = j
                    end if
                 end do
                 ! if norm(c) < smini, use smini*identity.
                 if( cmax<smini ) then
                    bnorm = max( abs( b( 1_ilp, 1_ilp ) )+abs( b( 1_ilp, 2_ilp ) ),abs( b( 2_ilp, 1_ilp ) )+abs( b( 2_ilp, 2_ilp )&
                               ) )
                    if( smini<one .and. bnorm>one ) then
                       if( bnorm>bignum*smini )scale = one / bnorm
                    end if
                    temp = scale / smini
                    x( 1_ilp, 1_ilp ) = temp*b( 1_ilp, 1_ilp )
                    x( 2_ilp, 1_ilp ) = temp*b( 2_ilp, 1_ilp )
                    x( 1_ilp, 2_ilp ) = temp*b( 1_ilp, 2_ilp )
                    x( 2_ilp, 2_ilp ) = temp*b( 2_ilp, 2_ilp )
                    xnorm = temp*bnorm
                    info = 1_ilp
                    return
                 end if
                 ! gaussian elimination with complete pivoting.
                 ur11 = crv( icmax )
                 ui11 = civ( icmax )
                 cr21 = crv( ipivot( 2_ilp, icmax ) )
                 ci21 = civ( ipivot( 2_ilp, icmax ) )
                 ur12 = crv( ipivot( 3_ilp, icmax ) )
                 ui12 = civ( ipivot( 3_ilp, icmax ) )
                 cr22 = crv( ipivot( 4_ilp, icmax ) )
                 ci22 = civ( ipivot( 4_ilp, icmax ) )
                 if( icmax==1_ilp .or. icmax==4_ilp ) then
                    ! code when off-diagonals of pivoted c are real
                    if( abs( ur11 )>abs( ui11 ) ) then
                       temp = ui11 / ur11
                       ur11r = one / ( ur11*( one+temp**2_ilp ) )
                       ui11r = -temp*ur11r
                    else
                       temp = ur11 / ui11
                       ui11r = -one / ( ui11*( one+temp**2_ilp ) )
                       ur11r = -temp*ui11r
                    end if
                    lr21 = cr21*ur11r
                    li21 = cr21*ui11r
                    ur12s = ur12*ur11r
                    ui12s = ur12*ui11r
                    ur22 = cr22 - ur12*lr21
                    ui22 = ci22 - ur12*li21
                 else
                    ! code when diagonals of pivoted c are real
                    ur11r = one / ur11
                    ui11r = zero
                    lr21 = cr21*ur11r
                    li21 = ci21*ur11r
                    ur12s = ur12*ur11r
                    ui12s = ui12*ur11r
                    ur22 = cr22 - ur12*lr21 + ui12*li21
                    ui22 = -ur12*li21 - ui12*lr21
                 end if
                 u22abs = abs( ur22 ) + abs( ui22 )
                 ! if smaller pivot < smini, use smini
                 if( u22abs<smini ) then
                    ur22 = smini
                    ui22 = zero
                    info = 1_ilp
                 end if
                 if( rswap( icmax ) ) then
                    br2 = b( 1_ilp, 1_ilp )
                    br1 = b( 2_ilp, 1_ilp )
                    bi2 = b( 1_ilp, 2_ilp )
                    bi1 = b( 2_ilp, 2_ilp )
                 else
                    br1 = b( 1_ilp, 1_ilp )
                    br2 = b( 2_ilp, 1_ilp )
                    bi1 = b( 1_ilp, 2_ilp )
                    bi2 = b( 2_ilp, 2_ilp )
                 end if
                 br2 = br2 - lr21*br1 + li21*bi1
                 bi2 = bi2 - li21*br1 - lr21*bi1
                 bbnd = max( ( abs( br1 )+abs( bi1 ) )*( u22abs*( abs( ur11r )+abs( ui11r ) ) ),&
                           abs( br2 )+abs( bi2 ) )
                 if( bbnd>one .and. u22abs<one ) then
                    if( bbnd>=bignum*u22abs ) then
                       scale = one / bbnd
                       br1 = scale*br1
                       bi1 = scale*bi1
                       br2 = scale*br2
                       bi2 = scale*bi2
                    end if
                 end if
                 call stdlib_sladiv( br2, bi2, ur22, ui22, xr2, xi2 )
                 xr1 = ur11r*br1 - ui11r*bi1 - ur12s*xr2 + ui12s*xi2
                 xi1 = ui11r*br1 + ur11r*bi1 - ui12s*xr2 - ur12s*xi2
                 if( cswap( icmax ) ) then
                    x( 1_ilp, 1_ilp ) = xr2
                    x( 2_ilp, 1_ilp ) = xr1
                    x( 1_ilp, 2_ilp ) = xi2
                    x( 2_ilp, 2_ilp ) = xi1
                 else
                    x( 1_ilp, 1_ilp ) = xr1
                    x( 2_ilp, 1_ilp ) = xr2
                    x( 1_ilp, 2_ilp ) = xi1
                    x( 2_ilp, 2_ilp ) = xi2
                 end if
                 xnorm = max( abs( xr1 )+abs( xi1 ), abs( xr2 )+abs( xi2 ) )
                 ! further scaling if  norm(a) norm(x) > overflow
                 if( xnorm>one .and. cmax>one ) then
                    if( xnorm>bignum / cmax ) then
                       temp = cmax / bignum
                       x( 1_ilp, 1_ilp ) = temp*x( 1_ilp, 1_ilp )
                       x( 2_ilp, 1_ilp ) = temp*x( 2_ilp, 1_ilp )
                       x( 1_ilp, 2_ilp ) = temp*x( 1_ilp, 2_ilp )
                       x( 2_ilp, 2_ilp ) = temp*x( 2_ilp, 2_ilp )
                       xnorm = temp*xnorm
                       scale = temp*scale
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_slaln2

     pure module subroutine stdlib_dlaln2( ltrans, na, nw, smin, ca, a, lda, d1, d2, b,ldb, wr, wi, x, &
     !! DLALN2 solves a system of the form  (ca A - w D ) X = s B
     !! or (ca A**T - w D) X = s B   with possible scaling ("s") and
     !! perturbation of A.  (A**T means A-transpose.)
     !! A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
     !! real diagonal matrix, w is a real or complex value, and X and B are
     !! NA x 1 matrices -- real if w is real, complex if w is complex.  NA
     !! may be 1 or 2.
     !! If w is complex, X and B are represented as NA x 2 matrices,
     !! the first column of each being the real part and the second
     !! being the imaginary part.
     !! "s" is a scaling factor (<= 1), computed by DLALN2, which is
     !! so chosen that X can be computed without overflow.  X is further
     !! scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
     !! than overflow.
     !! If both singular values of (ca A - w D) are less than SMIN,
     !! SMIN*identity will be used instead of (ca A - w D).  If only one
     !! singular value is less than SMIN, one element of (ca A - w D) will be
     !! perturbed enough to make the smallest singular value roughly SMIN.
     !! If both singular values are at least SMIN, (ca A - w D) will not be
     !! perturbed.  In any case, the perturbation will be at most some small
     !! multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
     !! are computed by infinity-norm approximations, and thus will only be
     !! correct to a factor of 2 or so.
     !! Note: all input quantities are assumed to be smaller than overflow
     !! by a reasonable factor.  (See BIGNUM.)
               ldx, scale, xnorm, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ltrans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, na, nw
           real(dp), intent(in) :: ca, d1, d2, smin, wi, wr
           real(dp), intent(out) :: scale, xnorm
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: x(ldx,*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: icmax, j
           real(dp) :: bbnd, bi1, bi2, bignum, bnorm, br1, br2, ci21, ci22, cmax, cnorm, cr21, &
           cr22, csi, csr, li21, lr21, smini, smlnum, temp, u22abs, ui11, ui11r, ui12, ui12s, &
                     ui22, ur11, ur11r, ur12, ur12s, ur22, xi1, xi2, xr1, xr2
           ! Local Arrays 
           logical(lk) :: rswap(4_ilp), zswap(4_ilp)
           integer(ilp) :: ipivot(4_ilp,4_ilp)
           real(dp) :: ci(2_ilp,2_ilp), civ(4_ilp), cr(2_ilp,2_ilp), crv(4_ilp)
           ! Intrinsic Functions 
           ! Equivalences 
           equivalence        ( ci( 1_ilp, 1_ilp ), civ( 1_ilp ) ),( cr( 1_ilp, 1_ilp ), crv( 1_ilp ) )
           ! Data Statements 
           zswap = [.false.,.false.,.true.,.true.]
           rswap = [.false.,.true.,.false.,.true.]
           ipivot = reshape([1_ilp,2_ilp,3_ilp,4_ilp,2_ilp,1_ilp,4_ilp,3_ilp,3_ilp,4_ilp,1_ilp,2_ilp,4_ilp,3_ilp,2_ilp,1_ilp],[4_il&
               &p,4_ilp])
           ! Executable Statements 
           ! compute bignum
           smlnum = two*stdlib_dlamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           smini = max( smin, smlnum )
           ! don't check for input errors
           info = 0_ilp
           ! standard initializations
           scale = one
           if( na==1_ilp ) then
              ! 1 x 1  (i.e., scalar) system   c x = b
              if( nw==1_ilp ) then
                 ! real 1x1 system.
                 ! c = ca a - w d
                 csr = ca*a( 1_ilp, 1_ilp ) - wr*d1
                 cnorm = abs( csr )
                 ! if | c | < smini, use c = smini
                 if( cnorm<smini ) then
                    csr = smini
                    cnorm = smini
                    info = 1_ilp
                 end if
                 ! check scaling for  x = b / c
                 bnorm = abs( b( 1_ilp, 1_ilp ) )
                 if( cnorm<one .and. bnorm>one ) then
                    if( bnorm>bignum*cnorm )scale = one / bnorm
                 end if
                 ! compute x
                 x( 1_ilp, 1_ilp ) = ( b( 1_ilp, 1_ilp )*scale ) / csr
                 xnorm = abs( x( 1_ilp, 1_ilp ) )
              else
                 ! complex 1x1 system (w is complex)
                 ! c = ca a - w d
                 csr = ca*a( 1_ilp, 1_ilp ) - wr*d1
                 csi = -wi*d1
                 cnorm = abs( csr ) + abs( csi )
                 ! if | c | < smini, use c = smini
                 if( cnorm<smini ) then
                    csr = smini
                    csi = zero
                    cnorm = smini
                    info = 1_ilp
                 end if
                 ! check scaling for  x = b / c
                 bnorm = abs( b( 1_ilp, 1_ilp ) ) + abs( b( 1_ilp, 2_ilp ) )
                 if( cnorm<one .and. bnorm>one ) then
                    if( bnorm>bignum*cnorm )scale = one / bnorm
                 end if
                 ! compute x
                 call stdlib_dladiv( scale*b( 1_ilp, 1_ilp ), scale*b( 1_ilp, 2_ilp ), csr, csi,x( 1_ilp, 1_ilp ), x( 1_ilp, &
                           2_ilp ) )
                 xnorm = abs( x( 1_ilp, 1_ilp ) ) + abs( x( 1_ilp, 2_ilp ) )
              end if
           else
              ! 2x2 system
              ! compute the realpart of  c = ca a - w d  (or  ca a**t - w d,KIND=dp)
              cr( 1_ilp, 1_ilp ) = ca*a( 1_ilp, 1_ilp ) - wr*d1
              cr( 2_ilp, 2_ilp ) = ca*a( 2_ilp, 2_ilp ) - wr*d2
              if( ltrans ) then
                 cr( 1_ilp, 2_ilp ) = ca*a( 2_ilp, 1_ilp )
                 cr( 2_ilp, 1_ilp ) = ca*a( 1_ilp, 2_ilp )
              else
                 cr( 2_ilp, 1_ilp ) = ca*a( 2_ilp, 1_ilp )
                 cr( 1_ilp, 2_ilp ) = ca*a( 1_ilp, 2_ilp )
              end if
              if( nw==1_ilp ) then
                 ! real2x2 system  (w is real,KIND=dp)
                 ! find the largest element in c
                 cmax = zero
                 icmax = 0_ilp
                 do j = 1, 4
                    if( abs( crv( j ) )>cmax ) then
                       cmax = abs( crv( j ) )
                       icmax = j
                    end if
                 end do
                 ! if norm(c) < smini, use smini*identity.
                 if( cmax<smini ) then
                    bnorm = max( abs( b( 1_ilp, 1_ilp ) ), abs( b( 2_ilp, 1_ilp ) ) )
                    if( smini<one .and. bnorm>one ) then
                       if( bnorm>bignum*smini )scale = one / bnorm
                    end if
                    temp = scale / smini
                    x( 1_ilp, 1_ilp ) = temp*b( 1_ilp, 1_ilp )
                    x( 2_ilp, 1_ilp ) = temp*b( 2_ilp, 1_ilp )
                    xnorm = temp*bnorm
                    info = 1_ilp
                    return
                 end if
                 ! gaussian elimination with complete pivoting.
                 ur11 = crv( icmax )
                 cr21 = crv( ipivot( 2_ilp, icmax ) )
                 ur12 = crv( ipivot( 3_ilp, icmax ) )
                 cr22 = crv( ipivot( 4_ilp, icmax ) )
                 ur11r = one / ur11
                 lr21 = ur11r*cr21
                 ur22 = cr22 - ur12*lr21
                 ! if smaller pivot < smini, use smini
                 if( abs( ur22 )<smini ) then
                    ur22 = smini
                    info = 1_ilp
                 end if
                 if( rswap( icmax ) ) then
                    br1 = b( 2_ilp, 1_ilp )
                    br2 = b( 1_ilp, 1_ilp )
                 else
                    br1 = b( 1_ilp, 1_ilp )
                    br2 = b( 2_ilp, 1_ilp )
                 end if
                 br2 = br2 - lr21*br1
                 bbnd = max( abs( br1*( ur22*ur11r ) ), abs( br2 ) )
                 if( bbnd>one .and. abs( ur22 )<one ) then
                    if( bbnd>=bignum*abs( ur22 ) )scale = one / bbnd
                 end if
                 xr2 = ( br2*scale ) / ur22
                 xr1 = ( scale*br1 )*ur11r - xr2*( ur11r*ur12 )
                 if( zswap( icmax ) ) then
                    x( 1_ilp, 1_ilp ) = xr2
                    x( 2_ilp, 1_ilp ) = xr1
                 else
                    x( 1_ilp, 1_ilp ) = xr1
                    x( 2_ilp, 1_ilp ) = xr2
                 end if
                 xnorm = max( abs( xr1 ), abs( xr2 ) )
                 ! further scaling if  norm(a) norm(x) > overflow
                 if( xnorm>one .and. cmax>one ) then
                    if( xnorm>bignum / cmax ) then
                       temp = cmax / bignum
                       x( 1_ilp, 1_ilp ) = temp*x( 1_ilp, 1_ilp )
                       x( 2_ilp, 1_ilp ) = temp*x( 2_ilp, 1_ilp )
                       xnorm = temp*xnorm
                       scale = temp*scale
                    end if
                 end if
              else
                 ! complex 2x2 system  (w is complex)
                 ! find the largest element in c
                 ci( 1_ilp, 1_ilp ) = -wi*d1
                 ci( 2_ilp, 1_ilp ) = zero
                 ci( 1_ilp, 2_ilp ) = zero
                 ci( 2_ilp, 2_ilp ) = -wi*d2
                 cmax = zero
                 icmax = 0_ilp
                 do j = 1, 4
                    if( abs( crv( j ) )+abs( civ( j ) )>cmax ) then
                       cmax = abs( crv( j ) ) + abs( civ( j ) )
                       icmax = j
                    end if
                 end do
                 ! if norm(c) < smini, use smini*identity.
                 if( cmax<smini ) then
                    bnorm = max( abs( b( 1_ilp, 1_ilp ) )+abs( b( 1_ilp, 2_ilp ) ),abs( b( 2_ilp, 1_ilp ) )+abs( b( 2_ilp, 2_ilp )&
                               ) )
                    if( smini<one .and. bnorm>one ) then
                       if( bnorm>bignum*smini )scale = one / bnorm
                    end if
                    temp = scale / smini
                    x( 1_ilp, 1_ilp ) = temp*b( 1_ilp, 1_ilp )
                    x( 2_ilp, 1_ilp ) = temp*b( 2_ilp, 1_ilp )
                    x( 1_ilp, 2_ilp ) = temp*b( 1_ilp, 2_ilp )
                    x( 2_ilp, 2_ilp ) = temp*b( 2_ilp, 2_ilp )
                    xnorm = temp*bnorm
                    info = 1_ilp
                    return
                 end if
                 ! gaussian elimination with complete pivoting.
                 ur11 = crv( icmax )
                 ui11 = civ( icmax )
                 cr21 = crv( ipivot( 2_ilp, icmax ) )
                 ci21 = civ( ipivot( 2_ilp, icmax ) )
                 ur12 = crv( ipivot( 3_ilp, icmax ) )
                 ui12 = civ( ipivot( 3_ilp, icmax ) )
                 cr22 = crv( ipivot( 4_ilp, icmax ) )
                 ci22 = civ( ipivot( 4_ilp, icmax ) )
                 if( icmax==1_ilp .or. icmax==4_ilp ) then
                    ! code when off-diagonals of pivoted c are real
                    if( abs( ur11 )>abs( ui11 ) ) then
                       temp = ui11 / ur11
                       ur11r = one / ( ur11*( one+temp**2_ilp ) )
                       ui11r = -temp*ur11r
                    else
                       temp = ur11 / ui11
                       ui11r = -one / ( ui11*( one+temp**2_ilp ) )
                       ur11r = -temp*ui11r
                    end if
                    lr21 = cr21*ur11r
                    li21 = cr21*ui11r
                    ur12s = ur12*ur11r
                    ui12s = ur12*ui11r
                    ur22 = cr22 - ur12*lr21
                    ui22 = ci22 - ur12*li21
                 else
                    ! code when diagonals of pivoted c are real
                    ur11r = one / ur11
                    ui11r = zero
                    lr21 = cr21*ur11r
                    li21 = ci21*ur11r
                    ur12s = ur12*ur11r
                    ui12s = ui12*ur11r
                    ur22 = cr22 - ur12*lr21 + ui12*li21
                    ui22 = -ur12*li21 - ui12*lr21
                 end if
                 u22abs = abs( ur22 ) + abs( ui22 )
                 ! if smaller pivot < smini, use smini
                 if( u22abs<smini ) then
                    ur22 = smini
                    ui22 = zero
                    info = 1_ilp
                 end if
                 if( rswap( icmax ) ) then
                    br2 = b( 1_ilp, 1_ilp )
                    br1 = b( 2_ilp, 1_ilp )
                    bi2 = b( 1_ilp, 2_ilp )
                    bi1 = b( 2_ilp, 2_ilp )
                 else
                    br1 = b( 1_ilp, 1_ilp )
                    br2 = b( 2_ilp, 1_ilp )
                    bi1 = b( 1_ilp, 2_ilp )
                    bi2 = b( 2_ilp, 2_ilp )
                 end if
                 br2 = br2 - lr21*br1 + li21*bi1
                 bi2 = bi2 - li21*br1 - lr21*bi1
                 bbnd = max( ( abs( br1 )+abs( bi1 ) )*( u22abs*( abs( ur11r )+abs( ui11r ) ) ),&
                           abs( br2 )+abs( bi2 ) )
                 if( bbnd>one .and. u22abs<one ) then
                    if( bbnd>=bignum*u22abs ) then
                       scale = one / bbnd
                       br1 = scale*br1
                       bi1 = scale*bi1
                       br2 = scale*br2
                       bi2 = scale*bi2
                    end if
                 end if
                 call stdlib_dladiv( br2, bi2, ur22, ui22, xr2, xi2 )
                 xr1 = ur11r*br1 - ui11r*bi1 - ur12s*xr2 + ui12s*xi2
                 xi1 = ui11r*br1 + ur11r*bi1 - ui12s*xr2 - ur12s*xi2
                 if( zswap( icmax ) ) then
                    x( 1_ilp, 1_ilp ) = xr2
                    x( 2_ilp, 1_ilp ) = xr1
                    x( 1_ilp, 2_ilp ) = xi2
                    x( 2_ilp, 2_ilp ) = xi1
                 else
                    x( 1_ilp, 1_ilp ) = xr1
                    x( 2_ilp, 1_ilp ) = xr2
                    x( 1_ilp, 2_ilp ) = xi1
                    x( 2_ilp, 2_ilp ) = xi2
                 end if
                 xnorm = max( abs( xr1 )+abs( xi1 ), abs( xr2 )+abs( xi2 ) )
                 ! further scaling if  norm(a) norm(x) > overflow
                 if( xnorm>one .and. cmax>one ) then
                    if( xnorm>bignum / cmax ) then
                       temp = cmax / bignum
                       x( 1_ilp, 1_ilp ) = temp*x( 1_ilp, 1_ilp )
                       x( 2_ilp, 1_ilp ) = temp*x( 2_ilp, 1_ilp )
                       x( 1_ilp, 2_ilp ) = temp*x( 1_ilp, 2_ilp )
                       x( 2_ilp, 2_ilp ) = temp*x( 2_ilp, 2_ilp )
                       xnorm = temp*xnorm
                       scale = temp*scale
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_dlaln2




     module subroutine stdlib_strsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
     !! STRSYL solves the real Sylvester matrix equation:
     !! op(A)*X + X*op(B) = scale*C or
     !! op(A)*X - X*op(B) = scale*C,
     !! where op(A) = A or A**T, and  A and B are both upper quasi-
     !! triangular. A is M-by-M and B is N-by-N; the right hand side C and
     !! the solution X are M-by-N; and scale is an output scale factor, set
     !! <= 1 to avoid overflow in X.
     !! A and B must be in Schur canonical form (as returned by SHSEQR), that
     !! is, block upper triangular with 1-by-1 and 2-by-2 diagonal blocks;
     !! each 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notrna, notrnb
           integer(ilp) :: ierr, j, k, k1, k2, knext, l, l1, l2, lnext
           real(sp) :: a11, bignum, da11, db, eps, scaloc, sgn, smin, smlnum, suml, sumr, &
                     xnorm
           ! Local Arrays 
           real(sp) :: dum(1_ilp), vec(2_ilp,2_ilp), x(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           notrna = stdlib_lsame( trana, 'N' )
           notrnb = stdlib_lsame( tranb, 'N' )
           info = 0_ilp
           if( .not.notrna .and. .not.stdlib_lsame( trana, 'T' ) .and. .not.stdlib_lsame( trana, &
                     'C' ) ) then
              info = -1_ilp
           else if( .not.notrnb .and. .not.stdlib_lsame( tranb, 'T' ) .and. .not.stdlib_lsame( &
                     tranb, 'C' ) ) then
              info = -2_ilp
           else if( isgn/=1_ilp .and. isgn/=-1_ilp ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRSYL', -info )
              return
           end if
           ! quick return if possible
           scale = one
           if( m==0 .or. n==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = smlnum*real( m*n,KIND=sp) / eps
           bignum = one / smlnum
           smin = max( smlnum, eps*stdlib_slange( 'M', m, m, a, lda, dum ),eps*stdlib_slange( 'M',&
                      n, n, b, ldb, dum ) )
           sgn = isgn
           if( notrna .and. notrnb ) then
              ! solve    a*x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
               ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                        ! m                         l-1
              ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b(j,l)].
                      ! i=k+1                       j=1
              ! start column loop (index = l)
              ! l1 (l2) : column index of the first (first) row of x(k,l).
              lnext = 1_ilp
              loop_70: do l = 1, n
                 if( l<lnext )cycle loop_70
                 if( l==n ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l+1, l )/=zero ) then
                       l1 = l
                       l2 = l + 1_ilp
                       lnext = l + 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l + 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l).
                 knext = m
                 loop_60: do k = m, 1, -1
                    if( k>knext )cycle loop_60
                    if( k==1_ilp ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k, k-1 )/=zero ) then
                          k1 = k - 1_ilp
                          k2 = k
                          knext = k - 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k - 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_sdot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_slasy2( .false., .false., isgn, 2_ilp, 2_ilp,a( k1, k1 ), lda, b( l1, &
                                 l1 ), ldb, vec,2_ilp, scaloc, x, 2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_60
              end do loop_70
           else if( .not.notrna .and. notrnb ) then
              ! solve    a**t *x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! upper-left corner column by column by
                ! a(k,k)**t*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                         ! k-1                          l-1
                ! r(k,l) = sum [a(i,k)**t*x(i,l)] +isgn*sum [x(k,j)*b(j,l)]
                         ! i=1                          j=1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = 1_ilp
              loop_130: do l = 1, n
                 if( l<lnext )cycle loop_130
                 if( l==n ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l+1, l )/=zero ) then
                       l1 = l
                       l2 = l + 1_ilp
                       lnext = l + 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l + 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = 1_ilp
                 loop_120: do k = 1, m
                    if( k<knext )cycle loop_120
                    if( k==m ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k+1, k )/=zero ) then
                          k1 = k
                          k2 = k + 1_ilp
                          knext = k + 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k + 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_slasy2( .true., .false., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, &
                                 l1 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_120
              end do loop_130
           else if( .not.notrna .and. .not.notrnb ) then
              ! solve    a**t*x + isgn*x*b**t = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! top-right corner column by column by
                 ! a(k,k)**t*x(k,l) + isgn*x(k,l)*b(l,l)**t = c(k,l) - r(k,l)
              ! where
                           ! k-1                            n
                  ! r(k,l) = sum [a(i,k)**t*x(i,l)] + isgn*sum [x(k,j)*b(l,j)**t].
                           ! i=1                          j=l+1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = n
              loop_190: do l = n, 1, -1
                 if( l>lnext )cycle loop_190
                 if( l==1_ilp ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l, l-1 )/=zero ) then
                       l1 = l - 1_ilp
                       l2 = l
                       lnext = l - 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l - 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = 1_ilp
                 loop_180: do k = 1, m
                    if( k<knext )cycle loop_180
                    if( k==m ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k+1, k )/=zero ) then
                          k1 = k
                          k2 = k + 1_ilp
                          knext = k + 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k + 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l1, c( k1, min( l1+1, n ) ), ldc,b( l1, min( l1+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l2, min(l2+1, n )&
                                  ), ldb )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_slasy2( .true., .true., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, l1 &
                                 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_180
              end do loop_190
           else if( notrna .and. .not.notrnb ) then
              ! solve    a*x + isgn*x*b**t = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-right corner column by column by
                  ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l)**t = c(k,l) - r(k,l)
              ! where
                            ! m                          n
                  ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b(l,j)**t].
                          ! i=k+1                      j=l+1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = n
              loop_250: do l = n, 1, -1
                 if( l>lnext )cycle loop_250
                 if( l==1_ilp ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l, l-1 )/=zero ) then
                       l1 = l - 1_ilp
                       l2 = l
                       lnext = l - 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l - 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = m
                 loop_240: do k = m, 1, -1
                    if( k>knext )cycle loop_240
                    if( k==1_ilp ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k, k-1 )/=zero ) then
                          k1 = k - 1_ilp
                          k2 = k
                          knext = k - 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k - 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( m-k1, a( k1, min(k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l1, c( k1, min( l1+1, n ) ), ldc,b( l1, min( l1+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_sdot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_sdot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_sdot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_sdot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_slasy2( .false., .true., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, &
                                 l1 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_sscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_240
              end do loop_250
           end if
           return
     end subroutine stdlib_strsyl

     module subroutine stdlib_dtrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
     !! DTRSYL solves the real Sylvester matrix equation:
     !! op(A)*X + X*op(B) = scale*C or
     !! op(A)*X - X*op(B) = scale*C,
     !! where op(A) = A or A**T, and  A and B are both upper quasi-
     !! triangular. A is M-by-M and B is N-by-N; the right hand side C and
     !! the solution X are M-by-N; and scale is an output scale factor, set
     !! <= 1 to avoid overflow in X.
     !! A and B must be in Schur canonical form (as returned by DHSEQR), that
     !! is, block upper triangular with 1-by-1 and 2-by-2 diagonal blocks;
     !! each 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notrna, notrnb
           integer(ilp) :: ierr, j, k, k1, k2, knext, l, l1, l2, lnext
           real(dp) :: a11, bignum, da11, db, eps, scaloc, sgn, smin, smlnum, suml, sumr, &
                     xnorm
           ! Local Arrays 
           real(dp) :: dum(1_ilp), vec(2_ilp,2_ilp), x(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           notrna = stdlib_lsame( trana, 'N' )
           notrnb = stdlib_lsame( tranb, 'N' )
           info = 0_ilp
           if( .not.notrna .and. .not.stdlib_lsame( trana, 'T' ) .and. .not.stdlib_lsame( trana, &
                     'C' ) ) then
              info = -1_ilp
           else if( .not.notrnb .and. .not.stdlib_lsame( tranb, 'T' ) .and. .not.stdlib_lsame( &
                     tranb, 'C' ) ) then
              info = -2_ilp
           else if( isgn/=1_ilp .and. isgn/=-1_ilp ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRSYL', -info )
              return
           end if
           ! quick return if possible
           scale = one
           if( m==0 .or. n==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = smlnum*real( m*n,KIND=dp) / eps
           bignum = one / smlnum
           smin = max( smlnum, eps*stdlib_dlange( 'M', m, m, a, lda, dum ),eps*stdlib_dlange( 'M',&
                      n, n, b, ldb, dum ) )
           sgn = isgn
           if( notrna .and. notrnb ) then
              ! solve    a*x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
               ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                        ! m                         l-1
              ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b(j,l)].
                      ! i=k+1                       j=1
              ! start column loop (index = l)
              ! l1 (l2) : column index of the first (first) row of x(k,l).
              lnext = 1_ilp
              loop_60: do l = 1, n
                 if( l<lnext )cycle loop_60
                 if( l==n ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l+1, l )/=zero ) then
                       l1 = l
                       l2 = l + 1_ilp
                       lnext = l + 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l + 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l).
                 knext = m
                 loop_50: do k = m, 1, -1
                    if( k>knext )cycle loop_50
                    if( k==1_ilp ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k, k-1 )/=zero ) then
                          k1 = k - 1_ilp
                          k2 = k
                          knext = k - 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k - 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_dlasy2( .false., .false., isgn, 2_ilp, 2_ilp,a( k1, k1 ), lda, b( l1, &
                                 l1 ), ldb, vec,2_ilp, scaloc, x, 2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_50
              end do loop_60
           else if( .not.notrna .and. notrnb ) then
              ! solve    a**t *x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! upper-left corner column by column by
                ! a(k,k)**t*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                         ! k-1        t                    l-1
                ! r(k,l) = sum [a(i,k)**t*x(i,l)] +isgn*sum [x(k,j)*b(j,l)]
                         ! i=1                          j=1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = 1_ilp
              loop_120: do l = 1, n
                 if( l<lnext )cycle loop_120
                 if( l==n ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l+1, l )/=zero ) then
                       l1 = l
                       l2 = l + 1_ilp
                       lnext = l + 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l + 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = 1_ilp
                 loop_110: do k = 1, m
                    if( k<knext )cycle loop_110
                    if( k==m ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k+1, k )/=zero ) then
                          k1 = k
                          k2 = k + 1_ilp
                          knext = k + 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k + 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k1, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l1 ), 1_ilp )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( l1-1, c( k2, 1_ilp ), ldc, b( 1_ilp, l2 ), 1_ilp )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_dlasy2( .true., .false., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, &
                                 l1 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_110
              end do loop_120
           else if( .not.notrna .and. .not.notrnb ) then
              ! solve    a**t*x + isgn*x*b**t = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! top-right corner column by column by
                 ! a(k,k)**t*x(k,l) + isgn*x(k,l)*b(l,l)**t = c(k,l) - r(k,l)
              ! where
                           ! k-1                            n
                  ! r(k,l) = sum [a(i,k)**t*x(i,l)] + isgn*sum [x(k,j)*b(l,j)**t].
                           ! i=1                          j=l+1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = n
              loop_180: do l = n, 1, -1
                 if( l>lnext )cycle loop_180
                 if( l==1_ilp ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l, l-1 )/=zero ) then
                       l1 = l - 1_ilp
                       l2 = l
                       lnext = l - 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l - 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = 1_ilp
                 loop_170: do k = 1, m
                    if( k<knext )cycle loop_170
                    if( k==m ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k+1, k )/=zero ) then
                          k1 = k
                          k2 = k + 1_ilp
                          knext = k + 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k + 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l1, c( k1, min( l1+1, n ) ), ldc,b( l1, min( l1+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k1 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( k1-1, a( 1_ilp, k2 ), 1_ilp, c( 1_ilp, l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_dlasy2( .true., .true., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, l1 &
                                 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_170
              end do loop_180
           else if( notrna .and. .not.notrnb ) then
              ! solve    a*x + isgn*x*b**t = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-right corner column by column by
                  ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l)**t = c(k,l) - r(k,l)
              ! where
                            ! m                          n
                  ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b(l,j)**t].
                          ! i=k+1                      j=l+1
              ! start column loop (index = l)
              ! l1 (l2): column index of the first (last) row of x(k,l)
              lnext = n
              loop_240: do l = n, 1, -1
                 if( l>lnext )cycle loop_240
                 if( l==1_ilp ) then
                    l1 = l
                    l2 = l
                 else
                    if( b( l, l-1 )/=zero ) then
                       l1 = l - 1_ilp
                       l2 = l
                       lnext = l - 2_ilp
                    else
                       l1 = l
                       l2 = l
                       lnext = l - 1_ilp
                    end if
                 end if
                 ! start row loop (index = k)
                 ! k1 (k2): row index of the first (last) row of x(k,l)
                 knext = m
                 loop_230: do k = m, 1, -1
                    if( k>knext )cycle loop_230
                    if( k==1_ilp ) then
                       k1 = k
                       k2 = k
                    else
                       if( a( k, k-1 )/=zero ) then
                          k1 = k - 1_ilp
                          k2 = k
                          knext = k - 2_ilp
                       else
                          k1 = k
                          k2 = k
                          knext = k - 1_ilp
                       end if
                    end if
                    if( l1==l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l1, c( k1, min( l1+1, n ) ), ldc,b( l1, min( l1+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       scaloc = one
                       a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                       da11 = abs( a11 )
                       if( da11<=smin ) then
                          a11 = smin
                          da11 = smin
                          info = 1_ilp
                       end if
                       db = abs( vec( 1_ilp, 1_ilp ) )
                       if( da11<one .and. db>one ) then
                          if( db>bignum*da11 )scaloc = one / db
                       end if
                       x( 1_ilp, 1_ilp ) = ( vec( 1_ilp, 1_ilp )*scaloc ) / a11
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                    else if( l1==l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one, a( k1, k1 ),lda, one, one, &
                                 vec, 2_ilp, -sgn*b( l1, l1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1==k2 ) then
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
                       suml = stdlib_ddot( m-k1, a( k1, min( k1+1, m ) ), lda,c( min( k1+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
                       call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one, b( l1, l1 ),ldb, one, one, &
                                 vec, 2_ilp, -sgn*a( k1, k1 ),zero, x, 2_ilp, scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 2_ilp, 1_ilp )
                    else if( l1/=l2 .and. k1/=k2 ) then
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 1_ilp ) = c( k1, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k1, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k1, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 1_ilp, 2_ilp ) = c( k1, l2 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l1 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l1, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 1_ilp ) = c( k2, l1 ) - ( suml+sgn*sumr )
                       suml = stdlib_ddot( m-k2, a( k2, min( k2+1, m ) ), lda,c( min( k2+1, m ), &
                                 l2 ), 1_ilp )
                       sumr = stdlib_ddot( n-l2, c( k2, min( l2+1, n ) ), ldc,b( l2, min( l2+1, n &
                                 ) ), ldb )
                       vec( 2_ilp, 2_ilp ) = c( k2, l2 ) - ( suml+sgn*sumr )
                       call stdlib_dlasy2( .false., .true., isgn, 2_ilp, 2_ilp, a( k1, k1 ),lda, b( l1, &
                                 l1 ), ldb, vec, 2_ilp, scaloc, x,2_ilp, xnorm, ierr )
                       if( ierr/=0_ilp )info = 1_ilp
                       if( scaloc/=one ) then
                          do j = 1, n
                             call stdlib_dscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                          end do
                          scale = scale*scaloc
                       end if
                       c( k1, l1 ) = x( 1_ilp, 1_ilp )
                       c( k1, l2 ) = x( 1_ilp, 2_ilp )
                       c( k2, l1 ) = x( 2_ilp, 1_ilp )
                       c( k2, l2 ) = x( 2_ilp, 2_ilp )
                    end if
                 end do loop_230
              end do loop_240
           end if
           return
     end subroutine stdlib_dtrsyl


     module subroutine stdlib_ctrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
     !! CTRSYL solves the complex Sylvester matrix equation:
     !! op(A)*X + X*op(B) = scale*C or
     !! op(A)*X - X*op(B) = scale*C,
     !! where op(A) = A or A**H, and A and B are both upper triangular. A is
     !! M-by-M and B is N-by-N; the right hand side C and the solution X are
     !! M-by-N; and scale is an output scale factor, set <= 1 to avoid
     !! overflow in X.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notrna, notrnb
           integer(ilp) :: j, k, l
           real(sp) :: bignum, da11, db, eps, scaloc, sgn, smin, smlnum
           complex(sp) :: a11, suml, sumr, vec, x11
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           notrna = stdlib_lsame( trana, 'N' )
           notrnb = stdlib_lsame( tranb, 'N' )
           info = 0_ilp
           if( .not.notrna .and. .not.stdlib_lsame( trana, 'C' ) ) then
              info = -1_ilp
           else if( .not.notrnb .and. .not.stdlib_lsame( tranb, 'C' ) ) then
              info = -2_ilp
           else if( isgn/=1_ilp .and. isgn/=-1_ilp ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRSYL', -info )
              return
           end if
           ! quick return if possible
           scale = one
           if( m==0 .or. n==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = smlnum*real( m*n,KIND=sp) / eps
           bignum = one / smlnum
           smin = max( smlnum, eps*stdlib_clange( 'M', m, m, a, lda, dum ),eps*stdlib_clange( 'M',&
                      n, n, b, ldb, dum ) )
           sgn = isgn
           if( notrna .and. notrnb ) then
              ! solve    a*x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
                  ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                          ! m                        l-1
                ! r(k,l) = sum [a(k,i)*x(i,l)] +isgn*sum [x(k,j)*b(j,l)].
                        ! i=k+1                      j=1
              loop_30: do l = 1, n
                 do k = m, 1, -1
                    suml = stdlib_cdotu( m-k, a( k, min( k+1, m ) ), lda,c( min( k+1, m ), l ), 1_ilp &
                              )
                    sumr = stdlib_cdotu( l-1, c( k, 1_ilp ), ldc, b( 1_ilp, l ), 1_ilp )
                    vec = c( k, l ) - ( suml+sgn*sumr )
                    scaloc = one
                    a11 = a( k, k ) + sgn*b( l, l )
                    da11 = abs( real( a11,KIND=sp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=sp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_cladiv( vec*cmplx( scaloc,KIND=sp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_csscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_30
           else if( .not.notrna .and. notrnb ) then
              ! solve    a**h *x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! upper-left corner column by column by
                  ! a**h(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                         ! k-1                           l-1
                ! r(k,l) = sum [a**h(i,k)*x(i,l)] + isgn*sum [x(k,j)*b(j,l)]
                         ! i=1                           j=1
              loop_60: do l = 1, n
                 do k = 1, m
                    suml = stdlib_cdotc( k-1, a( 1_ilp, k ), 1_ilp, c( 1_ilp, l ), 1_ilp )
                    sumr = stdlib_cdotu( l-1, c( k, 1_ilp ), ldc, b( 1_ilp, l ), 1_ilp )
                    vec = c( k, l ) - ( suml+sgn*sumr )
                    scaloc = one
                    a11 = conjg( a( k, k ) ) + sgn*b( l, l )
                    da11 = abs( real( a11,KIND=sp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=sp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_cladiv( vec*cmplx( scaloc,KIND=sp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_csscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_60
           else if( .not.notrna .and. .not.notrnb ) then
              ! solve    a**h*x + isgn*x*b**h = c.
              ! the (k,l)th block of x is determined starting from
              ! upper-right corner column by column by
                  ! a**h(k,k)*x(k,l) + isgn*x(k,l)*b**h(l,l) = c(k,l) - r(k,l)
              ! where
                          ! k-1
                 ! r(k,l) = sum [a**h(i,k)*x(i,l)] +
                          ! i=1
                                 ! n
                           ! isgn*sum [x(k,j)*b**h(l,j)].
                                ! j=l+1
              loop_90: do l = n, 1, -1
                 do k = 1, m
                    suml = stdlib_cdotc( k-1, a( 1_ilp, k ), 1_ilp, c( 1_ilp, l ), 1_ilp )
                    sumr = stdlib_cdotc( n-l, c( k, min( l+1, n ) ), ldc,b( l, min( l+1, n ) ), &
                              ldb )
                    vec = c( k, l ) - ( suml+sgn*conjg( sumr ) )
                    scaloc = one
                    a11 = conjg( a( k, k )+sgn*b( l, l ) )
                    da11 = abs( real( a11,KIND=sp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=sp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_cladiv( vec*cmplx( scaloc,KIND=sp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_csscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_90
           else if( notrna .and. .not.notrnb ) then
              ! solve    a*x + isgn*x*b**h = c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
                 ! a(k,k)*x(k,l) + isgn*x(k,l)*b**h(l,l) = c(k,l) - r(k,l)
              ! where
                          ! m                          n
                ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b**h(l,j)]
                        ! i=k+1                      j=l+1
              loop_120: do l = n, 1, -1
                 do k = m, 1, -1
                    suml = stdlib_cdotu( m-k, a( k, min( k+1, m ) ), lda,c( min( k+1, m ), l ), 1_ilp &
                              )
                    sumr = stdlib_cdotc( n-l, c( k, min( l+1, n ) ), ldc,b( l, min( l+1, n ) ), &
                              ldb )
                    vec = c( k, l ) - ( suml+sgn*conjg( sumr ) )
                    scaloc = one
                    a11 = a( k, k ) + sgn*conjg( b( l, l ) )
                    da11 = abs( real( a11,KIND=sp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=sp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_cladiv( vec*cmplx( scaloc,KIND=sp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_csscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_120
           end if
           return
     end subroutine stdlib_ctrsyl

     module subroutine stdlib_ztrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
     !! ZTRSYL solves the complex Sylvester matrix equation:
     !! op(A)*X + X*op(B) = scale*C or
     !! op(A)*X - X*op(B) = scale*C,
     !! where op(A) = A or A**H, and A and B are both upper triangular. A is
     !! M-by-M and B is N-by-N; the right hand side C and the solution X are
     !! M-by-N; and scale is an output scale factor, set <= 1 to avoid
     !! overflow in X.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notrna, notrnb
           integer(ilp) :: j, k, l
           real(dp) :: bignum, da11, db, eps, scaloc, sgn, smin, smlnum
           complex(dp) :: a11, suml, sumr, vec, x11
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test input parameters
           notrna = stdlib_lsame( trana, 'N' )
           notrnb = stdlib_lsame( tranb, 'N' )
           info = 0_ilp
           if( .not.notrna .and. .not.stdlib_lsame( trana, 'C' ) ) then
              info = -1_ilp
           else if( .not.notrnb .and. .not.stdlib_lsame( tranb, 'C' ) ) then
              info = -2_ilp
           else if( isgn/=1_ilp .and. isgn/=-1_ilp ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRSYL', -info )
              return
           end if
           ! quick return if possible
           scale = one
           if( m==0 .or. n==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = smlnum*real( m*n,KIND=dp) / eps
           bignum = one / smlnum
           smin = max( smlnum, eps*stdlib_zlange( 'M', m, m, a, lda, dum ),eps*stdlib_zlange( 'M',&
                      n, n, b, ldb, dum ) )
           sgn = isgn
           if( notrna .and. notrnb ) then
              ! solve    a*x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
                  ! a(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                          ! m                        l-1
                ! r(k,l) = sum [a(k,i)*x(i,l)] +isgn*sum [x(k,j)*b(j,l)].
                        ! i=k+1                      j=1
              loop_30: do l = 1, n
                 do k = m, 1, -1
                    suml = stdlib_zdotu( m-k, a( k, min( k+1, m ) ), lda,c( min( k+1, m ), l ), 1_ilp &
                              )
                    sumr = stdlib_zdotu( l-1, c( k, 1_ilp ), ldc, b( 1_ilp, l ), 1_ilp )
                    vec = c( k, l ) - ( suml+sgn*sumr )
                    scaloc = one
                    a11 = a( k, k ) + sgn*b( l, l )
                    da11 = abs( real( a11,KIND=dp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=dp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_zladiv( vec*cmplx( scaloc,KIND=dp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_zdscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_30
           else if( .not.notrna .and. notrnb ) then
              ! solve    a**h *x + isgn*x*b = scale*c.
              ! the (k,l)th block of x is determined starting from
              ! upper-left corner column by column by
                  ! a**h(k,k)*x(k,l) + isgn*x(k,l)*b(l,l) = c(k,l) - r(k,l)
              ! where
                         ! k-1                           l-1
                ! r(k,l) = sum [a**h(i,k)*x(i,l)] + isgn*sum [x(k,j)*b(j,l)]
                         ! i=1                           j=1
              loop_60: do l = 1, n
                 do k = 1, m
                    suml = stdlib_zdotc( k-1, a( 1_ilp, k ), 1_ilp, c( 1_ilp, l ), 1_ilp )
                    sumr = stdlib_zdotu( l-1, c( k, 1_ilp ), ldc, b( 1_ilp, l ), 1_ilp )
                    vec = c( k, l ) - ( suml+sgn*sumr )
                    scaloc = one
                    a11 = conjg( a( k, k ) ) + sgn*b( l, l )
                    da11 = abs( real( a11,KIND=dp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=dp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_zladiv( vec*cmplx( scaloc,KIND=dp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_zdscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_60
           else if( .not.notrna .and. .not.notrnb ) then
              ! solve    a**h*x + isgn*x*b**h = c.
              ! the (k,l)th block of x is determined starting from
              ! upper-right corner column by column by
                  ! a**h(k,k)*x(k,l) + isgn*x(k,l)*b**h(l,l) = c(k,l) - r(k,l)
              ! where
                          ! k-1
                 ! r(k,l) = sum [a**h(i,k)*x(i,l)] +
                          ! i=1
                                 ! n
                           ! isgn*sum [x(k,j)*b**h(l,j)].
                                ! j=l+1
              loop_90: do l = n, 1, -1
                 do k = 1, m
                    suml = stdlib_zdotc( k-1, a( 1_ilp, k ), 1_ilp, c( 1_ilp, l ), 1_ilp )
                    sumr = stdlib_zdotc( n-l, c( k, min( l+1, n ) ), ldc,b( l, min( l+1, n ) ), &
                              ldb )
                    vec = c( k, l ) - ( suml+sgn*conjg( sumr ) )
                    scaloc = one
                    a11 = conjg( a( k, k )+sgn*b( l, l ) )
                    da11 = abs( real( a11,KIND=dp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=dp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_zladiv( vec*cmplx( scaloc,KIND=dp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_zdscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_90
           else if( notrna .and. .not.notrnb ) then
              ! solve    a*x + isgn*x*b**h = c.
              ! the (k,l)th block of x is determined starting from
              ! bottom-left corner column by column by
                 ! a(k,k)*x(k,l) + isgn*x(k,l)*b**h(l,l) = c(k,l) - r(k,l)
              ! where
                          ! m                          n
                ! r(k,l) = sum [a(k,i)*x(i,l)] + isgn*sum [x(k,j)*b**h(l,j)]
                        ! i=k+1                      j=l+1
              loop_120: do l = n, 1, -1
                 do k = m, 1, -1
                    suml = stdlib_zdotu( m-k, a( k, min( k+1, m ) ), lda,c( min( k+1, m ), l ), 1_ilp &
                              )
                    sumr = stdlib_zdotc( n-l, c( k, min( l+1, n ) ), ldc,b( l, min( l+1, n ) ), &
                              ldb )
                    vec = c( k, l ) - ( suml+sgn*conjg( sumr ) )
                    scaloc = one
                    a11 = a( k, k ) + sgn*conjg( b( l, l ) )
                    da11 = abs( real( a11,KIND=dp) ) + abs( aimag( a11 ) )
                    if( da11<=smin ) then
                       a11 = smin
                       da11 = smin
                       info = 1_ilp
                    end if
                    db = abs( real( vec,KIND=dp) ) + abs( aimag( vec ) )
                    if( da11<one .and. db>one ) then
                       if( db>bignum*da11 )scaloc = one / db
                    end if
                    x11 = stdlib_zladiv( vec*cmplx( scaloc,KIND=dp), a11 )
                    if( scaloc/=one ) then
                       do j = 1, n
                          call stdlib_zdscal( m, scaloc, c( 1_ilp, j ), 1_ilp )
                       end do
                       scale = scale*scaloc
                    end if
                    c( k, l ) = x11
                 end do
              end do loop_120
           end if
           return
     end subroutine stdlib_ztrsyl




     pure module subroutine stdlib_slasy2( ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr,ldtr, b, ldb, &
     !! SLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in
     !! op(TL)*X + ISGN*X*op(TR) = SCALE*B,
     !! where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
     !! -1.  op(T) = T or T**T, where T**T denotes the transpose of T.
               scale, x, ldx, xnorm, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ltranl, ltranr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, ldb, ldtl, ldtr, ldx, n1, n2
           real(sp), intent(out) :: scale, xnorm
           ! Array Arguments 
           real(sp), intent(in) :: b(ldb,*), tl(ldtl,*), tr(ldtr,*)
           real(sp), intent(out) :: x(ldx,*)
       ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: bswap, xswap
           integer(ilp) :: i, ip, ipiv, ipsv, j, jp, jpsv, k
           real(sp) :: bet, eps, gam, l21, sgn, smin, smlnum, tau1, temp, u11, u12, u22, &
                     xmax
           ! Local Arrays 
           logical(lk) :: bswpiv(4_ilp), xswpiv(4_ilp)
           integer(ilp) :: jpiv(4_ilp), locl21(4_ilp), locu12(4_ilp), locu22(4_ilp)
           real(sp) :: btmp(4_ilp), t16(4_ilp,4_ilp), tmp(4_ilp), x2(2_ilp)
           ! Intrinsic Functions 
           ! Data Statements 
           locu12 = [3_ilp,4_ilp,1_ilp,2_ilp]
           locl21 = [2_ilp,1_ilp,4_ilp,3_ilp]
           locu22 = [4_ilp,3_ilp,2_ilp,1_ilp]
           xswpiv = [.false.,.false.,.true.,.true.]
           bswpiv = [.false.,.true.,.false.,.true.]
           ! Executable Statements 
           ! do not check the input parameters for errors
           info = 0_ilp
           ! quick return if possible
           if( n1==0 .or. n2==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           sgn = isgn
           k = n1 + n1 + n2 - 2_ilp
           go to ( 10, 20, 30, 50 )k
           ! 1 by 1: tl11*x + sgn*x*tr11 = b11
           10 continue
           tau1 = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           bet = abs( tau1 )
           if( bet<=smlnum ) then
              tau1 = smlnum
              bet = smlnum
              info = 1_ilp
           end if
           scale = one
           gam = abs( b( 1_ilp, 1_ilp ) )
           if( smlnum*gam>bet )scale = one / gam
           x( 1_ilp, 1_ilp ) = ( b( 1_ilp, 1_ilp )*scale ) / tau1
           xnorm = abs( x( 1_ilp, 1_ilp ) )
           return
           ! 1 by 2:
           ! tl11*[x11 x12] + isgn*[x11 x12]*op[tr11 tr12]  = [b11 b12]
                                             ! [tr21 tr22]
                                             20 continue
           smin = max( eps*max( abs( tl( 1_ilp, 1_ilp ) ), abs( tr( 1_ilp, 1_ilp ) ),abs( tr( 1_ilp, 2_ilp ) ), abs( tr( &
                     2_ilp, 1_ilp ) ), abs( tr( 2_ilp, 2_ilp ) ) ),smlnum )
           tmp( 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           tmp( 4_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           if( ltranr ) then
              tmp( 2_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              tmp( 3_ilp ) = sgn*tr( 1_ilp, 2_ilp )
           else
              tmp( 2_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              tmp( 3_ilp ) = sgn*tr( 2_ilp, 1_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 1_ilp, 2_ilp )
           go to 40
           ! 2 by 1:
                ! op[tl11 tl12]*[x11] + isgn* [x11]*tr11  = [b11]
                  ! [tl21 tl22] [x21]         [x21]         [b21]
                  30 continue
           smin = max( eps*max( abs( tr( 1_ilp, 1_ilp ) ), abs( tl( 1_ilp, 1_ilp ) ),abs( tl( 1_ilp, 2_ilp ) ), abs( tl( &
                     2_ilp, 1_ilp ) ), abs( tl( 2_ilp, 2_ilp ) ) ),smlnum )
           tmp( 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           tmp( 4_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           if( ltranl ) then
              tmp( 2_ilp ) = tl( 1_ilp, 2_ilp )
              tmp( 3_ilp ) = tl( 2_ilp, 1_ilp )
           else
              tmp( 2_ilp ) = tl( 2_ilp, 1_ilp )
              tmp( 3_ilp ) = tl( 1_ilp, 2_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 2_ilp, 1_ilp )
           40 continue
           ! solve 2 by 2 system using complete pivoting.
           ! set pivots less than smin to smin.
           ipiv = stdlib_isamax( 4_ilp, tmp, 1_ilp )
           u11 = tmp( ipiv )
           if( abs( u11 )<=smin ) then
              info = 1_ilp
              u11 = smin
           end if
           u12 = tmp( locu12( ipiv ) )
           l21 = tmp( locl21( ipiv ) ) / u11
           u22 = tmp( locu22( ipiv ) ) - u12*l21
           xswap = xswpiv( ipiv )
           bswap = bswpiv( ipiv )
           if( abs( u22 )<=smin ) then
              info = 1_ilp
              u22 = smin
           end if
           if( bswap ) then
              temp = btmp( 2_ilp )
              btmp( 2_ilp ) = btmp( 1_ilp ) - l21*temp
              btmp( 1_ilp ) = temp
           else
              btmp( 2_ilp ) = btmp( 2_ilp ) - l21*btmp( 1_ilp )
           end if
           scale = one
           if( ( two*smlnum )*abs( btmp( 2_ilp ) )>abs( u22 ) .or.( two*smlnum )*abs( btmp( 1_ilp ) )>abs(&
                      u11 ) ) then
              scale = half / max( abs( btmp( 1_ilp ) ), abs( btmp( 2_ilp ) ) )
              btmp( 1_ilp ) = btmp( 1_ilp )*scale
              btmp( 2_ilp ) = btmp( 2_ilp )*scale
           end if
           x2( 2_ilp ) = btmp( 2_ilp ) / u22
           x2( 1_ilp ) = btmp( 1_ilp ) / u11 - ( u12 / u11 )*x2( 2_ilp )
           if( xswap ) then
              temp = x2( 2_ilp )
              x2( 2_ilp ) = x2( 1_ilp )
              x2( 1_ilp ) = temp
           end if
           x( 1_ilp, 1_ilp ) = x2( 1_ilp )
           if( n1==1_ilp ) then
              x( 1_ilp, 2_ilp ) = x2( 2_ilp )
              xnorm = abs( x( 1_ilp, 1_ilp ) ) + abs( x( 1_ilp, 2_ilp ) )
           else
              x( 2_ilp, 1_ilp ) = x2( 2_ilp )
              xnorm = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 2_ilp, 1_ilp ) ) )
           end if
           return
           ! 2 by 2:
           ! op[tl11 tl12]*[x11 x12] +isgn* [x11 x12]*op[tr11 tr12] = [b11 b12]
             ! [tl21 tl22] [x21 x22]        [x21 x22]   [tr21 tr22]   [b21 b22]
           ! solve equivalent 4 by 4 system using complete pivoting.
           ! set pivots less than smin to smin.
           50 continue
           smin = max( abs( tr( 1_ilp, 1_ilp ) ), abs( tr( 1_ilp, 2_ilp ) ),abs( tr( 2_ilp, 1_ilp ) ), abs( tr( 2_ilp, 2_ilp ) ) )
                     
           smin = max( smin, abs( tl( 1_ilp, 1_ilp ) ), abs( tl( 1_ilp, 2_ilp ) ),abs( tl( 2_ilp, 1_ilp ) ), abs( tl( 2_ilp, &
                     2_ilp ) ) )
           smin = max( eps*smin, smlnum )
           btmp( 1_ilp ) = zero
           call stdlib_scopy( 16_ilp, btmp, 0_ilp, t16, 1_ilp )
           t16( 1_ilp, 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           t16( 2_ilp, 2_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           t16( 3_ilp, 3_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           t16( 4_ilp, 4_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           if( ltranl ) then
              t16( 1_ilp, 2_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 2_ilp, 1_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 3_ilp, 4_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 4_ilp, 3_ilp ) = tl( 1_ilp, 2_ilp )
           else
              t16( 1_ilp, 2_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 2_ilp, 1_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 3_ilp, 4_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 4_ilp, 3_ilp ) = tl( 2_ilp, 1_ilp )
           end if
           if( ltranr ) then
              t16( 1_ilp, 3_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 2_ilp, 4_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 3_ilp, 1_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 4_ilp, 2_ilp ) = sgn*tr( 2_ilp, 1_ilp )
           else
              t16( 1_ilp, 3_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 2_ilp, 4_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 3_ilp, 1_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 4_ilp, 2_ilp ) = sgn*tr( 1_ilp, 2_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 2_ilp, 1_ilp )
           btmp( 3_ilp ) = b( 1_ilp, 2_ilp )
           btmp( 4_ilp ) = b( 2_ilp, 2_ilp )
           ! perform elimination
           loop_100: do i = 1, 3
              xmax = zero
              do ip = i, 4
                 do jp = i, 4
                    if( abs( t16( ip, jp ) )>=xmax ) then
                       xmax = abs( t16( ip, jp ) )
                       ipsv = ip
                       jpsv = jp
                    end if
                 end do
              end do
              if( ipsv/=i ) then
                 call stdlib_sswap( 4_ilp, t16( ipsv, 1_ilp ), 4_ilp, t16( i, 1_ilp ), 4_ilp )
                 temp = btmp( i )
                 btmp( i ) = btmp( ipsv )
                 btmp( ipsv ) = temp
              end if
              if( jpsv/=i )call stdlib_sswap( 4_ilp, t16( 1_ilp, jpsv ), 1_ilp, t16( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpsv
              if( abs( t16( i, i ) )<smin ) then
                 info = 1_ilp
                 t16( i, i ) = smin
              end if
              do j = i + 1, 4
                 t16( j, i ) = t16( j, i ) / t16( i, i )
                 btmp( j ) = btmp( j ) - t16( j, i )*btmp( i )
                 do k = i + 1, 4
                    t16( j, k ) = t16( j, k ) - t16( j, i )*t16( i, k )
                 end do
              end do
           end do loop_100
           if( abs( t16( 4_ilp, 4_ilp ) )<smin ) then
              info = 1_ilp
              t16( 4_ilp, 4_ilp ) = smin
           end if
           scale = one
           if( ( eight*smlnum )*abs( btmp( 1_ilp ) )>abs( t16( 1_ilp, 1_ilp ) ) .or.( eight*smlnum )*abs( &
           btmp( 2_ilp ) )>abs( t16( 2_ilp, 2_ilp ) ) .or.( eight*smlnum )*abs( btmp( 3_ilp ) )>abs( t16( 3_ilp, 3_ilp ) )&
                      .or.( eight*smlnum )*abs( btmp( 4_ilp ) )>abs( t16( 4_ilp, 4_ilp ) ) ) then
              scale = ( one / eight ) / max( abs( btmp( 1_ilp ) ),abs( btmp( 2_ilp ) ), abs( btmp( 3_ilp ) ), &
                        abs( btmp( 4_ilp ) ) )
              btmp( 1_ilp ) = btmp( 1_ilp )*scale
              btmp( 2_ilp ) = btmp( 2_ilp )*scale
              btmp( 3_ilp ) = btmp( 3_ilp )*scale
              btmp( 4_ilp ) = btmp( 4_ilp )*scale
           end if
           do i = 1, 4
              k = 5_ilp - i
              temp = one / t16( k, k )
              tmp( k ) = btmp( k )*temp
              do j = k + 1, 4
                 tmp( k ) = tmp( k ) - ( temp*t16( k, j ) )*tmp( j )
              end do
           end do
           do i = 1, 3
              if( jpiv( 4_ilp-i )/=4_ilp-i ) then
                 temp = tmp( 4_ilp-i )
                 tmp( 4_ilp-i ) = tmp( jpiv( 4_ilp-i ) )
                 tmp( jpiv( 4_ilp-i ) ) = temp
              end if
           end do
           x( 1_ilp, 1_ilp ) = tmp( 1_ilp )
           x( 2_ilp, 1_ilp ) = tmp( 2_ilp )
           x( 1_ilp, 2_ilp ) = tmp( 3_ilp )
           x( 2_ilp, 2_ilp ) = tmp( 4_ilp )
           xnorm = max( abs( tmp( 1_ilp ) )+abs( tmp( 3_ilp ) ),abs( tmp( 2_ilp ) )+abs( tmp( 4_ilp ) ) )
           return
     end subroutine stdlib_slasy2

     pure module subroutine stdlib_dlasy2( ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr,ldtr, b, ldb, &
     !! DLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in
     !! op(TL)*X + ISGN*X*op(TR) = SCALE*B,
     !! where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
     !! -1.  op(T) = T or T**T, where T**T denotes the transpose of T.
               scale, x, ldx, xnorm, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: ltranl, ltranr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, ldb, ldtl, ldtr, ldx, n1, n2
           real(dp), intent(out) :: scale, xnorm
           ! Array Arguments 
           real(dp), intent(in) :: b(ldb,*), tl(ldtl,*), tr(ldtr,*)
           real(dp), intent(out) :: x(ldx,*)
       ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: bswap, xswap
           integer(ilp) :: i, ip, ipiv, ipsv, j, jp, jpsv, k
           real(dp) :: bet, eps, gam, l21, sgn, smin, smlnum, tau1, temp, u11, u12, u22, &
                     xmax
           ! Local Arrays 
           logical(lk) :: bswpiv(4_ilp), xswpiv(4_ilp)
           integer(ilp) :: jpiv(4_ilp), locl21(4_ilp), locu12(4_ilp), locu22(4_ilp)
           real(dp) :: btmp(4_ilp), t16(4_ilp,4_ilp), tmp(4_ilp), x2(2_ilp)
           ! Intrinsic Functions 
           ! Data Statements 
           locu12 = [3_ilp,4_ilp,1_ilp,2_ilp]
           locl21 = [2_ilp,1_ilp,4_ilp,3_ilp]
           locu22 = [4_ilp,3_ilp,2_ilp,1_ilp]
           xswpiv = [.false.,.false.,.true.,.true.]
           bswpiv = [.false.,.true.,.false.,.true.]
           ! Executable Statements 
           ! do not check the input parameters for errors
           info = 0_ilp
           ! quick return if possible
           if( n1==0 .or. n2==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           sgn = isgn
           k = n1 + n1 + n2 - 2_ilp
           go to ( 10, 20, 30, 50 )k
           ! 1 by 1: tl11*x + sgn*x*tr11 = b11
           10 continue
           tau1 = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           bet = abs( tau1 )
           if( bet<=smlnum ) then
              tau1 = smlnum
              bet = smlnum
              info = 1_ilp
           end if
           scale = one
           gam = abs( b( 1_ilp, 1_ilp ) )
           if( smlnum*gam>bet )scale = one / gam
           x( 1_ilp, 1_ilp ) = ( b( 1_ilp, 1_ilp )*scale ) / tau1
           xnorm = abs( x( 1_ilp, 1_ilp ) )
           return
           ! 1 by 2:
           ! tl11*[x11 x12] + isgn*[x11 x12]*op[tr11 tr12]  = [b11 b12]
                                             ! [tr21 tr22]
                                             20 continue
           smin = max( eps*max( abs( tl( 1_ilp, 1_ilp ) ), abs( tr( 1_ilp, 1_ilp ) ),abs( tr( 1_ilp, 2_ilp ) ), abs( tr( &
                     2_ilp, 1_ilp ) ), abs( tr( 2_ilp, 2_ilp ) ) ),smlnum )
           tmp( 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           tmp( 4_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           if( ltranr ) then
              tmp( 2_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              tmp( 3_ilp ) = sgn*tr( 1_ilp, 2_ilp )
           else
              tmp( 2_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              tmp( 3_ilp ) = sgn*tr( 2_ilp, 1_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 1_ilp, 2_ilp )
           go to 40
           ! 2 by 1:
                ! op[tl11 tl12]*[x11] + isgn* [x11]*tr11  = [b11]
                  ! [tl21 tl22] [x21]         [x21]         [b21]
                  30 continue
           smin = max( eps*max( abs( tr( 1_ilp, 1_ilp ) ), abs( tl( 1_ilp, 1_ilp ) ),abs( tl( 1_ilp, 2_ilp ) ), abs( tl( &
                     2_ilp, 1_ilp ) ), abs( tl( 2_ilp, 2_ilp ) ) ),smlnum )
           tmp( 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           tmp( 4_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           if( ltranl ) then
              tmp( 2_ilp ) = tl( 1_ilp, 2_ilp )
              tmp( 3_ilp ) = tl( 2_ilp, 1_ilp )
           else
              tmp( 2_ilp ) = tl( 2_ilp, 1_ilp )
              tmp( 3_ilp ) = tl( 1_ilp, 2_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 2_ilp, 1_ilp )
           40 continue
           ! solve 2 by 2 system using complete pivoting.
           ! set pivots less than smin to smin.
           ipiv = stdlib_idamax( 4_ilp, tmp, 1_ilp )
           u11 = tmp( ipiv )
           if( abs( u11 )<=smin ) then
              info = 1_ilp
              u11 = smin
           end if
           u12 = tmp( locu12( ipiv ) )
           l21 = tmp( locl21( ipiv ) ) / u11
           u22 = tmp( locu22( ipiv ) ) - u12*l21
           xswap = xswpiv( ipiv )
           bswap = bswpiv( ipiv )
           if( abs( u22 )<=smin ) then
              info = 1_ilp
              u22 = smin
           end if
           if( bswap ) then
              temp = btmp( 2_ilp )
              btmp( 2_ilp ) = btmp( 1_ilp ) - l21*temp
              btmp( 1_ilp ) = temp
           else
              btmp( 2_ilp ) = btmp( 2_ilp ) - l21*btmp( 1_ilp )
           end if
           scale = one
           if( ( two*smlnum )*abs( btmp( 2_ilp ) )>abs( u22 ) .or.( two*smlnum )*abs( btmp( 1_ilp ) )>abs(&
                      u11 ) ) then
              scale = half / max( abs( btmp( 1_ilp ) ), abs( btmp( 2_ilp ) ) )
              btmp( 1_ilp ) = btmp( 1_ilp )*scale
              btmp( 2_ilp ) = btmp( 2_ilp )*scale
           end if
           x2( 2_ilp ) = btmp( 2_ilp ) / u22
           x2( 1_ilp ) = btmp( 1_ilp ) / u11 - ( u12 / u11 )*x2( 2_ilp )
           if( xswap ) then
              temp = x2( 2_ilp )
              x2( 2_ilp ) = x2( 1_ilp )
              x2( 1_ilp ) = temp
           end if
           x( 1_ilp, 1_ilp ) = x2( 1_ilp )
           if( n1==1_ilp ) then
              x( 1_ilp, 2_ilp ) = x2( 2_ilp )
              xnorm = abs( x( 1_ilp, 1_ilp ) ) + abs( x( 1_ilp, 2_ilp ) )
           else
              x( 2_ilp, 1_ilp ) = x2( 2_ilp )
              xnorm = max( abs( x( 1_ilp, 1_ilp ) ), abs( x( 2_ilp, 1_ilp ) ) )
           end if
           return
           ! 2 by 2:
           ! op[tl11 tl12]*[x11 x12] +isgn* [x11 x12]*op[tr11 tr12] = [b11 b12]
             ! [tl21 tl22] [x21 x22]        [x21 x22]   [tr21 tr22]   [b21 b22]
           ! solve equivalent 4 by 4 system using complete pivoting.
           ! set pivots less than smin to smin.
           50 continue
           smin = max( abs( tr( 1_ilp, 1_ilp ) ), abs( tr( 1_ilp, 2_ilp ) ),abs( tr( 2_ilp, 1_ilp ) ), abs( tr( 2_ilp, 2_ilp ) ) )
                     
           smin = max( smin, abs( tl( 1_ilp, 1_ilp ) ), abs( tl( 1_ilp, 2_ilp ) ),abs( tl( 2_ilp, 1_ilp ) ), abs( tl( 2_ilp, &
                     2_ilp ) ) )
           smin = max( eps*smin, smlnum )
           btmp( 1_ilp ) = zero
           call stdlib_dcopy( 16_ilp, btmp, 0_ilp, t16, 1_ilp )
           t16( 1_ilp, 1_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           t16( 2_ilp, 2_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 1_ilp, 1_ilp )
           t16( 3_ilp, 3_ilp ) = tl( 1_ilp, 1_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           t16( 4_ilp, 4_ilp ) = tl( 2_ilp, 2_ilp ) + sgn*tr( 2_ilp, 2_ilp )
           if( ltranl ) then
              t16( 1_ilp, 2_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 2_ilp, 1_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 3_ilp, 4_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 4_ilp, 3_ilp ) = tl( 1_ilp, 2_ilp )
           else
              t16( 1_ilp, 2_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 2_ilp, 1_ilp ) = tl( 2_ilp, 1_ilp )
              t16( 3_ilp, 4_ilp ) = tl( 1_ilp, 2_ilp )
              t16( 4_ilp, 3_ilp ) = tl( 2_ilp, 1_ilp )
           end if
           if( ltranr ) then
              t16( 1_ilp, 3_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 2_ilp, 4_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 3_ilp, 1_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 4_ilp, 2_ilp ) = sgn*tr( 2_ilp, 1_ilp )
           else
              t16( 1_ilp, 3_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 2_ilp, 4_ilp ) = sgn*tr( 2_ilp, 1_ilp )
              t16( 3_ilp, 1_ilp ) = sgn*tr( 1_ilp, 2_ilp )
              t16( 4_ilp, 2_ilp ) = sgn*tr( 1_ilp, 2_ilp )
           end if
           btmp( 1_ilp ) = b( 1_ilp, 1_ilp )
           btmp( 2_ilp ) = b( 2_ilp, 1_ilp )
           btmp( 3_ilp ) = b( 1_ilp, 2_ilp )
           btmp( 4_ilp ) = b( 2_ilp, 2_ilp )
           ! perform elimination
           loop_100: do i = 1, 3
              xmax = zero
              do ip = i, 4
                 do jp = i, 4
                    if( abs( t16( ip, jp ) )>=xmax ) then
                       xmax = abs( t16( ip, jp ) )
                       ipsv = ip
                       jpsv = jp
                    end if
                 end do
              end do
              if( ipsv/=i ) then
                 call stdlib_dswap( 4_ilp, t16( ipsv, 1_ilp ), 4_ilp, t16( i, 1_ilp ), 4_ilp )
                 temp = btmp( i )
                 btmp( i ) = btmp( ipsv )
                 btmp( ipsv ) = temp
              end if
              if( jpsv/=i )call stdlib_dswap( 4_ilp, t16( 1_ilp, jpsv ), 1_ilp, t16( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpsv
              if( abs( t16( i, i ) )<smin ) then
                 info = 1_ilp
                 t16( i, i ) = smin
              end if
              do j = i + 1, 4
                 t16( j, i ) = t16( j, i ) / t16( i, i )
                 btmp( j ) = btmp( j ) - t16( j, i )*btmp( i )
                 do k = i + 1, 4
                    t16( j, k ) = t16( j, k ) - t16( j, i )*t16( i, k )
                 end do
              end do
           end do loop_100
           if( abs( t16( 4_ilp, 4_ilp ) )<smin ) then
              info = 1_ilp
              t16( 4_ilp, 4_ilp ) = smin
           end if
           scale = one
           if( ( eight*smlnum )*abs( btmp( 1_ilp ) )>abs( t16( 1_ilp, 1_ilp ) ) .or.( eight*smlnum )*abs( &
           btmp( 2_ilp ) )>abs( t16( 2_ilp, 2_ilp ) ) .or.( eight*smlnum )*abs( btmp( 3_ilp ) )>abs( t16( 3_ilp, 3_ilp ) )&
                      .or.( eight*smlnum )*abs( btmp( 4_ilp ) )>abs( t16( 4_ilp, 4_ilp ) ) ) then
              scale = ( one / eight ) / max( abs( btmp( 1_ilp ) ),abs( btmp( 2_ilp ) ), abs( btmp( 3_ilp ) ), &
                        abs( btmp( 4_ilp ) ) )
              btmp( 1_ilp ) = btmp( 1_ilp )*scale
              btmp( 2_ilp ) = btmp( 2_ilp )*scale
              btmp( 3_ilp ) = btmp( 3_ilp )*scale
              btmp( 4_ilp ) = btmp( 4_ilp )*scale
           end if
           do i = 1, 4
              k = 5_ilp - i
              temp = one / t16( k, k )
              tmp( k ) = btmp( k )*temp
              do j = k + 1, 4
                 tmp( k ) = tmp( k ) - ( temp*t16( k, j ) )*tmp( j )
              end do
           end do
           do i = 1, 3
              if( jpiv( 4_ilp-i )/=4_ilp-i ) then
                 temp = tmp( 4_ilp-i )
                 tmp( 4_ilp-i ) = tmp( jpiv( 4_ilp-i ) )
                 tmp( jpiv( 4_ilp-i ) ) = temp
              end if
           end do
           x( 1_ilp, 1_ilp ) = tmp( 1_ilp )
           x( 2_ilp, 1_ilp ) = tmp( 2_ilp )
           x( 1_ilp, 2_ilp ) = tmp( 3_ilp )
           x( 2_ilp, 2_ilp ) = tmp( 4_ilp )
           xnorm = max( abs( tmp( 1_ilp ) )+abs( tmp( 3_ilp ) ),abs( tmp( 2_ilp ) )+abs( tmp( 4_ilp ) ) )
           return
     end subroutine stdlib_dlasy2




     module subroutine stdlib_strsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm, m, &
     !! STRSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or right eigenvectors of a real upper
     !! quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q
     !! orthogonal).
     !! T must be in Schur canonical form (as returned by SHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               work, ldwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: s(*), sep(*), work(ldwork,*)
           real(sp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: pair, somcon, wantbh, wants, wantsp
           integer(ilp) :: i, ierr, ifst, ilst, j, k, kase, ks, n2, nn
           real(sp) :: bignum, cond, cs, delta, dumm, eps, est, lnrm, mu, prod, prod1, prod2, &
                     rnrm, scale, smlnum, sn
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           real(sp) :: dummy(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           if( .not.wants .and. .not.wantsp ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( wants .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wants .and. ldvr<n ) ) then
              info = -10_ilp
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
                          if( t( k+1, k )==zero ) then
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
              if( mm<m ) then
                 info = -13_ilp
              else if( ldwork<1_ilp .or. ( wantsp .and. ldwork<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRSNA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( somcon ) then
                 if( .not.select( 1 ) )return
              end if
              if( wants )s( 1_ilp ) = one
              if( wantsp )sep( 1_ilp ) = abs( t( 1_ilp, 1_ilp ) )
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ks = 0_ilp
           pair = .false.
           loop_60: do k = 1, n
              ! determine whether t(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_60
              else
                 if( k<n )pair = t( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_60
                 else
                    if( .not.select( k ) )cycle loop_60
                 end if
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( .not.pair ) then
                    ! real eigenvalue.
                    prod = stdlib_sdot( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    rnrm = stdlib_snrm2( n, vr( 1_ilp, ks ), 1_ilp )
                    lnrm = stdlib_snrm2( n, vl( 1_ilp, ks ), 1_ilp )
                    s( ks ) = abs( prod ) / ( rnrm*lnrm )
                 else
                    ! complex eigenvalue.
                    prod1 = stdlib_sdot( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    prod1 = prod1 + stdlib_sdot( n, vr( 1_ilp, ks+1 ), 1_ilp, vl( 1_ilp, ks+1 ),1_ilp )
                    prod2 = stdlib_sdot( n, vl( 1_ilp, ks ), 1_ilp, vr( 1_ilp, ks+1 ), 1_ilp )
                    prod2 = prod2 - stdlib_sdot( n, vl( 1_ilp, ks+1 ), 1_ilp, vr( 1_ilp, ks ),1_ilp )
                    rnrm = stdlib_slapy2( stdlib_snrm2( n, vr( 1_ilp, ks ), 1_ilp ),stdlib_snrm2( n, vr( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    lnrm = stdlib_slapy2( stdlib_snrm2( n, vl( 1_ilp, ks ), 1_ilp ),stdlib_snrm2( n, vl( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    cond = stdlib_slapy2( prod1, prod2 ) / ( rnrm*lnrm )
                    s( ks ) = cond
                    s( ks+1 ) = cond
                 end if
              end if
              if( wantsp ) then
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvector.
                 ! copy the matrix t to the array work and swap the diagonal
                 ! block beginning at t(k,k) to the (1,1) position.
                 call stdlib_slacpy( 'FULL', n, n, t, ldt, work, ldwork )
                 ifst = k
                 ilst = 1_ilp
                 call stdlib_strexc( 'NO Q', n, work, ldwork, dummy, 1_ilp, ifst, ilst,work( 1_ilp, n+1 ),&
                            ierr )
                 if( ierr==1_ilp .or. ierr==2_ilp ) then
                    ! could not swap because blocks not well separated
                    scale = one
                    est = bignum
                 else
                    ! reordering successful
                    if( work( 2_ilp, 1_ilp )==zero ) then
                       ! form c = t22 - lambda*i in work(2:n,2:n).
                       do i = 2, n
                          work( i, i ) = work( i, i ) - work( 1_ilp, 1_ilp )
                       end do
                       n2 = 1_ilp
                       nn = n - 1_ilp
                    else
                       ! triangularize the 2 by 2 block by unitary
                       ! transformation u = [  cs   i*ss ]
                                          ! [ i*ss   cs  ].
                       ! such that the (1,1) position of work is complex
                       ! eigenvalue lambda with positive imaginary part. (2,2)
                       ! position of work is the complex eigenvalue lambda
                       ! with negative imaginary  part.
                       mu = sqrt( abs( work( 1_ilp, 2_ilp ) ) )*sqrt( abs( work( 2_ilp, 1_ilp ) ) )
                       delta = stdlib_slapy2( mu, work( 2_ilp, 1_ilp ) )
                       cs = mu / delta
                       sn = -work( 2_ilp, 1_ilp ) / delta
                       ! form
                       ! c**t = work(2:n,2:n) + i*[rwork(1) ..... rwork(n-1) ]
                                                ! [   mu                     ]
                                                ! [         ..               ]
                                                ! [             ..           ]
                                                ! [                  mu      ]
                       ! where c**t is transpose of matrix c,
                       ! and rwork is stored starting in the n+1-st column of
                       ! work.
                       do j = 3, n
                          work( 2_ilp, j ) = cs*work( 2_ilp, j )
                          work( j, j ) = work( j, j ) - work( 1_ilp, 1_ilp )
                       end do
                       work( 2_ilp, 2_ilp ) = zero
                       work( 1_ilp, n+1 ) = two*mu
                       do i = 2, n - 1
                          work( i, n+1 ) = sn*work( 1_ilp, i+1 )
                       end do
                       n2 = 2_ilp
                       nn = 2_ilp*( n-1 )
                    end if
                    ! estimate norm(inv(c**t))
                    est = zero
                    kase = 0_ilp
                    50 continue
                    call stdlib_slacn2( nn, work( 1_ilp, n+2 ), work( 1_ilp, n+4 ), iwork,est, kase, &
                              isave )
                    if( kase/=0_ilp ) then
                       if( kase==1_ilp ) then
                          if( n2==1_ilp ) then
                             ! real eigenvalue: solve c**t*x = scale*c.
                             call stdlib_slaqtr( .true., .true., n-1, work( 2_ilp, 2_ilp ),ldwork, dummy, &
                                       dumm, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          else
                             ! complex eigenvalue: solve
                             ! c**t*(p+iq) = scale*(c+id) in real arithmetic.
                             call stdlib_slaqtr( .true., .false., n-1, work( 2_ilp, 2_ilp ),ldwork, work( &
                                       1_ilp, n+1 ), mu, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          end if
                       else
                          if( n2==1_ilp ) then
                             ! real eigenvalue: solve c*x = scale*c.
                             call stdlib_slaqtr( .false., .true., n-1, work( 2_ilp, 2_ilp ),ldwork, dummy,&
                                        dumm, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          else
                             ! complex eigenvalue: solve
                             ! c*(p+iq) = scale*(c+id) in real arithmetic.
                             call stdlib_slaqtr( .false., .false., n-1,work( 2_ilp, 2_ilp ), ldwork,work( &
                                       1_ilp, n+1 ), mu, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          end if
                       end if
                       go to 50
                    end if
                 end if
                 sep( ks ) = scale / max( est, smlnum )
                 if( pair )sep( ks+1 ) = sep( ks )
              end if
              if( pair )ks = ks + 1_ilp
           end do loop_60
           return
     end subroutine stdlib_strsna

     module subroutine stdlib_dtrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm, m, &
     !! DTRSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or right eigenvectors of a real upper
     !! quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q
     !! orthogonal).
     !! T must be in Schur canonical form (as returned by DHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               work, ldwork, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: s(*), sep(*), work(ldwork,*)
           real(dp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: pair, somcon, wantbh, wants, wantsp
           integer(ilp) :: i, ierr, ifst, ilst, j, k, kase, ks, n2, nn
           real(dp) :: bignum, cond, cs, delta, dumm, eps, est, lnrm, mu, prod, prod1, prod2, &
                     rnrm, scale, smlnum, sn
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           real(dp) :: dummy(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           info = 0_ilp
           if( .not.wants .and. .not.wantsp ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( wants .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wants .and. ldvr<n ) ) then
              info = -10_ilp
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
                          if( t( k+1, k )==zero ) then
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
              if( mm<m ) then
                 info = -13_ilp
              else if( ldwork<1_ilp .or. ( wantsp .and. ldwork<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRSNA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( somcon ) then
                 if( .not.select( 1 ) )return
              end if
              if( wants )s( 1_ilp ) = one
              if( wantsp )sep( 1_ilp ) = abs( t( 1_ilp, 1_ilp ) )
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ks = 0_ilp
           pair = .false.
           loop_60: do k = 1, n
              ! determine whether t(k,k) begins a 1-by-1 or 2-by-2 block.
              if( pair ) then
                 pair = .false.
                 cycle loop_60
              else
                 if( k<n )pair = t( k+1, k )/=zero
              end if
              ! determine whether condition numbers are required for the k-th
              ! eigenpair.
              if( somcon ) then
                 if( pair ) then
                    if( .not.select( k ) .and. .not.select( k+1 ) )cycle loop_60
                 else
                    if( .not.select( k ) )cycle loop_60
                 end if
              end if
              ks = ks + 1_ilp
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 if( .not.pair ) then
                    ! real eigenvalue.
                    prod = stdlib_ddot( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    rnrm = stdlib_dnrm2( n, vr( 1_ilp, ks ), 1_ilp )
                    lnrm = stdlib_dnrm2( n, vl( 1_ilp, ks ), 1_ilp )
                    s( ks ) = abs( prod ) / ( rnrm*lnrm )
                 else
                    ! complex eigenvalue.
                    prod1 = stdlib_ddot( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                    prod1 = prod1 + stdlib_ddot( n, vr( 1_ilp, ks+1 ), 1_ilp, vl( 1_ilp, ks+1 ),1_ilp )
                    prod2 = stdlib_ddot( n, vl( 1_ilp, ks ), 1_ilp, vr( 1_ilp, ks+1 ), 1_ilp )
                    prod2 = prod2 - stdlib_ddot( n, vl( 1_ilp, ks+1 ), 1_ilp, vr( 1_ilp, ks ),1_ilp )
                    rnrm = stdlib_dlapy2( stdlib_dnrm2( n, vr( 1_ilp, ks ), 1_ilp ),stdlib_dnrm2( n, vr( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    lnrm = stdlib_dlapy2( stdlib_dnrm2( n, vl( 1_ilp, ks ), 1_ilp ),stdlib_dnrm2( n, vl( &
                              1_ilp, ks+1 ), 1_ilp ) )
                    cond = stdlib_dlapy2( prod1, prod2 ) / ( rnrm*lnrm )
                    s( ks ) = cond
                    s( ks+1 ) = cond
                 end if
              end if
              if( wantsp ) then
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvector.
                 ! copy the matrix t to the array work and swap the diagonal
                 ! block beginning at t(k,k) to the (1,1) position.
                 call stdlib_dlacpy( 'FULL', n, n, t, ldt, work, ldwork )
                 ifst = k
                 ilst = 1_ilp
                 call stdlib_dtrexc( 'NO Q', n, work, ldwork, dummy, 1_ilp, ifst, ilst,work( 1_ilp, n+1 ),&
                            ierr )
                 if( ierr==1_ilp .or. ierr==2_ilp ) then
                    ! could not swap because blocks not well separated
                    scale = one
                    est = bignum
                 else
                    ! reordering successful
                    if( work( 2_ilp, 1_ilp )==zero ) then
                       ! form c = t22 - lambda*i in work(2:n,2:n).
                       do i = 2, n
                          work( i, i ) = work( i, i ) - work( 1_ilp, 1_ilp )
                       end do
                       n2 = 1_ilp
                       nn = n - 1_ilp
                    else
                       ! triangularize the 2 by 2 block by unitary
                       ! transformation u = [  cs   i*ss ]
                                          ! [ i*ss   cs  ].
                       ! such that the (1,1) position of work is complex
                       ! eigenvalue lambda with positive imaginary part. (2,2)
                       ! position of work is the complex eigenvalue lambda
                       ! with negative imaginary  part.
                       mu = sqrt( abs( work( 1_ilp, 2_ilp ) ) )*sqrt( abs( work( 2_ilp, 1_ilp ) ) )
                       delta = stdlib_dlapy2( mu, work( 2_ilp, 1_ilp ) )
                       cs = mu / delta
                       sn = -work( 2_ilp, 1_ilp ) / delta
                       ! form
                       ! c**t = work(2:n,2:n) + i*[rwork(1) ..... rwork(n-1) ]
                                                ! [   mu                     ]
                                                ! [         ..               ]
                                                ! [             ..           ]
                                                ! [                  mu      ]
                       ! where c**t is transpose of matrix c,
                       ! and rwork is stored starting in the n+1-st column of
                       ! work.
                       do j = 3, n
                          work( 2_ilp, j ) = cs*work( 2_ilp, j )
                          work( j, j ) = work( j, j ) - work( 1_ilp, 1_ilp )
                       end do
                       work( 2_ilp, 2_ilp ) = zero
                       work( 1_ilp, n+1 ) = two*mu
                       do i = 2, n - 1
                          work( i, n+1 ) = sn*work( 1_ilp, i+1 )
                       end do
                       n2 = 2_ilp
                       nn = 2_ilp*( n-1 )
                    end if
                    ! estimate norm(inv(c**t))
                    est = zero
                    kase = 0_ilp
                    50 continue
                    call stdlib_dlacn2( nn, work( 1_ilp, n+2 ), work( 1_ilp, n+4 ), iwork,est, kase, &
                              isave )
                    if( kase/=0_ilp ) then
                       if( kase==1_ilp ) then
                          if( n2==1_ilp ) then
                             ! real eigenvalue: solve c**t*x = scale*c.
                             call stdlib_dlaqtr( .true., .true., n-1, work( 2_ilp, 2_ilp ),ldwork, dummy, &
                                       dumm, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          else
                             ! complex eigenvalue: solve
                             ! c**t*(p+iq) = scale*(c+id) in real arithmetic.
                             call stdlib_dlaqtr( .true., .false., n-1, work( 2_ilp, 2_ilp ),ldwork, work( &
                                       1_ilp, n+1 ), mu, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          end if
                       else
                          if( n2==1_ilp ) then
                             ! real eigenvalue: solve c*x = scale*c.
                             call stdlib_dlaqtr( .false., .true., n-1, work( 2_ilp, 2_ilp ),ldwork, dummy,&
                                        dumm, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          else
                             ! complex eigenvalue: solve
                             ! c*(p+iq) = scale*(c+id) in real arithmetic.
                             call stdlib_dlaqtr( .false., .false., n-1,work( 2_ilp, 2_ilp ), ldwork,work( &
                                       1_ilp, n+1 ), mu, scale,work( 1_ilp, n+4 ), work( 1_ilp, n+6 ),ierr )
                          end if
                       end if
                       go to 50
                    end if
                 end if
                 sep( ks ) = scale / max( est, smlnum )
                 if( pair )sep( ks+1 ) = sep( ks )
              end if
              if( pair )ks = ks + 1_ilp
           end do loop_60
           return
     end subroutine stdlib_dtrsna


     pure module subroutine stdlib_ctrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm,&
     !! CTRSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or right eigenvectors of a complex upper triangular
     !! matrix T (or of any matrix Q*T*Q**H with Q unitary).
                m, work, ldwork, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*), s(*), sep(*)
           complex(sp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: somcon, wantbh, wants, wantsp
           character :: normin
           integer(ilp) :: i, ierr, ix, j, k, kase, ks
           real(sp) :: bignum, eps, est, lnrm, rnrm, scale, smlnum, xnorm
           complex(sp) :: cdum, prod
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           complex(sp) :: dummy(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           ! set m to the number of eigenpairs for which condition numbers are
           ! to be computed.
           if( somcon ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           if( .not.wants .and. .not.wantsp ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( wants .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wants .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -13_ilp
           else if( ldwork<1_ilp .or. ( wantsp .and. ldwork<n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRSNA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( somcon ) then
                 if( .not.select( 1 ) )return
              end if
              if( wants )s( 1_ilp ) = one
              if( wantsp )sep( 1_ilp ) = abs( t( 1_ilp, 1_ilp ) )
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ks = 1_ilp
           loop_50: do k = 1, n
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_50
              end if
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 prod = stdlib_cdotc( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 rnrm = stdlib_scnrm2( n, vr( 1_ilp, ks ), 1_ilp )
                 lnrm = stdlib_scnrm2( n, vl( 1_ilp, ks ), 1_ilp )
                 s( ks ) = abs( prod ) / ( rnrm*lnrm )
              end if
              if( wantsp ) then
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvector.
                 ! copy the matrix t to the array work and swap the k-th
                 ! diagonal element to the (1,1) position.
                 call stdlib_clacpy( 'FULL', n, n, t, ldt, work, ldwork )
                 call stdlib_ctrexc( 'NO Q', n, work, ldwork, dummy, 1_ilp, k, 1_ilp, ierr )
                 ! form  c = t22 - lambda*i in work(2:n,2:n).
                 do i = 2, n
                    work( i, i ) = work( i, i ) - work( 1_ilp, 1_ilp )
                 end do
                 ! estimate a lower bound for the 1-norm of inv(c**h). the 1st
                 ! and (n+1)th columns of work are used to store work vectors.
                 sep( ks ) = zero
                 est = zero
                 kase = 0_ilp
                 normin = 'N'
                 30 continue
                 call stdlib_clacn2( n-1, work( 1_ilp, n+1 ), work, est, kase, isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve c**h*x = scale*b
                       call stdlib_clatrs( 'UPPER', 'CONJUGATE TRANSPOSE','NONUNIT', normin, n-1, &
                                 work( 2_ilp, 2_ilp ),ldwork, work, scale, rwork, ierr )
                    else
                       ! solve c*x = scale*b
                       call stdlib_clatrs( 'UPPER', 'NO TRANSPOSE', 'NONUNIT',normin, n-1, work( &
                                 2_ilp, 2_ilp ), ldwork, work,scale, rwork, ierr )
                    end if
                    normin = 'Y'
                    if( scale/=one ) then
                       ! multiply by 1/scale if doing so will not cause
                       ! overflow.
                       ix = stdlib_icamax( n-1, work, 1_ilp )
                       xnorm = cabs1( work( ix, 1_ilp ) )
                       if( scale<xnorm*smlnum .or. scale==zero )go to 40
                       call stdlib_csrscl( n, scale, work, 1_ilp )
                    end if
                    go to 30
                 end if
                 sep( ks ) = one / max( est, smlnum )
              end if
              40 continue
              ks = ks + 1_ilp
           end do loop_50
           return
     end subroutine stdlib_ctrsna

     pure module subroutine stdlib_ztrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm,&
     !! ZTRSNA estimates reciprocal condition numbers for specified
     !! eigenvalues and/or right eigenvectors of a complex upper triangular
     !! matrix T (or of any matrix Q*T*Q**H with Q unitary).
                m, work, ldwork, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*), s(*), sep(*)
           complex(dp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: somcon, wantbh, wants, wantsp
           character :: normin
           integer(ilp) :: i, ierr, ix, j, k, kase, ks
           real(dp) :: bignum, eps, est, lnrm, rnrm, scale, smlnum, xnorm
           complex(dp) :: cdum, prod
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           complex(dp) :: dummy(1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           somcon = stdlib_lsame( howmny, 'S' )
           ! set m to the number of eigenpairs for which condition numbers are
           ! to be computed.
           if( somcon ) then
              m = 0_ilp
              do j = 1, n
                 if( select( j ) )m = m + 1_ilp
              end do
           else
              m = n
           end if
           info = 0_ilp
           if( .not.wants .and. .not.wantsp ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( howmny, 'A' ) .and. .not.somcon ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldvl<1_ilp .or. ( wants .and. ldvl<n ) ) then
              info = -8_ilp
           else if( ldvr<1_ilp .or. ( wants .and. ldvr<n ) ) then
              info = -10_ilp
           else if( mm<m ) then
              info = -13_ilp
           else if( ldwork<1_ilp .or. ( wantsp .and. ldwork<n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRSNA', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( somcon ) then
                 if( .not.select( 1 ) )return
              end if
              if( wants )s( 1_ilp ) = one
              if( wantsp )sep( 1_ilp ) = abs( t( 1_ilp, 1_ilp ) )
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ks = 1_ilp
           loop_50: do k = 1, n
              if( somcon ) then
                 if( .not.select( k ) )cycle loop_50
              end if
              if( wants ) then
                 ! compute the reciprocal condition number of the k-th
                 ! eigenvalue.
                 prod = stdlib_zdotc( n, vr( 1_ilp, ks ), 1_ilp, vl( 1_ilp, ks ), 1_ilp )
                 rnrm = stdlib_dznrm2( n, vr( 1_ilp, ks ), 1_ilp )
                 lnrm = stdlib_dznrm2( n, vl( 1_ilp, ks ), 1_ilp )
                 s( ks ) = abs( prod ) / ( rnrm*lnrm )
              end if
              if( wantsp ) then
                 ! estimate the reciprocal condition number of the k-th
                 ! eigenvector.
                 ! copy the matrix t to the array work and swap the k-th
                 ! diagonal element to the (1,1) position.
                 call stdlib_zlacpy( 'FULL', n, n, t, ldt, work, ldwork )
                 call stdlib_ztrexc( 'NO Q', n, work, ldwork, dummy, 1_ilp, k, 1_ilp, ierr )
                 ! form  c = t22 - lambda*i in work(2:n,2:n).
                 do i = 2, n
                    work( i, i ) = work( i, i ) - work( 1_ilp, 1_ilp )
                 end do
                 ! estimate a lower bound for the 1-norm of inv(c**h). the 1st
                 ! and (n+1)th columns of work are used to store work vectors.
                 sep( ks ) = zero
                 est = zero
                 kase = 0_ilp
                 normin = 'N'
                 30 continue
                 call stdlib_zlacn2( n-1, work( 1_ilp, n+1 ), work, est, kase, isave )
                 if( kase/=0_ilp ) then
                    if( kase==1_ilp ) then
                       ! solve c**h*x = scale*b
                       call stdlib_zlatrs( 'UPPER', 'CONJUGATE TRANSPOSE','NONUNIT', normin, n-1, &
                                 work( 2_ilp, 2_ilp ),ldwork, work, scale, rwork, ierr )
                    else
                       ! solve c*x = scale*b
                       call stdlib_zlatrs( 'UPPER', 'NO TRANSPOSE', 'NONUNIT',normin, n-1, work( &
                                 2_ilp, 2_ilp ), ldwork, work,scale, rwork, ierr )
                    end if
                    normin = 'Y'
                    if( scale/=one ) then
                       ! multiply by 1/scale if doing so will not cause
                       ! overflow.
                       ix = stdlib_izamax( n-1, work, 1_ilp )
                       xnorm = cabs1( work( ix, 1_ilp ) )
                       if( scale<xnorm*smlnum .or. scale==zero )go to 40
                       call stdlib_zdrscl( n, scale, work, 1_ilp )
                    end if
                    go to 30
                 end if
                 sep( ks ) = one / max( est, smlnum )
              end if
              40 continue
              ks = ks + 1_ilp
           end do loop_50
           return
     end subroutine stdlib_ztrsna




     module subroutine stdlib_strexc( compq, n, t, ldt, q, ldq, ifst, ilst, work,info )
     !! STREXC reorders the real Schur factorization of a real matrix
     !! A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
     !! moved to row ILST.
     !! The real Schur form T is reordered by an orthogonal similarity
     !! transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
     !! is updated by postmultiplying it with Z.
     !! T must be in Schur canonical form (as returned by SHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldt, n
           ! Array Arguments 
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantq
           integer(ilp) :: here, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input arguments.
           info = 0_ilp
           wantq = stdlib_lsame( compq, 'V' )
           if( .not.wantq .and. .not.stdlib_lsame( compq, 'N' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if(( ifst<1_ilp .or. ifst>n ).and.( n>0_ilp )) then
              info = -7_ilp
           else if(( ilst<1_ilp .or. ilst>n ).and.( n>0_ilp )) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STREXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of specified block
           ! and find out it is 1 by 1 or 2 by 2.
           if( ifst>1_ilp ) then
              if( t( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp
           end if
           nbf = 1_ilp
           if( ifst<n ) then
              if( t( ifst+1, ifst )/=zero )nbf = 2_ilp
           end if
           ! determine the first row of the final block
           ! and find out it is 1 by 1 or 2 by 2.
           if( ilst>1_ilp ) then
              if( t( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp
           end if
           nbl = 1_ilp
           if( ilst<n ) then
              if( t( ilst+1, ilst )/=zero )nbl = 2_ilp
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst
              if( nbf==2_ilp .and. nbl==1_ilp )ilst = ilst - 1_ilp
              if( nbf==1_ilp .and. nbl==2_ilp )ilst = ilst + 1_ilp
              here = ifst
              10 continue
              ! swap block with next one below
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1 by 1 or 2 by 2
                 nbnext = 1_ilp
                 if( here+nbf+1<=n ) then
                    if( t( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, nbf, nbnext,work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2 by 2 block breaks into two 1 by 1 blocks
                 if( nbf==2_ilp ) then
                    if( t( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1 by 1 blocks each of which
                 ! must be swapped individually
                 nbnext = 1_ilp
                 if( here+3<=n ) then
                    if( t( here+3, here+2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here+1, 1_ilp, nbnext,work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1 by 1 blocks, no problems possible
                    call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, nbnext,work, info )
                              
                    here = here + 1_ilp
                 else
                    ! recompute nbnext in case 2 by 2 split
                    if( t( here+2, here+1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2 by 2 block did not split
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp,nbnext, work, info )
                                 
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp
                    else
                       ! 2 by 2 block did split
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, 1_ilp,work, info )
                                 
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here+1, 1_ilp, 1_ilp,work, info )
                                 
                       here = here + 2_ilp
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap block with next one above
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1 by 1 or 2 by 2
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( t( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here-nbnext, nbnext,nbf, work, &
                           info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2 by 2 block breaks into two 1 by 1 blocks
                 if( nbf==2_ilp ) then
                    if( t( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1 by 1 blocks each of which
                 ! must be swapped individually
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( t( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here-nbnext, nbnext,1_ilp, work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1 by 1 blocks, no problems possible
                    call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, nbnext, 1_ilp,work, info )
                              
                    here = here - 1_ilp
                 else
                    ! recompute nbnext in case 2 by 2 split
                    if( t( here, here-1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2 by 2 block did not split
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here-1, 2_ilp, 1_ilp,work, info )
                                 
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp
                    else
                       ! 2 by 2 block did split
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, 1_ilp,work, info )
                                 
                       call stdlib_slaexc( wantq, n, t, ldt, q, ldq, here-1, 1_ilp, 1_ilp,work, info )
                                 
                       here = here - 2_ilp
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           return
     end subroutine stdlib_strexc

     module subroutine stdlib_dtrexc( compq, n, t, ldt, q, ldq, ifst, ilst, work,info )
     !! DTREXC reorders the real Schur factorization of a real matrix
     !! A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
     !! moved to row ILST.
     !! The real Schur form T is reordered by an orthogonal similarity
     !! transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
     !! is updated by postmultiplying it with Z.
     !! T must be in Schur canonical form (as returned by DHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldt, n
           ! Array Arguments 
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantq
           integer(ilp) :: here, nbf, nbl, nbnext
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input arguments.
           info = 0_ilp
           wantq = stdlib_lsame( compq, 'V' )
           if( .not.wantq .and. .not.stdlib_lsame( compq, 'N' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if(( ifst<1_ilp .or. ifst>n ).and.( n>0_ilp )) then
              info = -7_ilp
           else if(( ilst<1_ilp .or. ilst>n ).and.( n>0_ilp )) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTREXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           ! determine the first row of specified block
           ! and find out it is 1 by 1 or 2 by 2.
           if( ifst>1_ilp ) then
              if( t( ifst, ifst-1 )/=zero )ifst = ifst - 1_ilp
           end if
           nbf = 1_ilp
           if( ifst<n ) then
              if( t( ifst+1, ifst )/=zero )nbf = 2_ilp
           end if
           ! determine the first row of the final block
           ! and find out it is 1 by 1 or 2 by 2.
           if( ilst>1_ilp ) then
              if( t( ilst, ilst-1 )/=zero )ilst = ilst - 1_ilp
           end if
           nbl = 1_ilp
           if( ilst<n ) then
              if( t( ilst+1, ilst )/=zero )nbl = 2_ilp
           end if
           if( ifst==ilst )return
           if( ifst<ilst ) then
              ! update ilst
              if( nbf==2_ilp .and. nbl==1_ilp )ilst = ilst - 1_ilp
              if( nbf==1_ilp .and. nbl==2_ilp )ilst = ilst + 1_ilp
              here = ifst
              10 continue
              ! swap block with next one below
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1 by 1 or 2 by 2
                 nbnext = 1_ilp
                 if( here+nbf+1<=n ) then
                    if( t( here+nbf+1, here+nbf )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, nbf, nbnext,work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here + nbnext
                 ! test if 2 by 2 block breaks into two 1 by 1 blocks
                 if( nbf==2_ilp ) then
                    if( t( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1 by 1 blocks each of which
                 ! must be swapped individually
                 nbnext = 1_ilp
                 if( here+3<=n ) then
                    if( t( here+3, here+2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here+1, 1_ilp, nbnext,work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1 by 1 blocks, no problems possible
                    call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, nbnext,work, info )
                              
                    here = here + 1_ilp
                 else
                    ! recompute nbnext in case 2 by 2 split
                    if( t( here+2, here+1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2 by 2 block did not split
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp,nbnext, work, info )
                                 
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here + 2_ilp
                    else
                       ! 2 by 2 block did split
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, 1_ilp,work, info )
                                 
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here+1, 1_ilp, 1_ilp,work, info )
                                 
                       here = here + 2_ilp
                    end if
                 end if
              end if
              if( here<ilst )go to 10
           else
              here = ifst
              20 continue
              ! swap block with next one above
              if( nbf==1_ilp .or. nbf==2_ilp ) then
                 ! current block either 1 by 1 or 2 by 2
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( t( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here-nbnext, nbnext,nbf, work, &
                           info )
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 here = here - nbnext
                 ! test if 2 by 2 block breaks into two 1 by 1 blocks
                 if( nbf==2_ilp ) then
                    if( t( here+1, here )==zero )nbf = 3_ilp
                 end if
              else
                 ! current block consists of two 1 by 1 blocks each of which
                 ! must be swapped individually
                 nbnext = 1_ilp
                 if( here>=3_ilp ) then
                    if( t( here-1, here-2 )/=zero )nbnext = 2_ilp
                 end if
                 call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here-nbnext, nbnext,1_ilp, work, info )
                           
                 if( info/=0_ilp ) then
                    ilst = here
                    return
                 end if
                 if( nbnext==1_ilp ) then
                    ! swap two 1 by 1 blocks, no problems possible
                    call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, nbnext, 1_ilp,work, info )
                              
                    here = here - 1_ilp
                 else
                    ! recompute nbnext in case 2 by 2 split
                    if( t( here, here-1 )==zero )nbnext = 1_ilp
                    if( nbnext==2_ilp ) then
                       ! 2 by 2 block did not split
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here-1, 2_ilp, 1_ilp,work, info )
                                 
                       if( info/=0_ilp ) then
                          ilst = here
                          return
                       end if
                       here = here - 2_ilp
                    else
                       ! 2 by 2 block did split
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here, 1_ilp, 1_ilp,work, info )
                                 
                       call stdlib_dlaexc( wantq, n, t, ldt, q, ldq, here-1, 1_ilp, 1_ilp,work, info )
                                 
                       here = here - 2_ilp
                    end if
                 end if
              end if
              if( here>ilst )go to 20
           end if
           ilst = here
           return
     end subroutine stdlib_dtrexc


     pure module subroutine stdlib_ctrexc( compq, n, t, ldt, q, ldq, ifst, ilst, info )
     !! CTREXC reorders the Schur factorization of a complex matrix
     !! A = Q*T*Q**H, so that the diagonal element of T with row index IFST
     !! is moved to row ILST.
     !! The Schur form T is reordered by a unitary similarity transformation
     !! Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
     !! postmultplying it with Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq
           integer(ilp), intent(in) :: ifst, ilst, ldq, ldt, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: q(ldq,*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: wantq
           integer(ilp) :: k, m1, m2, m3
           real(sp) :: cs
           complex(sp) :: sn, t11, t22, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           wantq = stdlib_lsame( compq, 'V' )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if(( ifst<1_ilp .or. ifst>n ).and.( n>0_ilp )) then
              info = -7_ilp
           else if(( ilst<1_ilp .or. ilst>n ).and.( n>0_ilp )) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTREXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 .or. ifst==ilst )return
           if( ifst<ilst ) then
              ! move the ifst-th diagonal element forward down the diagonal.
              m1 = 0_ilp
              m2 = -1_ilp
              m3 = 1_ilp
           else
              ! move the ifst-th diagonal element backward up the diagonal.
              m1 = -1_ilp
              m2 = 0_ilp
              m3 = -1_ilp
           end if
           do k = ifst + m1, ilst + m2, m3
              ! interchange the k-th and (k+1)-th diagonal elements.
              t11 = t( k, k )
              t22 = t( k+1, k+1 )
              ! determine the transformation to perform the interchange.
              call stdlib_clartg( t( k, k+1 ), t22-t11, cs, sn, temp )
              ! apply transformation to the matrix t.
              if( k+2<=n )call stdlib_crot( n-k-1, t( k, k+2 ), ldt, t( k+1, k+2 ), ldt, cs,sn )
                        
              call stdlib_crot( k-1, t( 1_ilp, k ), 1_ilp, t( 1_ilp, k+1 ), 1_ilp, cs, conjg( sn ) )
              t( k, k ) = t22
              t( k+1, k+1 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_crot( n, q( 1_ilp, k ), 1_ilp, q( 1_ilp, k+1 ), 1_ilp, cs,conjg( sn ) )
              end if
           end do
           return
     end subroutine stdlib_ctrexc

     pure module subroutine stdlib_ztrexc( compq, n, t, ldt, q, ldq, ifst, ilst, info )
     !! ZTREXC reorders the Schur factorization of a complex matrix
     !! A = Q*T*Q**H, so that the diagonal element of T with row index IFST
     !! is moved to row ILST.
     !! The Schur form T is reordered by a unitary similarity transformation
     !! Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
     !! postmultplying it with Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq
           integer(ilp), intent(in) :: ifst, ilst, ldq, ldt, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: q(ldq,*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: wantq
           integer(ilp) :: k, m1, m2, m3
           real(dp) :: cs
           complex(dp) :: sn, t11, t22, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           wantq = stdlib_lsame( compq, 'V' )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<max( 1_ilp, n ) ) ) then
              info = -6_ilp
           else if(( ifst<1_ilp .or. ifst>n ).and.( n>0_ilp )) then
              info = -7_ilp
           else if(( ilst<1_ilp .or. ilst>n ).and.( n>0_ilp )) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTREXC', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 .or. ifst==ilst )return
           if( ifst<ilst ) then
              ! move the ifst-th diagonal element forward down the diagonal.
              m1 = 0_ilp
              m2 = -1_ilp
              m3 = 1_ilp
           else
              ! move the ifst-th diagonal element backward up the diagonal.
              m1 = -1_ilp
              m2 = 0_ilp
              m3 = -1_ilp
           end if
           do k = ifst + m1, ilst + m2, m3
              ! interchange the k-th and (k+1)-th diagonal elements.
              t11 = t( k, k )
              t22 = t( k+1, k+1 )
              ! determine the transformation to perform the interchange.
              call stdlib_zlartg( t( k, k+1 ), t22-t11, cs, sn, temp )
              ! apply transformation to the matrix t.
              if( k+2<=n )call stdlib_zrot( n-k-1, t( k, k+2 ), ldt, t( k+1, k+2 ), ldt, cs,sn )
                        
              call stdlib_zrot( k-1, t( 1_ilp, k ), 1_ilp, t( 1_ilp, k+1 ), 1_ilp, cs,conjg( sn ) )
              t( k, k ) = t22
              t( k+1, k+1 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_zrot( n, q( 1_ilp, k ), 1_ilp, q( 1_ilp, k+1 ), 1_ilp, cs,conjg( sn ) )
              end if
           end do
           return
     end subroutine stdlib_ztrexc




     module subroutine stdlib_strsen( job, compq, select, n, t, ldt, q, ldq, wr, wi,m, s, sep, work, &
     !! STRSEN reorders the real Schur factorization of a real matrix
     !! A = Q*T*Q**T, so that a selected cluster of eigenvalues appears in
     !! the leading diagonal blocks of the upper quasi-triangular matrix T,
     !! and the leading columns of Q form an orthonormal basis of the
     !! corresponding right invariant subspace.
     !! Optionally the routine computes the reciprocal condition numbers of
     !! the cluster of eigenvalues and/or the invariant subspace.
     !! T must be in Schur canonical form (as returned by SHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, liwork, lwork, n
           real(sp), intent(out) :: s, sep
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantbh, wantq, wants, wantsp
           integer(ilp) :: ierr, k, kase, kk, ks, liwmin, lwmin, n1, n2, nn
           real(sp) :: est, rnorm, scale
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           wantq = stdlib_lsame( compq, 'V' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.wants .and. .not.wantsp )then
              info = -1_ilp
           else if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -8_ilp
           else
              ! set m to the dimension of the specified invariant subspace,
              ! and test lwork and liwork.
              m = 0_ilp
              pair = .false.
              do k = 1, n
                 if( pair ) then
                    pair = .false.
                 else
                    if( k<n ) then
                       if( t( k+1, k )==zero ) then
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
              n1 = m
              n2 = n - m
              nn = n1*n2
              if(  wantsp ) then
                 lwmin = max( 1_ilp, 2_ilp*nn )
                 liwmin = max( 1_ilp, nn )
              else if( stdlib_lsame( job, 'N' ) ) then
                 lwmin = max( 1_ilp, n )
                 liwmin = 1_ilp
              else if( stdlib_lsame( job, 'E' ) ) then
                 lwmin = max( 1_ilp, nn )
                 liwmin = 1_ilp
              end if
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -15_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -17_ilp
              end if
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wants )s = one
              if( wantsp )sep = stdlib_slange( '1', n, n, t, ldt, work )
              go to 40
           end if
           ! collect the selected blocks at the top-left corner of t.
           ks = 0_ilp
           pair = .false.
           loop_20: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( t( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp
                    ! swap the k-th block to position ks.
                    ierr = 0_ilp
                    kk = k
                    if( k/=ks )call stdlib_strexc( compq, n, t, ldt, q, ldq, kk, ks, work,ierr )
                              
                    if( ierr==1_ilp .or. ierr==2_ilp ) then
                       ! blocks too close to swap: exit.
                       info = 1_ilp
                       if( wants )s = zero
                       if( wantsp )sep = zero
                       go to 40
                    end if
                    if( pair )ks = ks + 1_ilp
                 end if
              end if
           end do loop_20
           if( wants ) then
              ! solve sylvester equation for r:
                 ! t11*r - r*t22 = scale*t12
              call stdlib_slacpy( 'F', n1, n2, t( 1_ilp, n1+1 ), ldt, work, n1 )
              call stdlib_strsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt, t( n1+1, n1+1 ),ldt, work, n1, &
                        scale, ierr )
              ! estimate the reciprocal of the condition number of the cluster
              ! of eigenvalues.
              rnorm = stdlib_slange( 'F', n1, n2, work, n1, work )
              if( rnorm==zero ) then
                 s = one
              else
                 s = scale / ( sqrt( scale*scale / rnorm+rnorm )*sqrt( rnorm ) )
              end if
           end if
           if( wantsp ) then
              ! estimate sep(t11,t22).
              est = zero
              kase = 0_ilp
              30 continue
              call stdlib_slacn2( nn, work( nn+1 ), work, iwork, est, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! solve  t11*r - r*t22 = scale*x.
                    call stdlib_strsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 else
                    ! solve t11**t*r - r*t22**t = scale*x.
                    call stdlib_strsyl( 'T', 'T', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 end if
                 go to 30
              end if
              sep = scale / est
           end if
           40 continue
           ! store the output eigenvalues in wr and wi.
           do k = 1, n
              wr( k ) = t( k, k )
              wi( k ) = zero
           end do
           do k = 1, n - 1
              if( t( k+1, k )/=zero ) then
                 wi( k ) = sqrt( abs( t( k, k+1 ) ) )*sqrt( abs( t( k+1, k ) ) )
                 wi( k+1 ) = -wi( k )
              end if
           end do
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_strsen

     module subroutine stdlib_dtrsen( job, compq, select, n, t, ldt, q, ldq, wr, wi,m, s, sep, work, &
     !! DTRSEN reorders the real Schur factorization of a real matrix
     !! A = Q*T*Q**T, so that a selected cluster of eigenvalues appears in
     !! the leading diagonal blocks of the upper quasi-triangular matrix T,
     !! and the leading columns of Q form an orthonormal basis of the
     !! corresponding right invariant subspace.
     !! Optionally the routine computes the reciprocal condition numbers of
     !! the cluster of eigenvalues and/or the invariant subspace.
     !! T must be in Schur canonical form (as returned by DHSEQR), that is,
     !! block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
     !! 2-by-2 diagonal block has its diagonal elements equal and its
     !! off-diagonal elements of opposite sign.
               lwork, iwork, liwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, liwork, lwork, n
           real(dp), intent(out) :: s, sep
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, pair, swap, wantbh, wantq, wants, wantsp
           integer(ilp) :: ierr, k, kase, kk, ks, liwmin, lwmin, n1, n2, nn
           real(dp) :: est, rnorm, scale
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           wantq = stdlib_lsame( compq, 'V' )
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.wants .and. .not.wantsp )then
              info = -1_ilp
           else if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -8_ilp
           else
              ! set m to the dimension of the specified invariant subspace,
              ! and test lwork and liwork.
              m = 0_ilp
              pair = .false.
              do k = 1, n
                 if( pair ) then
                    pair = .false.
                 else
                    if( k<n ) then
                       if( t( k+1, k )==zero ) then
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
              n1 = m
              n2 = n - m
              nn = n1*n2
              if( wantsp ) then
                 lwmin = max( 1_ilp, 2_ilp*nn )
                 liwmin = max( 1_ilp, nn )
              else if( stdlib_lsame( job, 'N' ) ) then
                 lwmin = max( 1_ilp, n )
                 liwmin = 1_ilp
              else if( stdlib_lsame( job, 'E' ) ) then
                 lwmin = max( 1_ilp, nn )
                 liwmin = 1_ilp
              end if
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -15_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -17_ilp
              end if
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible.
           if( m==n .or. m==0_ilp ) then
              if( wants )s = one
              if( wantsp )sep = stdlib_dlange( '1', n, n, t, ldt, work )
              go to 40
           end if
           ! collect the selected blocks at the top-left corner of t.
           ks = 0_ilp
           pair = .false.
           loop_20: do k = 1, n
              if( pair ) then
                 pair = .false.
              else
                 swap = select( k )
                 if( k<n ) then
                    if( t( k+1, k )/=zero ) then
                       pair = .true.
                       swap = swap .or. select( k+1 )
                    end if
                 end if
                 if( swap ) then
                    ks = ks + 1_ilp
                    ! swap the k-th block to position ks.
                    ierr = 0_ilp
                    kk = k
                    if( k/=ks )call stdlib_dtrexc( compq, n, t, ldt, q, ldq, kk, ks, work,ierr )
                              
                    if( ierr==1_ilp .or. ierr==2_ilp ) then
                       ! blocks too close to swap: exit.
                       info = 1_ilp
                       if( wants )s = zero
                       if( wantsp )sep = zero
                       go to 40
                    end if
                    if( pair )ks = ks + 1_ilp
                 end if
              end if
           end do loop_20
           if( wants ) then
              ! solve sylvester equation for r:
                 ! t11*r - r*t22 = scale*t12
              call stdlib_dlacpy( 'F', n1, n2, t( 1_ilp, n1+1 ), ldt, work, n1 )
              call stdlib_dtrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt, t( n1+1, n1+1 ),ldt, work, n1, &
                        scale, ierr )
              ! estimate the reciprocal of the condition number of the cluster
              ! of eigenvalues.
              rnorm = stdlib_dlange( 'F', n1, n2, work, n1, work )
              if( rnorm==zero ) then
                 s = one
              else
                 s = scale / ( sqrt( scale*scale / rnorm+rnorm )*sqrt( rnorm ) )
              end if
           end if
           if( wantsp ) then
              ! estimate sep(t11,t22).
              est = zero
              kase = 0_ilp
              30 continue
              call stdlib_dlacn2( nn, work( nn+1 ), work, iwork, est, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! solve  t11*r - r*t22 = scale*x.
                    call stdlib_dtrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 else
                    ! solve t11**t*r - r*t22**t = scale*x.
                    call stdlib_dtrsyl( 'T', 'T', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 end if
                 go to 30
              end if
              sep = scale / est
           end if
           40 continue
           ! store the output eigenvalues in wr and wi.
           do k = 1, n
              wr( k ) = t( k, k )
              wi( k ) = zero
           end do
           do k = 1, n - 1
              if( t( k+1, k )/=zero ) then
                 wi( k ) = sqrt( abs( t( k, k+1 ) ) )*sqrt( abs( t( k+1, k ) ) )
                 wi( k+1 ) = -wi( k )
              end if
           end do
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dtrsen


     module subroutine stdlib_ctrsen( job, compq, select, n, t, ldt, q, ldq, w, m, s,sep, work, lwork, &
     !! CTRSEN reorders the Schur factorization of a complex matrix
     !! A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in
     !! the leading positions on the diagonal of the upper triangular matrix
     !! T, and the leading columns of Q form an orthonormal basis of the
     !! corresponding right invariant subspace.
     !! Optionally the routine computes the reciprocal condition numbers of
     !! the cluster of eigenvalues and/or the invariant subspace.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, lwork, n
           real(sp), intent(out) :: s, sep
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           complex(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           complex(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantbh, wantq, wants, wantsp
           integer(ilp) :: ierr, k, kase, ks, lwmin, n1, n2, nn
           real(sp) :: est, rnorm, scale
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           real(sp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           wantq = stdlib_lsame( compq, 'V' )
           ! set m to the number of selected eigenvalues.
           m = 0_ilp
           do k = 1, n
              if( select( k ) )m = m + 1_ilp
           end do
           n1 = m
           n2 = n - m
           nn = n1*n2
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( wantsp ) then
              lwmin = max( 1_ilp, 2_ilp*nn )
           else if( stdlib_lsame( job, 'N' ) ) then
              lwmin = 1_ilp
           else if( stdlib_lsame( job, 'E' ) ) then
              lwmin = max( 1_ilp, nn )
           end if
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.wants .and. .not.wantsp )then
              info = -1_ilp
           else if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -8_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -14_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==n .or. m==0_ilp ) then
              if( wants )s = one
              if( wantsp )sep = stdlib_clange( '1', n, n, t, ldt, rwork )
              go to 40
           end if
           ! collect the selected eigenvalues at the top left corner of t.
           ks = 0_ilp
           do k = 1, n
              if( select( k ) ) then
                 ks = ks + 1_ilp
                 ! swap the k-th eigenvalue to position ks.
                 if( k/=ks )call stdlib_ctrexc( compq, n, t, ldt, q, ldq, k, ks, ierr )
              end if
           end do
           if( wants ) then
              ! solve the sylvester equation for r:
                 ! t11*r - r*t22 = scale*t12
              call stdlib_clacpy( 'F', n1, n2, t( 1_ilp, n1+1 ), ldt, work, n1 )
              call stdlib_ctrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt, t( n1+1, n1+1 ),ldt, work, n1, &
                        scale, ierr )
              ! estimate the reciprocal of the condition number of the cluster
              ! of eigenvalues.
              rnorm = stdlib_clange( 'F', n1, n2, work, n1, rwork )
              if( rnorm==zero ) then
                 s = one
              else
                 s = scale / ( sqrt( scale*scale / rnorm+rnorm )*sqrt( rnorm ) )
              end if
           end if
           if( wantsp ) then
              ! estimate sep(t11,t22).
              est = zero
              kase = 0_ilp
              30 continue
              call stdlib_clacn2( nn, work( nn+1 ), work, est, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! solve t11*r - r*t22 = scale*x.
                    call stdlib_ctrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 else
                    ! solve t11**h*r - r*t22**h = scale*x.
                    call stdlib_ctrsyl( 'C', 'C', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 end if
                 go to 30
              end if
              sep = scale / est
           end if
           40 continue
           ! copy reordered eigenvalues to w.
           do k = 1, n
              w( k ) = t( k, k )
           end do
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ctrsen

     module subroutine stdlib_ztrsen( job, compq, select, n, t, ldt, q, ldq, w, m, s,sep, work, lwork, &
     !! ZTRSEN reorders the Schur factorization of a complex matrix
     !! A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in
     !! the leading positions on the diagonal of the upper triangular matrix
     !! T, and the leading columns of Q form an orthonormal basis of the
     !! corresponding right invariant subspace.
     !! Optionally the routine computes the reciprocal condition numbers of
     !! the cluster of eigenvalues and/or the invariant subspace.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, lwork, n
           real(dp), intent(out) :: s, sep
           ! Array Arguments 
           logical(lk), intent(in) :: select(*)
           complex(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           complex(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantbh, wantq, wants, wantsp
           integer(ilp) :: ierr, k, kase, ks, lwmin, n1, n2, nn
           real(dp) :: est, rnorm, scale
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           real(dp) :: rwork(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           wantbh = stdlib_lsame( job, 'B' )
           wants = stdlib_lsame( job, 'E' ) .or. wantbh
           wantsp = stdlib_lsame( job, 'V' ) .or. wantbh
           wantq = stdlib_lsame( compq, 'V' )
           ! set m to the number of selected eigenvalues.
           m = 0_ilp
           do k = 1, n
              if( select( k ) )m = m + 1_ilp
           end do
           n1 = m
           n2 = n - m
           nn = n1*n2
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( wantsp ) then
              lwmin = max( 1_ilp, 2_ilp*nn )
           else if( stdlib_lsame( job, 'N' ) ) then
              lwmin = 1_ilp
           else if( stdlib_lsame( job, 'E' ) ) then
              lwmin = max( 1_ilp, nn )
           end if
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.wants .and. .not.wantsp )then
              info = -1_ilp
           else if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -8_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -14_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRSEN', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==n .or. m==0_ilp ) then
              if( wants )s = one
              if( wantsp )sep = stdlib_zlange( '1', n, n, t, ldt, rwork )
              go to 40
           end if
           ! collect the selected eigenvalues at the top left corner of t.
           ks = 0_ilp
           do k = 1, n
              if( select( k ) ) then
                 ks = ks + 1_ilp
                 ! swap the k-th eigenvalue to position ks.
                 if( k/=ks )call stdlib_ztrexc( compq, n, t, ldt, q, ldq, k, ks, ierr )
              end if
           end do
           if( wants ) then
              ! solve the sylvester equation for r:
                 ! t11*r - r*t22 = scale*t12
              call stdlib_zlacpy( 'F', n1, n2, t( 1_ilp, n1+1 ), ldt, work, n1 )
              call stdlib_ztrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt, t( n1+1, n1+1 ),ldt, work, n1, &
                        scale, ierr )
              ! estimate the reciprocal of the condition number of the cluster
              ! of eigenvalues.
              rnorm = stdlib_zlange( 'F', n1, n2, work, n1, rwork )
              if( rnorm==zero ) then
                 s = one
              else
                 s = scale / ( sqrt( scale*scale / rnorm+rnorm )*sqrt( rnorm ) )
              end if
           end if
           if( wantsp ) then
              ! estimate sep(t11,t22).
              est = zero
              kase = 0_ilp
              30 continue
              call stdlib_zlacn2( nn, work( nn+1 ), work, est, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! solve t11*r - r*t22 = scale*x.
                    call stdlib_ztrsyl( 'N', 'N', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 else
                    ! solve t11**h*r - r*t22**h = scale*x.
                    call stdlib_ztrsyl( 'C', 'C', -1_ilp, n1, n2, t, ldt,t( n1+1, n1+1 ), ldt, work, &
                              n1, scale,ierr )
                 end if
                 go to 30
              end if
              sep = scale / est
           end if
           40 continue
           ! copy reordered eigenvalues to w.
           do k = 1, n
              w( k ) = t( k, k )
           end do
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ztrsen




     module subroutine stdlib_slaexc( wantq, n, t, ldt, q, ldq, j1, n1, n2, work,info )
     !! SLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
     !! an upper quasi-triangular matrix T by an orthogonal similarity
     !! transformation.
     !! T must be in Schur canonical form, that is, block upper triangular
     !! with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
     !! has its diagonal elements equal and its off-diagonal elements of
     !! opposite sign.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, ldq, ldt, n, n1, n2
           ! Array Arguments 
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ldd = 4_ilp
           integer(ilp), parameter :: ldx = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: ierr, j2, j3, j4, k, nd
           real(sp) :: cs, dnorm, eps, scale, smlnum, sn, t11, t22, t33, tau, tau1, tau2, temp, &
                     thresh, wi1, wi2, wr1, wr2, xnorm
           ! Local Arrays 
           real(sp) :: d(ldd,4_ilp), u(3_ilp), u1(3_ilp), u2(3_ilp), x(ldx,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 .or. n1==0 .or. n2==0 )return
           if( j1+n1>n )return
           j2 = j1 + 1_ilp
           j3 = j1 + 2_ilp
           j4 = j1 + 3_ilp
           if( n1==1_ilp .and. n2==1_ilp ) then
              ! swap two 1-by-1 blocks.
              t11 = t( j1, j1 )
              t22 = t( j2, j2 )
              ! determine the transformation to perform the interchange.
              call stdlib_slartg( t( j1, j2 ), t22-t11, cs, sn, temp )
              ! apply transformation to the matrix t.
              if( j3<=n )call stdlib_srot( n-j1-1, t( j1, j3 ), ldt, t( j2, j3 ), ldt, cs,sn )
                        
              call stdlib_srot( j1-1, t( 1_ilp, j1 ), 1_ilp, t( 1_ilp, j2 ), 1_ilp, cs, sn )
              t( j1, j1 ) = t22
              t( j2, j2 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_srot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j2 ), 1_ilp, cs, sn )
              end if
           else
              ! swapping involves at least one 2-by-2 block.
              ! copy the diagonal block of order n1+n2 to the local array d
              ! and compute its norm.
              nd = n1 + n2
              call stdlib_slacpy( 'FULL', nd, nd, t( j1, j1 ), ldt, d, ldd )
              dnorm = stdlib_slange( 'MAX', nd, nd, d, ldd, work )
              ! compute machine-dependent threshold for test for accepting
              ! swap.
              eps = stdlib_slamch( 'P' )
              smlnum = stdlib_slamch( 'S' ) / eps
              thresh = max( ten*eps*dnorm, smlnum )
              ! solve t11*x - x*t22 = scale*t12 for x.
              call stdlib_slasy2( .false., .false., -1_ilp, n1, n2, d, ldd,d( n1+1, n1+1 ), ldd, d( 1_ilp,&
                         n1+1 ), ldd, scale, x,ldx, xnorm, ierr )
              ! swap the adjacent diagonal blocks.
              k = n1 + n1 + n2 - 3_ilp
              go to ( 10, 20, 30 )k
              10 continue
              ! n1 = 1, n2 = 2: generate elementary reflector h so that:
              ! ( scale, x11, x12 ) h = ( 0, 0, * )
              u( 1_ilp ) = scale
              u( 2_ilp ) = x( 1_ilp, 1_ilp )
              u( 3_ilp ) = x( 1_ilp, 2_ilp )
              call stdlib_slarfg( 3_ilp, u( 3_ilp ), u, 1_ilp, tau )
              u( 3_ilp ) = one
              t11 = t( j1, j1 )
              ! perform swap provisionally on diagonal block in d.
              call stdlib_slarfx( 'L', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              call stdlib_slarfx( 'R', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 3, 1 ) ), abs( d( 3, 2 ) ), abs( d( 3,3 )-t11 ) )>thresh )go to 50

              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_slarfx( 'L', 3_ilp, n-j1+1, u, tau, t( j1, j1 ), ldt, work )
              call stdlib_slarfx( 'R', j2, 3_ilp, u, tau, t( 1_ilp, j1 ), ldt, work )
              t( j3, j1 ) = zero
              t( j3, j2 ) = zero
              t( j3, j3 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_slarfx( 'R', n, 3_ilp, u, tau, q( 1_ilp, j1 ), ldq, work )
              end if
              go to 40
              20 continue
              ! n1 = 2, n2 = 1: generate elementary reflector h so that:
              ! h (  -x11 ) = ( * )
                ! (  -x21 ) = ( 0 )
                ! ( scale ) = ( 0 )
              u( 1_ilp ) = -x( 1_ilp, 1_ilp )
              u( 2_ilp ) = -x( 2_ilp, 1_ilp )
              u( 3_ilp ) = scale
              call stdlib_slarfg( 3_ilp, u( 1_ilp ), u( 2_ilp ), 1_ilp, tau )
              u( 1_ilp ) = one
              t33 = t( j3, j3 )
              ! perform swap provisionally on diagonal block in d.
              call stdlib_slarfx( 'L', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              call stdlib_slarfx( 'R', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 2, 1 ) ), abs( d( 3, 1 ) ), abs( d( 1,1 )-t33 ) )>thresh )go to 50

              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_slarfx( 'R', j3, 3_ilp, u, tau, t( 1_ilp, j1 ), ldt, work )
              call stdlib_slarfx( 'L', 3_ilp, n-j1, u, tau, t( j1, j2 ), ldt, work )
              t( j1, j1 ) = t33
              t( j2, j1 ) = zero
              t( j3, j1 ) = zero
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_slarfx( 'R', n, 3_ilp, u, tau, q( 1_ilp, j1 ), ldq, work )
              end if
              go to 40
              30 continue
              ! n1 = 2, n2 = 2: generate elementary reflectors h(1) and h(2) so
              ! that:
              ! h(2) h(1) (  -x11  -x12 ) = (  *  * )
                        ! (  -x21  -x22 )   (  0  * )
                        ! ( scale    0  )   (  0  0 )
                        ! (    0  scale )   (  0  0 )
              u1( 1_ilp ) = -x( 1_ilp, 1_ilp )
              u1( 2_ilp ) = -x( 2_ilp, 1_ilp )
              u1( 3_ilp ) = scale
              call stdlib_slarfg( 3_ilp, u1( 1_ilp ), u1( 2_ilp ), 1_ilp, tau1 )
              u1( 1_ilp ) = one
              temp = -tau1*( x( 1_ilp, 2_ilp )+u1( 2_ilp )*x( 2_ilp, 2_ilp ) )
              u2( 1_ilp ) = -temp*u1( 2_ilp ) - x( 2_ilp, 2_ilp )
              u2( 2_ilp ) = -temp*u1( 3_ilp )
              u2( 3_ilp ) = scale
              call stdlib_slarfg( 3_ilp, u2( 1_ilp ), u2( 2_ilp ), 1_ilp, tau2 )
              u2( 1_ilp ) = one
              ! perform swap provisionally on diagonal block in d.
              call stdlib_slarfx( 'L', 3_ilp, 4_ilp, u1, tau1, d, ldd, work )
              call stdlib_slarfx( 'R', 4_ilp, 3_ilp, u1, tau1, d, ldd, work )
              call stdlib_slarfx( 'L', 3_ilp, 4_ilp, u2, tau2, d( 2_ilp, 1_ilp ), ldd, work )
              call stdlib_slarfx( 'R', 4_ilp, 3_ilp, u2, tau2, d( 1_ilp, 2_ilp ), ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 3_ilp, 1_ilp ) ), abs( d( 3_ilp, 2_ilp ) ), abs( d( 4_ilp, 1_ilp ) ),abs( d( 4_ilp, 2_ilp ) ) )&
                        >thresh )go to 50
              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_slarfx( 'L', 3_ilp, n-j1+1, u1, tau1, t( j1, j1 ), ldt, work )
              call stdlib_slarfx( 'R', j4, 3_ilp, u1, tau1, t( 1_ilp, j1 ), ldt, work )
              call stdlib_slarfx( 'L', 3_ilp, n-j1+1, u2, tau2, t( j2, j1 ), ldt, work )
              call stdlib_slarfx( 'R', j4, 3_ilp, u2, tau2, t( 1_ilp, j2 ), ldt, work )
              t( j3, j1 ) = zero
              t( j3, j2 ) = zero
              t( j4, j1 ) = zero
              t( j4, j2 ) = zero
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_slarfx( 'R', n, 3_ilp, u1, tau1, q( 1_ilp, j1 ), ldq, work )
                 call stdlib_slarfx( 'R', n, 3_ilp, u2, tau2, q( 1_ilp, j2 ), ldq, work )
              end if
              40 continue
              if( n2==2_ilp ) then
                 ! standardize new 2-by-2 block t11
                 call stdlib_slanv2( t( j1, j1 ), t( j1, j2 ), t( j2, j1 ),t( j2, j2 ), wr1, wi1, &
                           wr2, wi2, cs, sn )
                 call stdlib_srot( n-j1-1, t( j1, j1+2 ), ldt, t( j2, j1+2 ), ldt,cs, sn )
                 call stdlib_srot( j1-1, t( 1_ilp, j1 ), 1_ilp, t( 1_ilp, j2 ), 1_ilp, cs, sn )
                 if( wantq )call stdlib_srot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j2 ), 1_ilp, cs, sn )
              end if
              if( n1==2_ilp ) then
                 ! standardize new 2-by-2 block t22
                 j3 = j1 + n2
                 j4 = j3 + 1_ilp
                 call stdlib_slanv2( t( j3, j3 ), t( j3, j4 ), t( j4, j3 ),t( j4, j4 ), wr1, wi1, &
                           wr2, wi2, cs, sn )
                 if( j3+2<=n )call stdlib_srot( n-j3-1, t( j3, j3+2 ), ldt, t( j4, j3+2 ),ldt, cs,&
                            sn )
                 call stdlib_srot( j3-1, t( 1_ilp, j3 ), 1_ilp, t( 1_ilp, j4 ), 1_ilp, cs, sn )
                 if( wantq )call stdlib_srot( n, q( 1_ilp, j3 ), 1_ilp, q( 1_ilp, j4 ), 1_ilp, cs, sn )
              end if
           end if
           return
           ! exit with info = 1 if swap was rejected.
        50 continue
           info = 1_ilp
           return
     end subroutine stdlib_slaexc

     module subroutine stdlib_dlaexc( wantq, n, t, ldt, q, ldq, j1, n1, n2, work,info )
     !! DLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
     !! an upper quasi-triangular matrix T by an orthogonal similarity
     !! transformation.
     !! T must be in Schur canonical form, that is, block upper triangular
     !! with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
     !! has its diagonal elements equal and its off-diagonal elements of
     !! opposite sign.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: wantq
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, ldq, ldt, n, n1, n2
           ! Array Arguments 
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: ldd = 4_ilp
           integer(ilp), parameter :: ldx = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: ierr, j2, j3, j4, k, nd
           real(dp) :: cs, dnorm, eps, scale, smlnum, sn, t11, t22, t33, tau, tau1, tau2, temp, &
                     thresh, wi1, wi2, wr1, wr2, xnorm
           ! Local Arrays 
           real(dp) :: d(ldd,4_ilp), u(3_ilp), u1(3_ilp), u2(3_ilp), x(ldx,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 .or. n1==0 .or. n2==0 )return
           if( j1+n1>n )return
           j2 = j1 + 1_ilp
           j3 = j1 + 2_ilp
           j4 = j1 + 3_ilp
           if( n1==1_ilp .and. n2==1_ilp ) then
              ! swap two 1-by-1 blocks.
              t11 = t( j1, j1 )
              t22 = t( j2, j2 )
              ! determine the transformation to perform the interchange.
              call stdlib_dlartg( t( j1, j2 ), t22-t11, cs, sn, temp )
              ! apply transformation to the matrix t.
              if( j3<=n )call stdlib_drot( n-j1-1, t( j1, j3 ), ldt, t( j2, j3 ), ldt, cs,sn )
                        
              call stdlib_drot( j1-1, t( 1_ilp, j1 ), 1_ilp, t( 1_ilp, j2 ), 1_ilp, cs, sn )
              t( j1, j1 ) = t22
              t( j2, j2 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_drot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j2 ), 1_ilp, cs, sn )
              end if
           else
              ! swapping involves at least one 2-by-2 block.
              ! copy the diagonal block of order n1+n2 to the local array d
              ! and compute its norm.
              nd = n1 + n2
              call stdlib_dlacpy( 'FULL', nd, nd, t( j1, j1 ), ldt, d, ldd )
              dnorm = stdlib_dlange( 'MAX', nd, nd, d, ldd, work )
              ! compute machine-dependent threshold for test for accepting
              ! swap.
              eps = stdlib_dlamch( 'P' )
              smlnum = stdlib_dlamch( 'S' ) / eps
              thresh = max( ten*eps*dnorm, smlnum )
              ! solve t11*x - x*t22 = scale*t12 for x.
              call stdlib_dlasy2( .false., .false., -1_ilp, n1, n2, d, ldd,d( n1+1, n1+1 ), ldd, d( 1_ilp,&
                         n1+1 ), ldd, scale, x,ldx, xnorm, ierr )
              ! swap the adjacent diagonal blocks.
              k = n1 + n1 + n2 - 3_ilp
              go to ( 10, 20, 30 )k
              10 continue
              ! n1 = 1, n2 = 2: generate elementary reflector h so that:
              ! ( scale, x11, x12 ) h = ( 0, 0, * )
              u( 1_ilp ) = scale
              u( 2_ilp ) = x( 1_ilp, 1_ilp )
              u( 3_ilp ) = x( 1_ilp, 2_ilp )
              call stdlib_dlarfg( 3_ilp, u( 3_ilp ), u, 1_ilp, tau )
              u( 3_ilp ) = one
              t11 = t( j1, j1 )
              ! perform swap provisionally on diagonal block in d.
              call stdlib_dlarfx( 'L', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              call stdlib_dlarfx( 'R', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 3, 1 ) ), abs( d( 3, 2 ) ), abs( d( 3,3 )-t11 ) )>thresh ) go to 50
              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_dlarfx( 'L', 3_ilp, n-j1+1, u, tau, t( j1, j1 ), ldt, work )
              call stdlib_dlarfx( 'R', j2, 3_ilp, u, tau, t( 1_ilp, j1 ), ldt, work )
              t( j3, j1 ) = zero
              t( j3, j2 ) = zero
              t( j3, j3 ) = t11
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_dlarfx( 'R', n, 3_ilp, u, tau, q( 1_ilp, j1 ), ldq, work )
              end if
              go to 40
              20 continue
              ! n1 = 2, n2 = 1: generate elementary reflector h so that:
              ! h (  -x11 ) = ( * )
                ! (  -x21 ) = ( 0 )
                ! ( scale ) = ( 0 )
              u( 1_ilp ) = -x( 1_ilp, 1_ilp )
              u( 2_ilp ) = -x( 2_ilp, 1_ilp )
              u( 3_ilp ) = scale
              call stdlib_dlarfg( 3_ilp, u( 1_ilp ), u( 2_ilp ), 1_ilp, tau )
              u( 1_ilp ) = one
              t33 = t( j3, j3 )
              ! perform swap provisionally on diagonal block in d.
              call stdlib_dlarfx( 'L', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              call stdlib_dlarfx( 'R', 3_ilp, 3_ilp, u, tau, d, ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 2, 1 ) ), abs( d( 3, 1 ) ), abs( d( 1,1 )-t33 ) )>thresh ) go to 50
              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_dlarfx( 'R', j3, 3_ilp, u, tau, t( 1_ilp, j1 ), ldt, work )
              call stdlib_dlarfx( 'L', 3_ilp, n-j1, u, tau, t( j1, j2 ), ldt, work )
              t( j1, j1 ) = t33
              t( j2, j1 ) = zero
              t( j3, j1 ) = zero
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_dlarfx( 'R', n, 3_ilp, u, tau, q( 1_ilp, j1 ), ldq, work )
              end if
              go to 40
              30 continue
              ! n1 = 2, n2 = 2: generate elementary reflectors h(1) and h(2) so
              ! that:
              ! h(2) h(1) (  -x11  -x12 ) = (  *  * )
                        ! (  -x21  -x22 )   (  0  * )
                        ! ( scale    0  )   (  0  0 )
                        ! (    0  scale )   (  0  0 )
              u1( 1_ilp ) = -x( 1_ilp, 1_ilp )
              u1( 2_ilp ) = -x( 2_ilp, 1_ilp )
              u1( 3_ilp ) = scale
              call stdlib_dlarfg( 3_ilp, u1( 1_ilp ), u1( 2_ilp ), 1_ilp, tau1 )
              u1( 1_ilp ) = one
              temp = -tau1*( x( 1_ilp, 2_ilp )+u1( 2_ilp )*x( 2_ilp, 2_ilp ) )
              u2( 1_ilp ) = -temp*u1( 2_ilp ) - x( 2_ilp, 2_ilp )
              u2( 2_ilp ) = -temp*u1( 3_ilp )
              u2( 3_ilp ) = scale
              call stdlib_dlarfg( 3_ilp, u2( 1_ilp ), u2( 2_ilp ), 1_ilp, tau2 )
              u2( 1_ilp ) = one
              ! perform swap provisionally on diagonal block in d.
              call stdlib_dlarfx( 'L', 3_ilp, 4_ilp, u1, tau1, d, ldd, work )
              call stdlib_dlarfx( 'R', 4_ilp, 3_ilp, u1, tau1, d, ldd, work )
              call stdlib_dlarfx( 'L', 3_ilp, 4_ilp, u2, tau2, d( 2_ilp, 1_ilp ), ldd, work )
              call stdlib_dlarfx( 'R', 4_ilp, 3_ilp, u2, tau2, d( 1_ilp, 2_ilp ), ldd, work )
              ! test whether to reject swap.
              if( max( abs( d( 3_ilp, 1_ilp ) ), abs( d( 3_ilp, 2_ilp ) ), abs( d( 4_ilp, 1_ilp ) ),abs( d( 4_ilp, 2_ilp ) ) )&
                        >thresh )go to 50
              ! accept swap: apply transformation to the entire matrix t.
              call stdlib_dlarfx( 'L', 3_ilp, n-j1+1, u1, tau1, t( j1, j1 ), ldt, work )
              call stdlib_dlarfx( 'R', j4, 3_ilp, u1, tau1, t( 1_ilp, j1 ), ldt, work )
              call stdlib_dlarfx( 'L', 3_ilp, n-j1+1, u2, tau2, t( j2, j1 ), ldt, work )
              call stdlib_dlarfx( 'R', j4, 3_ilp, u2, tau2, t( 1_ilp, j2 ), ldt, work )
              t( j3, j1 ) = zero
              t( j3, j2 ) = zero
              t( j4, j1 ) = zero
              t( j4, j2 ) = zero
              if( wantq ) then
                 ! accumulate transformation in the matrix q.
                 call stdlib_dlarfx( 'R', n, 3_ilp, u1, tau1, q( 1_ilp, j1 ), ldq, work )
                 call stdlib_dlarfx( 'R', n, 3_ilp, u2, tau2, q( 1_ilp, j2 ), ldq, work )
              end if
              40 continue
              if( n2==2_ilp ) then
                 ! standardize new 2-by-2 block t11
                 call stdlib_dlanv2( t( j1, j1 ), t( j1, j2 ), t( j2, j1 ),t( j2, j2 ), wr1, wi1, &
                           wr2, wi2, cs, sn )
                 call stdlib_drot( n-j1-1, t( j1, j1+2 ), ldt, t( j2, j1+2 ), ldt,cs, sn )
                 call stdlib_drot( j1-1, t( 1_ilp, j1 ), 1_ilp, t( 1_ilp, j2 ), 1_ilp, cs, sn )
                 if( wantq )call stdlib_drot( n, q( 1_ilp, j1 ), 1_ilp, q( 1_ilp, j2 ), 1_ilp, cs, sn )
              end if
              if( n1==2_ilp ) then
                 ! standardize new 2-by-2 block t22
                 j3 = j1 + n2
                 j4 = j3 + 1_ilp
                 call stdlib_dlanv2( t( j3, j3 ), t( j3, j4 ), t( j4, j3 ),t( j4, j4 ), wr1, wi1, &
                           wr2, wi2, cs, sn )
                 if( j3+2<=n )call stdlib_drot( n-j3-1, t( j3, j3+2 ), ldt, t( j4, j3+2 ),ldt, cs,&
                            sn )
                 call stdlib_drot( j3-1, t( 1_ilp, j3 ), 1_ilp, t( 1_ilp, j4 ), 1_ilp, cs, sn )
                 if( wantq )call stdlib_drot( n, q( 1_ilp, j3 ), 1_ilp, q( 1_ilp, j4 ), 1_ilp, cs, sn )
              end if
           end if
           return
           ! exit with info = 1 if swap was rejected.
           50 continue
           info = 1_ilp
           return
     end subroutine stdlib_dlaexc




     pure module subroutine stdlib_slanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn )
     !! SLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
     !! matrix in standard form:
     !! [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
     !! [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
     !! where either
     !! 1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
     !! 2) AA = DD and BB*CC < 0, so that AA + or - sqrt(BB*CC) are complex
     !! conjugate eigenvalues.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(inout) :: a, b, c, d
           real(sp), intent(out) :: cs, rt1i, rt1r, rt2i, rt2r, sn
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: multpl = 4.0e+0_sp
           
           
           ! Local Scalars 
           real(sp) :: aa, bb, bcmax, bcmis, cc, cs1, dd, eps, p, sab, sac, scale, sigma, sn1, &
                     tau, temp, z, safmin, safmn2, safmx2
           integer(ilp) :: count
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_slamch( 'S' )
           eps = stdlib_slamch( 'P' )
           safmn2 = stdlib_slamch( 'B' )**int( log( safmin / eps ) /log( stdlib_slamch( 'B' ) ) / &
                     two,KIND=ilp)
           safmx2 = one / safmn2
           if( c==zero ) then
              cs = one
              sn = zero
           else if( b==zero ) then
              ! swap rows and columns
              cs = zero
              sn = one
              temp = d
              d = a
              a = temp
              b = -c
              c = zero
           else if( (a-d)==zero .and. sign( one, b )/=sign( one, c ) ) then
              cs = one
              sn = zero
           else
              temp = a - d
              p = half*temp
              bcmax = max( abs( b ), abs( c ) )
              bcmis = min( abs( b ), abs( c ) )*sign( one, b )*sign( one, c )
              scale = max( abs( p ), bcmax )
              z = ( p / scale )*p + ( bcmax / scale )*bcmis
              ! if z is of the order of the machine accuracy, postpone the
              ! decision on the nature of eigenvalues
              if( z>=multpl*eps ) then
                 ! real eigenvalues. compute a and d.
                 z = p + sign( sqrt( scale )*sqrt( z ), p )
                 a = d + z
                 d = d - ( bcmax / z )*bcmis
                 ! compute b and the rotation matrix
                 tau = stdlib_slapy2( c, z )
                 cs = z / tau
                 sn = c / tau
                 b = b - c
                 c = zero
              else
                 ! complex eigenvalues, or real(almost,KIND=sp) equal eigenvalues.
                 ! make diagonal elements equal.
                 count = 0_ilp
                 sigma = b + c
                 10 continue
                 count = count + 1_ilp
                 scale = max( abs(temp), abs(sigma) )
                 if( scale>=safmx2 ) then
                    sigma = sigma * safmn2
                    temp = temp * safmn2
                    if (count <= 20)goto 10
                 end if
                 if( scale<=safmn2 ) then
                    sigma = sigma * safmx2
                    temp = temp * safmx2
                    if (count <= 20)goto 10
                 end if
                 p = half*temp
                 tau = stdlib_slapy2( sigma, temp )
                 cs = sqrt( half*( one+abs( sigma ) / tau ) )
                 sn = -( p / ( tau*cs ) )*sign( one, sigma )
                 ! compute [ aa  bb ] = [ a  b ] [ cs -sn ]
                         ! [ cc  dd ]   [ c  d ] [ sn  cs ]
                 aa = a*cs + b*sn
                 bb = -a*sn + b*cs
                 cc = c*cs + d*sn
                 dd = -c*sn + d*cs
                 ! compute [ a  b ] = [ cs  sn ] [ aa  bb ]
                         ! [ c  d ]   [-sn  cs ] [ cc  dd ]
                 a = aa*cs + cc*sn
                 b = bb*cs + dd*sn
                 c = -aa*sn + cc*cs
                 d = -bb*sn + dd*cs
                 temp = half*( a+d )
                 a = temp
                 d = temp
                 if( c/=zero ) then
                    if( b/=zero ) then
                       if( sign( one, b )==sign( one, c ) ) then
                          ! real eigenvalues: reduce to upper triangular form
                          sab = sqrt( abs( b ) )
                          sac = sqrt( abs( c ) )
                          p = sign( sab*sac, c )
                          tau = one / sqrt( abs( b+c ) )
                          a = temp + p
                          d = temp - p
                          b = b - c
                          c = zero
                          cs1 = sab*tau
                          sn1 = sac*tau
                          temp = cs*cs1 - sn*sn1
                          sn = cs*sn1 + sn*cs1
                          cs = temp
                       end if
                    else
                       b = -c
                       c = zero
                       temp = cs
                       cs = -sn
                       sn = temp
                    end if
                 end if
              end if
           end if
           ! store eigenvalues in (rt1r,rt1i) and (rt2r,rt2i).
           rt1r = a
           rt2r = d
           if( c==zero ) then
              rt1i = zero
              rt2i = zero
           else
              rt1i = sqrt( abs( b ) )*sqrt( abs( c ) )
              rt2i = -rt1i
           end if
           return
     end subroutine stdlib_slanv2

     pure module subroutine stdlib_dlanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn )
     !! DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
     !! matrix in standard form:
     !! [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
     !! [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
     !! where either
     !! 1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
     !! 2) AA = DD and BB*CC < 0, so that AA + or - sqrt(BB*CC) are complex
     !! conjugate eigenvalues.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(inout) :: a, b, c, d
           real(dp), intent(out) :: cs, rt1i, rt1r, rt2i, rt2r, sn
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: multpl = 4.0e+0_dp
           
           
           ! Local Scalars 
           real(dp) :: aa, bb, bcmax, bcmis, cc, cs1, dd, eps, p, sab, sac, scale, sigma, sn1, &
                     tau, temp, z, safmin, safmn2, safmx2
           integer(ilp) :: count
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_dlamch( 'S' )
           eps = stdlib_dlamch( 'P' )
           safmn2 = stdlib_dlamch( 'B' )**int( log( safmin / eps ) /log( stdlib_dlamch( 'B' ) ) / &
                     two,KIND=ilp)
           safmx2 = one / safmn2
           if( c==zero ) then
              cs = one
              sn = zero
           else if( b==zero ) then
              ! swap rows and columns
              cs = zero
              sn = one
              temp = d
              d = a
              a = temp
              b = -c
              c = zero
           else if( ( a-d )==zero .and. sign( one, b )/=sign( one, c ) )then
              cs = one
              sn = zero
           else
              temp = a - d
              p = half*temp
              bcmax = max( abs( b ), abs( c ) )
              bcmis = min( abs( b ), abs( c ) )*sign( one, b )*sign( one, c )
              scale = max( abs( p ), bcmax )
              z = ( p / scale )*p + ( bcmax / scale )*bcmis
              ! if z is of the order of the machine accuracy, postpone the
              ! decision on the nature of eigenvalues
              if( z>=multpl*eps ) then
                 ! real eigenvalues. compute a and d.
                 z = p + sign( sqrt( scale )*sqrt( z ), p )
                 a = d + z
                 d = d - ( bcmax / z )*bcmis
                 ! compute b and the rotation matrix
                 tau = stdlib_dlapy2( c, z )
                 cs = z / tau
                 sn = c / tau
                 b = b - c
                 c = zero
              else
                 ! complex eigenvalues, or real(almost,KIND=dp) equal eigenvalues.
                 ! make diagonal elements equal.
                 count = 0_ilp
                 sigma = b + c
                 10 continue
                 count = count + 1_ilp
                 scale = max( abs(temp), abs(sigma) )
                 if( scale>=safmx2 ) then
                    sigma = sigma * safmn2
                    temp = temp * safmn2
                    if (count <= 20)goto 10
                 end if
                 if( scale<=safmn2 ) then
                    sigma = sigma * safmx2
                    temp = temp * safmx2
                    if (count <= 20)goto 10
                 end if
                 p = half*temp
                 tau = stdlib_dlapy2( sigma, temp )
                 cs = sqrt( half*( one+abs( sigma ) / tau ) )
                 sn = -( p / ( tau*cs ) )*sign( one, sigma )
                 ! compute [ aa  bb ] = [ a  b ] [ cs -sn ]
                         ! [ cc  dd ]   [ c  d ] [ sn  cs ]
                 aa = a*cs + b*sn
                 bb = -a*sn + b*cs
                 cc = c*cs + d*sn
                 dd = -c*sn + d*cs
                 ! compute [ a  b ] = [ cs  sn ] [ aa  bb ]
                         ! [ c  d ]   [-sn  cs ] [ cc  dd ]
                 a = aa*cs + cc*sn
                 b = bb*cs + dd*sn
                 c = -aa*sn + cc*cs
                 d = -bb*sn + dd*cs
                 temp = half*( a+d )
                 a = temp
                 d = temp
                 if( c/=zero ) then
                    if( b/=zero ) then
                       if( sign( one, b )==sign( one, c ) ) then
                          ! real eigenvalues: reduce to upper triangular form
                          sab = sqrt( abs( b ) )
                          sac = sqrt( abs( c ) )
                          p = sign( sab*sac, c )
                          tau = one / sqrt( abs( b+c ) )
                          a = temp + p
                          d = temp - p
                          b = b - c
                          c = zero
                          cs1 = sab*tau
                          sn1 = sac*tau
                          temp = cs*cs1 - sn*sn1
                          sn = cs*sn1 + sn*cs1
                          cs = temp
                       end if
                    else
                       b = -c
                       c = zero
                       temp = cs
                       cs = -sn
                       sn = temp
                    end if
                 end if
              end if
           end if
           ! store eigenvalues in (rt1r,rt1i) and (rt2r,rt2i).
           rt1r = a
           rt2r = d
           if( c==zero ) then
              rt1i = zero
              rt2i = zero
           else
              rt1i = sqrt( abs( b ) )*sqrt( abs( c ) )
              rt2i = -rt1i
           end if
           return
     end subroutine stdlib_dlanv2




     pure module subroutine stdlib_slaein( rightv, noinit, n, h, ldh, wr, wi, vr, vi, b,ldb, work, eps3, &
     !! SLAEIN uses inverse iteration to find a right or left eigenvector
     !! corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg
     !! matrix H.
               smlnum, bignum, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(sp), intent(in) :: bignum, eps3, smlnum, wi, wr
           ! Array Arguments 
           real(sp), intent(out) :: b(ldb,*), work(*)
           real(sp), intent(in) :: h(ldh,*)
           real(sp), intent(inout) :: vi(*), vr(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: tenth = 1.0e-1_sp
           
           ! Local Scalars 
           character :: normin, trans
           integer(ilp) :: i, i1, i2, i3, ierr, its, j
           real(sp) :: absbii, absbjj, ei, ej, growto, norm, nrmsml, rec, rootn, scale, temp, &
                     vcrit, vmax, vnorm, w, w1, x, xi, xr, y
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! growto is the threshold used in the acceptance test for an
           ! eigenvector.
           rootn = sqrt( real( n,KIND=sp) )
           growto = tenth / rootn
           nrmsml = max( one, eps3*rootn )*smlnum
           ! form b = h - (wr,wi)*i (except that the subdiagonal elements and
           ! the imaginary parts of the diagonal elements are not stored).
           do j = 1, n
              do i = 1, j - 1
                 b( i, j ) = h( i, j )
              end do
              b( j, j ) = h( j, j ) - wr
           end do
           if( wi==zero ) then
              ! real eigenvalue.
              if( noinit ) then
                 ! set initial vector.
                 do i = 1, n
                    vr( i ) = eps3
                 end do
              else
                 ! scale supplied initial vector.
                 vnorm = stdlib_snrm2( n, vr, 1_ilp )
                 call stdlib_sscal( n, ( eps3*rootn ) / max( vnorm, nrmsml ), vr,1_ilp )
              end if
              if( rightv ) then
                 ! lu decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 do i = 1, n - 1
                    ei = h( i+1, i )
                    if( abs( b( i, i ) )<abs( ei ) ) then
                       ! interchange rows and eliminate.
                       x = b( i, i ) / ei
                       b( i, i ) = ei
                       do j = i + 1, n
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - x*temp
                          b( i, j ) = temp
                       end do
                    else
                       ! eliminate without interchange.
                       if( b( i, i )==zero )b( i, i ) = eps3
                       x = ei / b( i, i )
                       if( x/=zero ) then
                          do j = i + 1, n
                             b( i+1, j ) = b( i+1, j ) - x*b( i, j )
                          end do
                       end if
                    end if
                 end do
                 if( b( n, n )==zero )b( n, n ) = eps3
                 trans = 'N'
              else
                 ! ul decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 do j = n, 2, -1
                    ej = h( j, j-1 )
                    if( abs( b( j, j ) )<abs( ej ) ) then
                       ! interchange columns and eliminate.
                       x = b( j, j ) / ej
                       b( j, j ) = ej
                       do i = 1, j - 1
                          temp = b( i, j-1 )
                          b( i, j-1 ) = b( i, j ) - x*temp
                          b( i, j ) = temp
                       end do
                    else
                       ! eliminate without interchange.
                       if( b( j, j )==zero )b( j, j ) = eps3
                       x = ej / b( j, j )
                       if( x/=zero ) then
                          do i = 1, j - 1
                             b( i, j-1 ) = b( i, j-1 ) - x*b( i, j )
                          end do
                       end if
                    end if
                 end do
                 if( b( 1_ilp, 1_ilp )==zero )b( 1_ilp, 1_ilp ) = eps3
                 trans = 'T'
              end if
              normin = 'N'
              do its = 1, n
                 ! solve u*x = scale*v for a right eigenvector
                   ! or u**t*x = scale*v for a left eigenvector,
                 ! overwriting x on v.
                 call stdlib_slatrs( 'UPPER', trans, 'NONUNIT', normin, n, b, ldb,vr, scale, work,&
                            ierr )
                 normin = 'Y'
                 ! test for sufficient growth in the norm of v.
                 vnorm = stdlib_sasum( n, vr, 1_ilp )
                 if( vnorm>=growto*scale )go to 120
                 ! choose new orthogonal starting vector and try again.
                 temp = eps3 / ( rootn+one )
                 vr( 1_ilp ) = eps3
                 do i = 2, n
                    vr( i ) = temp
                 end do
                 vr( n-its+1 ) = vr( n-its+1 ) - eps3*rootn
              end do
              ! failure to find eigenvector in n iterations.
              info = 1_ilp
              120 continue
              ! normalize eigenvector.
              i = stdlib_isamax( n, vr, 1_ilp )
              call stdlib_sscal( n, one / abs( vr( i ) ), vr, 1_ilp )
           else
              ! complex eigenvalue.
              if( noinit ) then
                 ! set initial vector.
                 do i = 1, n
                    vr( i ) = eps3
                    vi( i ) = zero
                 end do
              else
                 ! scale supplied initial vector.
                 norm = stdlib_slapy2( stdlib_snrm2( n, vr, 1_ilp ), stdlib_snrm2( n, vi, 1_ilp ) )
                           
                 rec = ( eps3*rootn ) / max( norm, nrmsml )
                 call stdlib_sscal( n, rec, vr, 1_ilp )
                 call stdlib_sscal( n, rec, vi, 1_ilp )
              end if
              if( rightv ) then
                 ! lu decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 ! the imaginary part of the (i,j)-th element of u is stored in
                 ! b(j+1,i).
                 b( 2_ilp, 1_ilp ) = -wi
                 do i = 2, n
                    b( i+1, 1_ilp ) = zero
                 end do
                 loop_170: do i = 1, n - 1
                    absbii = stdlib_slapy2( b( i, i ), b( i+1, i ) )
                    ei = h( i+1, i )
                    if( absbii<abs( ei ) ) then
                       ! interchange rows and eliminate.
                       xr = b( i, i ) / ei
                       xi = b( i+1, i ) / ei
                       b( i, i ) = ei
                       b( i+1, i ) = zero
                       do j = i + 1, n
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - xr*temp
                          b( j+1, i+1 ) = b( j+1, i ) - xi*temp
                          b( i, j ) = temp
                          b( j+1, i ) = zero
                       end do
                       b( i+2, i ) = -wi
                       b( i+1, i+1 ) = b( i+1, i+1 ) - xi*wi
                       b( i+2, i+1 ) = b( i+2, i+1 ) + xr*wi
                    else
                       ! eliminate without interchanging rows.
                       if( absbii==zero ) then
                          b( i, i ) = eps3
                          b( i+1, i ) = zero
                          absbii = eps3
                       end if
                       ei = ( ei / absbii ) / absbii
                       xr = b( i, i )*ei
                       xi = -b( i+1, i )*ei
                       do j = i + 1, n
                          b( i+1, j ) = b( i+1, j ) - xr*b( i, j ) +xi*b( j+1, i )
                          b( j+1, i+1 ) = -xr*b( j+1, i ) - xi*b( i, j )
                       end do
                       b( i+2, i+1 ) = b( i+2, i+1 ) - wi
                    end if
                    ! compute 1-norm of offdiagonal elements of i-th row.
                    work( i ) = stdlib_sasum( n-i, b( i, i+1 ), ldb ) +stdlib_sasum( n-i, b( i+2, &
                              i ), 1_ilp )
                 end do loop_170
                 if( b( n, n )==zero .and. b( n+1, n )==zero )b( n, n ) = eps3
                 work( n ) = zero
                 i1 = n
                 i2 = 1_ilp
                 i3 = -1_ilp
              else
                 ! ul decomposition with partial pivoting of conjg(b),
                 ! replacing zero pivots by eps3.
                 ! the imaginary part of the (i,j)-th element of u is stored in
                 ! b(j+1,i).
                 b( n+1, n ) = wi
                 do j = 1, n - 1
                    b( n+1, j ) = zero
                 end do
                 loop_210: do j = n, 2, -1
                    ej = h( j, j-1 )
                    absbjj = stdlib_slapy2( b( j, j ), b( j+1, j ) )
                    if( absbjj<abs( ej ) ) then
                       ! interchange columns and eliminate
                       xr = b( j, j ) / ej
                       xi = b( j+1, j ) / ej
                       b( j, j ) = ej
                       b( j+1, j ) = zero
                       do i = 1, j - 1
                          temp = b( i, j-1 )
                          b( i, j-1 ) = b( i, j ) - xr*temp
                          b( j, i ) = b( j+1, i ) - xi*temp
                          b( i, j ) = temp
                          b( j+1, i ) = zero
                       end do
                       b( j+1, j-1 ) = wi
                       b( j-1, j-1 ) = b( j-1, j-1 ) + xi*wi
                       b( j, j-1 ) = b( j, j-1 ) - xr*wi
                    else
                       ! eliminate without interchange.
                       if( absbjj==zero ) then
                          b( j, j ) = eps3
                          b( j+1, j ) = zero
                          absbjj = eps3
                       end if
                       ej = ( ej / absbjj ) / absbjj
                       xr = b( j, j )*ej
                       xi = -b( j+1, j )*ej
                       do i = 1, j - 1
                          b( i, j-1 ) = b( i, j-1 ) - xr*b( i, j ) +xi*b( j+1, i )
                          b( j, i ) = -xr*b( j+1, i ) - xi*b( i, j )
                       end do
                       b( j, j-1 ) = b( j, j-1 ) + wi
                    end if
                    ! compute 1-norm of offdiagonal elements of j-th column.
                    work( j ) = stdlib_sasum( j-1, b( 1_ilp, j ), 1_ilp ) +stdlib_sasum( j-1, b( j+1, 1_ilp ),&
                               ldb )
                 end do loop_210
                 if( b( 1_ilp, 1_ilp )==zero .and. b( 2_ilp, 1_ilp )==zero )b( 1_ilp, 1_ilp ) = eps3
                 work( 1_ilp ) = zero
                 i1 = 1_ilp
                 i2 = n
                 i3 = 1_ilp
              end if
              loop_270: do its = 1, n
                 scale = one
                 vmax = one
                 vcrit = bignum
                 ! solve u*(xr,xi) = scale*(vr,vi) for a right eigenvector,
                   ! or u**t*(xr,xi) = scale*(vr,vi) for a left eigenvector,
                 ! overwriting (xr,xi) on (vr,vi).
                 loop_250: do i = i1, i2, i3
                    if( work( i )>vcrit ) then
                       rec = one / vmax
                       call stdlib_sscal( n, rec, vr, 1_ilp )
                       call stdlib_sscal( n, rec, vi, 1_ilp )
                       scale = scale*rec
                       vmax = one
                       vcrit = bignum
                    end if
                    xr = vr( i )
                    xi = vi( i )
                    if( rightv ) then
                       do j = i + 1, n
                          xr = xr - b( i, j )*vr( j ) + b( j+1, i )*vi( j )
                          xi = xi - b( i, j )*vi( j ) - b( j+1, i )*vr( j )
                       end do
                    else
                       do j = 1, i - 1
                          xr = xr - b( j, i )*vr( j ) + b( i+1, j )*vi( j )
                          xi = xi - b( j, i )*vi( j ) - b( i+1, j )*vr( j )
                       end do
                    end if
                    w = abs( b( i, i ) ) + abs( b( i+1, i ) )
                    if( w>smlnum ) then
                       if( w<one ) then
                          w1 = abs( xr ) + abs( xi )
                          if( w1>w*bignum ) then
                             rec = one / w1
                             call stdlib_sscal( n, rec, vr, 1_ilp )
                             call stdlib_sscal( n, rec, vi, 1_ilp )
                             xr = vr( i )
                             xi = vi( i )
                             scale = scale*rec
                             vmax = vmax*rec
                          end if
                       end if
                       ! divide by diagonal element of b.
                       call stdlib_sladiv( xr, xi, b( i, i ), b( i+1, i ), vr( i ),vi( i ) )
                                 
                       vmax = max( abs( vr( i ) )+abs( vi( i ) ), vmax )
                       vcrit = bignum / vmax
                    else
                       do j = 1, n
                          vr( j ) = zero
                          vi( j ) = zero
                       end do
                       vr( i ) = one
                       vi( i ) = one
                       scale = zero
                       vmax = one
                       vcrit = bignum
                    end if
                 end do loop_250
                 ! test for sufficient growth in the norm of (vr,vi).
                 vnorm = stdlib_sasum( n, vr, 1_ilp ) + stdlib_sasum( n, vi, 1_ilp )
                 if( vnorm>=growto*scale )go to 280
                 ! choose a new orthogonal starting vector and try again.
                 y = eps3 / ( rootn+one )
                 vr( 1_ilp ) = eps3
                 vi( 1_ilp ) = zero
                 do i = 2, n
                    vr( i ) = y
                    vi( i ) = zero
                 end do
                 vr( n-its+1 ) = vr( n-its+1 ) - eps3*rootn
              end do loop_270
              ! failure to find eigenvector in n iterations
              info = 1_ilp
              280 continue
              ! normalize eigenvector.
              vnorm = zero
              do i = 1, n
                 vnorm = max( vnorm, abs( vr( i ) )+abs( vi( i ) ) )
              end do
              call stdlib_sscal( n, one / vnorm, vr, 1_ilp )
              call stdlib_sscal( n, one / vnorm, vi, 1_ilp )
           end if
           return
     end subroutine stdlib_slaein

     pure module subroutine stdlib_dlaein( rightv, noinit, n, h, ldh, wr, wi, vr, vi, b,ldb, work, eps3, &
     !! DLAEIN uses inverse iteration to find a right or left eigenvector
     !! corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg
     !! matrix H.
               smlnum, bignum, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(dp), intent(in) :: bignum, eps3, smlnum, wi, wr
           ! Array Arguments 
           real(dp), intent(out) :: b(ldb,*), work(*)
           real(dp), intent(in) :: h(ldh,*)
           real(dp), intent(inout) :: vi(*), vr(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: tenth = 1.0e-1_dp
           
           ! Local Scalars 
           character :: normin, trans
           integer(ilp) :: i, i1, i2, i3, ierr, its, j
           real(dp) :: absbii, absbjj, ei, ej, growto, norm, nrmsml, rec, rootn, scale, temp, &
                     vcrit, vmax, vnorm, w, w1, x, xi, xr, y
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! growto is the threshold used in the acceptance test for an
           ! eigenvector.
           rootn = sqrt( real( n,KIND=dp) )
           growto = tenth / rootn
           nrmsml = max( one, eps3*rootn )*smlnum
           ! form b = h - (wr,wi)*i (except that the subdiagonal elements and
           ! the imaginary parts of the diagonal elements are not stored).
           do j = 1, n
              do i = 1, j - 1
                 b( i, j ) = h( i, j )
              end do
              b( j, j ) = h( j, j ) - wr
           end do
           if( wi==zero ) then
              ! real eigenvalue.
              if( noinit ) then
                 ! set initial vector.
                 do i = 1, n
                    vr( i ) = eps3
                 end do
              else
                 ! scale supplied initial vector.
                 vnorm = stdlib_dnrm2( n, vr, 1_ilp )
                 call stdlib_dscal( n, ( eps3*rootn ) / max( vnorm, nrmsml ), vr,1_ilp )
              end if
              if( rightv ) then
                 ! lu decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 do i = 1, n - 1
                    ei = h( i+1, i )
                    if( abs( b( i, i ) )<abs( ei ) ) then
                       ! interchange rows and eliminate.
                       x = b( i, i ) / ei
                       b( i, i ) = ei
                       do j = i + 1, n
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - x*temp
                          b( i, j ) = temp
                       end do
                    else
                       ! eliminate without interchange.
                       if( b( i, i )==zero )b( i, i ) = eps3
                       x = ei / b( i, i )
                       if( x/=zero ) then
                          do j = i + 1, n
                             b( i+1, j ) = b( i+1, j ) - x*b( i, j )
                          end do
                       end if
                    end if
                 end do
                 if( b( n, n )==zero )b( n, n ) = eps3
                 trans = 'N'
              else
                 ! ul decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 do j = n, 2, -1
                    ej = h( j, j-1 )
                    if( abs( b( j, j ) )<abs( ej ) ) then
                       ! interchange columns and eliminate.
                       x = b( j, j ) / ej
                       b( j, j ) = ej
                       do i = 1, j - 1
                          temp = b( i, j-1 )
                          b( i, j-1 ) = b( i, j ) - x*temp
                          b( i, j ) = temp
                       end do
                    else
                       ! eliminate without interchange.
                       if( b( j, j )==zero )b( j, j ) = eps3
                       x = ej / b( j, j )
                       if( x/=zero ) then
                          do i = 1, j - 1
                             b( i, j-1 ) = b( i, j-1 ) - x*b( i, j )
                          end do
                       end if
                    end if
                 end do
                 if( b( 1_ilp, 1_ilp )==zero )b( 1_ilp, 1_ilp ) = eps3
                 trans = 'T'
              end if
              normin = 'N'
              do its = 1, n
                 ! solve u*x = scale*v for a right eigenvector
                   ! or u**t*x = scale*v for a left eigenvector,
                 ! overwriting x on v.
                 call stdlib_dlatrs( 'UPPER', trans, 'NONUNIT', normin, n, b, ldb,vr, scale, work,&
                            ierr )
                 normin = 'Y'
                 ! test for sufficient growth in the norm of v.
                 vnorm = stdlib_dasum( n, vr, 1_ilp )
                 if( vnorm>=growto*scale )go to 120
                 ! choose new orthogonal starting vector and try again.
                 temp = eps3 / ( rootn+one )
                 vr( 1_ilp ) = eps3
                 do i = 2, n
                    vr( i ) = temp
                 end do
                 vr( n-its+1 ) = vr( n-its+1 ) - eps3*rootn
              end do
              ! failure to find eigenvector in n iterations.
              info = 1_ilp
              120 continue
              ! normalize eigenvector.
              i = stdlib_idamax( n, vr, 1_ilp )
              call stdlib_dscal( n, one / abs( vr( i ) ), vr, 1_ilp )
           else
              ! complex eigenvalue.
              if( noinit ) then
                 ! set initial vector.
                 do i = 1, n
                    vr( i ) = eps3
                    vi( i ) = zero
                 end do
              else
                 ! scale supplied initial vector.
                 norm = stdlib_dlapy2( stdlib_dnrm2( n, vr, 1_ilp ), stdlib_dnrm2( n, vi, 1_ilp ) )
                           
                 rec = ( eps3*rootn ) / max( norm, nrmsml )
                 call stdlib_dscal( n, rec, vr, 1_ilp )
                 call stdlib_dscal( n, rec, vi, 1_ilp )
              end if
              if( rightv ) then
                 ! lu decomposition with partial pivoting of b, replacing zero
                 ! pivots by eps3.
                 ! the imaginary part of the (i,j)-th element of u is stored in
                 ! b(j+1,i).
                 b( 2_ilp, 1_ilp ) = -wi
                 do i = 2, n
                    b( i+1, 1_ilp ) = zero
                 end do
                 loop_170: do i = 1, n - 1
                    absbii = stdlib_dlapy2( b( i, i ), b( i+1, i ) )
                    ei = h( i+1, i )
                    if( absbii<abs( ei ) ) then
                       ! interchange rows and eliminate.
                       xr = b( i, i ) / ei
                       xi = b( i+1, i ) / ei
                       b( i, i ) = ei
                       b( i+1, i ) = zero
                       do j = i + 1, n
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - xr*temp
                          b( j+1, i+1 ) = b( j+1, i ) - xi*temp
                          b( i, j ) = temp
                          b( j+1, i ) = zero
                       end do
                       b( i+2, i ) = -wi
                       b( i+1, i+1 ) = b( i+1, i+1 ) - xi*wi
                       b( i+2, i+1 ) = b( i+2, i+1 ) + xr*wi
                    else
                       ! eliminate without interchanging rows.
                       if( absbii==zero ) then
                          b( i, i ) = eps3
                          b( i+1, i ) = zero
                          absbii = eps3
                       end if
                       ei = ( ei / absbii ) / absbii
                       xr = b( i, i )*ei
                       xi = -b( i+1, i )*ei
                       do j = i + 1, n
                          b( i+1, j ) = b( i+1, j ) - xr*b( i, j ) +xi*b( j+1, i )
                          b( j+1, i+1 ) = -xr*b( j+1, i ) - xi*b( i, j )
                       end do
                       b( i+2, i+1 ) = b( i+2, i+1 ) - wi
                    end if
                    ! compute 1-norm of offdiagonal elements of i-th row.
                    work( i ) = stdlib_dasum( n-i, b( i, i+1 ), ldb ) +stdlib_dasum( n-i, b( i+2, &
                              i ), 1_ilp )
                 end do loop_170
                 if( b( n, n )==zero .and. b( n+1, n )==zero )b( n, n ) = eps3
                 work( n ) = zero
                 i1 = n
                 i2 = 1_ilp
                 i3 = -1_ilp
              else
                 ! ul decomposition with partial pivoting of conjg(b),
                 ! replacing zero pivots by eps3.
                 ! the imaginary part of the (i,j)-th element of u is stored in
                 ! b(j+1,i).
                 b( n+1, n ) = wi
                 do j = 1, n - 1
                    b( n+1, j ) = zero
                 end do
                 loop_210: do j = n, 2, -1
                    ej = h( j, j-1 )
                    absbjj = stdlib_dlapy2( b( j, j ), b( j+1, j ) )
                    if( absbjj<abs( ej ) ) then
                       ! interchange columns and eliminate
                       xr = b( j, j ) / ej
                       xi = b( j+1, j ) / ej
                       b( j, j ) = ej
                       b( j+1, j ) = zero
                       do i = 1, j - 1
                          temp = b( i, j-1 )
                          b( i, j-1 ) = b( i, j ) - xr*temp
                          b( j, i ) = b( j+1, i ) - xi*temp
                          b( i, j ) = temp
                          b( j+1, i ) = zero
                       end do
                       b( j+1, j-1 ) = wi
                       b( j-1, j-1 ) = b( j-1, j-1 ) + xi*wi
                       b( j, j-1 ) = b( j, j-1 ) - xr*wi
                    else
                       ! eliminate without interchange.
                       if( absbjj==zero ) then
                          b( j, j ) = eps3
                          b( j+1, j ) = zero
                          absbjj = eps3
                       end if
                       ej = ( ej / absbjj ) / absbjj
                       xr = b( j, j )*ej
                       xi = -b( j+1, j )*ej
                       do i = 1, j - 1
                          b( i, j-1 ) = b( i, j-1 ) - xr*b( i, j ) +xi*b( j+1, i )
                          b( j, i ) = -xr*b( j+1, i ) - xi*b( i, j )
                       end do
                       b( j, j-1 ) = b( j, j-1 ) + wi
                    end if
                    ! compute 1-norm of offdiagonal elements of j-th column.
                    work( j ) = stdlib_dasum( j-1, b( 1_ilp, j ), 1_ilp ) +stdlib_dasum( j-1, b( j+1, 1_ilp ),&
                               ldb )
                 end do loop_210
                 if( b( 1_ilp, 1_ilp )==zero .and. b( 2_ilp, 1_ilp )==zero )b( 1_ilp, 1_ilp ) = eps3
                 work( 1_ilp ) = zero
                 i1 = 1_ilp
                 i2 = n
                 i3 = 1_ilp
              end if
              loop_270: do its = 1, n
                 scale = one
                 vmax = one
                 vcrit = bignum
                 ! solve u*(xr,xi) = scale*(vr,vi) for a right eigenvector,
                   ! or u**t*(xr,xi) = scale*(vr,vi) for a left eigenvector,
                 ! overwriting (xr,xi) on (vr,vi).
                 loop_250: do i = i1, i2, i3
                    if( work( i )>vcrit ) then
                       rec = one / vmax
                       call stdlib_dscal( n, rec, vr, 1_ilp )
                       call stdlib_dscal( n, rec, vi, 1_ilp )
                       scale = scale*rec
                       vmax = one
                       vcrit = bignum
                    end if
                    xr = vr( i )
                    xi = vi( i )
                    if( rightv ) then
                       do j = i + 1, n
                          xr = xr - b( i, j )*vr( j ) + b( j+1, i )*vi( j )
                          xi = xi - b( i, j )*vi( j ) - b( j+1, i )*vr( j )
                       end do
                    else
                       do j = 1, i - 1
                          xr = xr - b( j, i )*vr( j ) + b( i+1, j )*vi( j )
                          xi = xi - b( j, i )*vi( j ) - b( i+1, j )*vr( j )
                       end do
                    end if
                    w = abs( b( i, i ) ) + abs( b( i+1, i ) )
                    if( w>smlnum ) then
                       if( w<one ) then
                          w1 = abs( xr ) + abs( xi )
                          if( w1>w*bignum ) then
                             rec = one / w1
                             call stdlib_dscal( n, rec, vr, 1_ilp )
                             call stdlib_dscal( n, rec, vi, 1_ilp )
                             xr = vr( i )
                             xi = vi( i )
                             scale = scale*rec
                             vmax = vmax*rec
                          end if
                       end if
                       ! divide by diagonal element of b.
                       call stdlib_dladiv( xr, xi, b( i, i ), b( i+1, i ), vr( i ),vi( i ) )
                                 
                       vmax = max( abs( vr( i ) )+abs( vi( i ) ), vmax )
                       vcrit = bignum / vmax
                    else
                       do j = 1, n
                          vr( j ) = zero
                          vi( j ) = zero
                       end do
                       vr( i ) = one
                       vi( i ) = one
                       scale = zero
                       vmax = one
                       vcrit = bignum
                    end if
                 end do loop_250
                 ! test for sufficient growth in the norm of (vr,vi).
                 vnorm = stdlib_dasum( n, vr, 1_ilp ) + stdlib_dasum( n, vi, 1_ilp )
                 if( vnorm>=growto*scale )go to 280
                 ! choose a new orthogonal starting vector and try again.
                 y = eps3 / ( rootn+one )
                 vr( 1_ilp ) = eps3
                 vi( 1_ilp ) = zero
                 do i = 2, n
                    vr( i ) = y
                    vi( i ) = zero
                 end do
                 vr( n-its+1 ) = vr( n-its+1 ) - eps3*rootn
              end do loop_270
              ! failure to find eigenvector in n iterations
              info = 1_ilp
              280 continue
              ! normalize eigenvector.
              vnorm = zero
              do i = 1, n
                 vnorm = max( vnorm, abs( vr( i ) )+abs( vi( i ) ) )
              end do
              call stdlib_dscal( n, one / vnorm, vr, 1_ilp )
              call stdlib_dscal( n, one / vnorm, vi, 1_ilp )
           end if
           return
     end subroutine stdlib_dlaein


     pure module subroutine stdlib_claein( rightv, noinit, n, h, ldh, w, v, b, ldb, rwork,eps3, smlnum, &
     !! CLAEIN uses inverse iteration to find a right or left eigenvector
     !! corresponding to the eigenvalue W of a complex upper Hessenberg
     !! matrix H.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(sp), intent(in) :: eps3, smlnum
           complex(sp), intent(in) :: w
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: b(ldb,*)
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(inout) :: v(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: tenth = 1.0e-1_sp
           
           
           ! Local Scalars 
           character :: normin, trans
           integer(ilp) :: i, ierr, its, j
           real(sp) :: growto, nrmsml, rootn, rtemp, scale, vnorm
           complex(sp) :: cdum, ei, ej, temp, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! growto is the threshold used in the acceptance test for an
           ! eigenvector.
           rootn = sqrt( real( n,KIND=sp) )
           growto = tenth / rootn
           nrmsml = max( one, eps3*rootn )*smlnum
           ! form b = h - w*i (except that the subdiagonal elements are not
           ! stored).
           do j = 1, n
              do i = 1, j - 1
                 b( i, j ) = h( i, j )
              end do
              b( j, j ) = h( j, j ) - w
           end do
           if( noinit ) then
              ! initialize v.
              do i = 1, n
                 v( i ) = eps3
              end do
           else
              ! scale supplied initial vector.
              vnorm = stdlib_scnrm2( n, v, 1_ilp )
              call stdlib_csscal( n, ( eps3*rootn ) / max( vnorm, nrmsml ), v, 1_ilp )
           end if
           if( rightv ) then
              ! lu decomposition with partial pivoting of b, replacing czero
              ! pivots by eps3.
              do i = 1, n - 1
                 ei = h( i+1, i )
                 if( cabs1( b( i, i ) )<cabs1( ei ) ) then
                    ! interchange rows and eliminate.
                    x = stdlib_cladiv( b( i, i ), ei )
                    b( i, i ) = ei
                    do j = i + 1, n
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - x*temp
                       b( i, j ) = temp
                    end do
                 else
                    ! eliminate without interchange.
                    if( b( i, i )==czero )b( i, i ) = eps3
                    x = stdlib_cladiv( ei, b( i, i ) )
                    if( x/=czero ) then
                       do j = i + 1, n
                          b( i+1, j ) = b( i+1, j ) - x*b( i, j )
                       end do
                    end if
                 end if
              end do
              if( b( n, n )==czero )b( n, n ) = eps3
              trans = 'N'
           else
              ! ul decomposition with partial pivoting of b, replacing czero
              ! pivots by eps3.
              do j = n, 2, -1
                 ej = h( j, j-1 )
                 if( cabs1( b( j, j ) )<cabs1( ej ) ) then
                    ! interchange columns and eliminate.
                    x = stdlib_cladiv( b( j, j ), ej )
                    b( j, j ) = ej
                    do i = 1, j - 1
                       temp = b( i, j-1 )
                       b( i, j-1 ) = b( i, j ) - x*temp
                       b( i, j ) = temp
                    end do
                 else
                    ! eliminate without interchange.
                    if( b( j, j )==czero )b( j, j ) = eps3
                    x = stdlib_cladiv( ej, b( j, j ) )
                    if( x/=czero ) then
                       do i = 1, j - 1
                          b( i, j-1 ) = b( i, j-1 ) - x*b( i, j )
                       end do
                    end if
                 end if
              end do
              if( b( 1_ilp, 1_ilp )==czero )b( 1_ilp, 1_ilp ) = eps3
              trans = 'C'
           end if
           normin = 'N'
           do its = 1, n
              ! solve u*x = scale*v for a right eigenvector
                ! or u**h *x = scale*v for a left eigenvector,
              ! overwriting x on v.
              call stdlib_clatrs( 'UPPER', trans, 'NONUNIT', normin, n, b, ldb, v,scale, rwork, &
                        ierr )
              normin = 'Y'
              ! test for sufficient growth in the norm of v.
              vnorm = stdlib_scasum( n, v, 1_ilp )
              if( vnorm>=growto*scale )go to 120
              ! choose new orthogonal starting vector and try again.
              rtemp = eps3 / ( rootn+one )
              v( 1_ilp ) = eps3
              do i = 2, n
                 v( i ) = rtemp
              end do
              v( n-its+1 ) = v( n-its+1 ) - eps3*rootn
           end do
           ! failure to find eigenvector in n iterations.
           info = 1_ilp
           120 continue
           ! normalize eigenvector.
           i = stdlib_icamax( n, v, 1_ilp )
           call stdlib_csscal( n, one / cabs1( v( i ) ), v, 1_ilp )
           return
     end subroutine stdlib_claein

     pure module subroutine stdlib_zlaein( rightv, noinit, n, h, ldh, w, v, b, ldb, rwork,eps3, smlnum, &
     !! ZLAEIN uses inverse iteration to find a right or left eigenvector
     !! corresponding to the eigenvalue W of a complex upper Hessenberg
     !! matrix H.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(dp), intent(in) :: eps3, smlnum
           complex(dp), intent(in) :: w
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: b(ldb,*)
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(inout) :: v(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: tenth = 1.0e-1_dp
           
           
           ! Local Scalars 
           character :: normin, trans
           integer(ilp) :: i, ierr, its, j
           real(dp) :: growto, nrmsml, rootn, rtemp, scale, vnorm
           complex(dp) :: cdum, ei, ej, temp, x
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! growto is the threshold used in the acceptance test for an
           ! eigenvector.
           rootn = sqrt( real( n,KIND=dp) )
           growto = tenth / rootn
           nrmsml = max( one, eps3*rootn )*smlnum
           ! form b = h - w*i (except that the subdiagonal elements are not
           ! stored).
           do j = 1, n
              do i = 1, j - 1
                 b( i, j ) = h( i, j )
              end do
              b( j, j ) = h( j, j ) - w
           end do
           if( noinit ) then
              ! initialize v.
              do i = 1, n
                 v( i ) = eps3
              end do
           else
              ! scale supplied initial vector.
              vnorm = stdlib_dznrm2( n, v, 1_ilp )
              call stdlib_zdscal( n, ( eps3*rootn ) / max( vnorm, nrmsml ), v, 1_ilp )
           end if
           if( rightv ) then
              ! lu decomposition with partial pivoting of b, replacing czero
              ! pivots by eps3.
              do i = 1, n - 1
                 ei = h( i+1, i )
                 if( cabs1( b( i, i ) )<cabs1( ei ) ) then
                    ! interchange rows and eliminate.
                    x = stdlib_zladiv( b( i, i ), ei )
                    b( i, i ) = ei
                    do j = i + 1, n
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - x*temp
                       b( i, j ) = temp
                    end do
                 else
                    ! eliminate without interchange.
                    if( b( i, i )==czero )b( i, i ) = eps3
                    x = stdlib_zladiv( ei, b( i, i ) )
                    if( x/=czero ) then
                       do j = i + 1, n
                          b( i+1, j ) = b( i+1, j ) - x*b( i, j )
                       end do
                    end if
                 end if
              end do
              if( b( n, n )==czero )b( n, n ) = eps3
              trans = 'N'
           else
              ! ul decomposition with partial pivoting of b, replacing czero
              ! pivots by eps3.
              do j = n, 2, -1
                 ej = h( j, j-1 )
                 if( cabs1( b( j, j ) )<cabs1( ej ) ) then
                    ! interchange columns and eliminate.
                    x = stdlib_zladiv( b( j, j ), ej )
                    b( j, j ) = ej
                    do i = 1, j - 1
                       temp = b( i, j-1 )
                       b( i, j-1 ) = b( i, j ) - x*temp
                       b( i, j ) = temp
                    end do
                 else
                    ! eliminate without interchange.
                    if( b( j, j )==czero )b( j, j ) = eps3
                    x = stdlib_zladiv( ej, b( j, j ) )
                    if( x/=czero ) then
                       do i = 1, j - 1
                          b( i, j-1 ) = b( i, j-1 ) - x*b( i, j )
                       end do
                    end if
                 end if
              end do
              if( b( 1_ilp, 1_ilp )==czero )b( 1_ilp, 1_ilp ) = eps3
              trans = 'C'
           end if
           normin = 'N'
           do its = 1, n
              ! solve u*x = scale*v for a right eigenvector
                ! or u**h *x = scale*v for a left eigenvector,
              ! overwriting x on v.
              call stdlib_zlatrs( 'UPPER', trans, 'NONUNIT', normin, n, b, ldb, v,scale, rwork, &
                        ierr )
              normin = 'Y'
              ! test for sufficient growth in the norm of v.
              vnorm = stdlib_dzasum( n, v, 1_ilp )
              if( vnorm>=growto*scale )go to 120
              ! choose new orthogonal starting vector and try again.
              rtemp = eps3 / ( rootn+one )
              v( 1_ilp ) = eps3
              do i = 2, n
                 v( i ) = rtemp
              end do
              v( n-its+1 ) = v( n-its+1 ) - eps3*rootn
           end do
           ! failure to find eigenvector in n iterations.
           info = 1_ilp
           120 continue
           ! normalize eigenvector.
           i = stdlib_izamax( n, v, 1_ilp )
           call stdlib_zdscal( n, one / cabs1( v( i ) ), v, 1_ilp )
           return
     end subroutine stdlib_zlaein



end submodule stdlib_lapack_eigv_gen2
