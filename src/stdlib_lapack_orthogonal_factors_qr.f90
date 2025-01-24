submodule(stdlib_lapack_orthogonal_factors) stdlib_lapack_orthogonal_factors_qr
  implicit none


  contains

     pure module subroutine stdlib_sgeqr( m, n, a, lda, t, tsize, work, lwork,info )
     !! SGEQR computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, lminws, mint, minw
           integer(ilp) :: mb, nb, mintsz, nblcks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( tsize==-1_ilp .or. tsize==-2_ilp .or.lwork==-1_ilp .or. lwork==-2_ilp )
           mint = .false.
           minw = .false.
           if( tsize==-2_ilp .or. lwork==-2_ilp ) then
             if( tsize/=-1_ilp ) mint = .true.
             if( lwork/=-1_ilp ) minw = .true.
           end if
           ! determine the block size
           if( min( m, n )>0_ilp ) then
             mb = stdlib_ilaenv( 1_ilp, 'SGEQR ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'SGEQR ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = m
             nb = 1_ilp
           end if
           if( mb>m .or. mb<=n ) mb = m
           if( nb>min( m, n ) .or. nb<1_ilp ) nb = 1_ilp
           mintsz = n + 5_ilp
           if ( mb>n .and. m>n ) then
             if( mod( m - n, mb - n )==0_ilp ) then
               nblcks = ( m - n ) / ( mb - n )
             else
               nblcks = ( m - n ) / ( mb - n ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           lminws = .false.
           if( ( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) .or. lwork<nb*n ).and. ( lwork>=n ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) ) then
               lminws = .true.
               nb = 1_ilp
               mb = m
             end if
             if( lwork<nb*n ) then
               lminws = .true.
               nb = 1_ilp
             end if
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<max( 1_ilp, n*nb ) ) .and. ( .not.lquery ).and. ( .not.lminws ) ) &
                     then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = nb*n*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = max( 1_ilp, n )
             else
               work( 1_ilp ) = max( 1_ilp, nb*n )
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SGEQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the qr decomposition
           if( ( m<=n ) .or. ( mb<=n ) .or. ( mb>=m ) ) then
             call stdlib_sgeqrt( m, n, nb, a, lda, t( 6_ilp ), nb, work, info )
           else
             call stdlib_slatsqr( m, n, mb, nb, a, lda, t( 6_ilp ), nb, work,lwork, info )
           end if
           work( 1_ilp ) = max( 1_ilp, nb*n )
           return
     end subroutine stdlib_sgeqr

     pure module subroutine stdlib_dgeqr( m, n, a, lda, t, tsize, work, lwork,info )
     !! DGEQR computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, lminws, mint, minw
           integer(ilp) :: mb, nb, mintsz, nblcks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( tsize==-1_ilp .or. tsize==-2_ilp .or.lwork==-1_ilp .or. lwork==-2_ilp )
           mint = .false.
           minw = .false.
           if( tsize==-2_ilp .or. lwork==-2_ilp ) then
             if( tsize/=-1_ilp ) mint = .true.
             if( lwork/=-1_ilp ) minw = .true.
           end if
           ! determine the block size
           if( min( m, n )>0_ilp ) then
             mb = stdlib_ilaenv( 1_ilp, 'DGEQR ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'DGEQR ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = m
             nb = 1_ilp
           end if
           if( mb>m .or. mb<=n ) mb = m
           if( nb>min( m, n ) .or. nb<1_ilp ) nb = 1_ilp
           mintsz = n + 5_ilp
           if( mb>n .and. m>n ) then
             if( mod( m - n, mb - n )==0_ilp ) then
               nblcks = ( m - n ) / ( mb - n )
             else
               nblcks = ( m - n ) / ( mb - n ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           lminws = .false.
           if( ( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) .or. lwork<nb*n ).and. ( lwork>=n ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) ) then
               lminws = .true.
               nb = 1_ilp
               mb = m
             end if
             if( lwork<nb*n ) then
               lminws = .true.
               nb = 1_ilp
             end if
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<max( 1_ilp, n*nb ) ) .and. ( .not.lquery ).and. ( .not.lminws ) ) &
                     then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = nb*n*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = max( 1_ilp, n )
             else
               work( 1_ilp ) = max( 1_ilp, nb*n )
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DGEQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the qr decomposition
           if( ( m<=n ) .or. ( mb<=n ) .or. ( mb>=m ) ) then
             call stdlib_dgeqrt( m, n, nb, a, lda, t( 6_ilp ), nb, work, info )
           else
             call stdlib_dlatsqr( m, n, mb, nb, a, lda, t( 6_ilp ), nb, work,lwork, info )
           end if
           work( 1_ilp ) = max( 1_ilp, nb*n )
           return
     end subroutine stdlib_dgeqr


     pure module subroutine stdlib_cgeqr( m, n, a, lda, t, tsize, work, lwork,info )
     !! CGEQR computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, lminws, mint, minw
           integer(ilp) :: mb, nb, mintsz, nblcks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( tsize==-1_ilp .or. tsize==-2_ilp .or.lwork==-1_ilp .or. lwork==-2_ilp )
           mint = .false.
           minw = .false.
           if( tsize==-2_ilp .or. lwork==-2_ilp ) then
             if( tsize/=-1_ilp ) mint = .true.
             if( lwork/=-1_ilp ) minw = .true.
           end if
           ! determine the block size
           if( min( m, n )>0_ilp ) then
             mb = stdlib_ilaenv( 1_ilp, 'CGEQR ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'CGEQR ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = m
             nb = 1_ilp
           end if
           if( mb>m .or. mb<=n ) mb = m
           if( nb>min( m, n ) .or. nb<1_ilp ) nb = 1_ilp
           mintsz = n + 5_ilp
           if( mb>n .and. m>n ) then
             if( mod( m - n, mb - n )==0_ilp ) then
               nblcks = ( m - n ) / ( mb - n )
             else
               nblcks = ( m - n ) / ( mb - n ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           lminws = .false.
           if( ( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) .or. lwork<nb*n ).and. ( lwork>=n ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) ) then
               lminws = .true.
               nb = 1_ilp
               mb = m
             end if
             if( lwork<nb*n ) then
               lminws = .true.
               nb = 1_ilp
             end if
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<max( 1_ilp, n*nb ) ) .and. ( .not.lquery ).and. ( .not.lminws ) ) &
                     then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = nb*n*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = max( 1_ilp, n )
             else
               work( 1_ilp ) = max( 1_ilp, nb*n )
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CGEQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the qr decomposition
           if( ( m<=n ) .or. ( mb<=n ) .or. ( mb>=m ) ) then
             call stdlib_cgeqrt( m, n, nb, a, lda, t( 6_ilp ), nb, work, info )
           else
             call stdlib_clatsqr( m, n, mb, nb, a, lda, t( 6_ilp ), nb, work,lwork, info )
           end if
           work( 1_ilp ) = max( 1_ilp, nb*n )
           return
     end subroutine stdlib_cgeqr

     pure module subroutine stdlib_zgeqr( m, n, a, lda, t, tsize, work, lwork,info )
     !! ZGEQR computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, lminws, mint, minw
           integer(ilp) :: mb, nb, mintsz, nblcks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( tsize==-1_ilp .or. tsize==-2_ilp .or.lwork==-1_ilp .or. lwork==-2_ilp )
           mint = .false.
           minw = .false.
           if( tsize==-2_ilp .or. lwork==-2_ilp ) then
             if( tsize/=-1_ilp ) mint = .true.
             if( lwork/=-1_ilp ) minw = .true.
           end if
           ! determine the block size
           if( min ( m, n )>0_ilp ) then
             mb = stdlib_ilaenv( 1_ilp, 'ZGEQR ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'ZGEQR ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = m
             nb = 1_ilp
           end if
           if( mb>m .or. mb<=n ) mb = m
           if( nb>min( m, n ) .or. nb<1_ilp ) nb = 1_ilp
           mintsz = n + 5_ilp
           if( mb>n .and. m>n ) then
             if( mod( m - n, mb - n )==0_ilp ) then
               nblcks = ( m - n ) / ( mb - n )
             else
               nblcks = ( m - n ) / ( mb - n ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           lminws = .false.
           if( ( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) .or. lwork<nb*n ).and. ( lwork>=n ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ) ) then
               lminws = .true.
               nb = 1_ilp
               mb = m
             end if
             if( lwork<nb*n ) then
               lminws = .true.
               nb = 1_ilp
             end if
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, nb*n*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<max( 1_ilp, n*nb ) ) .and. ( .not.lquery ).and. ( .not.lminws ) ) &
                     then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = nb*n*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = max( 1_ilp, n )
             else
               work( 1_ilp ) = max( 1_ilp, nb*n )
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZGEQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the qr decomposition
           if( ( m<=n ) .or. ( mb<=n ) .or. ( mb>=m ) ) then
             call stdlib_zgeqrt( m, n, nb, a, lda, t( 6_ilp ), nb, work, info )
           else
             call stdlib_zlatsqr( m, n, mb, nb, a, lda, t( 6_ilp ), nb, work,lwork, info )
           end if
           work( 1_ilp ) = max( 1_ilp, nb*n )
           return
     end subroutine stdlib_zgeqr




     pure module subroutine stdlib_sgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! SGEMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (SGEQR)
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), t(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: mb, nb, lw, nblcks, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork==-1_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           mb = int( t( 2_ilp ),KIND=ilp)
           nb = int( t( 3_ilp ),KIND=ilp)
           if( left ) then
             lw = n * nb
             mn = m
           else
             lw = mb * nb
             mn = n
           end if
           if( ( mb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, mb - k )==0_ilp ) then
               nblcks = ( mn - k ) / ( mb - k )
             else
               nblcks = ( mn - k ) / ( mb - k ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
             info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
             info = -2_ilp
           else if( m<0_ilp ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp .or. k>mn ) then
             info = -5_ilp
           else if( lda<max( 1_ilp, mn ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SGEMQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( mb<=k ) .or. ( mb>=max( m, n, &
                     k ) ) ) then
             call stdlib_sgemqrt( side, trans, m, n, k, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, info &
                       )
           else
             call stdlib_slamtsqr( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_sgemqr

     pure module subroutine stdlib_dgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! DGEMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (DGEQR)
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), t(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: mb, nb, lw, nblcks, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork==-1_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           mb = int( t( 2_ilp ),KIND=ilp)
           nb = int( t( 3_ilp ),KIND=ilp)
           if( left ) then
             lw = n * nb
             mn = m
           else
             lw = mb * nb
             mn = n
           end if
           if( ( mb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, mb - k )==0_ilp ) then
               nblcks = ( mn - k ) / ( mb - k )
             else
               nblcks = ( mn - k ) / ( mb - k ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
             info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
             info = -2_ilp
           else if( m<0_ilp ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp .or. k>mn ) then
             info = -5_ilp
           else if( lda<max( 1_ilp, mn ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DGEMQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( mb<=k ) .or. ( mb>=max( m, n, &
                     k ) ) ) then
             call stdlib_dgemqrt( side, trans, m, n, k, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, info &
                       )
           else
             call stdlib_dlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_dgemqr


     pure module subroutine stdlib_cgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! CGEMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (CGEQR)
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), t(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: mb, nb, lw, nblcks, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork==-1_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           mb = int( t( 2_ilp ),KIND=ilp)
           nb = int( t( 3_ilp ),KIND=ilp)
           if( left ) then
             lw = n * nb
             mn = m
           else
             lw = mb * nb
             mn = n
           end if
           if( ( mb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, mb - k )==0_ilp ) then
               nblcks = ( mn - k ) / ( mb - k )
             else
               nblcks = ( mn - k ) / ( mb - k ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
             info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
             info = -2_ilp
           else if( m<0_ilp ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp .or. k>mn ) then
             info = -5_ilp
           else if( lda<max( 1_ilp, mn ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CGEMQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( mb<=k ) .or. ( mb>=max( m, n, &
                     k ) ) ) then
             call stdlib_cgemqrt( side, trans, m, n, k, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, info &
                       )
           else
             call stdlib_clamtsqr( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_cgemqr

     pure module subroutine stdlib_zgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! ZGEMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (ZGEQR)
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), t(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: mb, nb, lw, nblcks, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork==-1_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           mb = int( t( 2_ilp ),KIND=ilp)
           nb = int( t( 3_ilp ),KIND=ilp)
           if( left ) then
             lw = n * nb
             mn = m
           else
             lw = mb * nb
             mn = n
           end if
           if( ( mb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, mb - k )==0_ilp ) then
               nblcks = ( mn - k ) / ( mb - k )
             else
               nblcks = ( mn - k ) / ( mb - k ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
             info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
             info = -2_ilp
           else if( m<0_ilp ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp .or. k>mn ) then
             info = -5_ilp
           else if( lda<max( 1_ilp, mn ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZGEMQR', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( mb<=k ) .or. ( mb>=max( m, n, &
                     k ) ) ) then
             call stdlib_zgemqrt( side, trans, m, n, k, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, info &
                       )
           else
             call stdlib_zlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),nb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_zgemqr




     pure module subroutine stdlib_sgeqrf( m, n, a, lda, tau, work, lwork, info )
     !! SGEQRF computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           k = min( m, n )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( .not.lquery ) then
              if( lwork<=0_ilp .or. ( m>0_ilp .and. lwork<max( 1_ilp, n ) ) )info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQRF', -info )
              return
           else if( lquery ) then
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_sgeqr2( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_slarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**t to a(i:m,i+ib:n) from the left
                    call stdlib_slarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-i-&
                    ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), ldwork &
                              )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_sgeqr2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgeqrf

     pure module subroutine stdlib_dgeqrf( m, n, a, lda, tau, work, lwork, info )
     !! DGEQRF computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           k = min( m, n )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( .not.lquery ) then
              if( lwork<=0_ilp .or. ( m>0_ilp .and. lwork<max( 1_ilp, n ) ) )info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQRF', -info )
              return
           else if( lquery ) then
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_dgeqr2( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_dlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**t to a(i:m,i+ib:n) from the left
                    call stdlib_dlarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-i-&
                    ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), ldwork &
                              )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_dgeqr2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgeqrf


     pure module subroutine stdlib_cgeqrf( m, n, a, lda, tau, work, lwork, info )
     !! CGEQRF computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           k = min( m, n )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( .not.lquery ) then
              if( lwork<=0_ilp .or. ( m>0_ilp .and. lwork<max( 1_ilp, n ) ) )info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQRF', -info )
              return
           else if( lquery ) then
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_cgeqr2( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_clarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**h to a(i:m,i+ib:n) from the left
                    call stdlib_clarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE', m-&
                    i+1, n-i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 )&
                              , ldwork )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_cgeqr2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgeqrf

     pure module subroutine stdlib_zgeqrf( m, n, a, lda, tau, work, lwork, info )
     !! ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           k = min( m, n )
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( .not.lquery ) then
              if( lwork<=0_ilp .or. ( m>0_ilp .and. lwork<max( 1_ilp, n ) ) )info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQRF', -info )
              return
           else if( lquery ) then
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_zgeqr2( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_zlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**h to a(i:m,i+ib:n) from the left
                    call stdlib_zlarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE', m-&
                    i+1, n-i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 )&
                              , ldwork )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_zgeqr2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgeqrf




     pure module subroutine stdlib_sgeqr2( m, n, a, lda, tau, work, info )
     !! SGEQR2 computes a QR factorization of a real m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQR2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_slarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_slarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_sgeqr2

     pure module subroutine stdlib_dgeqr2( m, n, a, lda, tau, work, info )
     !! DGEQR2 computes a QR factorization of a real m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQR2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_dlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_dlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_dgeqr2


     pure module subroutine stdlib_cgeqr2( m, n, a, lda, tau, work, info )
     !! CGEQR2 computes a QR factorization of a complex m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQR2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_clarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 alpha = a( i, i )
                 a( i, i ) = cone
                 call stdlib_clarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tau( i ) ), a( i, i+1 &
                           ), lda, work )
                 a( i, i ) = alpha
              end if
           end do
           return
     end subroutine stdlib_cgeqr2

     pure module subroutine stdlib_zgeqr2( m, n, a, lda, tau, work, info )
     !! ZGEQR2 computes a QR factorization of a complex m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQR2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_zlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 alpha = a( i, i )
                 a( i, i ) = cone
                 call stdlib_zlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tau( i ) ), a( i, i+1 &
                           ), lda, work )
                 a( i, i ) = alpha
              end if
           end do
           return
     end subroutine stdlib_zgeqr2




     pure module subroutine stdlib_cungqr( m, n, k, a, lda, tau, work, lwork, info )
     !! CUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
     !! which is defined as the first N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by CGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, j, ki, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, n )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CUNGQR', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNGQR', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(1:kk,kk+1:n) to czero.
              do j = kk + 1, n
                 do i = 1, kk
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<n )call stdlib_cung2r( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_clarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h to a(i:m,i+ib:n) from the left
                    call stdlib_clarfb( 'LEFT', 'NO TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-&
                    i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h to rows i:m of current block
                 call stdlib_cung2r( m-i+1, ib, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set rows 1:i-1 of current block to czero
                 do j = i, i + ib - 1
                    do l = 1, i - 1
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cungqr

     pure module subroutine stdlib_zungqr( m, n, k, a, lda, tau, work, lwork, info )
     !! ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
     !! which is defined as the first N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by ZGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, j, ki, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, n )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZUNGQR', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNGQR', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(1:kk,kk+1:n) to czero.
              do j = kk + 1, n
                 do i = 1, kk
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<n )call stdlib_zung2r( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_zlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h to a(i:m,i+ib:n) from the left
                    call stdlib_zlarfb( 'LEFT', 'NO TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-&
                    i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h to rows i:m of current block
                 call stdlib_zung2r( m-i+1, ib, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set rows 1:i-1 of current block to czero
                 do j = i, i + ib - 1
                    do l = 1, i - 1
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zungqr




     pure module subroutine stdlib_cung2r( m, n, k, a, lda, tau, work, info )
     !! CUNG2R generates an m by n complex matrix Q with orthonormal columns,
     !! which is defined as the first n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by CGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNG2R', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns k+1:n to columns of the unit matrix
           do j = k + 1, n
              do l = 1, m
                 a( l, j ) = czero
              end do
              a( j, j ) = cone
           end do
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the left
              if( i<n ) then
                 a( i, i ) = cone
                 call stdlib_clarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
              end if
              if( i<m )call stdlib_cscal( m-i, -tau( i ), a( i+1, i ), 1_ilp )
              a( i, i ) = cone - tau( i )
              ! set a(1:i-1,i) to czero
              do l = 1, i - 1
                 a( l, i ) = czero
              end do
           end do
           return
     end subroutine stdlib_cung2r

     pure module subroutine stdlib_zung2r( m, n, k, a, lda, tau, work, info )
     !! ZUNG2R generates an m by n complex matrix Q with orthonormal columns,
     !! which is defined as the first n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by ZGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNG2R', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns k+1:n to columns of the unit matrix
           do j = k + 1, n
              do l = 1, m
                 a( l, j ) = czero
              end do
              a( j, j ) = cone
           end do
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the left
              if( i<n ) then
                 a( i, i ) = cone
                 call stdlib_zlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
              end if
              if( i<m )call stdlib_zscal( m-i, -tau( i ), a( i+1, i ), 1_ilp )
              a( i, i ) = cone - tau( i )
              ! set a(1:i-1,i) to czero
              do l = 1, i - 1
                 a( l, i ) = czero
              end do
           end do
           return
     end subroutine stdlib_zung2r




     pure module subroutine stdlib_cunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! CUNMQR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CGEQRF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, jc, ldwork, lwkopt, mi, nb, nbmin, &
                     ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNMQR', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_cunm2r( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_clarft( 'FORWARD', 'COLUMNWISE', nq-i+1, ib, a( i, i ),lda, tau( i ),&
                            work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp
                    ic = i
                 else
                    ! h or h**h is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp
                    jc = i
                 end if
                 ! apply h or h**h
                 call stdlib_clarfb( side, trans, 'FORWARD', 'COLUMNWISE', mi, ni,ib, a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmqr

     pure module subroutine stdlib_zunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! ZUNMQR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZGEQRF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, jc, ldwork, lwkopt, mi, nb, nbmin, &
                     ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNMQR', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_zunm2r( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_zlarft( 'FORWARD', 'COLUMNWISE', nq-i+1, ib, a( i, i ),lda, tau( i ),&
                            work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp
                    ic = i
                 else
                    ! h or h**h is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp
                    jc = i
                 end if
                 ! apply h or h**h
                 call stdlib_zlarfb( side, trans, 'FORWARD', 'COLUMNWISE', mi, ni,ib, a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmqr




     pure module subroutine stdlib_cunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! CUNM2R overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CGEQRF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, jc, mi, ni, nq
           complex(sp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNM2R', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
              jc = 1_ilp
           else
              mi = m
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              aii = a( i, i )
              a( i, i ) = cone
              call stdlib_clarf( side, mi, ni, a( i, i ), 1_ilp, taui, c( ic, jc ), ldc,work )
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_cunm2r

     pure module subroutine stdlib_zunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! ZUNM2R overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZGEQRF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, jc, mi, ni, nq
           complex(dp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNM2R', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
              jc = 1_ilp
           else
              mi = m
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              aii = a( i, i )
              a( i, i ) = cone
              call stdlib_zlarf( side, mi, ni, a( i, i ), 1_ilp, taui, c( ic, jc ), ldc,work )
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_zunm2r




     pure module subroutine stdlib_sorgqr( m, n, k, a, lda, tau, work, lwork, info )
     !! SORGQR generates an M-by-N real matrix Q with orthonormal columns,
     !! which is defined as the first N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by SGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, j, ki, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, n )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SORGQR', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORGQR', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(1:kk,kk+1:n) to zero.
              do j = kk + 1, n
                 do i = 1, kk
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<n )call stdlib_sorg2r( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_slarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h to a(i:m,i+ib:n) from the left
                    call stdlib_slarfb( 'LEFT', 'NO TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-&
                    i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h to rows i:m of current block
                 call stdlib_sorg2r( m-i+1, ib, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set rows 1:i-1 of current block to zero
                 do j = i, i + ib - 1
                    do l = 1, i - 1
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sorgqr

     pure module subroutine stdlib_dorgqr( m, n, k, a, lda, tau, work, lwork, info )
     !! DORGQR generates an M-by-N real matrix Q with orthonormal columns,
     !! which is defined as the first N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by DGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, j, ki, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, n )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DORGQR', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORGQR', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(1:kk,kk+1:n) to zero.
              do j = kk + 1, n
                 do i = 1, kk
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<n )call stdlib_dorg2r( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_dlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h to a(i:m,i+ib:n) from the left
                    call stdlib_dlarfb( 'LEFT', 'NO TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-&
                    i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h to rows i:m of current block
                 call stdlib_dorg2r( m-i+1, ib, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set rows 1:i-1 of current block to zero
                 do j = i, i + ib - 1
                    do l = 1, i - 1
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dorgqr




     pure module subroutine stdlib_sorg2r( m, n, k, a, lda, tau, work, info )
     !! SORG2R generates an m by n real matrix Q with orthonormal columns,
     !! which is defined as the first n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by SGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORG2R', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns k+1:n to columns of the unit matrix
           do j = k + 1, n
              do l = 1, m
                 a( l, j ) = zero
              end do
              a( j, j ) = one
           end do
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the left
              if( i<n ) then
                 a( i, i ) = one
                 call stdlib_slarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
              end if
              if( i<m )call stdlib_sscal( m-i, -tau( i ), a( i+1, i ), 1_ilp )
              a( i, i ) = one - tau( i )
              ! set a(1:i-1,i) to zero
              do l = 1, i - 1
                 a( l, i ) = zero
              end do
           end do
           return
     end subroutine stdlib_sorg2r

     pure module subroutine stdlib_dorg2r( m, n, k, a, lda, tau, work, info )
     !! DORG2R generates an m by n real matrix Q with orthonormal columns,
     !! which is defined as the first n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by DGEQRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORG2R', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns k+1:n to columns of the unit matrix
           do j = k + 1, n
              do l = 1, m
                 a( l, j ) = zero
              end do
              a( j, j ) = one
           end do
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the left
              if( i<n ) then
                 a( i, i ) = one
                 call stdlib_dlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
              end if
              if( i<m )call stdlib_dscal( m-i, -tau( i ), a( i+1, i ), 1_ilp )
              a( i, i ) = one - tau( i )
              ! set a(1:i-1,i) to zero
              do l = 1, i - 1
                 a( l, i ) = zero
              end do
           end do
           return
     end subroutine stdlib_dorg2r




     pure module subroutine stdlib_sormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! SORMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by SGEQRF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, jc, ldwork, lwkopt, mi, nb, nbmin, &
                     ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORMQR', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_sorm2r( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_slarft( 'FORWARD', 'COLUMNWISE', nq-i+1, ib, a( i, i ),lda, tau( i ),&
                            work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp
                    ic = i
                 else
                    ! h or h**t is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp
                    jc = i
                 end if
                 ! apply h or h**t
                 call stdlib_slarfb( side, trans, 'FORWARD', 'COLUMNWISE', mi, ni,ib, a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormqr

     pure module subroutine stdlib_dormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! DORMQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, jc, ldwork, lwkopt, mi, nb, nbmin, &
                     ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMQR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORMQR', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_dorm2r( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_dlarft( 'FORWARD', 'COLUMNWISE', nq-i+1, ib, a( i, i ),lda, tau( i ),&
                            work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp
                    ic = i
                 else
                    ! h or h**t is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp
                    jc = i
                 end if
                 ! apply h or h**t
                 call stdlib_dlarfb( side, trans, 'FORWARD', 'COLUMNWISE', mi, ni,ib, a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormqr




     pure module subroutine stdlib_sorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! SORM2R overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by SGEQRF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, jc, mi, ni, nq
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORM2R', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran ) .or. ( .not.left .and. notran ) )then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
              jc = 1_ilp
           else
              mi = m
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i)
              aii = a( i, i )
              a( i, i ) = one
              call stdlib_slarf( side, mi, ni, a( i, i ), 1_ilp, tau( i ), c( ic, jc ),ldc, work )
                        
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_sorm2r

     pure module subroutine stdlib_dorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! DORM2R overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DGEQRF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, jc, mi, ni, nq
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORM2R', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran ) .or. ( .not.left .and. notran ) )then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
              jc = 1_ilp
           else
              mi = m
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i)
              aii = a( i, i )
              a( i, i ) = one
              call stdlib_dlarf( side, mi, ni, a( i, i ), 1_ilp, tau( i ), c( ic, jc ),ldc, work )
                        
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_dorm2r




     pure module subroutine stdlib_sgeqrt( m, n, nb, a, lda, t, ldt, work, info )
     !! SGEQRT computes a blocked QR factorization of a real M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk), parameter :: use_recursive_qr = .true.
           integer(ilp) :: i, ib, iinfo, k
           
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nb<1_ilp .or. ( nb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<nb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQRT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  nb
              ib = min( k-i+1, nb )
           ! compute the qr factorization of the current block a(i:m,i:i+ib-1)
              if( use_recursive_qr ) then
                 call stdlib_sgeqrt3( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              else
                 call stdlib_sgeqrt2( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              end if
              if( i+ib<=n ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the left
                 call stdlib_slarfb( 'L', 'T', 'F', 'C', m-i+1, n-i-ib+1, ib,a( i, i ), lda, t( 1_ilp,&
                            i ), ldt,a( i, i+ib ), lda, work , n-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_sgeqrt

     pure module subroutine stdlib_dgeqrt( m, n, nb, a, lda, t, ldt, work, info )
     !! DGEQRT computes a blocked QR factorization of a real M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk), parameter :: use_recursive_qr = .true.
           integer(ilp) :: i, ib, iinfo, k
           
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nb<1_ilp .or. ( nb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<nb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQRT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  nb
              ib = min( k-i+1, nb )
           ! compute the qr factorization of the current block a(i:m,i:i+ib-1)
              if( use_recursive_qr ) then
                 call stdlib_dgeqrt3( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              else
                 call stdlib_dgeqrt2( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              end if
              if( i+ib<=n ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the left
                 call stdlib_dlarfb( 'L', 'T', 'F', 'C', m-i+1, n-i-ib+1, ib,a( i, i ), lda, t( 1_ilp,&
                            i ), ldt,a( i, i+ib ), lda, work , n-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_dgeqrt


     pure module subroutine stdlib_cgeqrt( m, n, nb, a, lda, t, ldt, work, info )
     !! CGEQRT computes a blocked QR factorization of a complex M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk), parameter :: use_recursive_qr = .true.
           integer(ilp) :: i, ib, iinfo, k
           
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nb<1_ilp .or. ( nb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<nb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQRT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  nb
              ib = min( k-i+1, nb )
           ! compute the qr factorization of the current block a(i:m,i:i+ib-1)
              if( use_recursive_qr ) then
                 call stdlib_cgeqrt3( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              else
                 call stdlib_cgeqrt2( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              end if
              if( i+ib<=n ) then
           ! update by applying h**h to a(i:m,i+ib:n) from the left
                 call stdlib_clarfb( 'L', 'C', 'F', 'C', m-i+1, n-i-ib+1, ib,a( i, i ), lda, t( 1_ilp,&
                            i ), ldt,a( i, i+ib ), lda, work , n-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_cgeqrt

     pure module subroutine stdlib_zgeqrt( m, n, nb, a, lda, t, ldt, work, info )
     !! ZGEQRT computes a blocked QR factorization of a complex M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk), parameter :: use_recursive_qr = .true.
           integer(ilp) :: i, ib, iinfo, k
           
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nb<1_ilp .or. ( nb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<nb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQRT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  nb
              ib = min( k-i+1, nb )
           ! compute the qr factorization of the current block a(i:m,i:i+ib-1)
              if( use_recursive_qr ) then
                 call stdlib_zgeqrt3( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              else
                 call stdlib_zgeqrt2( m-i+1, ib, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              end if
              if( i+ib<=n ) then
           ! update by applying h**h to a(i:m,i+ib:n) from the left
                 call stdlib_zlarfb( 'L', 'C', 'F', 'C', m-i+1, n-i-ib+1, ib,a( i, i ), lda, t( 1_ilp,&
                            i ), ldt,a( i, i+ib ), lda, work , n-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_zgeqrt




     pure module subroutine stdlib_sgeqrt2( m, n, a, lda, t, ldt, info )
     !! SGEQRT2 computes a QR factorization of a real M-by-N matrix A,
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(sp) :: aii, alpha
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( m<n ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQRT2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elem. refl. h(i) to annihilate a(i+1:m,i), tau(i) -> t(i,1)
              call stdlib_slarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,t( i, 1_ilp ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 ! w(1:n-i) := a(i:m,i+1:n)^h * a(i:m,i) [w = t(:,n)]
                 call stdlib_sgemv( 'T',m-i+1, n-i, one, a( i, i+1 ), lda,a( i, i ), 1_ilp, zero, t( &
                           1_ilp, n ), 1_ilp )
                 ! a(i:m,i+1:n) = a(i:m,i+1:n) + alpha*a(i:m,i)*w(1:n-1)^h
                 alpha = -(t( i, 1_ilp ))
                 call stdlib_sger( m-i+1, n-i, alpha, a( i, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, a( i, i+1 ), lda &
                           )
                 a( i, i ) = aii
              end if
           end do
           do i = 2, n
              aii = a( i, i )
              a( i, i ) = one
              ! t(1:i-1,i) := alpha * a(i:m,1:i-1)**t * a(i:m,i)
              alpha = -t( i, 1_ilp )
              call stdlib_sgemv( 'T', m-i+1, i-1, alpha, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, zero, t( 1_ilp, &
                        i ), 1_ilp )
              a( i, i ) = aii
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_strmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
                 ! t(i,i) = tau(i)
                 t( i, i ) = t( i, 1_ilp )
                 t( i, 1_ilp) = zero
           end do
     end subroutine stdlib_sgeqrt2

     pure module subroutine stdlib_dgeqrt2( m, n, a, lda, t, ldt, info )
     !! DGEQRT2 computes a QR factorization of a real M-by-N matrix A,
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(dp) :: aii, alpha
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( m<n ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQRT2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elem. refl. h(i) to annihilate a(i+1:m,i), tau(i) -> t(i,1)
              call stdlib_dlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,t( i, 1_ilp ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 ! w(1:n-i) := a(i:m,i+1:n)^h * a(i:m,i) [w = t(:,n)]
                 call stdlib_dgemv( 'T',m-i+1, n-i, one, a( i, i+1 ), lda,a( i, i ), 1_ilp, zero, t( &
                           1_ilp, n ), 1_ilp )
                 ! a(i:m,i+1:n) = a(i:m,i+1:n) + alpha*a(i:m,i)*w(1:n-1)^h
                 alpha = -(t( i, 1_ilp ))
                 call stdlib_dger( m-i+1, n-i, alpha, a( i, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, a( i, i+1 ), lda &
                           )
                 a( i, i ) = aii
              end if
           end do
           do i = 2, n
              aii = a( i, i )
              a( i, i ) = one
              ! t(1:i-1,i) := alpha * a(i:m,1:i-1)**t * a(i:m,i)
              alpha = -t( i, 1_ilp )
              call stdlib_dgemv( 'T', m-i+1, i-1, alpha, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, zero, t( 1_ilp, &
                        i ), 1_ilp )
              a( i, i ) = aii
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_dtrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
                 ! t(i,i) = tau(i)
                 t( i, i ) = t( i, 1_ilp )
                 t( i, 1_ilp) = zero
           end do
     end subroutine stdlib_dgeqrt2


     pure module subroutine stdlib_cgeqrt2( m, n, a, lda, t, ldt, info )
     !! CGEQRT2 computes a QR factorization of a complex M-by-N matrix A,
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(sp) :: aii, alpha
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( m<n ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQRT2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elem. refl. h(i) to annihilate a(i+1:m,i), tau(i) -> t(i,1)
              call stdlib_clarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,t( i, 1_ilp ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = cone
                 ! w(1:n-i) := a(i:m,i+1:n)**h * a(i:m,i) [w = t(:,n)]
                 call stdlib_cgemv( 'C',m-i+1, n-i, cone, a( i, i+1 ), lda,a( i, i ), 1_ilp, czero, t(&
                            1_ilp, n ), 1_ilp )
                 ! a(i:m,i+1:n) = a(i:m,i+1:n) + alpha*a(i:m,i)*w(1:n-1)**h
                 alpha = -conjg(t( i, 1_ilp ))
                 call stdlib_cgerc( m-i+1, n-i, alpha, a( i, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, a( i, i+1 ), &
                           lda )
                 a( i, i ) = aii
              end if
           end do
           do i = 2, n
              aii = a( i, i )
              a( i, i ) = cone
              ! t(1:i-1,i) := alpha * a(i:m,1:i-1)**h * a(i:m,i)
              alpha = -t( i, 1_ilp )
              call stdlib_cgemv( 'C', m-i+1, i-1, alpha, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, czero, t( 1_ilp,&
                         i ), 1_ilp )
              a( i, i ) = aii
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_ctrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
                 ! t(i,i) = tau(i)
                 t( i, i ) = t( i, 1_ilp )
                 t( i, 1_ilp) = czero
           end do
     end subroutine stdlib_cgeqrt2

     pure module subroutine stdlib_zgeqrt2( m, n, a, lda, t, ldt, info )
     !! ZGEQRT2 computes a QR factorization of a complex M-by-N matrix A,
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(dp) :: aii, alpha
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( m<n ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQRT2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elem. refl. h(i) to annihilate a(i+1:m,i), tau(i) -> t(i,1)
              call stdlib_zlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,t( i, 1_ilp ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = cone
                 ! w(1:n-i) := a(i:m,i+1:n)^h * a(i:m,i) [w = t(:,n)]
                 call stdlib_zgemv( 'C',m-i+1, n-i, cone, a( i, i+1 ), lda,a( i, i ), 1_ilp, czero, t(&
                            1_ilp, n ), 1_ilp )
                 ! a(i:m,i+1:n) = a(i:m,i+1:n) + alpha*a(i:m,i)*w(1:n-1)^h
                 alpha = -conjg(t( i, 1_ilp ))
                 call stdlib_zgerc( m-i+1, n-i, alpha, a( i, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, a( i, i+1 ), &
                           lda )
                 a( i, i ) = aii
              end if
           end do
           do i = 2, n
              aii = a( i, i )
              a( i, i ) = cone
              ! t(1:i-1,i) := alpha * a(i:m,1:i-1)**h * a(i:m,i)
              alpha = -t( i, 1_ilp )
              call stdlib_zgemv( 'C', m-i+1, i-1, alpha, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, czero, t( 1_ilp,&
                         i ), 1_ilp )
              a( i, i ) = aii
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_ztrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
                 ! t(i,i) = tau(i)
                 t( i, i ) = t( i, 1_ilp )
                 t( i, 1_ilp) = czero
           end do
     end subroutine stdlib_zgeqrt2




     pure recursive module subroutine stdlib_sgeqrt3( m, n, a, lda, t, ldt, info )
     !! SGEQRT3 recursively computes a QR factorization of a real M-by-N
     !! matrix A, using the compact WY representation of Q.
     !! Based on the algorithm of Elmroth and Gustavson,
     !! IBM J. Res. Develop. Vol 44 No. 4 July 2000.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, j, j1, n1, n2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( n < 0_ilp ) then
              info = -2_ilp
           else if( m < n ) then
              info = -1_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQRT3', -info )
              return
           end if
           if( n==1_ilp ) then
              ! compute householder transform when n=1
              call stdlib_slarfg( m, a(1_ilp,1_ilp), a( min( 2_ilp, m ), 1_ilp ), 1_ilp, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              n1 = n/2_ilp
              n2 = n-n1
              j1 = min( n1+1, n )
              i1 = min( n+1, m )
              ! compute a(1:m,1:n1) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_sgeqrt3( m, n1, a, lda, t, ldt, iinfo )
              ! compute a(1:m,j1:n) = q1^h a(1:m,j1:n) [workspace: t(1:n1,j1:n)]
              do j=1,n2
                 do i=1,n1
                    t( i, j+n1 ) = a( i, j+n1 )
                 end do
              end do
              call stdlib_strmm( 'L', 'L', 'T', 'U', n1, n2, one,a, lda, t( 1_ilp, j1 ), ldt )
              call stdlib_sgemm( 'T', 'N', n1, n2, m-n1, one, a( j1, 1_ilp ), lda,a( j1, j1 ), lda, &
                        one, t( 1_ilp, j1 ), ldt)
              call stdlib_strmm( 'L', 'U', 'T', 'N', n1, n2, one,t, ldt, t( 1_ilp, j1 ), ldt )
              call stdlib_sgemm( 'N', 'N', m-n1, n2, n1, -one, a( j1, 1_ilp ), lda,t( 1_ilp, j1 ), ldt, &
                        one, a( j1, j1 ), lda )
              call stdlib_strmm( 'L', 'L', 'N', 'U', n1, n2, one,a, lda, t( 1_ilp, j1 ), ldt )
              do j=1,n2
                 do i=1,n1
                    a( i, j+n1 ) = a( i, j+n1 ) - t( i, j+n1 )
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_sgeqrt3( m-n1, n2, a( j1, j1 ), lda,t( j1, j1 ), ldt, iinfo )
              ! compute t3 = t(1:n1,j1:n) = -t1 y1^h y2 t2
              do i=1,n1
                 do j=1,n2
                    t( i, j+n1 ) = (a( j+n1, i ))
                 end do
              end do
              call stdlib_strmm( 'R', 'L', 'N', 'U', n1, n2, one,a( j1, j1 ), lda, t( 1_ilp, j1 ), &
                        ldt )
              call stdlib_sgemm( 'T', 'N', n1, n2, m-n, one, a( i1, 1_ilp ), lda,a( i1, j1 ), lda, &
                        one, t( 1_ilp, j1 ), ldt )
              call stdlib_strmm( 'L', 'U', 'N', 'N', n1, n2, -one, t, ldt,t( 1_ilp, j1 ), ldt )
                        
              call stdlib_strmm( 'R', 'U', 'N', 'N', n1, n2, one,t( j1, j1 ), ldt, t( 1_ilp, j1 ), &
                        ldt )
              ! y = (y1,y2); r = [ r1  a(1:n1,j1:n) ];  t = [t1 t3]
                               ! [  0        r2     ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_sgeqrt3

     pure recursive module subroutine stdlib_dgeqrt3( m, n, a, lda, t, ldt, info )
     !! DGEQRT3 recursively computes a QR factorization of a real M-by-N
     !! matrix A, using the compact WY representation of Q.
     !! Based on the algorithm of Elmroth and Gustavson,
     !! IBM J. Res. Develop. Vol 44 No. 4 July 2000.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, j, j1, n1, n2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( n < 0_ilp ) then
              info = -2_ilp
           else if( m < n ) then
              info = -1_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQRT3', -info )
              return
           end if
           if( n==1_ilp ) then
              ! compute householder transform when n=1
              call stdlib_dlarfg( m, a(1_ilp,1_ilp), a( min( 2_ilp, m ), 1_ilp ), 1_ilp, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              n1 = n/2_ilp
              n2 = n-n1
              j1 = min( n1+1, n )
              i1 = min( n+1, m )
              ! compute a(1:m,1:n1) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_dgeqrt3( m, n1, a, lda, t, ldt, iinfo )
              ! compute a(1:m,j1:n) = q1^h a(1:m,j1:n) [workspace: t(1:n1,j1:n)]
              do j=1,n2
                 do i=1,n1
                    t( i, j+n1 ) = a( i, j+n1 )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'L', 'T', 'U', n1, n2, one,a, lda, t( 1_ilp, j1 ), ldt )
              call stdlib_dgemm( 'T', 'N', n1, n2, m-n1, one, a( j1, 1_ilp ), lda,a( j1, j1 ), lda, &
                        one, t( 1_ilp, j1 ), ldt)
              call stdlib_dtrmm( 'L', 'U', 'T', 'N', n1, n2, one,t, ldt, t( 1_ilp, j1 ), ldt )
              call stdlib_dgemm( 'N', 'N', m-n1, n2, n1, -one, a( j1, 1_ilp ), lda,t( 1_ilp, j1 ), ldt, &
                        one, a( j1, j1 ), lda )
              call stdlib_dtrmm( 'L', 'L', 'N', 'U', n1, n2, one,a, lda, t( 1_ilp, j1 ), ldt )
              do j=1,n2
                 do i=1,n1
                    a( i, j+n1 ) = a( i, j+n1 ) - t( i, j+n1 )
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_dgeqrt3( m-n1, n2, a( j1, j1 ), lda,t( j1, j1 ), ldt, iinfo )
              ! compute t3 = t(1:n1,j1:n) = -t1 y1^h y2 t2
              do i=1,n1
                 do j=1,n2
                    t( i, j+n1 ) = (a( j+n1, i ))
                 end do
              end do
              call stdlib_dtrmm( 'R', 'L', 'N', 'U', n1, n2, one,a( j1, j1 ), lda, t( 1_ilp, j1 ), &
                        ldt )
              call stdlib_dgemm( 'T', 'N', n1, n2, m-n, one, a( i1, 1_ilp ), lda,a( i1, j1 ), lda, &
                        one, t( 1_ilp, j1 ), ldt )
              call stdlib_dtrmm( 'L', 'U', 'N', 'N', n1, n2, -one, t, ldt,t( 1_ilp, j1 ), ldt )
                        
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', n1, n2, one,t( j1, j1 ), ldt, t( 1_ilp, j1 ), &
                        ldt )
              ! y = (y1,y2); r = [ r1  a(1:n1,j1:n) ];  t = [t1 t3]
                               ! [  0        r2     ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_dgeqrt3


     pure recursive module subroutine stdlib_cgeqrt3( m, n, a, lda, t, ldt, info )
     !! CGEQRT3 recursively computes a QR factorization of a complex M-by-N matrix A,
     !! using the compact WY representation of Q.
     !! Based on the algorithm of Elmroth and Gustavson,
     !! IBM J. Res. Develop. Vol 44 No. 4 July 2000.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, j, j1, n1, n2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( n < 0_ilp ) then
              info = -2_ilp
           else if( m < n ) then
              info = -1_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQRT3', -info )
              return
           end if
           if( n==1_ilp ) then
              ! compute householder transform when n=1
              call stdlib_clarfg( m, a(1_ilp,1_ilp), a( min( 2_ilp, m ), 1_ilp ), 1_ilp, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              n1 = n/2_ilp
              n2 = n-n1
              j1 = min( n1+1, n )
              i1 = min( n+1, m )
              ! compute a(1:m,1:n1) <- (y1,r1,t1), where q1 = i - y1 t1 y1**h
              call stdlib_cgeqrt3( m, n1, a, lda, t, ldt, iinfo )
              ! compute a(1:m,j1:n) = q1**h a(1:m,j1:n) [workspace: t(1:n1,j1:n)]
              do j=1,n2
                 do i=1,n1
                    t( i, j+n1 ) = a( i, j+n1 )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'L', 'C', 'U', n1, n2, cone,a, lda, t( 1_ilp, j1 ), ldt )
                        
              call stdlib_cgemm( 'C', 'N', n1, n2, m-n1, cone, a( j1, 1_ilp ), lda,a( j1, j1 ), lda, &
                        cone, t( 1_ilp, j1 ), ldt)
              call stdlib_ctrmm( 'L', 'U', 'C', 'N', n1, n2, cone,t, ldt, t( 1_ilp, j1 ), ldt )
                        
              call stdlib_cgemm( 'N', 'N', m-n1, n2, n1, -cone, a( j1, 1_ilp ), lda,t( 1_ilp, j1 ), ldt, &
                        cone, a( j1, j1 ), lda )
              call stdlib_ctrmm( 'L', 'L', 'N', 'U', n1, n2, cone,a, lda, t( 1_ilp, j1 ), ldt )
                        
              do j=1,n2
                 do i=1,n1
                    a( i, j+n1 ) = a( i, j+n1 ) - t( i, j+n1 )
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2**h
              call stdlib_cgeqrt3( m-n1, n2, a( j1, j1 ), lda,t( j1, j1 ), ldt, iinfo )
              ! compute t3 = t(1:n1,j1:n) = -t1 y1**h y2 t2
              do i=1,n1
                 do j=1,n2
                    t( i, j+n1 ) = conjg(a( j+n1, i ))
                 end do
              end do
              call stdlib_ctrmm( 'R', 'L', 'N', 'U', n1, n2, cone,a( j1, j1 ), lda, t( 1_ilp, j1 ), &
                        ldt )
              call stdlib_cgemm( 'C', 'N', n1, n2, m-n, cone, a( i1, 1_ilp ), lda,a( i1, j1 ), lda, &
                        cone, t( 1_ilp, j1 ), ldt )
              call stdlib_ctrmm( 'L', 'U', 'N', 'N', n1, n2, -cone, t, ldt,t( 1_ilp, j1 ), ldt )
                        
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', n1, n2, cone,t( j1, j1 ), ldt, t( 1_ilp, j1 ), &
                        ldt )
              ! y = (y1,y2); r = [ r1  a(1:n1,j1:n) ];  t = [t1 t3]
                               ! [  0        r2     ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_cgeqrt3

     pure recursive module subroutine stdlib_zgeqrt3( m, n, a, lda, t, ldt, info )
     !! ZGEQRT3 recursively computes a QR factorization of a complex M-by-N
     !! matrix A, using the compact WY representation of Q.
     !! Based on the algorithm of Elmroth and Gustavson,
     !! IBM J. Res. Develop. Vol 44 No. 4 July 2000.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, j, j1, n1, n2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( n < 0_ilp ) then
              info = -2_ilp
           else if( m < n ) then
              info = -1_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQRT3', -info )
              return
           end if
           if( n==1_ilp ) then
              ! compute householder transform when n=1
              call stdlib_zlarfg( m, a(1_ilp,1_ilp), a( min( 2_ilp, m ), 1_ilp ), 1_ilp, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              n1 = n/2_ilp
              n2 = n-n1
              j1 = min( n1+1, n )
              i1 = min( n+1, m )
              ! compute a(1:m,1:n1) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_zgeqrt3( m, n1, a, lda, t, ldt, iinfo )
              ! compute a(1:m,j1:n) = q1^h a(1:m,j1:n) [workspace: t(1:n1,j1:n)]
              do j=1,n2
                 do i=1,n1
                    t( i, j+n1 ) = a( i, j+n1 )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'L', 'C', 'U', n1, n2, cone,a, lda, t( 1_ilp, j1 ), ldt )
                        
              call stdlib_zgemm( 'C', 'N', n1, n2, m-n1, cone, a( j1, 1_ilp ), lda,a( j1, j1 ), lda, &
                        cone, t( 1_ilp, j1 ), ldt)
              call stdlib_ztrmm( 'L', 'U', 'C', 'N', n1, n2, cone,t, ldt, t( 1_ilp, j1 ), ldt )
                        
              call stdlib_zgemm( 'N', 'N', m-n1, n2, n1, -cone, a( j1, 1_ilp ), lda,t( 1_ilp, j1 ), ldt, &
                        cone, a( j1, j1 ), lda )
              call stdlib_ztrmm( 'L', 'L', 'N', 'U', n1, n2, cone,a, lda, t( 1_ilp, j1 ), ldt )
                        
              do j=1,n2
                 do i=1,n1
                    a( i, j+n1 ) = a( i, j+n1 ) - t( i, j+n1 )
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_zgeqrt3( m-n1, n2, a( j1, j1 ), lda,t( j1, j1 ), ldt, iinfo )
              ! compute t3 = t(1:n1,j1:n) = -t1 y1^h y2 t2
              do i=1,n1
                 do j=1,n2
                    t( i, j+n1 ) = conjg(a( j+n1, i ))
                 end do
              end do
              call stdlib_ztrmm( 'R', 'L', 'N', 'U', n1, n2, cone,a( j1, j1 ), lda, t( 1_ilp, j1 ), &
                        ldt )
              call stdlib_zgemm( 'C', 'N', n1, n2, m-n, cone, a( i1, 1_ilp ), lda,a( i1, j1 ), lda, &
                        cone, t( 1_ilp, j1 ), ldt )
              call stdlib_ztrmm( 'L', 'U', 'N', 'N', n1, n2, -cone, t, ldt,t( 1_ilp, j1 ), ldt )
                        
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', n1, n2, cone,t( j1, j1 ), ldt, t( 1_ilp, j1 ), &
                        ldt )
              ! y = (y1,y2); r = [ r1  a(1:n1,j1:n) ];  t = [t1 t3]
                               ! [  0        r2     ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_zgeqrt3




     pure module subroutine stdlib_sgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
     !! SGEMQRT overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'T':   Q**T C            C Q**T
     !! where Q is a real orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**T
     !! generated using the compact WY representation as returned by SGEQRT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           ! Array Arguments 
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, ldwork, kf, q
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if( left ) then
              ldwork = max( 1_ilp, n )
              q = m
           else if ( right ) then
              ldwork = max( 1_ilp, m )
              q = n
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>q ) then
              info = -5_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, q ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_slarfb( 'L', 'T', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_slarfb( 'R', 'N', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_slarfb( 'L', 'N', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_slarfb( 'R', 'T', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_sgemqrt

     pure module subroutine stdlib_dgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
     !! DGEMQRT overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'T':   Q**T C            C Q**T
     !! where Q is a real orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**T
     !! generated using the compact WY representation as returned by DGEQRT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           ! Array Arguments 
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, ldwork, kf, q
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if( left ) then
              ldwork = max( 1_ilp, n )
              q = m
           else if ( right ) then
              ldwork = max( 1_ilp, m )
              q = n
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>q ) then
              info = -5_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, q ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_dlarfb( 'L', 'T', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_dlarfb( 'R', 'N', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_dlarfb( 'L', 'N', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_dlarfb( 'R', 'T', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_dgemqrt


     pure module subroutine stdlib_cgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
     !! CGEMQRT overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'C':    Q**H C            C Q**H
     !! where Q is a complex orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**H
     !! generated using the compact WY representation as returned by CGEQRT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           ! Array Arguments 
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, ldwork, kf, q
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if( left ) then
              ldwork = max( 1_ilp, n )
              q = m
           else if ( right ) then
              ldwork = max( 1_ilp, m )
              q = n
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>q ) then
              info = -5_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, q ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_clarfb( 'L', 'C', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_clarfb( 'R', 'N', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_clarfb( 'L', 'N', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_clarfb( 'R', 'C', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_cgemqrt

     pure module subroutine stdlib_zgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
     !! ZGEMQRT overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'C':    Q**H C            C Q**H
     !! where Q is a complex orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**H
     !! generated using the compact WY representation as returned by ZGEQRT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           ! Array Arguments 
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, ldwork, kf, q
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if( left ) then
              ldwork = max( 1_ilp, n )
              q = m
           else if ( right ) then
              ldwork = max( 1_ilp, m )
              q = n
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>q ) then
              info = -5_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, q ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_zlarfb( 'L', 'C', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 call stdlib_zlarfb( 'R', 'N', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_zlarfb( 'L', 'N', 'F', 'C', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 call stdlib_zlarfb( 'R', 'C', 'F', 'C', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_zgemqrt




     module subroutine stdlib_sgeqrfp( m, n, a, lda, tau, work, lwork, info )
     !! SGEQR2P computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQRFP', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_sgeqr2p( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_slarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**t to a(i:m,i+ib:n) from the left
                    call stdlib_slarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-i-&
                    ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), ldwork &
                              )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_sgeqr2p( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgeqrfp

     module subroutine stdlib_dgeqrfp( m, n, a, lda, tau, work, lwork, info )
     !! DGEQR2P computes a QR factorization of a real M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQRFP', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_dgeqr2p( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_dlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**t to a(i:m,i+ib:n) from the left
                    call stdlib_dlarfb( 'LEFT', 'TRANSPOSE', 'FORWARD','COLUMNWISE', m-i+1, n-i-&
                    ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 ), ldwork &
                              )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_dgeqr2p( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgeqrfp


     module subroutine stdlib_cgeqrfp( m, n, a, lda, tau, work, lwork, info )
     !! CGEQR2P computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQRFP', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_cgeqr2p( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_clarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**h to a(i:m,i+ib:n) from the left
                    call stdlib_clarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE', m-&
                    i+1, n-i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 )&
                              , ldwork )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_cgeqr2p( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgeqrfp

     module subroutine stdlib_zgeqrfp( m, n, a, lda, tau, work, lwork, info )
     !! ZGEQR2P computes a QR factorization of a complex M-by-N matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix;
     !! R is an upper-triangular N-by-N matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (M-N)-by-N zero matrix, if M > N.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQRFP', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGEQRF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the qr factorization of the current block
                 ! a(i:m,i:i+ib-1)
                 call stdlib_zgeqr2p( m-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=n ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_zlarft( 'FORWARD', 'COLUMNWISE', m-i+1, ib,a( i, i ), lda, tau( i &
                              ), work, ldwork )
                    ! apply h**h to a(i:m,i+ib:n) from the left
                    call stdlib_zlarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'FORWARD','COLUMNWISE', m-&
                    i+1, n-i-ib+1, ib,a( i, i ), lda, work, ldwork, a( i, i+ib ),lda, work( ib+1 )&
                              , ldwork )
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_zgeqr2p( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgeqrfp




     module subroutine stdlib_sgeqr2p( m, n, a, lda, tau, work, info )
     !! SGEQR2P computes a QR factorization of a real m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQR2P', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_slarfgp( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_slarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_sgeqr2p

     module subroutine stdlib_dgeqr2p( m, n, a, lda, tau, work, info )
     !! DGEQR2P computes a QR factorization of a real m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQR2P', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_dlarfgp( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_dlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tau( i ),a( i, i+1 ), lda, &
                           work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_dgeqr2p


     module subroutine stdlib_cgeqr2p( m, n, a, lda, tau, work, info )
     !! CGEQR2P computes a QR factorization of a complex m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQR2P', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_clarfgp( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 alpha = a( i, i )
                 a( i, i ) = cone
                 call stdlib_clarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tau( i ) ), a( i, i+1 &
                           ), lda, work )
                 a( i, i ) = alpha
              end if
           end do
           return
     end subroutine stdlib_cgeqr2p

     module subroutine stdlib_zgeqr2p( m, n, a, lda, tau, work, info )
     !! ZGEQR2P computes a QR factorization of a complex m-by-n matrix A:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a m-by-m orthogonal matrix;
     !! R is an upper-triangular n-by-n matrix with nonnegative diagonal
     !! entries;
     !! 0 is a (m-n)-by-n zero matrix, if m > n.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQR2P', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
              call stdlib_zlarfgp( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tau( i ) )
              if( i<n ) then
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 alpha = a( i, i )
                 a( i, i ) = cone
                 call stdlib_zlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tau( i ) ), a( i, i+1 &
                           ), lda, work )
                 a( i, i ) = alpha
              end if
           end do
           return
     end subroutine stdlib_zgeqr2p




     pure module subroutine stdlib_sgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
     !! SGEQP3 computes a QR factorization with column pivoting of a
     !! matrix A:  A*P = Q*R  using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: inb = 1_ilp
           integer(ilp), parameter :: inbmin = 2_ilp
           integer(ilp), parameter :: ixover = 3_ilp
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: fjb, iws, j, jb, lwkopt, minmn, minws, na, nb, nbmin, nfxd, nx, sm, &
                     sminmn, sn, topbmn
           ! Intrinsic Functions 
           ! test input arguments
        ! ====================
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              minmn = min( m, n )
              if( minmn==0_ilp ) then
                 iws = 1_ilp
                 lwkopt = 1_ilp
              else
                 iws = 3_ilp*n + 1_ilp
                 nb = stdlib_ilaenv( inb, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = 2_ilp*n + ( n + 1_ilp )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( ( lwork<iws ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQP3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! move initial columns up front.
           nfxd = 1_ilp
           do j = 1, n
              if( jpvt( j )/=0_ilp ) then
                 if( j/=nfxd ) then
                    call stdlib_sswap( m, a( 1_ilp, j ), 1_ilp, a( 1_ilp, nfxd ), 1_ilp )
                    jpvt( j ) = jpvt( nfxd )
                    jpvt( nfxd ) = j
                 else
                    jpvt( j ) = j
                 end if
                 nfxd = nfxd + 1_ilp
              else
                 jpvt( j ) = j
              end if
           end do
           nfxd = nfxd - 1_ilp
           ! factorize fixed columns
        ! =======================
           ! compute the qr factorization of fixed columns and update
           ! remaining columns.
           if( nfxd>0_ilp ) then
              na = min( m, nfxd )
      ! cc      call stdlib_sgeqr2( m, na, a, lda, tau, work, info )
              call stdlib_sgeqrf( m, na, a, lda, tau, work, lwork, info )
              iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              if( na<n ) then
      ! cc         call stdlib_sorm2r( 'left', 'transpose', m, n-na, na, a, lda,
      ! cc  $                   tau, a( 1, na+1 ), lda, work, info )
                 call stdlib_sormqr( 'LEFT', 'TRANSPOSE', m, n-na, na, a, lda, tau,a( 1_ilp, na+1 ), &
                           lda, work, lwork, info )
                 iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              end if
           end if
           ! factorize free columns
        ! ======================
           if( nfxd<minmn ) then
              sm = m - nfxd
              sn = n - nfxd
              sminmn = minmn - nfxd
              ! determine the block size.
              nb = stdlib_ilaenv( inb, 'SGEQRF', ' ', sm, sn, -1_ilp, -1_ilp )
              nbmin = 2_ilp
              nx = 0_ilp
              if( ( nb>1_ilp ) .and. ( nb<sminmn ) ) then
                 ! determine when to cross over from blocked to unblocked code.
                 nx = max( 0_ilp, stdlib_ilaenv( ixover, 'SGEQRF', ' ', sm, sn, -1_ilp,-1_ilp ) )
                 if( nx<sminmn ) then
                    ! determine if workspace is large enough for blocked code.
                    minws = 2_ilp*sn + ( sn+1 )*nb
                    iws = max( iws, minws )
                    if( lwork<minws ) then
                       ! not enough workspace to use optimal nb: reduce nb and
                       ! determine the minimum value of nb.
                       nb = ( lwork-2*sn ) / ( sn+1 )
                       nbmin = max( 2_ilp, stdlib_ilaenv( inbmin, 'SGEQRF', ' ', sm, sn,-1_ilp, -1_ilp ) )
                                 
                    end if
                 end if
              end if
              ! initialize partial column norms. the first n elements of work
              ! store the exact column norms.
              do j = nfxd + 1, n
                 work( j ) = stdlib_snrm2( sm, a( nfxd+1, j ), 1_ilp )
                 work( n+j ) = work( j )
              end do
              if( ( nb>=nbmin ) .and. ( nb<sminmn ) .and.( nx<sminmn ) ) then
                 ! use blocked code initially.
                 j = nfxd + 1_ilp
                 ! compute factorization: while loop.
                 topbmn = minmn - nx
                 30 continue
                 if( j<=topbmn ) then
                    jb = min( nb, topbmn-j+1 )
                    ! factorize jb columns among columns j:n.
                    call stdlib_slaqps( m, n-j+1, j-1, jb, fjb, a( 1_ilp, j ), lda,jpvt( j ), tau( j )&
                              , work( j ), work( n+j ),work( 2_ilp*n+1 ), work( 2_ilp*n+jb+1 ), n-j+1 )
                    j = j + fjb
                    go to 30
                 end if
              else
                 j = nfxd + 1_ilp
              end if
              ! use unblocked code to factor the last or only block.
              if( j<=minmn )call stdlib_slaqp2( m, n-j+1, j-1, a( 1_ilp, j ), lda, jpvt( j ),tau( j ),&
                         work( j ), work( n+j ),work( 2_ilp*n+1 ) )
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgeqp3

     pure module subroutine stdlib_dgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
     !! DGEQP3 computes a QR factorization with column pivoting of a
     !! matrix A:  A*P = Q*R  using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: inb = 1_ilp
           integer(ilp), parameter :: inbmin = 2_ilp
           integer(ilp), parameter :: ixover = 3_ilp
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: fjb, iws, j, jb, lwkopt, minmn, minws, na, nb, nbmin, nfxd, nx, sm, &
                     sminmn, sn, topbmn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
        ! ====================
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              minmn = min( m, n )
              if( minmn==0_ilp ) then
                 iws = 1_ilp
                 lwkopt = 1_ilp
              else
                 iws = 3_ilp*n + 1_ilp
                 nb = stdlib_ilaenv( inb, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = 2_ilp*n + ( n + 1_ilp )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( ( lwork<iws ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQP3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! move initial columns up front.
           nfxd = 1_ilp
           do j = 1, n
              if( jpvt( j )/=0_ilp ) then
                 if( j/=nfxd ) then
                    call stdlib_dswap( m, a( 1_ilp, j ), 1_ilp, a( 1_ilp, nfxd ), 1_ilp )
                    jpvt( j ) = jpvt( nfxd )
                    jpvt( nfxd ) = j
                 else
                    jpvt( j ) = j
                 end if
                 nfxd = nfxd + 1_ilp
              else
                 jpvt( j ) = j
              end if
           end do
           nfxd = nfxd - 1_ilp
           ! factorize fixed columns
        ! =======================
           ! compute the qr factorization of fixed columns and update
           ! remaining columns.
           if( nfxd>0_ilp ) then
              na = min( m, nfxd )
      ! cc      call stdlib_dgeqr2( m, na, a, lda, tau, work, info )
              call stdlib_dgeqrf( m, na, a, lda, tau, work, lwork, info )
              iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              if( na<n ) then
      ! cc         call stdlib_dorm2r( 'left', 'transpose', m, n-na, na, a, lda,
      ! cc  $                   tau, a( 1, na+1 ), lda, work, info )
                 call stdlib_dormqr( 'LEFT', 'TRANSPOSE', m, n-na, na, a, lda, tau,a( 1_ilp, na+1 ), &
                           lda, work, lwork, info )
                 iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              end if
           end if
           ! factorize free columns
        ! ======================
           if( nfxd<minmn ) then
              sm = m - nfxd
              sn = n - nfxd
              sminmn = minmn - nfxd
              ! determine the block size.
              nb = stdlib_ilaenv( inb, 'DGEQRF', ' ', sm, sn, -1_ilp, -1_ilp )
              nbmin = 2_ilp
              nx = 0_ilp
              if( ( nb>1_ilp ) .and. ( nb<sminmn ) ) then
                 ! determine when to cross over from blocked to unblocked code.
                 nx = max( 0_ilp, stdlib_ilaenv( ixover, 'DGEQRF', ' ', sm, sn, -1_ilp,-1_ilp ) )
                 if( nx<sminmn ) then
                    ! determine if workspace is large enough for blocked code.
                    minws = 2_ilp*sn + ( sn+1 )*nb
                    iws = max( iws, minws )
                    if( lwork<minws ) then
                       ! not enough workspace to use optimal nb: reduce nb and
                       ! determine the minimum value of nb.
                       nb = ( lwork-2*sn ) / ( sn+1 )
                       nbmin = max( 2_ilp, stdlib_ilaenv( inbmin, 'DGEQRF', ' ', sm, sn,-1_ilp, -1_ilp ) )
                                 
                    end if
                 end if
              end if
              ! initialize partial column norms. the first n elements of work
              ! store the exact column norms.
              do j = nfxd + 1, n
                 work( j ) = stdlib_dnrm2( sm, a( nfxd+1, j ), 1_ilp )
                 work( n+j ) = work( j )
              end do
              if( ( nb>=nbmin ) .and. ( nb<sminmn ) .and.( nx<sminmn ) ) then
                 ! use blocked code initially.
                 j = nfxd + 1_ilp
                 ! compute factorization: while loop.
                 topbmn = minmn - nx
                 30 continue
                 if( j<=topbmn ) then
                    jb = min( nb, topbmn-j+1 )
                    ! factorize jb columns among columns j:n.
                    call stdlib_dlaqps( m, n-j+1, j-1, jb, fjb, a( 1_ilp, j ), lda,jpvt( j ), tau( j )&
                              , work( j ), work( n+j ),work( 2_ilp*n+1 ), work( 2_ilp*n+jb+1 ), n-j+1 )
                    j = j + fjb
                    go to 30
                 end if
              else
                 j = nfxd + 1_ilp
              end if
              ! use unblocked code to factor the last or only block.
              if( j<=minmn )call stdlib_dlaqp2( m, n-j+1, j-1, a( 1_ilp, j ), lda, jpvt( j ),tau( j ),&
                         work( j ), work( n+j ),work( 2_ilp*n+1 ) )
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgeqp3


     pure module subroutine stdlib_cgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
     !! CGEQP3 computes a QR factorization with column pivoting of a
     !! matrix A:  A*P = Q*R  using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: inb = 1_ilp
           integer(ilp), parameter :: inbmin = 2_ilp
           integer(ilp), parameter :: ixover = 3_ilp
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: fjb, iws, j, jb, lwkopt, minmn, minws, na, nb, nbmin, nfxd, nx, sm, &
                     sminmn, sn, topbmn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
        ! ====================
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              minmn = min( m, n )
              if( minmn==0_ilp ) then
                 iws = 1_ilp
                 lwkopt = 1_ilp
              else
                 iws = n + 1_ilp
                 nb = stdlib_ilaenv( inb, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = ( n + 1_ilp )*nb
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
              if( ( lwork<iws ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQP3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! move initial columns up front.
           nfxd = 1_ilp
           do j = 1, n
              if( jpvt( j )/=0_ilp ) then
                 if( j/=nfxd ) then
                    call stdlib_cswap( m, a( 1_ilp, j ), 1_ilp, a( 1_ilp, nfxd ), 1_ilp )
                    jpvt( j ) = jpvt( nfxd )
                    jpvt( nfxd ) = j
                 else
                    jpvt( j ) = j
                 end if
                 nfxd = nfxd + 1_ilp
              else
                 jpvt( j ) = j
              end if
           end do
           nfxd = nfxd - 1_ilp
           ! factorize fixed columns
        ! =======================
           ! compute the qr factorization of fixed columns and update
           ! remaining columns.
           if( nfxd>0_ilp ) then
              na = min( m, nfxd )
      ! cc      call stdlib_cgeqr2( m, na, a, lda, tau, work, info )
              call stdlib_cgeqrf( m, na, a, lda, tau, work, lwork, info )
              iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              if( na<n ) then
      ! cc         call stdlib_cunm2r( 'left', 'conjugate transpose', m, n-na,
      ! cc  $                   na, a, lda, tau, a( 1, na+1 ), lda, work,
      ! cc  $                   info )
                 call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, n-na, na, a,lda, tau, a( 1_ilp,&
                            na+1 ), lda, work, lwork,info )
                 iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              end if
           end if
           ! factorize free columns
        ! ======================
           if( nfxd<minmn ) then
              sm = m - nfxd
              sn = n - nfxd
              sminmn = minmn - nfxd
              ! determine the block size.
              nb = stdlib_ilaenv( inb, 'CGEQRF', ' ', sm, sn, -1_ilp, -1_ilp )
              nbmin = 2_ilp
              nx = 0_ilp
              if( ( nb>1_ilp ) .and. ( nb<sminmn ) ) then
                 ! determine when to cross over from blocked to unblocked code.
                 nx = max( 0_ilp, stdlib_ilaenv( ixover, 'CGEQRF', ' ', sm, sn, -1_ilp,-1_ilp ) )
                 if( nx<sminmn ) then
                    ! determine if workspace is large enough for blocked code.
                    minws = ( sn+1 )*nb
                    iws = max( iws, minws )
                    if( lwork<minws ) then
                       ! not enough workspace to use optimal nb: reduce nb and
                       ! determine the minimum value of nb.
                       nb = lwork / ( sn+1 )
                       nbmin = max( 2_ilp, stdlib_ilaenv( inbmin, 'CGEQRF', ' ', sm, sn,-1_ilp, -1_ilp ) )
                                 
                    end if
                 end if
              end if
              ! initialize partial column norms. the first n elements of work
              ! store the exact column norms.
              do j = nfxd + 1, n
                 rwork( j ) = stdlib_scnrm2( sm, a( nfxd+1, j ), 1_ilp )
                 rwork( n+j ) = rwork( j )
              end do
              if( ( nb>=nbmin ) .and. ( nb<sminmn ) .and.( nx<sminmn ) ) then
                 ! use blocked code initially.
                 j = nfxd + 1_ilp
                 ! compute factorization: while loop.
                 topbmn = minmn - nx
                 30 continue
                 if( j<=topbmn ) then
                    jb = min( nb, topbmn-j+1 )
                    ! factorize jb columns among columns j:n.
                    call stdlib_claqps( m, n-j+1, j-1, jb, fjb, a( 1_ilp, j ), lda,jpvt( j ), tau( j )&
                              , rwork( j ),rwork( n+j ), work( 1_ilp ), work( jb+1 ),n-j+1 )
                    j = j + fjb
                    go to 30
                 end if
              else
                 j = nfxd + 1_ilp
              end if
              ! use unblocked code to factor the last or only block.
              if( j<=minmn )call stdlib_claqp2( m, n-j+1, j-1, a( 1_ilp, j ), lda, jpvt( j ),tau( j ),&
                         rwork( j ), rwork( n+j ), work( 1_ilp ) )
           end if
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cgeqp3

     pure module subroutine stdlib_zgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
     !! ZGEQP3 computes a QR factorization with column pivoting of a
     !! matrix A:  A*P = Q*R  using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: inb = 1_ilp
           integer(ilp), parameter :: inbmin = 2_ilp
           integer(ilp), parameter :: ixover = 3_ilp
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: fjb, iws, j, jb, lwkopt, minmn, minws, na, nb, nbmin, nfxd, nx, sm, &
                     sminmn, sn, topbmn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test input arguments
        ! ====================
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              minmn = min( m, n )
              if( minmn==0_ilp ) then
                 iws = 1_ilp
                 lwkopt = 1_ilp
              else
                 iws = n + 1_ilp
                 nb = stdlib_ilaenv( inb, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = ( n + 1_ilp )*nb
              end if
              work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
              if( ( lwork<iws ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQP3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! move initial columns up front.
           nfxd = 1_ilp
           do j = 1, n
              if( jpvt( j )/=0_ilp ) then
                 if( j/=nfxd ) then
                    call stdlib_zswap( m, a( 1_ilp, j ), 1_ilp, a( 1_ilp, nfxd ), 1_ilp )
                    jpvt( j ) = jpvt( nfxd )
                    jpvt( nfxd ) = j
                 else
                    jpvt( j ) = j
                 end if
                 nfxd = nfxd + 1_ilp
              else
                 jpvt( j ) = j
              end if
           end do
           nfxd = nfxd - 1_ilp
           ! factorize fixed columns
        ! =======================
           ! compute the qr factorization of fixed columns and update
           ! remaining columns.
           if( nfxd>0_ilp ) then
              na = min( m, nfxd )
      ! cc      call stdlib_zgeqr2( m, na, a, lda, tau, work, info )
              call stdlib_zgeqrf( m, na, a, lda, tau, work, lwork, info )
              iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              if( na<n ) then
      ! cc         call stdlib_zunm2r( 'left', 'conjugate transpose', m, n-na,
      ! cc  $                   na, a, lda, tau, a( 1, na+1 ), lda, work,
      ! cc  $                   info )
                 call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, n-na, na, a,lda, tau, a( 1_ilp,&
                            na+1 ), lda, work, lwork,info )
                 iws = max( iws, int( work( 1_ilp ),KIND=ilp) )
              end if
           end if
           ! factorize free columns
        ! ======================
           if( nfxd<minmn ) then
              sm = m - nfxd
              sn = n - nfxd
              sminmn = minmn - nfxd
              ! determine the block size.
              nb = stdlib_ilaenv( inb, 'ZGEQRF', ' ', sm, sn, -1_ilp, -1_ilp )
              nbmin = 2_ilp
              nx = 0_ilp
              if( ( nb>1_ilp ) .and. ( nb<sminmn ) ) then
                 ! determine when to cross over from blocked to unblocked code.
                 nx = max( 0_ilp, stdlib_ilaenv( ixover, 'ZGEQRF', ' ', sm, sn, -1_ilp,-1_ilp ) )
                 if( nx<sminmn ) then
                    ! determine if workspace is large enough for blocked code.
                    minws = ( sn+1 )*nb
                    iws = max( iws, minws )
                    if( lwork<minws ) then
                       ! not enough workspace to use optimal nb: reduce nb and
                       ! determine the minimum value of nb.
                       nb = lwork / ( sn+1 )
                       nbmin = max( 2_ilp, stdlib_ilaenv( inbmin, 'ZGEQRF', ' ', sm, sn,-1_ilp, -1_ilp ) )
                                 
                    end if
                 end if
              end if
              ! initialize partial column norms. the first n elements of work
              ! store the exact column norms.
              do j = nfxd + 1, n
                 rwork( j ) = stdlib_dznrm2( sm, a( nfxd+1, j ), 1_ilp )
                 rwork( n+j ) = rwork( j )
              end do
              if( ( nb>=nbmin ) .and. ( nb<sminmn ) .and.( nx<sminmn ) ) then
                 ! use blocked code initially.
                 j = nfxd + 1_ilp
                 ! compute factorization: while loop.
                 topbmn = minmn - nx
                 30 continue
                 if( j<=topbmn ) then
                    jb = min( nb, topbmn-j+1 )
                    ! factorize jb columns among columns j:n.
                    call stdlib_zlaqps( m, n-j+1, j-1, jb, fjb, a( 1_ilp, j ), lda,jpvt( j ), tau( j )&
                              , rwork( j ),rwork( n+j ), work( 1_ilp ), work( jb+1 ),n-j+1 )
                    j = j + fjb
                    go to 30
                 end if
              else
                 j = nfxd + 1_ilp
              end if
              ! use unblocked code to factor the last or only block.
              if( j<=minmn )call stdlib_zlaqp2( m, n-j+1, j-1, a( 1_ilp, j ), lda, jpvt( j ),tau( j ),&
                         rwork( j ), rwork( n+j ), work( 1_ilp ) )
           end if
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zgeqp3




     pure module subroutine stdlib_slaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
     !! SLAQP2 computes a QR factorization with column pivoting of
     !! the block A(OFFSET+1:M,1:N).
     !! The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, m, n, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, itemp, j, mn, offpi, pvt
           real(sp) :: aii, temp, temp2, tol3z
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m-offset, n )
           tol3z = sqrt(stdlib_slamch('EPSILON'))
           ! compute factorization.
           loop_20: do i = 1, mn
              offpi = offset + i
              ! determine ith pivot column and swap if necessary.
              pvt = ( i-1 ) + stdlib_isamax( n-i+1, vn1( i ), 1_ilp )
              if( pvt/=i ) then
                 call stdlib_sswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, i ), 1_ilp )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( i )
                 jpvt( i ) = itemp
                 vn1( pvt ) = vn1( i )
                 vn2( pvt ) = vn2( i )
              end if
              ! generate elementary reflector h(i).
              if( offpi<m ) then
                 call stdlib_slarfg( m-offpi+1, a( offpi, i ), a( offpi+1, i ), 1_ilp,tau( i ) )
                           
              else
                 call stdlib_slarfg( 1_ilp, a( m, i ), a( m, i ), 1_ilp, tau( i ) )
              end if
              if( i<n ) then
                 ! apply h(i)**t to a(offset+i:m,i+1:n) from the left.
                 aii = a( offpi, i )
                 a( offpi, i ) = one
                 call stdlib_slarf( 'LEFT', m-offpi+1, n-i, a( offpi, i ), 1_ilp,tau( i ), a( offpi, &
                           i+1 ), lda, work( 1_ilp ) )
                 a( offpi, i ) = aii
              end if
              ! update partial column norms.
              do j = i + 1, n
                 if( vn1( j )/=zero ) then
                    ! note: the following 4 lines follow from the analysis in
                    ! lapack working note 176.
                    temp = one - ( abs( a( offpi, j ) ) / vn1( j ) )**2_ilp
                    temp = max( temp, zero )
                    temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                    if( temp2 <= tol3z ) then
                       if( offpi<m ) then
                          vn1( j ) = stdlib_snrm2( m-offpi, a( offpi+1, j ), 1_ilp )
                          vn2( j ) = vn1( j )
                       else
                          vn1( j ) = zero
                          vn2( j ) = zero
                       end if
                    else
                       vn1( j ) = vn1( j )*sqrt( temp )
                    end if
                 end if
              end do
           end do loop_20
           return
     end subroutine stdlib_slaqp2

     pure module subroutine stdlib_dlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
     !! DLAQP2 computes a QR factorization with column pivoting of
     !! the block A(OFFSET+1:M,1:N).
     !! The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, m, n, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, itemp, j, mn, offpi, pvt
           real(dp) :: aii, temp, temp2, tol3z
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m-offset, n )
           tol3z = sqrt(stdlib_dlamch('EPSILON'))
           ! compute factorization.
           loop_20: do i = 1, mn
              offpi = offset + i
              ! determine ith pivot column and swap if necessary.
              pvt = ( i-1 ) + stdlib_idamax( n-i+1, vn1( i ), 1_ilp )
              if( pvt/=i ) then
                 call stdlib_dswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, i ), 1_ilp )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( i )
                 jpvt( i ) = itemp
                 vn1( pvt ) = vn1( i )
                 vn2( pvt ) = vn2( i )
              end if
              ! generate elementary reflector h(i).
              if( offpi<m ) then
                 call stdlib_dlarfg( m-offpi+1, a( offpi, i ), a( offpi+1, i ), 1_ilp,tau( i ) )
                           
              else
                 call stdlib_dlarfg( 1_ilp, a( m, i ), a( m, i ), 1_ilp, tau( i ) )
              end if
              if( i<n ) then
                 ! apply h(i)**t to a(offset+i:m,i+1:n) from the left.
                 aii = a( offpi, i )
                 a( offpi, i ) = one
                 call stdlib_dlarf( 'LEFT', m-offpi+1, n-i, a( offpi, i ), 1_ilp,tau( i ), a( offpi, &
                           i+1 ), lda, work( 1_ilp ) )
                 a( offpi, i ) = aii
              end if
              ! update partial column norms.
              do j = i + 1, n
                 if( vn1( j )/=zero ) then
                    ! note: the following 4 lines follow from the analysis in
                    ! lapack working note 176.
                    temp = one - ( abs( a( offpi, j ) ) / vn1( j ) )**2_ilp
                    temp = max( temp, zero )
                    temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                    if( temp2 <= tol3z ) then
                       if( offpi<m ) then
                          vn1( j ) = stdlib_dnrm2( m-offpi, a( offpi+1, j ), 1_ilp )
                          vn2( j ) = vn1( j )
                       else
                          vn1( j ) = zero
                          vn2( j ) = zero
                       end if
                    else
                       vn1( j ) = vn1( j )*sqrt( temp )
                    end if
                 end if
              end do
           end do loop_20
           return
     end subroutine stdlib_dlaqp2


     pure module subroutine stdlib_claqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
     !! CLAQP2 computes a QR factorization with column pivoting of
     !! the block A(OFFSET+1:M,1:N).
     !! The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, m, n, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, itemp, j, mn, offpi, pvt
           real(sp) :: temp, temp2, tol3z
           complex(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m-offset, n )
           tol3z = sqrt(stdlib_slamch('EPSILON'))
           ! compute factorization.
           loop_20: do i = 1, mn
              offpi = offset + i
              ! determine ith pivot column and swap if necessary.
              pvt = ( i-1 ) + stdlib_isamax( n-i+1, vn1( i ), 1_ilp )
              if( pvt/=i ) then
                 call stdlib_cswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, i ), 1_ilp )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( i )
                 jpvt( i ) = itemp
                 vn1( pvt ) = vn1( i )
                 vn2( pvt ) = vn2( i )
              end if
              ! generate elementary reflector h(i).
              if( offpi<m ) then
                 call stdlib_clarfg( m-offpi+1, a( offpi, i ), a( offpi+1, i ), 1_ilp,tau( i ) )
                           
              else
                 call stdlib_clarfg( 1_ilp, a( m, i ), a( m, i ), 1_ilp, tau( i ) )
              end if
              if( i<n ) then
                 ! apply h(i)**h to a(offset+i:m,i+1:n) from the left.
                 aii = a( offpi, i )
                 a( offpi, i ) = cone
                 call stdlib_clarf( 'LEFT', m-offpi+1, n-i, a( offpi, i ), 1_ilp,conjg( tau( i ) ), a(&
                            offpi, i+1 ), lda,work( 1_ilp ) )
                 a( offpi, i ) = aii
              end if
              ! update partial column norms.
              do j = i + 1, n
                 if( vn1( j )/=zero ) then
                    ! note: the following 4 lines follow from the analysis in
                    ! lapack working note 176.
                    temp = one - ( abs( a( offpi, j ) ) / vn1( j ) )**2_ilp
                    temp = max( temp, zero )
                    temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                    if( temp2 <= tol3z ) then
                       if( offpi<m ) then
                          vn1( j ) = stdlib_scnrm2( m-offpi, a( offpi+1, j ), 1_ilp )
                          vn2( j ) = vn1( j )
                       else
                          vn1( j ) = zero
                          vn2( j ) = zero
                       end if
                    else
                       vn1( j ) = vn1( j )*sqrt( temp )
                    end if
                 end if
              end do
           end do loop_20
           return
     end subroutine stdlib_claqp2

     pure module subroutine stdlib_zlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
     !! ZLAQP2 computes a QR factorization with column pivoting of
     !! the block A(OFFSET+1:M,1:N).
     !! The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, m, n, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, itemp, j, mn, offpi, pvt
           real(dp) :: temp, temp2, tol3z
           complex(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           mn = min( m-offset, n )
           tol3z = sqrt(stdlib_dlamch('EPSILON'))
           ! compute factorization.
           loop_20: do i = 1, mn
              offpi = offset + i
              ! determine ith pivot column and swap if necessary.
              pvt = ( i-1 ) + stdlib_idamax( n-i+1, vn1( i ), 1_ilp )
              if( pvt/=i ) then
                 call stdlib_zswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, i ), 1_ilp )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( i )
                 jpvt( i ) = itemp
                 vn1( pvt ) = vn1( i )
                 vn2( pvt ) = vn2( i )
              end if
              ! generate elementary reflector h(i).
              if( offpi<m ) then
                 call stdlib_zlarfg( m-offpi+1, a( offpi, i ), a( offpi+1, i ), 1_ilp,tau( i ) )
                           
              else
                 call stdlib_zlarfg( 1_ilp, a( m, i ), a( m, i ), 1_ilp, tau( i ) )
              end if
              if( i<n ) then
                 ! apply h(i)**h to a(offset+i:m,i+1:n) from the left.
                 aii = a( offpi, i )
                 a( offpi, i ) = cone
                 call stdlib_zlarf( 'LEFT', m-offpi+1, n-i, a( offpi, i ), 1_ilp,conjg( tau( i ) ), a(&
                            offpi, i+1 ), lda,work( 1_ilp ) )
                 a( offpi, i ) = aii
              end if
              ! update partial column norms.
              do j = i + 1, n
                 if( vn1( j )/=zero ) then
                    ! note: the following 4 lines follow from the analysis in
                    ! lapack working note 176.
                    temp = one - ( abs( a( offpi, j ) ) / vn1( j ) )**2_ilp
                    temp = max( temp, zero )
                    temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                    if( temp2 <= tol3z ) then
                       if( offpi<m ) then
                          vn1( j ) = stdlib_dznrm2( m-offpi, a( offpi+1, j ), 1_ilp )
                          vn2( j ) = vn1( j )
                       else
                          vn1( j ) = zero
                          vn2( j ) = zero
                       end if
                    else
                       vn1( j ) = vn1( j )*sqrt( temp )
                    end if
                 end if
              end do
           end do loop_20
           return
     end subroutine stdlib_zlaqp2




     pure module subroutine stdlib_slaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
     !! SLAQPS computes a step of QR factorization with column pivoting
     !! of a real M-by-N matrix A by using Blas-3.  It tries to factorize
     !! NB columns from A starting from the row OFFSET+1, and updates all
     !! of the matrix with Blas-3 xGEMM.
     !! In some cases, due to catastrophic cancellations, it cannot
     !! factorize NB columns.  Hence, the actual number of factorized
     !! columns is returned in KB.
     !! Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
               ldf )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: itemp, j, k, lastrk, lsticc, pvt, rk
           real(sp) :: akk, temp, temp2, tol3z
           ! Intrinsic Functions 
           ! Executable Statements 
           lastrk = min( m, n+offset )
           lsticc = 0_ilp
           k = 0_ilp
           tol3z = sqrt(stdlib_slamch('EPSILON'))
           ! beginning of while loop.
           10 continue
           if( ( k<nb ) .and. ( lsticc==0_ilp ) ) then
              k = k + 1_ilp
              rk = offset + k
              ! determine ith pivot column and swap if necessary
              pvt = ( k-1 ) + stdlib_isamax( n-k+1, vn1( k ), 1_ilp )
              if( pvt/=k ) then
                 call stdlib_sswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 call stdlib_sswap( k-1, f( pvt, 1_ilp ), ldf, f( k, 1_ilp ), ldf )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( k )
                 jpvt( k ) = itemp
                 vn1( pvt ) = vn1( k )
                 vn2( pvt ) = vn2( k )
              end if
              ! apply previous householder reflectors to column k:
              ! a(rk:m,k) := a(rk:m,k) - a(rk:m,1:k-1)*f(k,1:k-1)**t.
              if( k>1_ilp ) then
                 call stdlib_sgemv( 'NO TRANSPOSE', m-rk+1, k-1, -one, a( rk, 1_ilp ),lda, f( k, 1_ilp ), &
                           ldf, one, a( rk, k ), 1_ilp )
              end if
              ! generate elementary reflector h(k).
              if( rk<m ) then
                 call stdlib_slarfg( m-rk+1, a( rk, k ), a( rk+1, k ), 1_ilp, tau( k ) )
              else
                 call stdlib_slarfg( 1_ilp, a( rk, k ), a( rk, k ), 1_ilp, tau( k ) )
              end if
              akk = a( rk, k )
              a( rk, k ) = one
              ! compute kth column of f:
              ! compute  f(k+1:n,k) := tau(k)*a(rk:m,k+1:n)**t*a(rk:m,k).
              if( k<n ) then
                 call stdlib_sgemv( 'TRANSPOSE', m-rk+1, n-k, tau( k ),a( rk, k+1 ), lda, a( rk, &
                           k ), 1_ilp, zero,f( k+1, k ), 1_ilp )
              end if
              ! padding f(1:k,k) with zeros.
              do j = 1, k
                 f( j, k ) = zero
              end do
              ! incremental updating of f:
              ! f(1:n,k) := f(1:n,k) - tau(k)*f(1:n,1:k-1)*a(rk:m,1:k-1)**t
                          ! *a(rk:m,k).
              if( k>1_ilp ) then
                 call stdlib_sgemv( 'TRANSPOSE', m-rk+1, k-1, -tau( k ), a( rk, 1_ilp ),lda, a( rk, k &
                           ), 1_ilp, zero, auxv( 1_ilp ), 1_ilp )
                 call stdlib_sgemv( 'NO TRANSPOSE', n, k-1, one, f( 1_ilp, 1_ilp ), ldf,auxv( 1_ilp ), 1_ilp, one,&
                            f( 1_ilp, k ), 1_ilp )
              end if
              ! update the current row of a:
              ! a(rk,k+1:n) := a(rk,k+1:n) - a(rk,1:k)*f(k+1:n,1:k)**t.
              if( k<n ) then
                 call stdlib_sgemv( 'NO TRANSPOSE', n-k, k, -one, f( k+1, 1_ilp ), ldf,a( rk, 1_ilp ), &
                           lda, one, a( rk, k+1 ), lda )
              end if
              ! update partial column norms.
              if( rk<lastrk ) then
                 do j = k + 1, n
                    if( vn1( j )/=zero ) then
                       ! note: the following 4 lines follow from the analysis in
                       ! lapack working note 176.
                       temp = abs( a( rk, j ) ) / vn1( j )
                       temp = max( zero, ( one+temp )*( one-temp ) )
                       temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                       if( temp2 <= tol3z ) then
                          vn2( j ) = real( lsticc,KIND=sp)
                          lsticc = j
                       else
                          vn1( j ) = vn1( j )*sqrt( temp )
                       end if
                    end if
                 end do
              end if
              a( rk, k ) = akk
              ! end of while loop.
              go to 10
           end if
           kb = k
           rk = offset + kb
           ! apply the block reflector to the rest of the matrix:
           ! a(offset+kb+1:m,kb+1:n) := a(offset+kb+1:m,kb+1:n) -
                               ! a(offset+kb+1:m,1:kb)*f(kb+1:n,1:kb)**t.
           if( kb<min( n, m-offset ) ) then
              call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-rk, n-kb, kb, -one,a( rk+1, 1_ilp ), &
                        lda, f( kb+1, 1_ilp ), ldf, one,a( rk+1, kb+1 ), lda )
           end if
           ! recomputation of difficult columns.
           40 continue
           if( lsticc>0_ilp ) then
              itemp = nint( vn2( lsticc ),KIND=ilp)
              vn1( lsticc ) = stdlib_snrm2( m-rk, a( rk+1, lsticc ), 1_ilp )
              ! note: the computation of vn1( lsticc ) relies on the fact that
              ! stdlib_snrm2 does not fail on vectors with norm below the value of
              ! sqrt(stdlib_dlamch('s'))
              vn2( lsticc ) = vn1( lsticc )
              lsticc = itemp
              go to 40
           end if
           return
     end subroutine stdlib_slaqps

     pure module subroutine stdlib_dlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
     !! DLAQPS computes a step of QR factorization with column pivoting
     !! of a real M-by-N matrix A by using Blas-3.  It tries to factorize
     !! NB columns from A starting from the row OFFSET+1, and updates all
     !! of the matrix with Blas-3 xGEMM.
     !! In some cases, due to catastrophic cancellations, it cannot
     !! factorize NB columns.  Hence, the actual number of factorized
     !! columns is returned in KB.
     !! Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
               ldf )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: itemp, j, k, lastrk, lsticc, pvt, rk
           real(dp) :: akk, temp, temp2, tol3z
           ! Intrinsic Functions 
           ! Executable Statements 
           lastrk = min( m, n+offset )
           lsticc = 0_ilp
           k = 0_ilp
           tol3z = sqrt(stdlib_dlamch('EPSILON'))
           ! beginning of while loop.
           10 continue
           if( ( k<nb ) .and. ( lsticc==0_ilp ) ) then
              k = k + 1_ilp
              rk = offset + k
              ! determine ith pivot column and swap if necessary
              pvt = ( k-1 ) + stdlib_idamax( n-k+1, vn1( k ), 1_ilp )
              if( pvt/=k ) then
                 call stdlib_dswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 call stdlib_dswap( k-1, f( pvt, 1_ilp ), ldf, f( k, 1_ilp ), ldf )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( k )
                 jpvt( k ) = itemp
                 vn1( pvt ) = vn1( k )
                 vn2( pvt ) = vn2( k )
              end if
              ! apply previous householder reflectors to column k:
              ! a(rk:m,k) := a(rk:m,k) - a(rk:m,1:k-1)*f(k,1:k-1)**t.
              if( k>1_ilp ) then
                 call stdlib_dgemv( 'NO TRANSPOSE', m-rk+1, k-1, -one, a( rk, 1_ilp ),lda, f( k, 1_ilp ), &
                           ldf, one, a( rk, k ), 1_ilp )
              end if
              ! generate elementary reflector h(k).
              if( rk<m ) then
                 call stdlib_dlarfg( m-rk+1, a( rk, k ), a( rk+1, k ), 1_ilp, tau( k ) )
              else
                 call stdlib_dlarfg( 1_ilp, a( rk, k ), a( rk, k ), 1_ilp, tau( k ) )
              end if
              akk = a( rk, k )
              a( rk, k ) = one
              ! compute kth column of f:
              ! compute  f(k+1:n,k) := tau(k)*a(rk:m,k+1:n)**t*a(rk:m,k).
              if( k<n ) then
                 call stdlib_dgemv( 'TRANSPOSE', m-rk+1, n-k, tau( k ),a( rk, k+1 ), lda, a( rk, &
                           k ), 1_ilp, zero,f( k+1, k ), 1_ilp )
              end if
              ! padding f(1:k,k) with zeros.
              do j = 1, k
                 f( j, k ) = zero
              end do
              ! incremental updating of f:
              ! f(1:n,k) := f(1:n,k) - tau(k)*f(1:n,1:k-1)*a(rk:m,1:k-1)**t
                          ! *a(rk:m,k).
              if( k>1_ilp ) then
                 call stdlib_dgemv( 'TRANSPOSE', m-rk+1, k-1, -tau( k ), a( rk, 1_ilp ),lda, a( rk, k &
                           ), 1_ilp, zero, auxv( 1_ilp ), 1_ilp )
                 call stdlib_dgemv( 'NO TRANSPOSE', n, k-1, one, f( 1_ilp, 1_ilp ), ldf,auxv( 1_ilp ), 1_ilp, one,&
                            f( 1_ilp, k ), 1_ilp )
              end if
              ! update the current row of a:
              ! a(rk,k+1:n) := a(rk,k+1:n) - a(rk,1:k)*f(k+1:n,1:k)**t.
              if( k<n ) then
                 call stdlib_dgemv( 'NO TRANSPOSE', n-k, k, -one, f( k+1, 1_ilp ), ldf,a( rk, 1_ilp ), &
                           lda, one, a( rk, k+1 ), lda )
              end if
              ! update partial column norms.
              if( rk<lastrk ) then
                 do j = k + 1, n
                    if( vn1( j )/=zero ) then
                       ! note: the following 4 lines follow from the analysis in
                       ! lapack working note 176.
                       temp = abs( a( rk, j ) ) / vn1( j )
                       temp = max( zero, ( one+temp )*( one-temp ) )
                       temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                       if( temp2 <= tol3z ) then
                          vn2( j ) = real( lsticc,KIND=dp)
                          lsticc = j
                       else
                          vn1( j ) = vn1( j )*sqrt( temp )
                       end if
                    end if
                 end do
              end if
              a( rk, k ) = akk
              ! end of while loop.
              go to 10
           end if
           kb = k
           rk = offset + kb
           ! apply the block reflector to the rest of the matrix:
           ! a(offset+kb+1:m,kb+1:n) := a(offset+kb+1:m,kb+1:n) -
                               ! a(offset+kb+1:m,1:kb)*f(kb+1:n,1:kb)**t.
           if( kb<min( n, m-offset ) ) then
              call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-rk, n-kb, kb, -one,a( rk+1, 1_ilp ), &
                        lda, f( kb+1, 1_ilp ), ldf, one,a( rk+1, kb+1 ), lda )
           end if
           ! recomputation of difficult columns.
           40 continue
           if( lsticc>0_ilp ) then
              itemp = nint( vn2( lsticc ),KIND=ilp)
              vn1( lsticc ) = stdlib_dnrm2( m-rk, a( rk+1, lsticc ), 1_ilp )
              ! note: the computation of vn1( lsticc ) relies on the fact that
              ! stdlib_snrm2 does not fail on vectors with norm below the value of
              ! sqrt(stdlib_dlamch('s'))
              vn2( lsticc ) = vn1( lsticc )
              lsticc = itemp
              go to 40
           end if
           return
     end subroutine stdlib_dlaqps


     pure module subroutine stdlib_claqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
     !! CLAQPS computes a step of QR factorization with column pivoting
     !! of a complex M-by-N matrix A by using Blas-3.  It tries to factorize
     !! NB columns from A starting from the row OFFSET+1, and updates all
     !! of the matrix with Blas-3 xGEMM.
     !! In some cases, due to catastrophic cancellations, it cannot
     !! factorize NB columns.  Hence, the actual number of factorized
     !! columns is returned in KB.
     !! Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
               ldf )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(sp), intent(out) :: tau(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: itemp, j, k, lastrk, lsticc, pvt, rk
           real(sp) :: temp, temp2, tol3z
           complex(sp) :: akk
           ! Intrinsic Functions 
           ! Executable Statements 
           lastrk = min( m, n+offset )
           lsticc = 0_ilp
           k = 0_ilp
           tol3z = sqrt(stdlib_slamch('EPSILON'))
           ! beginning of while loop.
           10 continue
           if( ( k<nb ) .and. ( lsticc==0_ilp ) ) then
              k = k + 1_ilp
              rk = offset + k
              ! determine ith pivot column and swap if necessary
              pvt = ( k-1 ) + stdlib_isamax( n-k+1, vn1( k ), 1_ilp )
              if( pvt/=k ) then
                 call stdlib_cswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 call stdlib_cswap( k-1, f( pvt, 1_ilp ), ldf, f( k, 1_ilp ), ldf )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( k )
                 jpvt( k ) = itemp
                 vn1( pvt ) = vn1( k )
                 vn2( pvt ) = vn2( k )
              end if
              ! apply previous householder reflectors to column k:
              ! a(rk:m,k) := a(rk:m,k) - a(rk:m,1:k-1)*f(k,1:k-1)**h.
              if( k>1_ilp ) then
                 do j = 1, k - 1
                    f( k, j ) = conjg( f( k, j ) )
                 end do
                 call stdlib_cgemv( 'NO TRANSPOSE', m-rk+1, k-1, -cone, a( rk, 1_ilp ),lda, f( k, 1_ilp ),&
                            ldf, cone, a( rk, k ), 1_ilp )
                 do j = 1, k - 1
                    f( k, j ) = conjg( f( k, j ) )
                 end do
              end if
              ! generate elementary reflector h(k).
              if( rk<m ) then
                 call stdlib_clarfg( m-rk+1, a( rk, k ), a( rk+1, k ), 1_ilp, tau( k ) )
              else
                 call stdlib_clarfg( 1_ilp, a( rk, k ), a( rk, k ), 1_ilp, tau( k ) )
              end if
              akk = a( rk, k )
              a( rk, k ) = cone
              ! compute kth column of f:
              ! compute  f(k+1:n,k) := tau(k)*a(rk:m,k+1:n)**h*a(rk:m,k).
              if( k<n ) then
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-rk+1, n-k, tau( k ),a( rk, k+1 ), &
                           lda, a( rk, k ), 1_ilp, czero,f( k+1, k ), 1_ilp )
              end if
              ! padding f(1:k,k) with zeros.
              do j = 1, k
                 f( j, k ) = czero
              end do
              ! incremental updating of f:
              ! f(1:n,k) := f(1:n,k) - tau(k)*f(1:n,1:k-1)*a(rk:m,1:k-1)**h
                          ! *a(rk:m,k).
              if( k>1_ilp ) then
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-rk+1, k-1, -tau( k ),a( rk, 1_ilp ), lda,&
                            a( rk, k ), 1_ilp, czero,auxv( 1_ilp ), 1_ilp )
                 call stdlib_cgemv( 'NO TRANSPOSE', n, k-1, cone, f( 1_ilp, 1_ilp ), ldf,auxv( 1_ilp ), 1_ilp, &
                           cone, f( 1_ilp, k ), 1_ilp )
              end if
              ! update the current row of a:
              ! a(rk,k+1:n) := a(rk,k+1:n) - a(rk,1:k)*f(k+1:n,1:k)**h.
              if( k<n ) then
                 call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', 1_ilp, n-k,k, -cone, a( rk,&
                            1_ilp ), lda, f( k+1, 1_ilp ), ldf,cone, a( rk, k+1 ), lda )
              end if
              ! update partial column norms.
              if( rk<lastrk ) then
                 do j = k + 1, n
                    if( vn1( j )/=zero ) then
                       ! note: the following 4 lines follow from the analysis in
                       ! lapack working note 176.
                       temp = abs( a( rk, j ) ) / vn1( j )
                       temp = max( zero, ( one+temp )*( one-temp ) )
                       temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                       if( temp2 <= tol3z ) then
                          vn2( j ) = real( lsticc,KIND=sp)
                          lsticc = j
                       else
                          vn1( j ) = vn1( j )*sqrt( temp )
                       end if
                    end if
                 end do
              end if
              a( rk, k ) = akk
              ! end of while loop.
              go to 10
           end if
           kb = k
           rk = offset + kb
           ! apply the block reflector to the rest of the matrix:
           ! a(offset+kb+1:m,kb+1:n) := a(offset+kb+1:m,kb+1:n) -
                               ! a(offset+kb+1:m,1:kb)*f(kb+1:n,1:kb)**h.
           if( kb<min( n, m-offset ) ) then
              call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m-rk, n-kb,kb, -cone, a( &
                        rk+1, 1_ilp ), lda, f( kb+1, 1_ilp ), ldf,cone, a( rk+1, kb+1 ), lda )
           end if
           ! recomputation of difficult columns.
           60 continue
           if( lsticc>0_ilp ) then
              itemp = nint( vn2( lsticc ),KIND=ilp)
              vn1( lsticc ) = stdlib_scnrm2( m-rk, a( rk+1, lsticc ), 1_ilp )
              ! note: the computation of vn1( lsticc ) relies on the fact that
              ! stdlib_snrm2 does not fail on vectors with norm below the value of
              ! sqrt(stdlib_dlamch('s'))
              vn2( lsticc ) = vn1( lsticc )
              lsticc = itemp
              go to 60
           end if
           return
     end subroutine stdlib_claqps

     pure module subroutine stdlib_zlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
     !! ZLAQPS computes a step of QR factorization with column pivoting
     !! of a complex M-by-N matrix A by using Blas-3.  It tries to factorize
     !! NB columns from A starting from the row OFFSET+1, and updates all
     !! of the matrix with Blas-3 xGEMM.
     !! In some cases, due to catastrophic cancellations, it cannot
     !! factorize NB columns.  Hence, the actual number of factorized
     !! columns is returned in KB.
     !! Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
               ldf )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           ! Array Arguments 
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(dp), intent(out) :: tau(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: itemp, j, k, lastrk, lsticc, pvt, rk
           real(dp) :: temp, temp2, tol3z
           complex(dp) :: akk
           ! Intrinsic Functions 
           ! Executable Statements 
           lastrk = min( m, n+offset )
           lsticc = 0_ilp
           k = 0_ilp
           tol3z = sqrt(stdlib_dlamch('EPSILON'))
           ! beginning of while loop.
           10 continue
           if( ( k<nb ) .and. ( lsticc==0_ilp ) ) then
              k = k + 1_ilp
              rk = offset + k
              ! determine ith pivot column and swap if necessary
              pvt = ( k-1 ) + stdlib_idamax( n-k+1, vn1( k ), 1_ilp )
              if( pvt/=k ) then
                 call stdlib_zswap( m, a( 1_ilp, pvt ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 call stdlib_zswap( k-1, f( pvt, 1_ilp ), ldf, f( k, 1_ilp ), ldf )
                 itemp = jpvt( pvt )
                 jpvt( pvt ) = jpvt( k )
                 jpvt( k ) = itemp
                 vn1( pvt ) = vn1( k )
                 vn2( pvt ) = vn2( k )
              end if
              ! apply previous householder reflectors to column k:
              ! a(rk:m,k) := a(rk:m,k) - a(rk:m,1:k-1)*f(k,1:k-1)**h.
              if( k>1_ilp ) then
                 do j = 1, k - 1
                    f( k, j ) = conjg( f( k, j ) )
                 end do
                 call stdlib_zgemv( 'NO TRANSPOSE', m-rk+1, k-1, -cone, a( rk, 1_ilp ),lda, f( k, 1_ilp ),&
                            ldf, cone, a( rk, k ), 1_ilp )
                 do j = 1, k - 1
                    f( k, j ) = conjg( f( k, j ) )
                 end do
              end if
              ! generate elementary reflector h(k).
              if( rk<m ) then
                 call stdlib_zlarfg( m-rk+1, a( rk, k ), a( rk+1, k ), 1_ilp, tau( k ) )
              else
                 call stdlib_zlarfg( 1_ilp, a( rk, k ), a( rk, k ), 1_ilp, tau( k ) )
              end if
              akk = a( rk, k )
              a( rk, k ) = cone
              ! compute kth column of f:
              ! compute  f(k+1:n,k) := tau(k)*a(rk:m,k+1:n)**h*a(rk:m,k).
              if( k<n ) then
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-rk+1, n-k, tau( k ),a( rk, k+1 ), &
                           lda, a( rk, k ), 1_ilp, czero,f( k+1, k ), 1_ilp )
              end if
              ! padding f(1:k,k) with zeros.
              do j = 1, k
                 f( j, k ) = czero
              end do
              ! incremental updating of f:
              ! f(1:n,k) := f(1:n,k) - tau(k)*f(1:n,1:k-1)*a(rk:m,1:k-1)**h
                          ! *a(rk:m,k).
              if( k>1_ilp ) then
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-rk+1, k-1, -tau( k ),a( rk, 1_ilp ), lda,&
                            a( rk, k ), 1_ilp, czero,auxv( 1_ilp ), 1_ilp )
                 call stdlib_zgemv( 'NO TRANSPOSE', n, k-1, cone, f( 1_ilp, 1_ilp ), ldf,auxv( 1_ilp ), 1_ilp, &
                           cone, f( 1_ilp, k ), 1_ilp )
              end if
              ! update the current row of a:
              ! a(rk,k+1:n) := a(rk,k+1:n) - a(rk,1:k)*f(k+1:n,1:k)**h.
              if( k<n ) then
                 call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', 1_ilp, n-k,k, -cone, a( rk,&
                            1_ilp ), lda, f( k+1, 1_ilp ), ldf,cone, a( rk, k+1 ), lda )
              end if
              ! update partial column norms.
              if( rk<lastrk ) then
                 do j = k + 1, n
                    if( vn1( j )/=zero ) then
                       ! note: the following 4 lines follow from the analysis in
                       ! lapack working note 176.
                       temp = abs( a( rk, j ) ) / vn1( j )
                       temp = max( zero, ( one+temp )*( one-temp ) )
                       temp2 = temp*( vn1( j ) / vn2( j ) )**2_ilp
                       if( temp2 <= tol3z ) then
                          vn2( j ) = real( lsticc,KIND=dp)
                          lsticc = j
                       else
                          vn1( j ) = vn1( j )*sqrt( temp )
                       end if
                    end if
                 end do
              end if
              a( rk, k ) = akk
              ! end of while loop.
              go to 10
           end if
           kb = k
           rk = offset + kb
           ! apply the block reflector to the rest of the matrix:
           ! a(offset+kb+1:m,kb+1:n) := a(offset+kb+1:m,kb+1:n) -
                               ! a(offset+kb+1:m,1:kb)*f(kb+1:n,1:kb)**h.
           if( kb<min( n, m-offset ) ) then
              call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m-rk, n-kb,kb, -cone, a( &
                        rk+1, 1_ilp ), lda, f( kb+1, 1_ilp ), ldf,cone, a( rk+1, kb+1 ), lda )
           end if
           ! recomputation of difficult columns.
           60 continue
           if( lsticc>0_ilp ) then
              itemp = nint( vn2( lsticc ),KIND=ilp)
              vn1( lsticc ) = stdlib_dznrm2( m-rk, a( rk+1, lsticc ), 1_ilp )
              ! note: the computation of vn1( lsticc ) relies on the fact that
              ! stdlib_snrm2 does not fail on vectors with norm below the value of
              ! sqrt(stdlib_dlamch('s'))
              vn2( lsticc ) = vn1( lsticc )
              lsticc = itemp
              go to 60
           end if
           return
     end subroutine stdlib_zlaqps




     pure module subroutine stdlib_slatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
     !! SLATSQR computes a blocked Tall-Skinny QR factorization of
     !! a real M-by-N matrix A for M >= N:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix, stored on exit in an implicit
     !! form in the elements below the diagonal of the array A and in
     !! the elements of the array T;
     !! R is an upper-triangular N-by-N matrix, stored on exit in
     !! the elements on and above the diagonal of the array A.
     !! 0 is a (M-N)-by-N zero matrix, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ii, kk, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
             info = -2_ilp
           else if( mb<1_ilp ) then
             info = -3_ilp
           else if( nb<1_ilp .or. ( nb>n .and. n>0_ilp )) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<nb ) then
             info = -8_ilp
           else if( lwork<(n*nb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
             work(1_ilp) = nb*n
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SLATSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the qr decomposition
            if ((mb<=n).or.(mb>=m)) then
              call stdlib_sgeqrt( m, n, nb, a, lda, t, ldt, work, info)
              return
            end if
            kk = mod((m-n),(mb-n))
            ii=m-kk+1
            ! compute the qr factorization of the first block a(1:mb,1:n)
            call stdlib_sgeqrt( mb, n, nb, a(1_ilp,1_ilp), lda, t, ldt, work, info )
            ctr = 1_ilp
            do i = mb+1, ii-mb+n ,  (mb-n)
            ! compute the qr factorization of the current block a(i:i+mb-n,1:n)
              call stdlib_stpqrt( mb-n, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( i, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
            ! compute the qr factorization of the last block a(ii:m,1:n)
            if (ii<=m) then
              call stdlib_stpqrt( kk, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( ii, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp), &
                        ldt,work, info )
            end if
           work( 1_ilp ) = n*nb
           return
     end subroutine stdlib_slatsqr

     pure module subroutine stdlib_dlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
     !! DLATSQR computes a blocked Tall-Skinny QR factorization of
     !! a real M-by-N matrix A for M >= N:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix, stored on exit in an implicit
     !! form in the elements below the diagonal of the array A and in
     !! the elements of the array T;
     !! R is an upper-triangular N-by-N matrix, stored on exit in
     !! the elements on and above the diagonal of the array A.
     !! 0 is a (M-N)-by-N zero matrix, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ii, kk, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
             info = -2_ilp
           else if( mb<1_ilp ) then
             info = -3_ilp
           else if( nb<1_ilp .or. ( nb>n .and. n>0_ilp )) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<nb ) then
             info = -8_ilp
           else if( lwork<(n*nb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
             work(1_ilp) = nb*n
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DLATSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the qr decomposition
            if ((mb<=n).or.(mb>=m)) then
              call stdlib_dgeqrt( m, n, nb, a, lda, t, ldt, work, info)
              return
            end if
            kk = mod((m-n),(mb-n))
            ii=m-kk+1
            ! compute the qr factorization of the first block a(1:mb,1:n)
            call stdlib_dgeqrt( mb, n, nb, a(1_ilp,1_ilp), lda, t, ldt, work, info )
            ctr = 1_ilp
            do i = mb+1, ii-mb+n ,  (mb-n)
            ! compute the qr factorization of the current block a(i:i+mb-n,1:n)
              call stdlib_dtpqrt( mb-n, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( i, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
            ! compute the qr factorization of the last block a(ii:m,1:n)
            if (ii<=m) then
              call stdlib_dtpqrt( kk, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( ii, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp), &
                        ldt,work, info )
            end if
           work( 1_ilp ) = n*nb
           return
     end subroutine stdlib_dlatsqr


     pure module subroutine stdlib_clatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
     !! CLATSQR computes a blocked Tall-Skinny QR factorization of
     !! a complex M-by-N matrix A for M >= N:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix, stored on exit in an implicit
     !! form in the elements below the diagonal of the array A and in
     !! the elements of the array T;
     !! R is an upper-triangular N-by-N matrix, stored on exit in
     !! the elements on and above the diagonal of the array A.
     !! 0 is a (M-N)-by-N zero matrix, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ii, kk, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
             info = -2_ilp
           else if( mb<1_ilp ) then
             info = -3_ilp
           else if( nb<1_ilp .or. ( nb>n .and. n>0_ilp )) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<nb ) then
             info = -8_ilp
           else if( lwork<(n*nb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
             work(1_ilp) = nb*n
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CLATSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the qr decomposition
            if ((mb<=n).or.(mb>=m)) then
              call stdlib_cgeqrt( m, n, nb, a, lda, t, ldt, work, info)
              return
            end if
            kk = mod((m-n),(mb-n))
            ii=m-kk+1
            ! compute the qr factorization of the first block a(1:mb,1:n)
            call stdlib_cgeqrt( mb, n, nb, a(1_ilp,1_ilp), lda, t, ldt, work, info )
            ctr = 1_ilp
            do i = mb+1, ii-mb+n ,  (mb-n)
            ! compute the qr factorization of the current block a(i:i+mb-n,1:n)
              call stdlib_ctpqrt( mb-n, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( i, 1_ilp ), lda,t(1_ilp,ctr * n + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
            ! compute the qr factorization of the last block a(ii:m,1:n)
            if (ii<=m) then
              call stdlib_ctpqrt( kk, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( ii, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp), &
                        ldt,work, info )
            end if
           work( 1_ilp ) = n*nb
           return
     end subroutine stdlib_clatsqr

     pure module subroutine stdlib_zlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
     !! ZLATSQR computes a blocked Tall-Skinny QR factorization of
     !! a complex M-by-N matrix A for M >= N:
     !! A = Q * ( R ),
     !! ( 0 )
     !! where:
     !! Q is a M-by-M orthogonal matrix, stored on exit in an implicit
     !! form in the elements below the diagonal of the array A and in
     !! the elements of the array T;
     !! R is an upper-triangular N-by-N matrix, stored on exit in
     !! the elements on and above the diagonal of the array A.
     !! 0 is a (M-N)-by-N zero matrix, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), t(ldt,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ii, kk, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
             info = -2_ilp
           else if( mb<1_ilp ) then
             info = -3_ilp
           else if( nb<1_ilp .or. ( nb>n .and. n>0_ilp )) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<nb ) then
             info = -8_ilp
           else if( lwork<(n*nb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
             work(1_ilp) = nb*n
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZLATSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the qr decomposition
            if ((mb<=n).or.(mb>=m)) then
              call stdlib_zgeqrt( m, n, nb, a, lda, t, ldt, work, info)
              return
            end if
            kk = mod((m-n),(mb-n))
            ii=m-kk+1
            ! compute the qr factorization of the first block a(1:mb,1:n)
            call stdlib_zgeqrt( mb, n, nb, a(1_ilp,1_ilp), lda, t, ldt, work, info )
            ctr = 1_ilp
            do i = mb+1, ii-mb+n ,  (mb-n)
            ! compute the qr factorization of the current block a(i:i+mb-n,1:n)
              call stdlib_ztpqrt( mb-n, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( i, 1_ilp ), lda,t(1_ilp, ctr * n + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
            ! compute the qr factorization of the last block a(ii:m,1:n)
            if (ii<=m) then
              call stdlib_ztpqrt( kk, n, 0_ilp, nb, a(1_ilp,1_ilp), lda, a( ii, 1_ilp ), lda,t(1_ilp,ctr * n + 1_ilp), &
                        ldt,work, info )
            end if
           work( 1_ilp ) = n*nb
           return
     end subroutine stdlib_zlatsqr




     pure module subroutine stdlib_cungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
     !! CUNGTSQR generates an M-by-N complex matrix Q_out with orthonormal
     !! columns, which are the first N columns of a product of comlpex unitary
     !! matrices of order M which are returned by CLATSQR
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! See the documentation for CLATSQR.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iinfo, ldc, lworkopt, lc, lw, nblocal, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           lquery  = lwork==-1_ilp
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array c(ldc, n) and work(lwork)
              ! in the call to stdlib_clamtsqr. see the documentation for stdlib_clamtsqr.
              if( lwork<2_ilp .and. (.not.lquery) ) then
                 info = -10_ilp
              else
                 ! set block size for column blocks
                 nblocal = min( nb, n )
                 ! lwork = -1, then set the size for the array c(ldc,n)
                 ! in stdlib_clamtsqr call and set the optimal size of the work array
                 ! work(lwork) in stdlib_clamtsqr call.
                 ldc = m
                 lc = ldc*n
                 lw = n * nblocal
                 lworkopt = lc+lw
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -10_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGTSQR', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           ! (1) form explicitly the tall-skinny m-by-n left submatrix q1_in
           ! of m-by-m orthogonal matrix q_in, which is implicitly stored in
           ! the subdiagonal part of input array a and in the input array t.
           ! perform by the following operation using the routine stdlib_clamtsqr.
               ! q1_in = q_in * ( i ), where i is a n-by-n identity matrix,
                              ! ( 0 )        0 is a (m-n)-by-n zero matrix.
           ! (1a) form m-by-n matrix in the array work(1:ldc*n) with ones
           ! on the diagonal and zeros elsewhere.
           call stdlib_claset( 'F', m, n, czero, cone, work, ldc )
           ! (1b)  on input, work(1:ldc*n) stores ( i );
                                                ! ( 0 )
                 ! on output, work(1:ldc*n) stores q1_in.
           call stdlib_clamtsqr( 'L', 'N', m, n, n, mb, nblocal, a, lda, t, ldt,work, ldc, work( &
                     lc+1 ), lw, iinfo )
           ! (2) copy the result from the part of the work array (1:m,1:n)
           ! with the leading dimension ldc that starts at work(1) into
           ! the output array a(1:m,1:n) column-by-column.
           do j = 1, n
              call stdlib_ccopy( m, work( (j-1)*ldc + 1_ilp ), 1_ilp, a( 1_ilp, j ), 1_ilp )
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
           return
     end subroutine stdlib_cungtsqr

     pure module subroutine stdlib_zungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
     !! ZUNGTSQR generates an M-by-N complex matrix Q_out with orthonormal
     !! columns, which are the first N columns of a product of comlpex unitary
     !! matrices of order M which are returned by ZLATSQR
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! See the documentation for ZLATSQR.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iinfo, ldc, lworkopt, lc, lw, nblocal, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           lquery  = lwork==-1_ilp
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array c(ldc, n) and work(lwork)
              ! in the call to stdlib_zlamtsqr. see the documentation for stdlib_zlamtsqr.
              if( lwork<2_ilp .and. (.not.lquery) ) then
                 info = -10_ilp
              else
                 ! set block size for column blocks
                 nblocal = min( nb, n )
                 ! lwork = -1, then set the size for the array c(ldc,n)
                 ! in stdlib_zlamtsqr call and set the optimal size of the work array
                 ! work(lwork) in stdlib_zlamtsqr call.
                 ldc = m
                 lc = ldc*n
                 lw = n * nblocal
                 lworkopt = lc+lw
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -10_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGTSQR', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           ! (1) form explicitly the tall-skinny m-by-n left submatrix q1_in
           ! of m-by-m orthogonal matrix q_in, which is implicitly stored in
           ! the subdiagonal part of input array a and in the input array t.
           ! perform by the following operation using the routine stdlib_zlamtsqr.
               ! q1_in = q_in * ( i ), where i is a n-by-n identity matrix,
                              ! ( 0 )        0 is a (m-n)-by-n zero matrix.
           ! (1a) form m-by-n matrix in the array work(1:ldc*n) with ones
           ! on the diagonal and zeros elsewhere.
           call stdlib_zlaset( 'F', m, n, czero, cone, work, ldc )
           ! (1b)  on input, work(1:ldc*n) stores ( i );
                                                ! ( 0 )
                 ! on output, work(1:ldc*n) stores q1_in.
           call stdlib_zlamtsqr( 'L', 'N', m, n, n, mb, nblocal, a, lda, t, ldt,work, ldc, work( &
                     lc+1 ), lw, iinfo )
           ! (2) copy the result from the part of the work array (1:m,1:n)
           ! with the leading dimension ldc that starts at work(1) into
           ! the output array a(1:m,1:n) column-by-column.
           do j = 1, n
              call stdlib_zcopy( m, work( (j-1)*ldc + 1_ilp ), 1_ilp, a( 1_ilp, j ), 1_ilp )
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
           return
     end subroutine stdlib_zungtsqr




     pure module subroutine stdlib_cungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
     !! CUNGTSQR_ROW generates an M-by-N complex matrix Q_out with
     !! orthonormal columns from the output of CLATSQR. These N orthonormal
     !! columns are the first N columns of a product of complex unitary
     !! matrices Q(k)_in of order M, which are returned by CLATSQR in
     !! a special format.
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! The input matrices Q(k)_in are stored in row and column blocks in A.
     !! See the documentation of CLATSQR for more details on the format of
     !! Q(k)_in, where each Q(k)_in is represented by block Householder
     !! transformations. This routine calls an auxiliary routine CLARFB_GETT,
     !! where the computation is performed on each individual block. The
     !! algorithm first sweeps NB-sized column blocks from the right to left
     !! starting in the bottom row block and continues to the top row block
     !! (hence _ROW in the routine name). This sweep is in reverse order of
     !! the order in which CLATSQR generates the output blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: nblocal, mb2, m_plus_one, itmp, ib_bottom, lworkopt, &
                     num_all_row_blocks, jb_t, ib, imb, kb, kb_last, knb, mb1
           ! Local Arrays 
           complex(sp) :: dummy(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           nblocal = min( nb, n )
           ! determine the workspace size.
           if( info==0_ilp ) then
              lworkopt = nblocal * max( nblocal, ( n - nblocal ) )
           end if
           ! handle error in the input parameters and handle the workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGTSQR_ROW', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           ! (0) set the upper-triangular part of the matrix a to zero and
           ! its diagonal elements to one.
           call stdlib_claset('U', m, n, czero, cone, a, lda )
           ! kb_last is the column index of the last column block reflector
           ! in the matrices t and v.
           kb_last = ( ( n-1 ) / nblocal ) * nblocal + 1_ilp
           ! (1) bottom-up loop over row blocks of a, except the top row block.
           ! note: if mb>=m, then the loop is never executed.
           if ( mb<m ) then
              ! mb2 is the row blocking size for the row blocks before the
              ! first top row block in the matrix a. ib is the row index for
              ! the row blocks in the matrix a before the first top row block.
              ! ib_bottom is the row index for the last bottom row block
              ! in the matrix a. jb_t is the column index of the corresponding
              ! column block in the matrix t.
              ! initialize variables.
              ! num_all_row_blocks is the number of row blocks in the matrix a
              ! including the first row block.
              mb2 = mb - n
              m_plus_one = m + 1_ilp
              itmp = ( m - mb - 1_ilp ) / mb2
              ib_bottom = itmp * mb2 + mb + 1_ilp
              num_all_row_blocks = itmp + 2_ilp
              jb_t = num_all_row_blocks * n + 1_ilp
              do ib = ib_bottom, mb+1, -mb2
                 ! determine the block size imb for the current row block
                 ! in the matrix a.
                 imb = min( m_plus_one - ib, mb2 )
                 ! determine the column index jb_t for the current column block
                 ! in the matrix t.
                 jb_t = jb_t - n
                 ! apply column blocks of h in the row block from right to left.
                 ! kb is the column index of the current column block reflector
                 ! in the matrices t and v.
                 do kb = kb_last, 1, -nblocal
                    ! determine the size of the current column block knb in
                    ! the matrices t and v.
                    knb = min( nblocal, n - kb + 1_ilp )
                    call stdlib_clarfb_gett( 'I', imb, n-kb+1, knb,t( 1_ilp, jb_t+kb-1 ), ldt, a( kb, &
                              kb ), lda,a( ib, kb ), lda, work, knb )
                 end do
              end do
           end if
           ! (2) top row block of a.
           ! note: if mb>=m, then we have only one row block of a of size m
           ! and we work on the entire matrix a.
           mb1 = min( mb, m )
           ! apply column blocks of h in the top row block from right to left.
           ! kb is the column index of the current block reflector in
           ! the matrices t and v.
           do kb = kb_last, 1, -nblocal
              ! determine the size of the current column block knb in
              ! the matrices t and v.
              knb = min( nblocal, n - kb + 1_ilp )
              if( mb1-kb-knb+1==0_ilp ) then
                 ! in stdlib_slarfb_gett parameters, when m=0, then the matrix b
                 ! does not exist, hence we need to pass a dummy array
                 ! reference dummy(1,1) to b with lddummy=1.
                 call stdlib_clarfb_gett( 'N', 0_ilp, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, kb ), lda,&
                           dummy( 1_ilp, 1_ilp ), 1_ilp, work, knb )
              else
                 call stdlib_clarfb_gett( 'N', mb1-kb-knb+1, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, &
                           kb ), lda,a( kb+knb, kb), lda, work, knb )
              end if
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
           return
     end subroutine stdlib_cungtsqr_row

     pure module subroutine stdlib_zungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
     !! ZUNGTSQR_ROW generates an M-by-N complex matrix Q_out with
     !! orthonormal columns from the output of ZLATSQR. These N orthonormal
     !! columns are the first N columns of a product of complex unitary
     !! matrices Q(k)_in of order M, which are returned by ZLATSQR in
     !! a special format.
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! The input matrices Q(k)_in are stored in row and column blocks in A.
     !! See the documentation of ZLATSQR for more details on the format of
     !! Q(k)_in, where each Q(k)_in is represented by block Householder
     !! transformations. This routine calls an auxiliary routine ZLARFB_GETT,
     !! where the computation is performed on each individual block. The
     !! algorithm first sweeps NB-sized column blocks from the right to left
     !! starting in the bottom row block and continues to the top row block
     !! (hence _ROW in the routine name). This sweep is in reverse order of
     !! the order in which ZLATSQR generates the output blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: nblocal, mb2, m_plus_one, itmp, ib_bottom, lworkopt, &
                     num_all_row_blocks, jb_t, ib, imb, kb, kb_last, knb, mb1
           ! Local Arrays 
           complex(dp) :: dummy(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           nblocal = min( nb, n )
           ! determine the workspace size.
           if( info==0_ilp ) then
              lworkopt = nblocal * max( nblocal, ( n - nblocal ) )
           end if
           ! handle error in the input parameters and handle the workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGTSQR_ROW', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           ! (0) set the upper-triangular part of the matrix a to zero and
           ! its diagonal elements to one.
           call stdlib_zlaset('U', m, n, czero, cone, a, lda )
           ! kb_last is the column index of the last column block reflector
           ! in the matrices t and v.
           kb_last = ( ( n-1 ) / nblocal ) * nblocal + 1_ilp
           ! (1) bottom-up loop over row blocks of a, except the top row block.
           ! note: if mb>=m, then the loop is never executed.
           if ( mb<m ) then
              ! mb2 is the row blocking size for the row blocks before the
              ! first top row block in the matrix a. ib is the row index for
              ! the row blocks in the matrix a before the first top row block.
              ! ib_bottom is the row index for the last bottom row block
              ! in the matrix a. jb_t is the column index of the corresponding
              ! column block in the matrix t.
              ! initialize variables.
              ! num_all_row_blocks is the number of row blocks in the matrix a
              ! including the first row block.
              mb2 = mb - n
              m_plus_one = m + 1_ilp
              itmp = ( m - mb - 1_ilp ) / mb2
              ib_bottom = itmp * mb2 + mb + 1_ilp
              num_all_row_blocks = itmp + 2_ilp
              jb_t = num_all_row_blocks * n + 1_ilp
              do ib = ib_bottom, mb+1, -mb2
                 ! determine the block size imb for the current row block
                 ! in the matrix a.
                 imb = min( m_plus_one - ib, mb2 )
                 ! determine the column index jb_t for the current column block
                 ! in the matrix t.
                 jb_t = jb_t - n
                 ! apply column blocks of h in the row block from right to left.
                 ! kb is the column index of the current column block reflector
                 ! in the matrices t and v.
                 do kb = kb_last, 1, -nblocal
                    ! determine the size of the current column block knb in
                    ! the matrices t and v.
                    knb = min( nblocal, n - kb + 1_ilp )
                    call stdlib_zlarfb_gett( 'I', imb, n-kb+1, knb,t( 1_ilp, jb_t+kb-1 ), ldt, a( kb, &
                              kb ), lda,a( ib, kb ), lda, work, knb )
                 end do
              end do
           end if
           ! (2) top row block of a.
           ! note: if mb>=m, then we have only one row block of a of size m
           ! and we work on the entire matrix a.
           mb1 = min( mb, m )
           ! apply column blocks of h in the top row block from right to left.
           ! kb is the column index of the current block reflector in
           ! the matrices t and v.
           do kb = kb_last, 1, -nblocal
              ! determine the size of the current column block knb in
              ! the matrices t and v.
              knb = min( nblocal, n - kb + 1_ilp )
              if( mb1-kb-knb+1==0_ilp ) then
                 ! in stdlib_slarfb_gett parameters, when m=0, then the matrix b
                 ! does not exist, hence we need to pass a dummy array
                 ! reference dummy(1,1) to b with lddummy=1.
                 call stdlib_zlarfb_gett( 'N', 0_ilp, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, kb ), lda,&
                           dummy( 1_ilp, 1_ilp ), 1_ilp, work, knb )
              else
                 call stdlib_zlarfb_gett( 'N', mb1-kb-knb+1, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, &
                           kb ), lda,a( kb+knb, kb), lda, work, knb )
              end if
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
           return
     end subroutine stdlib_zungtsqr_row




     pure module subroutine stdlib_sorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
     !! SORGTSQR generates an M-by-N real matrix Q_out with orthonormal columns,
     !! which are the first N columns of a product of real orthogonal
     !! matrices of order M which are returned by SLATSQR
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! See the documentation for SLATSQR.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iinfo, ldc, lworkopt, lc, lw, nblocal, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           lquery  = lwork==-1_ilp
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array c(ldc, n) and work(lwork)
              ! in the call to stdlib_slamtsqr. see the documentation for stdlib_slamtsqr.
              if( lwork<2_ilp .and. (.not.lquery) ) then
                 info = -10_ilp
              else
                 ! set block size for column blocks
                 nblocal = min( nb, n )
                 ! lwork = -1, then set the size for the array c(ldc,n)
                 ! in stdlib_slamtsqr call and set the optimal size of the work array
                 ! work(lwork) in stdlib_slamtsqr call.
                 ldc = m
                 lc = ldc*n
                 lw = n * nblocal
                 lworkopt = lc+lw
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -10_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGTSQR', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           ! (1) form explicitly the tall-skinny m-by-n left submatrix q1_in
           ! of m-by-m orthogonal matrix q_in, which is implicitly stored in
           ! the subdiagonal part of input array a and in the input array t.
           ! perform by the following operation using the routine stdlib_slamtsqr.
               ! q1_in = q_in * ( i ), where i is a n-by-n identity matrix,
                              ! ( 0 )        0 is a (m-n)-by-n zero matrix.
           ! (1a) form m-by-n matrix in the array work(1:ldc*n) with ones
           ! on the diagonal and zeros elsewhere.
           call stdlib_slaset( 'F', m, n, zero, one, work, ldc )
           ! (1b)  on input, work(1:ldc*n) stores ( i );
                                                ! ( 0 )
                 ! on output, work(1:ldc*n) stores q1_in.
           call stdlib_slamtsqr( 'L', 'N', m, n, n, mb, nblocal, a, lda, t, ldt,work, ldc, work( &
                     lc+1 ), lw, iinfo )
           ! (2) copy the result from the part of the work array (1:m,1:n)
           ! with the leading dimension ldc that starts at work(1) into
           ! the output array a(1:m,1:n) column-by-column.
           do j = 1, n
              call stdlib_scopy( m, work( (j-1)*ldc + 1_ilp ), 1_ilp, a( 1_ilp, j ), 1_ilp )
           end do
           work( 1_ilp ) = real( lworkopt,KIND=sp)
           return
     end subroutine stdlib_sorgtsqr

     pure module subroutine stdlib_dorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
     !! DORGTSQR generates an M-by-N real matrix Q_out with orthonormal columns,
     !! which are the first N columns of a product of real orthogonal
     !! matrices of order M which are returned by DLATSQR
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! See the documentation for DLATSQR.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: iinfo, ldc, lworkopt, lc, lw, nblocal, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           lquery  = lwork==-1_ilp
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array c(ldc, n) and work(lwork)
              ! in the call to stdlib_dlamtsqr. see the documentation for stdlib_dlamtsqr.
              if( lwork<2_ilp .and. (.not.lquery) ) then
                 info = -10_ilp
              else
                 ! set block size for column blocks
                 nblocal = min( nb, n )
                 ! lwork = -1, then set the size for the array c(ldc,n)
                 ! in stdlib_dlamtsqr call and set the optimal size of the work array
                 ! work(lwork) in stdlib_dlamtsqr call.
                 ldc = m
                 lc = ldc*n
                 lw = n * nblocal
                 lworkopt = lc+lw
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -10_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGTSQR', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           ! (1) form explicitly the tall-skinny m-by-n left submatrix q1_in
           ! of m-by-m orthogonal matrix q_in, which is implicitly stored in
           ! the subdiagonal part of input array a and in the input array t.
           ! perform by the following operation using the routine stdlib_dlamtsqr.
               ! q1_in = q_in * ( i ), where i is a n-by-n identity matrix,
                              ! ( 0 )        0 is a (m-n)-by-n zero matrix.
           ! (1a) form m-by-n matrix in the array work(1:ldc*n) with ones
           ! on the diagonal and zeros elsewhere.
           call stdlib_dlaset( 'F', m, n, zero, one, work, ldc )
           ! (1b)  on input, work(1:ldc*n) stores ( i );
                                                ! ( 0 )
                 ! on output, work(1:ldc*n) stores q1_in.
           call stdlib_dlamtsqr( 'L', 'N', m, n, n, mb, nblocal, a, lda, t, ldt,work, ldc, work( &
                     lc+1 ), lw, iinfo )
           ! (2) copy the result from the part of the work array (1:m,1:n)
           ! with the leading dimension ldc that starts at work(1) into
           ! the output array a(1:m,1:n) column-by-column.
           do j = 1, n
              call stdlib_dcopy( m, work( (j-1)*ldc + 1_ilp ), 1_ilp, a( 1_ilp, j ), 1_ilp )
           end do
           work( 1_ilp ) = real( lworkopt,KIND=dp)
           return
     end subroutine stdlib_dorgtsqr




     pure module subroutine stdlib_sorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
     !! SORGTSQR_ROW generates an M-by-N real matrix Q_out with
     !! orthonormal columns from the output of SLATSQR. These N orthonormal
     !! columns are the first N columns of a product of complex unitary
     !! matrices Q(k)_in of order M, which are returned by SLATSQR in
     !! a special format.
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! The input matrices Q(k)_in are stored in row and column blocks in A.
     !! See the documentation of SLATSQR for more details on the format of
     !! Q(k)_in, where each Q(k)_in is represented by block Householder
     !! transformations. This routine calls an auxiliary routine SLARFB_GETT,
     !! where the computation is performed on each individual block. The
     !! algorithm first sweeps NB-sized column blocks from the right to left
     !! starting in the bottom row block and continues to the top row block
     !! (hence _ROW in the routine name). This sweep is in reverse order of
     !! the order in which SLATSQR generates the output blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: nblocal, mb2, m_plus_one, itmp, ib_bottom, lworkopt, &
                     num_all_row_blocks, jb_t, ib, imb, kb, kb_last, knb, mb1
           ! Local Arrays 
           real(sp) :: dummy(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           nblocal = min( nb, n )
           ! determine the workspace size.
           if( info==0_ilp ) then
              lworkopt = nblocal * max( nblocal, ( n - nblocal ) )
           end if
           ! handle error in the input parameters and handle the workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGTSQR_ROW', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           ! (0) set the upper-triangular part of the matrix a to zero and
           ! its diagonal elements to one.
           call stdlib_slaset('U', m, n, zero, one, a, lda )
           ! kb_last is the column index of the last column block reflector
           ! in the matrices t and v.
           kb_last = ( ( n-1 ) / nblocal ) * nblocal + 1_ilp
           ! (1) bottom-up loop over row blocks of a, except the top row block.
           ! note: if mb>=m, then the loop is never executed.
           if ( mb<m ) then
              ! mb2 is the row blocking size for the row blocks before the
              ! first top row block in the matrix a. ib is the row index for
              ! the row blocks in the matrix a before the first top row block.
              ! ib_bottom is the row index for the last bottom row block
              ! in the matrix a. jb_t is the column index of the corresponding
              ! column block in the matrix t.
              ! initialize variables.
              ! num_all_row_blocks is the number of row blocks in the matrix a
              ! including the first row block.
              mb2 = mb - n
              m_plus_one = m + 1_ilp
              itmp = ( m - mb - 1_ilp ) / mb2
              ib_bottom = itmp * mb2 + mb + 1_ilp
              num_all_row_blocks = itmp + 2_ilp
              jb_t = num_all_row_blocks * n + 1_ilp
              do ib = ib_bottom, mb+1, -mb2
                 ! determine the block size imb for the current row block
                 ! in the matrix a.
                 imb = min( m_plus_one - ib, mb2 )
                 ! determine the column index jb_t for the current column block
                 ! in the matrix t.
                 jb_t = jb_t - n
                 ! apply column blocks of h in the row block from right to left.
                 ! kb is the column index of the current column block reflector
                 ! in the matrices t and v.
                 do kb = kb_last, 1, -nblocal
                    ! determine the size of the current column block knb in
                    ! the matrices t and v.
                    knb = min( nblocal, n - kb + 1_ilp )
                    call stdlib_slarfb_gett( 'I', imb, n-kb+1, knb,t( 1_ilp, jb_t+kb-1 ), ldt, a( kb, &
                              kb ), lda,a( ib, kb ), lda, work, knb )
                 end do
              end do
           end if
           ! (2) top row block of a.
           ! note: if mb>=m, then we have only one row block of a of size m
           ! and we work on the entire matrix a.
           mb1 = min( mb, m )
           ! apply column blocks of h in the top row block from right to left.
           ! kb is the column index of the current block reflector in
           ! the matrices t and v.
           do kb = kb_last, 1, -nblocal
              ! determine the size of the current column block knb in
              ! the matrices t and v.
              knb = min( nblocal, n - kb + 1_ilp )
              if( mb1-kb-knb+1==0_ilp ) then
                 ! in stdlib_slarfb_gett parameters, when m=0, then the matrix b
                 ! does not exist, hence we need to pass a dummy array
                 ! reference dummy(1,1) to b with lddummy=1.
                 call stdlib_slarfb_gett( 'N', 0_ilp, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, kb ), lda,&
                           dummy( 1_ilp, 1_ilp ), 1_ilp, work, knb )
              else
                 call stdlib_slarfb_gett( 'N', mb1-kb-knb+1, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, &
                           kb ), lda,a( kb+knb, kb), lda, work, knb )
              end if
           end do
           work( 1_ilp ) = real( lworkopt,KIND=sp)
           return
     end subroutine stdlib_sorgtsqr_row

     pure module subroutine stdlib_dorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
     !! DORGTSQR_ROW generates an M-by-N real matrix Q_out with
     !! orthonormal columns from the output of DLATSQR. These N orthonormal
     !! columns are the first N columns of a product of complex unitary
     !! matrices Q(k)_in of order M, which are returned by DLATSQR in
     !! a special format.
     !! Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
     !! The input matrices Q(k)_in are stored in row and column blocks in A.
     !! See the documentation of DLATSQR for more details on the format of
     !! Q(k)_in, where each Q(k)_in is represented by block Householder
     !! transformations. This routine calls an auxiliary routine DLARFB_GETT,
     !! where the computation is performed on each individual block. The
     !! algorithm first sweeps NB-sized column blocks from the right to left
     !! starting in the bottom row block and continues to the top row block
     !! (hence _ROW in the routine name). This sweep is in reverse order of
     !! the order in which DLATSQR generates the output blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: nblocal, mb2, m_plus_one, itmp, ib_bottom, lworkopt, &
                     num_all_row_blocks, jb_t, ib, imb, kb, kb_last, knb, mb1
           ! Local Arrays 
           real(dp) :: dummy(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb<=n ) then
              info = -3_ilp
           else if( nb<1_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           nblocal = min( nb, n )
           ! determine the workspace size.
           if( info==0_ilp ) then
              lworkopt = nblocal * max( nblocal, ( n - nblocal ) )
           end if
           ! handle error in the input parameters and handle the workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGTSQR_ROW', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           ! (0) set the upper-triangular part of the matrix a to zero and
           ! its diagonal elements to one.
           call stdlib_dlaset('U', m, n, zero, one, a, lda )
           ! kb_last is the column index of the last column block reflector
           ! in the matrices t and v.
           kb_last = ( ( n-1 ) / nblocal ) * nblocal + 1_ilp
           ! (1) bottom-up loop over row blocks of a, except the top row block.
           ! note: if mb>=m, then the loop is never executed.
           if ( mb<m ) then
              ! mb2 is the row blocking size for the row blocks before the
              ! first top row block in the matrix a. ib is the row index for
              ! the row blocks in the matrix a before the first top row block.
              ! ib_bottom is the row index for the last bottom row block
              ! in the matrix a. jb_t is the column index of the corresponding
              ! column block in the matrix t.
              ! initialize variables.
              ! num_all_row_blocks is the number of row blocks in the matrix a
              ! including the first row block.
              mb2 = mb - n
              m_plus_one = m + 1_ilp
              itmp = ( m - mb - 1_ilp ) / mb2
              ib_bottom = itmp * mb2 + mb + 1_ilp
              num_all_row_blocks = itmp + 2_ilp
              jb_t = num_all_row_blocks * n + 1_ilp
              do ib = ib_bottom, mb+1, -mb2
                 ! determine the block size imb for the current row block
                 ! in the matrix a.
                 imb = min( m_plus_one - ib, mb2 )
                 ! determine the column index jb_t for the current column block
                 ! in the matrix t.
                 jb_t = jb_t - n
                 ! apply column blocks of h in the row block from right to left.
                 ! kb is the column index of the current column block reflector
                 ! in the matrices t and v.
                 do kb = kb_last, 1, -nblocal
                    ! determine the size of the current column block knb in
                    ! the matrices t and v.
                    knb = min( nblocal, n - kb + 1_ilp )
                    call stdlib_dlarfb_gett( 'I', imb, n-kb+1, knb,t( 1_ilp, jb_t+kb-1 ), ldt, a( kb, &
                              kb ), lda,a( ib, kb ), lda, work, knb )
                 end do
              end do
           end if
           ! (2) top row block of a.
           ! note: if mb>=m, then we have only one row block of a of size m
           ! and we work on the entire matrix a.
           mb1 = min( mb, m )
           ! apply column blocks of h in the top row block from right to left.
           ! kb is the column index of the current block reflector in
           ! the matrices t and v.
           do kb = kb_last, 1, -nblocal
              ! determine the size of the current column block knb in
              ! the matrices t and v.
              knb = min( nblocal, n - kb + 1_ilp )
              if( mb1-kb-knb+1==0_ilp ) then
                 ! in stdlib_slarfb_gett parameters, when m=0, then the matrix b
                 ! does not exist, hence we need to pass a dummy array
                 ! reference dummy(1,1) to b with lddummy=1.
                 call stdlib_dlarfb_gett( 'N', 0_ilp, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, kb ), lda,&
                           dummy( 1_ilp, 1_ilp ), 1_ilp, work, knb )
              else
                 call stdlib_dlarfb_gett( 'N', mb1-kb-knb+1, n-kb+1, knb,t( 1_ilp, kb ), ldt, a( kb, &
                           kb ), lda,a( kb+knb, kb), lda, work, knb )
              end if
           end do
           work( 1_ilp ) = real( lworkopt,KIND=dp)
           return
     end subroutine stdlib_dorgtsqr_row




     pure module subroutine stdlib_slarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
     !! SLARFB_GETT applies a real Householder block reflector H from the
     !! left to a real (K+M)-by-N  "triangular-pentagonal" matrix
     !! composed of two block matrices: an upper trapezoidal K-by-N matrix A
     !! stored in the array A, and a rectangular M-by-(N-K) matrix B, stored
     !! in the array B. The block reflector H is stored in a compact
     !! WY-representation, where the elementary reflectors are in the
     !! arrays A, B and T. See Further Details section.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnotident
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<0 .or. n<=0 .or. k==0 .or. k>n )return
           lnotident = .not.stdlib_lsame( ident, 'I' )
           ! ------------------------------------------------------------------
           ! first step. computation of the column block 2:
              ! ( a2 ) := h * ( a2 )
              ! ( b2 )        ( b2 )
           ! ------------------------------------------------------------------
           if( n>k ) then
              ! col2_(1) compute w2: = a2. therefore, copy a2 = a(1:k, k+1:n)
              ! into w2=work(1:k, 1:n-k) column-by-column.
              do j = 1, n-k
                 call stdlib_scopy( k, a( 1_ilp, k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              if( lnotident ) then
                 ! col2_(2) compute w2: = (v1**t) * w2 = (a1**t) * w2,
                 ! v1 is not an identy matrix, but unit lower-triangular
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_strmm( 'L', 'L', 'T', 'U', k, n-k, one, a, lda,work, ldwork )
              end if
              ! col2_(3) compute w2: = w2 + (v2**t) * b2 = w2 + (b1**t) * b2
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_sgemm( 'T', 'N', k, n-k, m, one, b, ldb,b( 1_ilp, k+1 ), ldb, one, work, &
                           ldwork )
              end if
              ! col2_(4) compute w2: = t * w2,
              ! t is upper-triangular.
              call stdlib_strmm( 'L', 'U', 'N', 'N', k, n-k, one, t, ldt,work, ldwork )
              ! col2_(5) compute b2: = b2 - v2 * w2 = b2 - b1 * w2,
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_sgemm( 'N', 'N', m, n-k, k, -one, b, ldb,work, ldwork, one, b( 1_ilp, k+&
                           1_ilp ), ldb )
              end if
              if( lnotident ) then
                 ! col2_(6) compute w2: = v1 * w2 = a1 * w2,
                 ! v1 is not an identity matrix, but unit lower-triangular,
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_strmm( 'L', 'L', 'N', 'U', k, n-k, one, a, lda,work, ldwork )
              end if
              ! col2_(7) compute a2: = a2 - w2 =
                                   ! = a(1:k, k+1:n-k) - work(1:k, 1:n-k),
              ! column-by-column.
              do j = 1, n-k
                 do i = 1, k
                    a( i, k+j ) = a( i, k+j ) - work( i, j )
                 end do
              end do
           end if
           ! ------------------------------------------------------------------
           ! second step. computation of the column block 1:
              ! ( a1 ) := h * ( a1 )
              ! ( b1 )        (  0 )
           ! ------------------------------------------------------------------
           ! col1_(1) compute w1: = a1. copy the upper-triangular
           ! a1 = a(1:k, 1:k) into the upper-triangular
           ! w1 = work(1:k, 1:k) column-by-column.
           do j = 1, k
              call stdlib_scopy( j, a( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
           end do
           ! set the subdiagonal elements of w1 to zero column-by-column.
           do j = 1, k - 1
              do i = j + 1, k
                 work( i, j ) = zero
              end do
           end do
           if( lnotident ) then
              ! col1_(2) compute w1: = (v1**t) * w1 = (a1**t) * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular with zeroes below the diagonal.
              call stdlib_strmm( 'L', 'L', 'T', 'U', k, k, one, a, lda,work, ldwork )
           end if
           ! col1_(3) compute w1: = t * w1,
           ! t is upper-triangular,
           ! w1 is upper-triangular with zeroes below the diagonal.
           call stdlib_strmm( 'L', 'U', 'N', 'N', k, k, one, t, ldt,work, ldwork )
           ! col1_(4) compute b1: = - v2 * w1 = - b1 * w1,
           ! v2 = b1, w1 is upper-triangular with zeroes below the diagonal.
           if( m>0_ilp ) then
              call stdlib_strmm( 'R', 'U', 'N', 'N', m, k, -one, work, ldwork,b, ldb )
           end if
           if( lnotident ) then
              ! col1_(5) compute w1: = v1 * w1 = a1 * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular on input with zeroes below the diagonal,
              ! and square on output.
              call stdlib_strmm( 'L', 'L', 'N', 'U', k, k, one, a, lda,work, ldwork )
              ! col1_(6) compute a1: = a1 - w1 = a(1:k, 1:k) - work(1:k, 1:k)
              ! column-by-column. a1 is upper-triangular on input.
              ! if ident, a1 is square on output, and w1 is square,
              ! if not ident, a1 is upper-triangular on output,
              ! w1 is upper-triangular.
              ! col1_(6)_a compute elements of a1 below the diagonal.
              do j = 1, k - 1
                 do i = j + 1, k
                    a( i, j ) = - work( i, j )
                 end do
              end do
           end if
           ! col1_(6)_b compute elements of a1 on and above the diagonal.
           do j = 1, k
              do i = 1, j
                 a( i, j ) = a( i, j ) - work( i, j )
              end do
           end do
           return
     end subroutine stdlib_slarfb_gett

     pure module subroutine stdlib_dlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
     !! DLARFB_GETT applies a real Householder block reflector H from the
     !! left to a real (K+M)-by-N  "triangular-pentagonal" matrix
     !! composed of two block matrices: an upper trapezoidal K-by-N matrix A
     !! stored in the array A, and a rectangular M-by-(N-K) matrix B, stored
     !! in the array B. The block reflector H is stored in a compact
     !! WY-representation, where the elementary reflectors are in the
     !! arrays A, B and T. See Further Details section.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnotident
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<0 .or. n<=0 .or. k==0 .or. k>n )return
           lnotident = .not.stdlib_lsame( ident, 'I' )
           ! ------------------------------------------------------------------
           ! first step. computation of the column block 2:
              ! ( a2 ) := h * ( a2 )
              ! ( b2 )        ( b2 )
           ! ------------------------------------------------------------------
           if( n>k ) then
              ! col2_(1) compute w2: = a2. therefore, copy a2 = a(1:k, k+1:n)
              ! into w2=work(1:k, 1:n-k) column-by-column.
              do j = 1, n-k
                 call stdlib_dcopy( k, a( 1_ilp, k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              if( lnotident ) then
                 ! col2_(2) compute w2: = (v1**t) * w2 = (a1**t) * w2,
                 ! v1 is not an identy matrix, but unit lower-triangular
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_dtrmm( 'L', 'L', 'T', 'U', k, n-k, one, a, lda,work, ldwork )
              end if
              ! col2_(3) compute w2: = w2 + (v2**t) * b2 = w2 + (b1**t) * b2
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_dgemm( 'T', 'N', k, n-k, m, one, b, ldb,b( 1_ilp, k+1 ), ldb, one, work, &
                           ldwork )
              end if
              ! col2_(4) compute w2: = t * w2,
              ! t is upper-triangular.
              call stdlib_dtrmm( 'L', 'U', 'N', 'N', k, n-k, one, t, ldt,work, ldwork )
              ! col2_(5) compute b2: = b2 - v2 * w2 = b2 - b1 * w2,
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_dgemm( 'N', 'N', m, n-k, k, -one, b, ldb,work, ldwork, one, b( 1_ilp, k+&
                           1_ilp ), ldb )
              end if
              if( lnotident ) then
                 ! col2_(6) compute w2: = v1 * w2 = a1 * w2,
                 ! v1 is not an identity matrix, but unit lower-triangular,
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_dtrmm( 'L', 'L', 'N', 'U', k, n-k, one, a, lda,work, ldwork )
              end if
              ! col2_(7) compute a2: = a2 - w2 =
                                   ! = a(1:k, k+1:n-k) - work(1:k, 1:n-k),
              ! column-by-column.
              do j = 1, n-k
                 do i = 1, k
                    a( i, k+j ) = a( i, k+j ) - work( i, j )
                 end do
              end do
           end if
           ! ------------------------------------------------------------------
           ! second step. computation of the column block 1:
              ! ( a1 ) := h * ( a1 )
              ! ( b1 )        (  0 )
           ! ------------------------------------------------------------------
           ! col1_(1) compute w1: = a1. copy the upper-triangular
           ! a1 = a(1:k, 1:k) into the upper-triangular
           ! w1 = work(1:k, 1:k) column-by-column.
           do j = 1, k
              call stdlib_dcopy( j, a( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
           end do
           ! set the subdiagonal elements of w1 to zero column-by-column.
           do j = 1, k - 1
              do i = j + 1, k
                 work( i, j ) = zero
              end do
           end do
           if( lnotident ) then
              ! col1_(2) compute w1: = (v1**t) * w1 = (a1**t) * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular with zeroes below the diagonal.
              call stdlib_dtrmm( 'L', 'L', 'T', 'U', k, k, one, a, lda,work, ldwork )
           end if
           ! col1_(3) compute w1: = t * w1,
           ! t is upper-triangular,
           ! w1 is upper-triangular with zeroes below the diagonal.
           call stdlib_dtrmm( 'L', 'U', 'N', 'N', k, k, one, t, ldt,work, ldwork )
           ! col1_(4) compute b1: = - v2 * w1 = - b1 * w1,
           ! v2 = b1, w1 is upper-triangular with zeroes below the diagonal.
           if( m>0_ilp ) then
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', m, k, -one, work, ldwork,b, ldb )
           end if
           if( lnotident ) then
              ! col1_(5) compute w1: = v1 * w1 = a1 * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular on input with zeroes below the diagonal,
              ! and square on output.
              call stdlib_dtrmm( 'L', 'L', 'N', 'U', k, k, one, a, lda,work, ldwork )
              ! col1_(6) compute a1: = a1 - w1 = a(1:k, 1:k) - work(1:k, 1:k)
              ! column-by-column. a1 is upper-triangular on input.
              ! if ident, a1 is square on output, and w1 is square,
              ! if not ident, a1 is upper-triangular on output,
              ! w1 is upper-triangular.
              ! col1_(6)_a compute elements of a1 below the diagonal.
              do j = 1, k - 1
                 do i = j + 1, k
                    a( i, j ) = - work( i, j )
                 end do
              end do
           end if
           ! col1_(6)_b compute elements of a1 on and above the diagonal.
           do j = 1, k
              do i = 1, j
                 a( i, j ) = a( i, j ) - work( i, j )
              end do
           end do
           return
     end subroutine stdlib_dlarfb_gett


     pure module subroutine stdlib_clarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
     !! CLARFB_GETT applies a complex Householder block reflector H from the
     !! left to a complex (K+M)-by-N  "triangular-pentagonal" matrix
     !! composed of two block matrices: an upper trapezoidal K-by-N matrix A
     !! stored in the array A, and a rectangular M-by-(N-K) matrix B, stored
     !! in the array B. The block reflector H is stored in a compact
     !! WY-representation, where the elementary reflectors are in the
     !! arrays A, B and T. See Further Details section.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnotident
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<0 .or. n<=0 .or. k==0 .or. k>n )return
           lnotident = .not.stdlib_lsame( ident, 'I' )
           ! ------------------------------------------------------------------
           ! first step. computation of the column block 2:
              ! ( a2 ) := h * ( a2 )
              ! ( b2 )        ( b2 )
           ! ------------------------------------------------------------------
           if( n>k ) then
              ! col2_(1) compute w2: = a2. therefore, copy a2 = a(1:k, k+1:n)
              ! into w2=work(1:k, 1:n-k) column-by-column.
              do j = 1, n-k
                 call stdlib_ccopy( k, a( 1_ilp, k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              if( lnotident ) then
                 ! col2_(2) compute w2: = (v1**h) * w2 = (a1**h) * w2,
                 ! v1 is not an identy matrix, but unit lower-triangular
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_ctrmm( 'L', 'L', 'C', 'U', k, n-k, cone, a, lda,work, ldwork )
                           
              end if
              ! col2_(3) compute w2: = w2 + (v2**h) * b2 = w2 + (b1**h) * b2
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_cgemm( 'C', 'N', k, n-k, m, cone, b, ldb,b( 1_ilp, k+1 ), ldb, cone, &
                           work, ldwork )
              end if
              ! col2_(4) compute w2: = t * w2,
              ! t is upper-triangular.
              call stdlib_ctrmm( 'L', 'U', 'N', 'N', k, n-k, cone, t, ldt,work, ldwork )
              ! col2_(5) compute b2: = b2 - v2 * w2 = b2 - b1 * w2,
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_cgemm( 'N', 'N', m, n-k, k, -cone, b, ldb,work, ldwork, cone, b( 1_ilp, &
                           k+1 ), ldb )
              end if
              if( lnotident ) then
                 ! col2_(6) compute w2: = v1 * w2 = a1 * w2,
                 ! v1 is not an identity matrix, but unit lower-triangular,
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_ctrmm( 'L', 'L', 'N', 'U', k, n-k, cone, a, lda,work, ldwork )
                           
              end if
              ! col2_(7) compute a2: = a2 - w2 =
                                   ! = a(1:k, k+1:n-k) - work(1:k, 1:n-k),
              ! column-by-column.
              do j = 1, n-k
                 do i = 1, k
                    a( i, k+j ) = a( i, k+j ) - work( i, j )
                 end do
              end do
           end if
           ! ------------------------------------------------------------------
           ! second step. computation of the column block 1:
              ! ( a1 ) := h * ( a1 )
              ! ( b1 )        (  0 )
           ! ------------------------------------------------------------------
           ! col1_(1) compute w1: = a1. copy the upper-triangular
           ! a1 = a(1:k, 1:k) into the upper-triangular
           ! w1 = work(1:k, 1:k) column-by-column.
           do j = 1, k
              call stdlib_ccopy( j, a( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
           end do
           ! set the subdiagonal elements of w1 to zero column-by-column.
           do j = 1, k - 1
              do i = j + 1, k
                 work( i, j ) = czero
              end do
           end do
           if( lnotident ) then
              ! col1_(2) compute w1: = (v1**h) * w1 = (a1**h) * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular with zeroes below the diagonal.
              call stdlib_ctrmm( 'L', 'L', 'C', 'U', k, k, cone, a, lda,work, ldwork )
           end if
           ! col1_(3) compute w1: = t * w1,
           ! t is upper-triangular,
           ! w1 is upper-triangular with zeroes below the diagonal.
           call stdlib_ctrmm( 'L', 'U', 'N', 'N', k, k, cone, t, ldt,work, ldwork )
           ! col1_(4) compute b1: = - v2 * w1 = - b1 * w1,
           ! v2 = b1, w1 is upper-triangular with zeroes below the diagonal.
           if( m>0_ilp ) then
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', m, k, -cone, work, ldwork,b, ldb )
           end if
           if( lnotident ) then
              ! col1_(5) compute w1: = v1 * w1 = a1 * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular on input with zeroes below the diagonal,
              ! and square on output.
              call stdlib_ctrmm( 'L', 'L', 'N', 'U', k, k, cone, a, lda,work, ldwork )
              ! col1_(6) compute a1: = a1 - w1 = a(1:k, 1:k) - work(1:k, 1:k)
              ! column-by-column. a1 is upper-triangular on input.
              ! if ident, a1 is square on output, and w1 is square,
              ! if not ident, a1 is upper-triangular on output,
              ! w1 is upper-triangular.
              ! col1_(6)_a compute elements of a1 below the diagonal.
              do j = 1, k - 1
                 do i = j + 1, k
                    a( i, j ) = - work( i, j )
                 end do
              end do
           end if
           ! col1_(6)_b compute elements of a1 on and above the diagonal.
           do j = 1, k
              do i = 1, j
                 a( i, j ) = a( i, j ) - work( i, j )
              end do
           end do
           return
     end subroutine stdlib_clarfb_gett

     pure module subroutine stdlib_zlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
     !! ZLARFB_GETT applies a complex Householder block reflector H from the
     !! left to a complex (K+M)-by-N  "triangular-pentagonal" matrix
     !! composed of two block matrices: an upper trapezoidal K-by-N matrix A
     !! stored in the array A, and a rectangular M-by-(N-K) matrix B, stored
     !! in the array B. The block reflector H is stored in a compact
     !! WY-representation, where the elementary reflectors are in the
     !! arrays A, B and T. See Further Details section.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnotident
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<0 .or. n<=0 .or. k==0 .or. k>n )return
           lnotident = .not.stdlib_lsame( ident, 'I' )
           ! ------------------------------------------------------------------
           ! first step. computation of the column block 2:
              ! ( a2 ) := h * ( a2 )
              ! ( b2 )        ( b2 )
           ! ------------------------------------------------------------------
           if( n>k ) then
              ! col2_(1) compute w2: = a2. therefore, copy a2 = a(1:k, k+1:n)
              ! into w2=work(1:k, 1:n-k) column-by-column.
              do j = 1, n-k
                 call stdlib_zcopy( k, a( 1_ilp, k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              if( lnotident ) then
                 ! col2_(2) compute w2: = (v1**h) * w2 = (a1**h) * w2,
                 ! v1 is not an identy matrix, but unit lower-triangular
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_ztrmm( 'L', 'L', 'C', 'U', k, n-k, cone, a, lda,work, ldwork )
                           
              end if
              ! col2_(3) compute w2: = w2 + (v2**h) * b2 = w2 + (b1**h) * b2
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_zgemm( 'C', 'N', k, n-k, m, cone, b, ldb,b( 1_ilp, k+1 ), ldb, cone, &
                           work, ldwork )
              end if
              ! col2_(4) compute w2: = t * w2,
              ! t is upper-triangular.
              call stdlib_ztrmm( 'L', 'U', 'N', 'N', k, n-k, cone, t, ldt,work, ldwork )
              ! col2_(5) compute b2: = b2 - v2 * w2 = b2 - b1 * w2,
              ! v2 stored in b1.
              if( m>0_ilp ) then
                 call stdlib_zgemm( 'N', 'N', m, n-k, k, -cone, b, ldb,work, ldwork, cone, b( 1_ilp, &
                           k+1 ), ldb )
              end if
              if( lnotident ) then
                 ! col2_(6) compute w2: = v1 * w2 = a1 * w2,
                 ! v1 is not an identity matrix, but unit lower-triangular,
                 ! v1 stored in a1 (diagonal ones are not stored).
                 call stdlib_ztrmm( 'L', 'L', 'N', 'U', k, n-k, cone, a, lda,work, ldwork )
                           
              end if
              ! col2_(7) compute a2: = a2 - w2 =
                                   ! = a(1:k, k+1:n-k) - work(1:k, 1:n-k),
              ! column-by-column.
              do j = 1, n-k
                 do i = 1, k
                    a( i, k+j ) = a( i, k+j ) - work( i, j )
                 end do
              end do
           end if
           ! ------------------------------------------------------------------
           ! second step. computation of the column block 1:
              ! ( a1 ) := h * ( a1 )
              ! ( b1 )        (  0 )
           ! ------------------------------------------------------------------
           ! col1_(1) compute w1: = a1. copy the upper-triangular
           ! a1 = a(1:k, 1:k) into the upper-triangular
           ! w1 = work(1:k, 1:k) column-by-column.
           do j = 1, k
              call stdlib_zcopy( j, a( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
           end do
           ! set the subdiagonal elements of w1 to zero column-by-column.
           do j = 1, k - 1
              do i = j + 1, k
                 work( i, j ) = czero
              end do
           end do
           if( lnotident ) then
              ! col1_(2) compute w1: = (v1**h) * w1 = (a1**h) * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular with zeroes below the diagonal.
              call stdlib_ztrmm( 'L', 'L', 'C', 'U', k, k, cone, a, lda,work, ldwork )
           end if
           ! col1_(3) compute w1: = t * w1,
           ! t is upper-triangular,
           ! w1 is upper-triangular with zeroes below the diagonal.
           call stdlib_ztrmm( 'L', 'U', 'N', 'N', k, k, cone, t, ldt,work, ldwork )
           ! col1_(4) compute b1: = - v2 * w1 = - b1 * w1,
           ! v2 = b1, w1 is upper-triangular with zeroes below the diagonal.
           if( m>0_ilp ) then
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', m, k, -cone, work, ldwork,b, ldb )
           end if
           if( lnotident ) then
              ! col1_(5) compute w1: = v1 * w1 = a1 * w1,
              ! v1 is not an identity matrix, but unit lower-triangular
              ! v1 stored in a1 (diagonal ones are not stored),
              ! w1 is upper-triangular on input with zeroes below the diagonal,
              ! and square on output.
              call stdlib_ztrmm( 'L', 'L', 'N', 'U', k, k, cone, a, lda,work, ldwork )
              ! col1_(6) compute a1: = a1 - w1 = a(1:k, 1:k) - work(1:k, 1:k)
              ! column-by-column. a1 is upper-triangular on input.
              ! if ident, a1 is square on output, and w1 is square,
              ! if not ident, a1 is upper-triangular on output,
              ! w1 is upper-triangular.
              ! col1_(6)_a compute elements of a1 below the diagonal.
              do j = 1, k - 1
                 do i = j + 1, k
                    a( i, j ) = - work( i, j )
                 end do
              end do
           end if
           ! col1_(6)_b compute elements of a1 on and above the diagonal.
           do j = 1, k
              do i = 1, j
                 a( i, j ) = a( i, j ) - work( i, j )
              end do
           end do
           return
     end subroutine stdlib_zlarfb_gett




     pure module subroutine stdlib_slamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! SLAMTSQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (SLATSQR)
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: c(ldc,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: i, ii, kk, lw, ctr, q
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * nb
             q = m
           else
             lw = mb * nb
             q = n
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( k<nb .or. nb<1_ilp ) then
             info = -7_ilp
           else if( lda<max( 1_ilp, q ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, nb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           ! determine the block size if it is tall skinny or short and wide
           if( info==0_ilp)  then
               work(1_ilp) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SLAMTSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((mb<=k).or.(mb>=max(m,n,k))) then
             call stdlib_sgemqrt( side, trans, m, n, k, nb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
            end if
           if(left.and.notran) then
               ! multiply q to the last block of c
              kk = mod((m-k),(mb-k))
              ctr = (m-k)/(mb-k)
              if (kk>0_ilp) then
                ii=m-kk+1
                call stdlib_stpmqrt('L','N',kk , n, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp,ctr*k+1),ldt , c(1_ilp,&
                          1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              else
                ii=m+1
              end if
              do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
                ctr = ctr - 1_ilp
                call stdlib_stpmqrt('L','N',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), ldt,&
                           c(1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
              end do
               ! multiply q to the first block of c (1:mb,1:n)
              call stdlib_sgemqrt('L','N',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
           else if (left.and.tran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(mb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_sgemqrt('L','T',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
               call stdlib_stpmqrt('L','T',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp),ldt, c(&
                         1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_stpmqrt('L','T',kk , n, k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), ldt, &
                         c(1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.tran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(mb-k))
               ctr = (n-k)/(mb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_stpmqrt('R','T',m , kk, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), &
                           ldt, c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                 ctr = ctr - 1_ilp
                 call stdlib_stpmqrt('R','T',m , mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), &
                           ldt, c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_sgemqrt('R','T',m , mb, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.notran) then
               ! multiply q to the first block of c
              kk = mod((n-k),(mb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_sgemqrt('R','N', m, mb , k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_stpmqrt('R','N', m, mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt, &
                         c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
               ! multiply q to the last block of c
               call stdlib_stpmqrt('R','N', m, kk , k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt, &
                         c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_slamtsqr

     pure module subroutine stdlib_dlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! DLAMTSQR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (DLATSQR)
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: c(ldc,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: i, ii, kk, lw, ctr, q
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * nb
             q = m
           else
             lw = mb * nb
             q = n
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( k<nb .or. nb<1_ilp ) then
             info = -7_ilp
           else if( lda<max( 1_ilp, q ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, nb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           ! determine the block size if it is tall skinny or short and wide
           if( info==0_ilp)  then
               work(1_ilp) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DLAMTSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((mb<=k).or.(mb>=max(m,n,k))) then
             call stdlib_dgemqrt( side, trans, m, n, k, nb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
            end if
           if(left.and.notran) then
               ! multiply q to the last block of c
              kk = mod((m-k),(mb-k))
              ctr = (m-k)/(mb-k)
              if (kk>0_ilp) then
                ii=m-kk+1
                call stdlib_dtpmqrt('L','N',kk , n, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp,ctr*k+1),ldt , c(1_ilp,&
                          1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              else
                ii=m+1
              end if
              do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
                ctr = ctr - 1_ilp
                call stdlib_dtpmqrt('L','N',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,&
                          1_ilp), ldc,c(i,1_ilp), ldc, work, info )
              end do
               ! multiply q to the first block of c (1:mb,1:n)
              call stdlib_dgemqrt('L','N',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
           else if (left.and.tran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(mb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_dgemqrt('L','T',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
               call stdlib_dtpmqrt('L','T',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp),ldt, c(&
                         1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_dtpmqrt('L','T',kk , n, k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp), ldt, c(&
                         1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.tran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(mb-k))
               ctr = (n-k)/(mb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_dtpmqrt('R','T',m , kk, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                 ctr = ctr - 1_ilp
                 call stdlib_dtpmqrt('R','T',m , mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_dgemqrt('R','T',m , mb, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.notran) then
               ! multiply q to the first block of c
              kk = mod((n-k),(mb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_dgemqrt('R','N', m, mb , k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_dtpmqrt('R','N', m, mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt, &
                         c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
               ! multiply q to the last block of c
               call stdlib_dtpmqrt('R','N', m, kk , k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt, &
                         c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_dlamtsqr


     pure module subroutine stdlib_clamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! CLAMTSQR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (CLATSQR)
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), t(ldt,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: c(ldc,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: i, ii, kk, lw, ctr, q
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * nb
             q = m
           else
             lw = m * nb
             q = n
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( k<nb .or. nb<1_ilp ) then
             info = -7_ilp
           else if( lda<max( 1_ilp, q ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, nb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           ! determine the block size if it is tall skinny or short and wide
           if( info==0_ilp)  then
               work(1_ilp) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CLAMTSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((mb<=k).or.(mb>=max(m,n,k))) then
             call stdlib_cgemqrt( side, trans, m, n, k, nb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
            end if
           if(left.and.notran) then
               ! multiply q to the last block of c
              kk = mod((m-k),(mb-k))
              ctr = (m-k)/(mb-k)
              if (kk>0_ilp) then
                ii=m-kk+1
                call stdlib_ctpmqrt('L','N',kk , n, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp, ctr*k+1),ldt , c(&
                          1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              else
                ii=m+1
              end if
              do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
                ctr = ctr - 1_ilp
                call stdlib_ctpmqrt('L','N',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,&
                          1_ilp), ldc,c(i,1_ilp), ldc, work, info )
              end do
               ! multiply q to the first block of c (1:mb,1:n)
              call stdlib_cgemqrt('L','N',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
           else if (left.and.tran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(mb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_cgemqrt('L','C',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
               call stdlib_ctpmqrt('L','C',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr*k+1),ldt, c(1_ilp,&
                         1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_ctpmqrt('L','C',kk , n, k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,1_ilp)&
                         , ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.tran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(mb-k))
               ctr = (n-k)/(mb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_ctpmqrt('R','C',m , kk, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp, ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                 ctr = ctr - 1_ilp
                 call stdlib_ctpmqrt('R','C',m , mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_cgemqrt('R','C',m , mb, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.notran) then
               ! multiply q to the first block of c
              kk = mod((n-k),(mb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_cgemqrt('R','N', m, mb , k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_ctpmqrt('R','N', m, mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,1_ilp)&
                         , ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
               ! multiply q to the last block of c
               call stdlib_ctpmqrt('R','N', m, kk , k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,1_ilp)&
                         , ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_clamtsqr

     pure module subroutine stdlib_zlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! ZLAMTSQR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by tall skinny
     !! QR factorization (ZLATSQR)
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), t(ldt,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: c(ldc,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran, lquery
           integer(ilp) :: i, ii, kk, lw, ctr, q
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * nb
             q = m
           else
             lw = m * nb
             q = n
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( k<nb .or. nb<1_ilp ) then
             info = -7_ilp
           else if( lda<max( 1_ilp, q ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, nb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           ! determine the block size if it is tall skinny or short and wide
           if( info==0_ilp)  then
               work(1_ilp) = lw
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZLAMTSQR', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((mb<=k).or.(mb>=max(m,n,k))) then
             call stdlib_zgemqrt( side, trans, m, n, k, nb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
            end if
           if(left.and.notran) then
               ! multiply q to the last block of c
              kk = mod((m-k),(mb-k))
              ctr = (m-k)/(mb-k)
              if (kk>0_ilp) then
                ii=m-kk+1
                call stdlib_ztpmqrt('L','N',kk , n, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt ,&
                           c(1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              else
                ii=m+1
              end if
              do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
                ctr = ctr - 1_ilp
                call stdlib_ztpmqrt('L','N',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp),ldt, &
                          c(1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
              end do
               ! multiply q to the first block of c (1:mb,1:n)
              call stdlib_zgemqrt('L','N',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
           else if (left.and.tran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(mb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_zgemqrt('L','C',mb , n, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (i:i+mb,1:n)
               call stdlib_ztpmqrt('L','C',mb-k , n, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp),ldt, c(&
                         1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_ztpmqrt('L','C',kk , n, k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), ldt, &
                         c(1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.tran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(mb-k))
               ctr = (n-k)/(mb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_ztpmqrt('R','C',m , kk, k, 0_ilp, nb, a(ii,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp), ldt,&
                            c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(mb-k),mb+1,-(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                 ctr = ctr - 1_ilp
                 call stdlib_ztpmqrt('R','C',m , mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp), &
                           ldt, c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_zgemqrt('R','C',m , mb, k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.notran) then
               ! multiply q to the first block of c
              kk = mod((n-k),(mb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_zgemqrt('R','N', m, mb , k, nb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=mb+1,ii-mb+k,(mb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_ztpmqrt('R','N', m, mb-k, k, 0_ilp,nb, a(i,1_ilp), lda,t(1_ilp, ctr * k + 1_ilp),ldt, &
                         c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
               ! multiply q to the last block of c
               call stdlib_ztpmqrt('R','N', m, kk , k, 0_ilp,nb, a(ii,1_ilp), lda,t(1_ilp,ctr * k + 1_ilp),ldt, c(&
                         1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_zlamtsqr




     pure module subroutine stdlib_sgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
     !! SGETSQRHRT computes a NB2-sized column blocked QR-factorization
     !! of a complex M-by-N matrix A with M >= N,
     !! A = Q * R.
     !! The routine uses internally a NB1-sized column blocked and MB1-sized
     !! row blocked TSQR-factorization and perfors the reconstruction
     !! of the Householder vectors from the TSQR output. The routine also
     !! converts the R_tsqr factor from the TSQR-factorization output into
     !! the R factor that corresponds to the Householder QR-factorization,
     !! A = Q_tsqr * R_tsqr = Q * R.
     !! The output Q and R factors are stored in the same format as in SGEQRT
     !! (Q is in blocked compact WY-representation). See the documentation
     !! of SGEQRT for more details on the format.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lw1, lw2, lwt, ldwt, lworkopt, nb1local, nb2local, &
                     num_all_row_blocks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb1<=n ) then
              info = -3_ilp
           else if( nb1<1_ilp ) then
              info = -4_ilp
           else if( nb2<1_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp,  min( nb2, n ) ) ) then
              info = -9_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array:
              ! a) matrix t and work for stdlib_slatsqr;
              ! b) n-by-n upper-triangular factor r_tsqr;
              ! c) matrix t and array work for stdlib_sorgtsqr_row;
              ! d) diagonal d for stdlib_sorhr_col.
              if( lwork<n*n+1 .and. .not.lquery ) then
                 info = -11_ilp
              else
                 ! set block size for column blocks
                 nb1local = min( nb1, n )
                 num_all_row_blocks = max( 1_ilp,ceiling( real( m - n,KIND=sp) / real( mb1 - n,&
                           KIND=sp) ) )
                 ! length and leading dimension of work array to place
                 ! t array in tsqr.
                 lwt = num_all_row_blocks * n * nb1local
                 ldwt = nb1local
                 ! length of tsqr work array
                 lw1 = nb1local * n
                 ! length of stdlib_sorgtsqr_row work array.
                 lw2 = nb1local * max( nb1local, ( n - nb1local ) )
                 lworkopt = max( lwt + lw1, max( lwt+n*n+lw2, lwt+n*n+n ) )
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -11_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETSQRHRT', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=sp)
              return
           end if
           nb2local = min( nb2, n )
           ! (1) perform tsqr-factorization of the m-by-n matrix a.
           call stdlib_slatsqr( m, n, mb1, nb1local, a, lda, work, ldwt,work(lwt+1), lw1, iinfo )
                     
           ! (2) copy the factor r_tsqr stored in the upper-triangular part
               ! of a into the square matrix in the work array
               ! work(lwt+1:lwt+n*n) column-by-column.
           do j = 1, n
              call stdlib_scopy( j, a( 1_ilp, j ), 1_ilp, work( lwt + n*(j-1)+1_ilp ), 1_ilp )
           end do
           ! (3) generate a m-by-n matrix q with orthonormal columns from
           ! the result stored below the diagonal in the array a in place.
           call stdlib_sorgtsqr_row( m, n, mb1, nb1local, a, lda, work, ldwt,work( lwt+n*n+1 ), &
                     lw2, iinfo )
           ! (4) perform the reconstruction of householder vectors from
           ! the matrix q (stored in a) in place.
           call stdlib_sorhr_col( m, n, nb2local, a, lda, t, ldt,work( lwt+n*n+1 ), iinfo )
                     
           ! (5) copy the factor r_tsqr stored in the square matrix in the
           ! work array work(lwt+1:lwt+n*n) into the upper-triangular
           ! part of a.
           ! (6) compute from r_tsqr the factor r_hr corresponding to
           ! the reconstructed householder vectors, i.e. r_hr = s * r_tsqr.
           ! this multiplication by the sign matrix s on the left means
           ! changing the sign of i-th row of the matrix r_tsqr according
           ! to sign of the i-th diagonal element diag(i) of the matrix s.
           ! diag is stored in work( lwt+n*n+1 ) from the stdlib_sorhr_col output.
           ! (5) and (6) can be combined in a single loop, so the rows in a
           ! are accessed only once.
           do i = 1, n
              if( work( lwt+n*n+i )==-one ) then
                 do j = i, n
                    a( i, j ) = -one * work( lwt+n*(j-1)+i )
                 end do
              else
                 call stdlib_scopy( n-i+1, work(lwt+n*(i-1)+i), n, a( i, i ), lda )
              end if
           end do
           work( 1_ilp ) = real( lworkopt,KIND=sp)
           return
     end subroutine stdlib_sgetsqrhrt

     pure module subroutine stdlib_dgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
     !! DGETSQRHRT computes a NB2-sized column blocked QR-factorization
     !! of a real M-by-N matrix A with M >= N,
     !! A = Q * R.
     !! The routine uses internally a NB1-sized column blocked and MB1-sized
     !! row blocked TSQR-factorization and perfors the reconstruction
     !! of the Householder vectors from the TSQR output. The routine also
     !! converts the R_tsqr factor from the TSQR-factorization output into
     !! the R factor that corresponds to the Householder QR-factorization,
     !! A = Q_tsqr * R_tsqr = Q * R.
     !! The output Q and R factors are stored in the same format as in DGEQRT
     !! (Q is in blocked compact WY-representation). See the documentation
     !! of DGEQRT for more details on the format.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lw1, lw2, lwt, ldwt, lworkopt, nb1local, nb2local, &
                     num_all_row_blocks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb1<=n ) then
              info = -3_ilp
           else if( nb1<1_ilp ) then
              info = -4_ilp
           else if( nb2<1_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp,  min( nb2, n ) ) ) then
              info = -9_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array:
              ! a) matrix t and work for stdlib_dlatsqr;
              ! b) n-by-n upper-triangular factor r_tsqr;
              ! c) matrix t and array work for stdlib_dorgtsqr_row;
              ! d) diagonal d for stdlib_dorhr_col.
              if( lwork<n*n+1 .and. .not.lquery ) then
                 info = -11_ilp
              else
                 ! set block size for column blocks
                 nb1local = min( nb1, n )
                 num_all_row_blocks = max( 1_ilp,ceiling( real( m - n,KIND=dp) / real( mb1 - n,&
                           KIND=dp) ) )
                 ! length and leading dimension of work array to place
                 ! t array in tsqr.
                 lwt = num_all_row_blocks * n * nb1local
                 ldwt = nb1local
                 ! length of tsqr work array
                 lw1 = nb1local * n
                 ! length of stdlib_dorgtsqr_row work array.
                 lw2 = nb1local * max( nb1local, ( n - nb1local ) )
                 lworkopt = max( lwt + lw1, max( lwt+n*n+lw2, lwt+n*n+n ) )
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -11_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETSQRHRT', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = real( lworkopt,KIND=dp)
              return
           end if
           nb2local = min( nb2, n )
           ! (1) perform tsqr-factorization of the m-by-n matrix a.
           call stdlib_dlatsqr( m, n, mb1, nb1local, a, lda, work, ldwt,work(lwt+1), lw1, iinfo )
                     
           ! (2) copy the factor r_tsqr stored in the upper-triangular part
               ! of a into the square matrix in the work array
               ! work(lwt+1:lwt+n*n) column-by-column.
           do j = 1, n
              call stdlib_dcopy( j, a( 1_ilp, j ), 1_ilp, work( lwt + n*(j-1)+1_ilp ), 1_ilp )
           end do
           ! (3) generate a m-by-n matrix q with orthonormal columns from
           ! the result stored below the diagonal in the array a in place.
           call stdlib_dorgtsqr_row( m, n, mb1, nb1local, a, lda, work, ldwt,work( lwt+n*n+1 ), &
                     lw2, iinfo )
           ! (4) perform the reconstruction of householder vectors from
           ! the matrix q (stored in a) in place.
           call stdlib_dorhr_col( m, n, nb2local, a, lda, t, ldt,work( lwt+n*n+1 ), iinfo )
                     
           ! (5) copy the factor r_tsqr stored in the square matrix in the
           ! work array work(lwt+1:lwt+n*n) into the upper-triangular
           ! part of a.
           ! (6) compute from r_tsqr the factor r_hr corresponding to
           ! the reconstructed householder vectors, i.e. r_hr = s * r_tsqr.
           ! this multiplication by the sign matrix s on the left means
           ! changing the sign of i-th row of the matrix r_tsqr according
           ! to sign of the i-th diagonal element diag(i) of the matrix s.
           ! diag is stored in work( lwt+n*n+1 ) from the stdlib_dorhr_col output.
           ! (5) and (6) can be combined in a single loop, so the rows in a
           ! are accessed only once.
           do i = 1, n
              if( work( lwt+n*n+i )==-one ) then
                 do j = i, n
                    a( i, j ) = -one * work( lwt+n*(j-1)+i )
                 end do
              else
                 call stdlib_dcopy( n-i+1, work(lwt+n*(i-1)+i), n, a( i, i ), lda )
              end if
           end do
           work( 1_ilp ) = real( lworkopt,KIND=dp)
           return
     end subroutine stdlib_dgetsqrhrt


     pure module subroutine stdlib_cgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
     !! CGETSQRHRT computes a NB2-sized column blocked QR-factorization
     !! of a complex M-by-N matrix A with M >= N,
     !! A = Q * R.
     !! The routine uses internally a NB1-sized column blocked and MB1-sized
     !! row blocked TSQR-factorization and perfors the reconstruction
     !! of the Householder vectors from the TSQR output. The routine also
     !! converts the R_tsqr factor from the TSQR-factorization output into
     !! the R factor that corresponds to the Householder QR-factorization,
     !! A = Q_tsqr * R_tsqr = Q * R.
     !! The output Q and R factors are stored in the same format as in CGEQRT
     !! (Q is in blocked compact WY-representation). See the documentation
     !! of CGEQRT for more details on the format.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lw1, lw2, lwt, ldwt, lworkopt, nb1local, nb2local, &
                     num_all_row_blocks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb1<=n ) then
              info = -3_ilp
           else if( nb1<1_ilp ) then
              info = -4_ilp
           else if( nb2<1_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp,  min( nb2, n ) ) ) then
              info = -9_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array:
              ! a) matrix t and work for stdlib_clatsqr;
              ! b) n-by-n upper-triangular factor r_tsqr;
              ! c) matrix t and array work for stdlib_cungtsqr_row;
              ! d) diagonal d for stdlib_cunhr_col.
              if( lwork<n*n+1 .and. .not.lquery ) then
                 info = -11_ilp
              else
                 ! set block size for column blocks
                 nb1local = min( nb1, n )
                 num_all_row_blocks = max( 1_ilp,ceiling( real( m - n,KIND=sp) / real( mb1 - n,&
                           KIND=sp) ) )
                 ! length and leading dimension of work array to place
                 ! t array in tsqr.
                 lwt = num_all_row_blocks * n * nb1local
                 ldwt = nb1local
                 ! length of tsqr work array
                 lw1 = nb1local * n
                 ! length of stdlib_cungtsqr_row work array.
                 lw2 = nb1local * max( nb1local, ( n - nb1local ) )
                 lworkopt = max( lwt + lw1, max( lwt+n*n+lw2, lwt+n*n+n ) )
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -11_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETSQRHRT', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
              return
           end if
           nb2local = min( nb2, n )
           ! (1) perform tsqr-factorization of the m-by-n matrix a.
           call stdlib_clatsqr( m, n, mb1, nb1local, a, lda, work, ldwt,work(lwt+1), lw1, iinfo )
                     
           ! (2) copy the factor r_tsqr stored in the upper-triangular part
               ! of a into the square matrix in the work array
               ! work(lwt+1:lwt+n*n) column-by-column.
           do j = 1, n
              call stdlib_ccopy( j, a( 1_ilp, j ), 1_ilp, work( lwt + n*(j-1)+1_ilp ), 1_ilp )
           end do
           ! (3) generate a m-by-n matrix q with orthonormal columns from
           ! the result stored below the diagonal in the array a in place.
           call stdlib_cungtsqr_row( m, n, mb1, nb1local, a, lda, work, ldwt,work( lwt+n*n+1 ), &
                     lw2, iinfo )
           ! (4) perform the reconstruction of householder vectors from
           ! the matrix q (stored in a) in place.
           call stdlib_cunhr_col( m, n, nb2local, a, lda, t, ldt,work( lwt+n*n+1 ), iinfo )
                     
           ! (5) copy the factor r_tsqr stored in the square matrix in the
           ! work array work(lwt+1:lwt+n*n) into the upper-triangular
           ! part of a.
           ! (6) compute from r_tsqr the factor r_hr corresponding to
           ! the reconstructed householder vectors, i.e. r_hr = s * r_tsqr.
           ! this multiplication by the sign matrix s on the left means
           ! changing the sign of i-th row of the matrix r_tsqr according
           ! to sign of the i-th diagonal element diag(i) of the matrix s.
           ! diag is stored in work( lwt+n*n+1 ) from the stdlib_cunhr_col output.
           ! (5) and (6) can be combined in a single loop, so the rows in a
           ! are accessed only once.
           do i = 1, n
              if( work( lwt+n*n+i )==-cone ) then
                 do j = i, n
                    a( i, j ) = -cone * work( lwt+n*(j-1)+i )
                 end do
              else
                 call stdlib_ccopy( n-i+1, work(lwt+n*(i-1)+i), n, a( i, i ), lda )
              end if
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=sp)
           return
     end subroutine stdlib_cgetsqrhrt

     pure module subroutine stdlib_zgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
     !! ZGETSQRHRT computes a NB2-sized column blocked QR-factorization
     !! of a complex M-by-N matrix A with M >= N,
     !! A = Q * R.
     !! The routine uses internally a NB1-sized column blocked and MB1-sized
     !! row blocked TSQR-factorization and perfors the reconstruction
     !! of the Householder vectors from the TSQR output. The routine also
     !! converts the R_tsqr factor from the TSQR-factorization output into
     !! the R factor that corresponds to the Householder QR-factorization,
     !! A = Q_tsqr * R_tsqr = Q * R.
     !! The output Q and R factors are stored in the same format as in ZGEQRT
     !! (Q is in blocked compact WY-representation). See the documentation
     !! of ZGEQRT for more details on the format.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, lw1, lw2, lwt, ldwt, lworkopt, nb1local, nb2local, &
                     num_all_row_blocks
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery  = lwork==-1_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. m<n ) then
              info = -2_ilp
           else if( mb1<=n ) then
              info = -3_ilp
           else if( nb1<1_ilp ) then
              info = -4_ilp
           else if( nb2<1_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp,  min( nb2, n ) ) ) then
              info = -9_ilp
           else
              ! test the input lwork for the dimension of the array work.
              ! this workspace is used to store array:
              ! a) matrix t and work for stdlib_zlatsqr;
              ! b) n-by-n upper-triangular factor r_tsqr;
              ! c) matrix t and array work for stdlib_zungtsqr_row;
              ! d) diagonal d for stdlib_zunhr_col.
              if( lwork<n*n+1 .and. .not.lquery ) then
                 info = -11_ilp
              else
                 ! set block size for column blocks
                 nb1local = min( nb1, n )
                 num_all_row_blocks = max( 1_ilp,ceiling( real( m - n,KIND=dp) / real( mb1 - n,&
                           KIND=dp) ) )
                 ! length and leading dimension of work array to place
                 ! t array in tsqr.
                 lwt = num_all_row_blocks * n * nb1local
                 ldwt = nb1local
                 ! length of tsqr work array
                 lw1 = nb1local * n
                 ! length of stdlib_zungtsqr_row work array.
                 lw2 = nb1local * max( nb1local, ( n - nb1local ) )
                 lworkopt = max( lwt + lw1, max( lwt+n*n+lw2, lwt+n*n+n ) )
                 if( ( lwork<max( 1_ilp, lworkopt ) ).and.(.not.lquery) ) then
                    info = -11_ilp
                 end if
              end if
           end if
           ! handle error in the input parameters and return workspace query.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETSQRHRT', -info )
              return
           else if ( lquery ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
              return
           end if
           nb2local = min( nb2, n )
           ! (1) perform tsqr-factorization of the m-by-n matrix a.
           call stdlib_zlatsqr( m, n, mb1, nb1local, a, lda, work, ldwt,work(lwt+1), lw1, iinfo )
                     
           ! (2) copy the factor r_tsqr stored in the upper-triangular part
               ! of a into the square matrix in the work array
               ! work(lwt+1:lwt+n*n) column-by-column.
           do j = 1, n
              call stdlib_zcopy( j, a( 1_ilp, j ), 1_ilp, work( lwt + n*(j-1)+1_ilp ), 1_ilp )
           end do
           ! (3) generate a m-by-n matrix q with orthonormal columns from
           ! the result stored below the diagonal in the array a in place.
           call stdlib_zungtsqr_row( m, n, mb1, nb1local, a, lda, work, ldwt,work( lwt+n*n+1 ), &
                     lw2, iinfo )
           ! (4) perform the reconstruction of householder vectors from
           ! the matrix q (stored in a) in place.
           call stdlib_zunhr_col( m, n, nb2local, a, lda, t, ldt,work( lwt+n*n+1 ), iinfo )
                     
           ! (5) copy the factor r_tsqr stored in the square matrix in the
           ! work array work(lwt+1:lwt+n*n) into the upper-triangular
           ! part of a.
           ! (6) compute from r_tsqr the factor r_hr corresponding to
           ! the reconstructed householder vectors, i.e. r_hr = s * r_tsqr.
           ! this multiplication by the sign matrix s on the left means
           ! changing the sign of i-th row of the matrix r_tsqr according
           ! to sign of the i-th diagonal element diag(i) of the matrix s.
           ! diag is stored in work( lwt+n*n+1 ) from the stdlib_zunhr_col output.
           ! (5) and (6) can be combined in a single loop, so the rows in a
           ! are accessed only once.
           do i = 1, n
              if( work( lwt+n*n+i )==-cone ) then
                 do j = i, n
                    a( i, j ) = -cone * work( lwt+n*(j-1)+i )
                 end do
              else
                 call stdlib_zcopy( n-i+1, work(lwt+n*(i-1)+i), n, a( i, i ), lda )
              end if
           end do
           work( 1_ilp ) = cmplx( lworkopt,KIND=dp)
           return
     end subroutine stdlib_zgetsqrhrt




     pure module subroutine stdlib_cunhr_col( m, n, nb, a, lda, t, ldt, d, info )
     !! CUNHR_COL takes an M-by-N complex matrix Q_in with orthonormal columns
     !! as input, stored in A, and performs Householder Reconstruction (HR),
     !! i.e. reconstructs Householder vectors V(i) implicitly representing
     !! another M-by-N matrix Q_out, with the property that Q_in = Q_out*S,
     !! where S is an N-by-N diagonal matrix with diagonal entries
     !! equal to +1 or -1. The Householder vectors (columns V(i) of V) are
     !! stored in A on output, and the diagonal entries of S are stored in D.
     !! Block reflectors are also returned in T
     !! (same output format as CGEQRT).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*), t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, jbtemp1, jbtemp2, jnb, nplusone
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( nb<1_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -7_ilp
           end if
           ! handle error in the input parameters.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNHR_COL', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              return
           end if
           ! on input, the m-by-n matrix a contains the unitary
           ! m-by-n matrix q_in.
           ! (1) compute the unit lower-trapezoidal v (ones on the diagonal
           ! are not stored) by performing the "modified" lu-decomposition.
           ! q_in - ( s ) = v * u = ( v1 ) * u,
                  ! ( 0 )           ( v2 )
           ! where 0 is an (m-n)-by-n zero matrix.
           ! (1-1) factor v1 and u.
           call stdlib_claunhr_col_getrfnp( n, n, a, lda, d, iinfo )
           ! (1-2) solve for v2.
           if( m>n ) then
              call stdlib_ctrsm( 'R', 'U', 'N', 'N', m-n, n, cone, a, lda,a( n+1, 1_ilp ), lda )
                        
           end if
           ! (2) reconstruct the block reflector t stored in t(1:nb, 1:n)
           ! as a sequence of upper-triangular blocks with nb-size column
           ! blocking.
           ! loop over the column blocks of size nb of the array a(1:m,1:n)
           ! and the array t(1:nb,1:n), jb is the column index of a column
           ! block, jnb is the column block size at each step jb.
           nplusone = n + 1_ilp
           do jb = 1, n, nb
              ! (2-0) determine the column block size jnb.
              jnb = min( nplusone-jb, nb )
              ! (2-1) copy the upper-triangular part of the current jnb-by-jnb
              ! diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in a(jb:jb+jnb-1,jb:jb+jnb-1) into the upper-triangular part
              ! of the current jnb-by-jnb block t(1:jnb,jb:jb+jnb-1)
              ! column-by-column, total jnb*(jnb+1)/2 elements.
              jbtemp1 = jb - 1_ilp
              do j = jb, jb+jnb-1
                 call stdlib_ccopy( j-jbtemp1, a( jb, j ), 1_ilp, t( 1_ilp, j ), 1_ilp )
              end do
              ! (2-2) perform on the upper-triangular part of the current
              ! jnb-by-jnb diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in t(1:jnb,jb:jb+jnb-1) the following operation in place:
              ! (-1)*u(jb)*s(jb), i.e the result will be stored in the upper-
              ! triangular part of t(1:jnb,jb:jb+jnb-1). this multiplication
              ! of the jnb-by-jnb diagonal block u(jb) by the jnb-by-jnb
              ! diagonal block s(jb) of the n-by-n sign matrix s from the
              ! right means changing the sign of each j-th column of the block
              ! u(jb) according to the sign of the diagonal element of the block
              ! s(jb), i.e. s(j,j) that is stored in the array element d(j).
              do j = jb, jb+jnb-1
                 if( d( j )==cone ) then
                    call stdlib_cscal( j-jbtemp1, -cone, t( 1_ilp, j ), 1_ilp )
                 end if
              end do
              ! (2-3) perform the triangular solve for the current block
              ! matrix x(jb):
                     ! x(jb) * (a(jb)**t) = b(jb), where:
                     ! a(jb)**t  is a jnb-by-jnb unit upper-triangular
                               ! coefficient block, and a(jb)=v1(jb), which
                               ! is a jnb-by-jnb unit lower-triangular block
                               ! stored in a(jb:jb+jnb-1,jb:jb+jnb-1).
                               ! the n-by-n matrix v1 is the upper part
                               ! of the m-by-n lower-trapezoidal matrix v
                               ! stored in a(1:m,1:n);
                     ! b(jb)     is a jnb-by-jnb  upper-triangular right-hand
                               ! side block, b(jb) = (-1)*u(jb)*s(jb), and
                               ! b(jb) is stored in t(1:jnb,jb:jb+jnb-1);
                     ! x(jb)     is a jnb-by-jnb upper-triangular solution
                               ! block, x(jb) is the upper-triangular block
                               ! reflector t(jb), and x(jb) is stored
                               ! in t(1:jnb,jb:jb+jnb-1).
                   ! in other words, we perform the triangular solve for the
                   ! upper-triangular block t(jb):
                     ! t(jb) * (v1(jb)**t) = (-1)*u(jb)*s(jb).
                   ! even though the blocks x(jb) and b(jb) are upper-
                   ! triangular, the routine stdlib_ctrsm will access all jnb**2
                   ! elements of the square t(1:jnb,jb:jb+jnb-1). therefore,
                   ! we need to set to zero the elements of the block
                   ! t(1:jnb,jb:jb+jnb-1) below the diagonal before the call
                   ! to stdlib_ctrsm.
              ! (2-3a) set the elements to zero.
              jbtemp2 = jb - 2_ilp
              do j = jb, jb+jnb-2
                 do i = j-jbtemp2, nb
                    t( i, j ) = czero
                 end do
              end do
              ! (2-3b) perform the triangular solve.
              call stdlib_ctrsm( 'R', 'L', 'C', 'U', jnb, jnb, cone,a( jb, jb ), lda, t( 1_ilp, jb ), &
                        ldt )
           end do
           return
     end subroutine stdlib_cunhr_col

     pure module subroutine stdlib_zunhr_col( m, n, nb, a, lda, t, ldt, d, info )
     !! ZUNHR_COL takes an M-by-N complex matrix Q_in with orthonormal columns
     !! as input, stored in A, and performs Householder Reconstruction (HR),
     !! i.e. reconstructs Householder vectors V(i) implicitly representing
     !! another M-by-N matrix Q_out, with the property that Q_in = Q_out*S,
     !! where S is an N-by-N diagonal matrix with diagonal entries
     !! equal to +1 or -1. The Householder vectors (columns V(i) of V) are
     !! stored in A on output, and the diagonal entries of S are stored in D.
     !! Block reflectors are also returned in T
     !! (same output format as ZGEQRT).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*), t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, jbtemp1, jbtemp2, jnb, nplusone
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( nb<1_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -7_ilp
           end if
           ! handle error in the input parameters.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNHR_COL', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              return
           end if
           ! on input, the m-by-n matrix a contains the unitary
           ! m-by-n matrix q_in.
           ! (1) compute the unit lower-trapezoidal v (ones on the diagonal
           ! are not stored) by performing the "modified" lu-decomposition.
           ! q_in - ( s ) = v * u = ( v1 ) * u,
                  ! ( 0 )           ( v2 )
           ! where 0 is an (m-n)-by-n zero matrix.
           ! (1-1) factor v1 and u.
           call stdlib_zlaunhr_col_getrfnp( n, n, a, lda, d, iinfo )
           ! (1-2) solve for v2.
           if( m>n ) then
              call stdlib_ztrsm( 'R', 'U', 'N', 'N', m-n, n, cone, a, lda,a( n+1, 1_ilp ), lda )
                        
           end if
           ! (2) reconstruct the block reflector t stored in t(1:nb, 1:n)
           ! as a sequence of upper-triangular blocks with nb-size column
           ! blocking.
           ! loop over the column blocks of size nb of the array a(1:m,1:n)
           ! and the array t(1:nb,1:n), jb is the column index of a column
           ! block, jnb is the column block size at each step jb.
           nplusone = n + 1_ilp
           do jb = 1, n, nb
              ! (2-0) determine the column block size jnb.
              jnb = min( nplusone-jb, nb )
              ! (2-1) copy the upper-triangular part of the current jnb-by-jnb
              ! diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in a(jb:jb+jnb-1,jb:jb+jnb-1) into the upper-triangular part
              ! of the current jnb-by-jnb block t(1:jnb,jb:jb+jnb-1)
              ! column-by-column, total jnb*(jnb+1)/2 elements.
              jbtemp1 = jb - 1_ilp
              do j = jb, jb+jnb-1
                 call stdlib_zcopy( j-jbtemp1, a( jb, j ), 1_ilp, t( 1_ilp, j ), 1_ilp )
              end do
              ! (2-2) perform on the upper-triangular part of the current
              ! jnb-by-jnb diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in t(1:jnb,jb:jb+jnb-1) the following operation in place:
              ! (-1)*u(jb)*s(jb), i.e the result will be stored in the upper-
              ! triangular part of t(1:jnb,jb:jb+jnb-1). this multiplication
              ! of the jnb-by-jnb diagonal block u(jb) by the jnb-by-jnb
              ! diagonal block s(jb) of the n-by-n sign matrix s from the
              ! right means changing the sign of each j-th column of the block
              ! u(jb) according to the sign of the diagonal element of the block
              ! s(jb), i.e. s(j,j) that is stored in the array element d(j).
              do j = jb, jb+jnb-1
                 if( d( j )==cone ) then
                    call stdlib_zscal( j-jbtemp1, -cone, t( 1_ilp, j ), 1_ilp )
                 end if
              end do
              ! (2-3) perform the triangular solve for the current block
              ! matrix x(jb):
                     ! x(jb) * (a(jb)**t) = b(jb), where:
                     ! a(jb)**t  is a jnb-by-jnb unit upper-triangular
                               ! coefficient block, and a(jb)=v1(jb), which
                               ! is a jnb-by-jnb unit lower-triangular block
                               ! stored in a(jb:jb+jnb-1,jb:jb+jnb-1).
                               ! the n-by-n matrix v1 is the upper part
                               ! of the m-by-n lower-trapezoidal matrix v
                               ! stored in a(1:m,1:n);
                     ! b(jb)     is a jnb-by-jnb  upper-triangular right-hand
                               ! side block, b(jb) = (-1)*u(jb)*s(jb), and
                               ! b(jb) is stored in t(1:jnb,jb:jb+jnb-1);
                     ! x(jb)     is a jnb-by-jnb upper-triangular solution
                               ! block, x(jb) is the upper-triangular block
                               ! reflector t(jb), and x(jb) is stored
                               ! in t(1:jnb,jb:jb+jnb-1).
                   ! in other words, we perform the triangular solve for the
                   ! upper-triangular block t(jb):
                     ! t(jb) * (v1(jb)**t) = (-1)*u(jb)*s(jb).
                   ! even though the blocks x(jb) and b(jb) are upper-
                   ! triangular, the routine stdlib_ztrsm will access all jnb**2
                   ! elements of the square t(1:jnb,jb:jb+jnb-1). therefore,
                   ! we need to set to zero the elements of the block
                   ! t(1:jnb,jb:jb+jnb-1) below the diagonal before the call
                   ! to stdlib_ztrsm.
              ! (2-3a) set the elements to zero.
              jbtemp2 = jb - 2_ilp
              do j = jb, jb+jnb-2
                 do i = j-jbtemp2, nb
                    t( i, j ) = czero
                 end do
              end do
              ! (2-3b) perform the triangular solve.
              call stdlib_ztrsm( 'R', 'L', 'C', 'U', jnb, jnb, cone,a( jb, jb ), lda, t( 1_ilp, jb ), &
                        ldt )
           end do
           return
     end subroutine stdlib_zunhr_col




     pure module subroutine stdlib_sorhr_col( m, n, nb, a, lda, t, ldt, d, info )
     !! SORHR_COL takes an M-by-N real matrix Q_in with orthonormal columns
     !! as input, stored in A, and performs Householder Reconstruction (HR),
     !! i.e. reconstructs Householder vectors V(i) implicitly representing
     !! another M-by-N matrix Q_out, with the property that Q_in = Q_out*S,
     !! where S is an N-by-N diagonal matrix with diagonal entries
     !! equal to +1 or -1. The Householder vectors (columns V(i) of V) are
     !! stored in A on output, and the diagonal entries of S are stored in D.
     !! Block reflectors are also returned in T
     !! (same output format as SGEQRT).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, jbtemp1, jbtemp2, jnb, nplusone
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( nb<1_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -7_ilp
           end if
           ! handle error in the input parameters.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORHR_COL', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              return
           end if
           ! on input, the m-by-n matrix a contains the orthogonal
           ! m-by-n matrix q_in.
           ! (1) compute the unit lower-trapezoidal v (ones on the diagonal
           ! are not stored) by performing the "modified" lu-decomposition.
           ! q_in - ( s ) = v * u = ( v1 ) * u,
                  ! ( 0 )           ( v2 )
           ! where 0 is an (m-n)-by-n zero matrix.
           ! (1-1) factor v1 and u.
           call stdlib_slaorhr_col_getrfnp( n, n, a, lda, d, iinfo )
           ! (1-2) solve for v2.
           if( m>n ) then
              call stdlib_strsm( 'R', 'U', 'N', 'N', m-n, n, one, a, lda,a( n+1, 1_ilp ), lda )
                        
           end if
           ! (2) reconstruct the block reflector t stored in t(1:nb, 1:n)
           ! as a sequence of upper-triangular blocks with nb-size column
           ! blocking.
           ! loop over the column blocks of size nb of the array a(1:m,1:n)
           ! and the array t(1:nb,1:n), jb is the column index of a column
           ! block, jnb is the column block size at each step jb.
           nplusone = n + 1_ilp
           do jb = 1, n, nb
              ! (2-0) determine the column block size jnb.
              jnb = min( nplusone-jb, nb )
              ! (2-1) copy the upper-triangular part of the current jnb-by-jnb
              ! diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in a(jb:jb+jnb-1,jb:jb+jnb-1) into the upper-triangular part
              ! of the current jnb-by-jnb block t(1:jnb,jb:jb+jnb-1)
              ! column-by-column, total jnb*(jnb+1)/2 elements.
              jbtemp1 = jb - 1_ilp
              do j = jb, jb+jnb-1
                 call stdlib_scopy( j-jbtemp1, a( jb, j ), 1_ilp, t( 1_ilp, j ), 1_ilp )
              end do
              ! (2-2) perform on the upper-triangular part of the current
              ! jnb-by-jnb diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in t(1:jnb,jb:jb+jnb-1) the following operation in place:
              ! (-1)*u(jb)*s(jb), i.e the result will be stored in the upper-
              ! triangular part of t(1:jnb,jb:jb+jnb-1). this multiplication
              ! of the jnb-by-jnb diagonal block u(jb) by the jnb-by-jnb
              ! diagonal block s(jb) of the n-by-n sign matrix s from the
              ! right means changing the sign of each j-th column of the block
              ! u(jb) according to the sign of the diagonal element of the block
              ! s(jb), i.e. s(j,j) that is stored in the array element d(j).
              do j = jb, jb+jnb-1
                 if( d( j )==one ) then
                    call stdlib_sscal( j-jbtemp1, -one, t( 1_ilp, j ), 1_ilp )
                 end if
              end do
              ! (2-3) perform the triangular solve for the current block
              ! matrix x(jb):
                     ! x(jb) * (a(jb)**t) = b(jb), where:
                     ! a(jb)**t  is a jnb-by-jnb unit upper-triangular
                               ! coefficient block, and a(jb)=v1(jb), which
                               ! is a jnb-by-jnb unit lower-triangular block
                               ! stored in a(jb:jb+jnb-1,jb:jb+jnb-1).
                               ! the n-by-n matrix v1 is the upper part
                               ! of the m-by-n lower-trapezoidal matrix v
                               ! stored in a(1:m,1:n);
                     ! b(jb)     is a jnb-by-jnb  upper-triangular right-hand
                               ! side block, b(jb) = (-1)*u(jb)*s(jb), and
                               ! b(jb) is stored in t(1:jnb,jb:jb+jnb-1);
                     ! x(jb)     is a jnb-by-jnb upper-triangular solution
                               ! block, x(jb) is the upper-triangular block
                               ! reflector t(jb), and x(jb) is stored
                               ! in t(1:jnb,jb:jb+jnb-1).
                   ! in other words, we perform the triangular solve for the
                   ! upper-triangular block t(jb):
                     ! t(jb) * (v1(jb)**t) = (-1)*u(jb)*s(jb).
                   ! even though the blocks x(jb) and b(jb) are upper-
                   ! triangular, the routine stdlib_strsm will access all jnb**2
                   ! elements of the square t(1:jnb,jb:jb+jnb-1). therefore,
                   ! we need to set to zero the elements of the block
                   ! t(1:jnb,jb:jb+jnb-1) below the diagonal before the call
                   ! to stdlib_strsm.
              ! (2-3a) set the elements to zero.
              jbtemp2 = jb - 2_ilp
              do j = jb, jb+jnb-2
                 do i = j-jbtemp2, nb
                    t( i, j ) = zero
                 end do
              end do
              ! (2-3b) perform the triangular solve.
              call stdlib_strsm( 'R', 'L', 'T', 'U', jnb, jnb, one,a( jb, jb ), lda, t( 1_ilp, jb ), &
                        ldt )
           end do
           return
     end subroutine stdlib_sorhr_col

     pure module subroutine stdlib_dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
     !! DORHR_COL takes an M-by-N real matrix Q_in with orthonormal columns
     !! as input, stored in A, and performs Householder Reconstruction (HR),
     !! i.e. reconstructs Householder vectors V(i) implicitly representing
     !! another M-by-N matrix Q_out, with the property that Q_in = Q_out*S,
     !! where S is an N-by-N diagonal matrix with diagonal entries
     !! equal to +1 or -1. The Householder vectors (columns V(i) of V) are
     !! stored in A on output, and the diagonal entries of S are stored in D.
     !! Block reflectors are also returned in T
     !! (same output format as DGEQRT).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, jbtemp1, jbtemp2, jnb, nplusone
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( nb<1_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<max( 1_ilp, min( nb, n ) ) ) then
              info = -7_ilp
           end if
           ! handle error in the input parameters.
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORHR_COL', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
              return
           end if
           ! on input, the m-by-n matrix a contains the orthogonal
           ! m-by-n matrix q_in.
           ! (1) compute the unit lower-trapezoidal v (ones on the diagonal
           ! are not stored) by performing the "modified" lu-decomposition.
           ! q_in - ( s ) = v * u = ( v1 ) * u,
                  ! ( 0 )           ( v2 )
           ! where 0 is an (m-n)-by-n zero matrix.
           ! (1-1) factor v1 and u.
           call stdlib_dlaorhr_col_getrfnp( n, n, a, lda, d, iinfo )
           ! (1-2) solve for v2.
           if( m>n ) then
              call stdlib_dtrsm( 'R', 'U', 'N', 'N', m-n, n, one, a, lda,a( n+1, 1_ilp ), lda )
                        
           end if
           ! (2) reconstruct the block reflector t stored in t(1:nb, 1:n)
           ! as a sequence of upper-triangular blocks with nb-size column
           ! blocking.
           ! loop over the column blocks of size nb of the array a(1:m,1:n)
           ! and the array t(1:nb,1:n), jb is the column index of a column
           ! block, jnb is the column block size at each step jb.
           nplusone = n + 1_ilp
           do jb = 1, n, nb
              ! (2-0) determine the column block size jnb.
              jnb = min( nplusone-jb, nb )
              ! (2-1) copy the upper-triangular part of the current jnb-by-jnb
              ! diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in a(jb:jb+jnb-1,jb:jb+jnb-1) into the upper-triangular part
              ! of the current jnb-by-jnb block t(1:jnb,jb:jb+jnb-1)
              ! column-by-column, total jnb*(jnb+1)/2 elements.
              jbtemp1 = jb - 1_ilp
              do j = jb, jb+jnb-1
                 call stdlib_dcopy( j-jbtemp1, a( jb, j ), 1_ilp, t( 1_ilp, j ), 1_ilp )
              end do
              ! (2-2) perform on the upper-triangular part of the current
              ! jnb-by-jnb diagonal block u(jb) (of the n-by-n matrix u) stored
              ! in t(1:jnb,jb:jb+jnb-1) the following operation in place:
              ! (-1)*u(jb)*s(jb), i.e the result will be stored in the upper-
              ! triangular part of t(1:jnb,jb:jb+jnb-1). this multiplication
              ! of the jnb-by-jnb diagonal block u(jb) by the jnb-by-jnb
              ! diagonal block s(jb) of the n-by-n sign matrix s from the
              ! right means changing the sign of each j-th column of the block
              ! u(jb) according to the sign of the diagonal element of the block
              ! s(jb), i.e. s(j,j) that is stored in the array element d(j).
              do j = jb, jb+jnb-1
                 if( d( j )==one ) then
                    call stdlib_dscal( j-jbtemp1, -one, t( 1_ilp, j ), 1_ilp )
                 end if
              end do
              ! (2-3) perform the triangular solve for the current block
              ! matrix x(jb):
                     ! x(jb) * (a(jb)**t) = b(jb), where:
                     ! a(jb)**t  is a jnb-by-jnb unit upper-triangular
                               ! coefficient block, and a(jb)=v1(jb), which
                               ! is a jnb-by-jnb unit lower-triangular block
                               ! stored in a(jb:jb+jnb-1,jb:jb+jnb-1).
                               ! the n-by-n matrix v1 is the upper part
                               ! of the m-by-n lower-trapezoidal matrix v
                               ! stored in a(1:m,1:n);
                     ! b(jb)     is a jnb-by-jnb  upper-triangular right-hand
                               ! side block, b(jb) = (-1)*u(jb)*s(jb), and
                               ! b(jb) is stored in t(1:jnb,jb:jb+jnb-1);
                     ! x(jb)     is a jnb-by-jnb upper-triangular solution
                               ! block, x(jb) is the upper-triangular block
                               ! reflector t(jb), and x(jb) is stored
                               ! in t(1:jnb,jb:jb+jnb-1).
                   ! in other words, we perform the triangular solve for the
                   ! upper-triangular block t(jb):
                     ! t(jb) * (v1(jb)**t) = (-1)*u(jb)*s(jb).
                   ! even though the blocks x(jb) and b(jb) are upper-
                   ! triangular, the routine stdlib_dtrsm will access all jnb**2
                   ! elements of the square t(1:jnb,jb:jb+jnb-1). therefore,
                   ! we need to set to zero the elements of the block
                   ! t(1:jnb,jb:jb+jnb-1) below the diagonal before the call
                   ! to stdlib_dtrsm.
              ! (2-3a) set the elements to zero.
              jbtemp2 = jb - 2_ilp
              do j = jb, jb+jnb-2
                 do i = j-jbtemp2, nb
                    t( i, j ) = zero
                 end do
              end do
              ! (2-3b) perform the triangular solve.
              call stdlib_dtrsm( 'R', 'L', 'T', 'U', jnb, jnb, one,a( jb, jb ), lda, t( 1_ilp, jb ), &
                        ldt )
           end do
           return
     end subroutine stdlib_dorhr_col




     pure module subroutine stdlib_claunhr_col_getrfnp( m, n, a, lda, d, info )
     !! CLAUNHR_COL_GETRFNP computes the modified LU factorization without
     !! pivoting of a complex general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is
     !! at least one in absolute value (so that division-by-zero not
     !! not possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine CUNHR_COL. In CUNHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the blocked right-looking version of the algorithm,
     !! calling Level 3 BLAS to update the submatrix. To factorize a block,
     !! this routine calls the recursive routine CLAUNHR_COL_GETRFNP2.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAUNHR_COL_GETRFNP', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CLAUNHR_COL_GETRFNP', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_claunhr_col_getrfnp2( m, n, a, lda, d, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks.
                 call stdlib_claunhr_col_getrfnp2( m-j+1, jb, a( j, j ), lda,d( j ), iinfo )
                           
                 if( j+jb<=n ) then
                    ! compute block row of u.
                    call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, cone,&
                               a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       cone, a( j+jb, j ), lda,a( j, j+jb ), lda, cone, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_claunhr_col_getrfnp

     pure module subroutine stdlib_zlaunhr_col_getrfnp( m, n, a, lda, d, info )
     !! ZLAUNHR_COL_GETRFNP computes the modified LU factorization without
     !! pivoting of a complex general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is
     !! at least one in absolute value (so that division-by-zero not
     !! not possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine ZUNHR_COL. In ZUNHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the blocked right-looking version of the algorithm,
     !! calling Level 3 BLAS to update the submatrix. To factorize a block,
     !! this routine calls the recursive routine ZLAUNHR_COL_GETRFNP2.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAUNHR_COL_GETRFNP', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZLAUNHR_COL_GETRFNP', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_zlaunhr_col_getrfnp2( m, n, a, lda, d, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks.
                 call stdlib_zlaunhr_col_getrfnp2( m-j+1, jb, a( j, j ), lda,d( j ), iinfo )
                           
                 if( j+jb<=n ) then
                    ! compute block row of u.
                    call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, cone,&
                               a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       cone, a( j+jb, j ), lda,a( j, j+jb ), lda, cone, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_zlaunhr_col_getrfnp




     pure module subroutine stdlib_slaorhr_col_getrfnp( m, n, a, lda, d, info )
     !! SLAORHR_COL_GETRFNP computes the modified LU factorization without
     !! pivoting of a real general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is
     !! at least one in absolute value (so that division-by-zero not
     !! not possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine SORHR_COL. In SORHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the blocked right-looking version of the algorithm,
     !! calling Level 3 BLAS to update the submatrix. To factorize a block,
     !! this routine calls the recursive routine SLAORHR_COL_GETRFNP2.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAORHR_COL_GETRFNP', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'SLAORHR_COL_GETRFNP', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_slaorhr_col_getrfnp2( m, n, a, lda, d, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks.
                 call stdlib_slaorhr_col_getrfnp2( m-j+1, jb, a( j, j ), lda,d( j ), iinfo )
                           
                 if( j+jb<=n ) then
                    ! compute block row of u.
                    call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, one, &
                              a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       one, a( j+jb, j ), lda,a( j, j+jb ), lda, one, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_slaorhr_col_getrfnp

     pure module subroutine stdlib_dlaorhr_col_getrfnp( m, n, a, lda, d, info )
     !! DLAORHR_COL_GETRFNP computes the modified LU factorization without
     !! pivoting of a real general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is
     !! at least one in absolute value (so that division-by-zero not
     !! not possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine DORHR_COL. In DORHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the blocked right-looking version of the algorithm,
     !! calling Level 3 BLAS to update the submatrix. To factorize a block,
     !! this routine calls the recursive routine DLAORHR_COL_GETRFNP2.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAORHR_COL_GETRFNP', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DLAORHR_COL_GETRFNP', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_dlaorhr_col_getrfnp2( m, n, a, lda, d, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks.
                 call stdlib_dlaorhr_col_getrfnp2( m-j+1, jb, a( j, j ), lda,d( j ), iinfo )
                           
                 if( j+jb<=n ) then
                    ! compute block row of u.
                    call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, one, &
                              a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       one, a( j+jb, j ), lda,a( j, j+jb ), lda, one, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_dlaorhr_col_getrfnp




     pure recursive module subroutine stdlib_claunhr_col_getrfnp2( m, n, a, lda, d, info )
     !! CLAUNHR_COL_GETRFNP2 computes the modified LU factorization without
     !! pivoting of a complex general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is at
     !! least one in absolute value (so that division-by-zero not
     !! possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine CUNHR_COL. In CUNHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the recursive version of the LU factorization algorithm.
     !! Denote A - S by B. The algorithm divides the matrix B into four
     !! submatrices:
     !! [  B11 | B12  ]  where B11 is n1 by n1,
     !! B = [ -----|----- ]        B21 is (m-n1) by n1,
     !! [  B21 | B22  ]        B12 is n1 by n2,
     !! B22 is (m-n1) by n2,
     !! with n1 = min(m,n)/2, n2 = n-n1.
     !! The subroutine calls itself to factor B11, solves for B21,
     !! solves for B12, updates B22, then calls itself to factor B22.
     !! For more details on the recursive LU algorithm, see [2].
     !! CLAUNHR_COL_GETRFNP2 is called to factorize a block by the blocked
     !! routine CLAUNHR_COL_GETRFNP, which uses blocked code calling
     !! Level 3 BLAS to update the submatrix. However, CLAUNHR_COL_GETRFNP2
     !! is self-sufficient and can be used without CLAUNHR_COL_GETRFNP.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
     !! [2] "Recursion leads to automatic variable blocking for dense linear
     !! algebra algorithms", F. Gustavson, IBM J. of Res. and Dev.,
     !! vol. 41, no. 6, pp. 737-755, 1997.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: sfmin
           integer(ilp) :: i, iinfo, n1, n2
           complex(sp) :: z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAUNHR_COL_GETRFNP2', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           if ( m==1_ilp ) then
              ! one row case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = cmplx( -sign( one, real( a( 1_ilp, 1_ilp ),KIND=sp) ),KIND=sp)
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
           else if( n==1_ilp ) then
              ! one column case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = cmplx( -sign( one, real( a( 1_ilp, 1_ilp ),KIND=sp) ),KIND=sp)
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
              ! scale the elements 2:m of the column
              ! determine machine safe minimum
              sfmin = stdlib_slamch('S')
              ! construct the subdiagonal elements of l
              if( cabs1( a( 1_ilp, 1_ilp ) ) >= sfmin ) then
                 call stdlib_cscal( m-1, cone / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 2, m
                    a( i, 1_ilp ) = a( i, 1_ilp ) / a( 1_ilp, 1_ilp )
                 end do
              end if
           else
              ! divide the matrix b into four submatrices
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
              ! factor b11, recursive call
              call stdlib_claunhr_col_getrfnp2( n1, n1, a, lda, d, iinfo )
              ! solve for b21
              call stdlib_ctrsm( 'R', 'U', 'N', 'N', m-n1, n1, cone, a, lda,a( n1+1, 1_ilp ), lda )
                        
              ! solve for b12
              call stdlib_ctrsm( 'L', 'L', 'N', 'U', n1, n2, cone, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update b22, i.e. compute the schur complement
              ! b22 := b22 - b21*b12
              call stdlib_cgemm( 'N', 'N', m-n1, n2, n1, -cone, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, cone, a( n1+1, n1+1 ), lda )
              ! factor b22, recursive call
              call stdlib_claunhr_col_getrfnp2( m-n1, n2, a( n1+1, n1+1 ), lda,d( n1+1 ), iinfo )
                        
           end if
           return
     end subroutine stdlib_claunhr_col_getrfnp2

     pure recursive module subroutine stdlib_zlaunhr_col_getrfnp2( m, n, a, lda, d, info )
     !! ZLAUNHR_COL_GETRFNP2 computes the modified LU factorization without
     !! pivoting of a complex general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is at
     !! least one in absolute value (so that division-by-zero not
     !! possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine ZUNHR_COL. In ZUNHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the recursive version of the LU factorization algorithm.
     !! Denote A - S by B. The algorithm divides the matrix B into four
     !! submatrices:
     !! [  B11 | B12  ]  where B11 is n1 by n1,
     !! B = [ -----|----- ]        B21 is (m-n1) by n1,
     !! [  B21 | B22  ]        B12 is n1 by n2,
     !! B22 is (m-n1) by n2,
     !! with n1 = min(m,n)/2, n2 = n-n1.
     !! The subroutine calls itself to factor B11, solves for B21,
     !! solves for B12, updates B22, then calls itself to factor B22.
     !! For more details on the recursive LU algorithm, see [2].
     !! ZLAUNHR_COL_GETRFNP2 is called to factorize a block by the blocked
     !! routine ZLAUNHR_COL_GETRFNP, which uses blocked code calling
     !! Level 3 BLAS to update the submatrix. However, ZLAUNHR_COL_GETRFNP2
     !! is self-sufficient and can be used without ZLAUNHR_COL_GETRFNP.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
     !! [2] "Recursion leads to automatic variable blocking for dense linear
     !! algebra algorithms", F. Gustavson, IBM J. of Res. and Dev.,
     !! vol. 41, no. 6, pp. 737-755, 1997.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: sfmin
           integer(ilp) :: i, iinfo, n1, n2
           complex(dp) :: z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAUNHR_COL_GETRFNP2', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           if ( m==1_ilp ) then
              ! one row case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = cmplx( -sign( one, real( a( 1_ilp, 1_ilp ),KIND=dp) ),KIND=dp)
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
           else if( n==1_ilp ) then
              ! one column case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = cmplx( -sign( one, real( a( 1_ilp, 1_ilp ),KIND=dp) ),KIND=dp)
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
              ! scale the elements 2:m of the column
              ! determine machine safe minimum
              sfmin = stdlib_dlamch('S')
              ! construct the subdiagonal elements of l
              if( cabs1( a( 1_ilp, 1_ilp ) ) >= sfmin ) then
                 call stdlib_zscal( m-1, cone / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 2, m
                    a( i, 1_ilp ) = a( i, 1_ilp ) / a( 1_ilp, 1_ilp )
                 end do
              end if
           else
              ! divide the matrix b into four submatrices
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
              ! factor b11, recursive call
              call stdlib_zlaunhr_col_getrfnp2( n1, n1, a, lda, d, iinfo )
              ! solve for b21
              call stdlib_ztrsm( 'R', 'U', 'N', 'N', m-n1, n1, cone, a, lda,a( n1+1, 1_ilp ), lda )
                        
              ! solve for b12
              call stdlib_ztrsm( 'L', 'L', 'N', 'U', n1, n2, cone, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update b22, i.e. compute the schur complement
              ! b22 := b22 - b21*b12
              call stdlib_zgemm( 'N', 'N', m-n1, n2, n1, -cone, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, cone, a( n1+1, n1+1 ), lda )
              ! factor b22, recursive call
              call stdlib_zlaunhr_col_getrfnp2( m-n1, n2, a( n1+1, n1+1 ), lda,d( n1+1 ), iinfo )
                        
           end if
           return
     end subroutine stdlib_zlaunhr_col_getrfnp2




     pure recursive module subroutine stdlib_slaorhr_col_getrfnp2( m, n, a, lda, d, info )
     !! SLAORHR_COL_GETRFNP2 computes the modified LU factorization without
     !! pivoting of a real general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is at
     !! least one in absolute value (so that division-by-zero not
     !! possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine SORHR_COL. In SORHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the recursive version of the LU factorization algorithm.
     !! Denote A - S by B. The algorithm divides the matrix B into four
     !! submatrices:
     !! [  B11 | B12  ]  where B11 is n1 by n1,
     !! B = [ -----|----- ]        B21 is (m-n1) by n1,
     !! [  B21 | B22  ]        B12 is n1 by n2,
     !! B22 is (m-n1) by n2,
     !! with n1 = min(m,n)/2, n2 = n-n1.
     !! The subroutine calls itself to factor B11, solves for B21,
     !! solves for B12, updates B22, then calls itself to factor B22.
     !! For more details on the recursive LU algorithm, see [2].
     !! SLAORHR_COL_GETRFNP2 is called to factorize a block by the blocked
     !! routine SLAORHR_COL_GETRFNP, which uses blocked code calling
     !! Level 3 BLAS to update the submatrix. However, SLAORHR_COL_GETRFNP2
     !! is self-sufficient and can be used without SLAORHR_COL_GETRFNP.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
     !! [2] "Recursion leads to automatic variable blocking for dense linear
     !! algebra algorithms", F. Gustavson, IBM J. of Res. and Dev.,
     !! vol. 41, no. 6, pp. 737-755, 1997.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAORHR_COL_GETRFNP2', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           if ( m==1_ilp ) then
              ! one row case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = -sign( one, a( 1_ilp, 1_ilp ) )
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
           else if( n==1_ilp ) then
              ! one column case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = -sign( one, a( 1_ilp, 1_ilp ) )
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
              ! scale the elements 2:m of the column
              ! determine machine safe minimum
              sfmin = stdlib_slamch('S')
              ! construct the subdiagonal elements of l
              if( abs( a( 1_ilp, 1_ilp ) ) >= sfmin ) then
                 call stdlib_sscal( m-1, one / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 2, m
                    a( i, 1_ilp ) = a( i, 1_ilp ) / a( 1_ilp, 1_ilp )
                 end do
              end if
           else
              ! divide the matrix b into four submatrices
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
              ! factor b11, recursive call
              call stdlib_slaorhr_col_getrfnp2( n1, n1, a, lda, d, iinfo )
              ! solve for b21
              call stdlib_strsm( 'R', 'U', 'N', 'N', m-n1, n1, one, a, lda,a( n1+1, 1_ilp ), lda )
                        
              ! solve for b12
              call stdlib_strsm( 'L', 'L', 'N', 'U', n1, n2, one, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update b22, i.e. compute the schur complement
              ! b22 := b22 - b21*b12
              call stdlib_sgemm( 'N', 'N', m-n1, n2, n1, -one, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, one, a( n1+1, n1+1 ), lda )
              ! factor b22, recursive call
              call stdlib_slaorhr_col_getrfnp2( m-n1, n2, a( n1+1, n1+1 ), lda,d( n1+1 ), iinfo )
                        
           end if
           return
     end subroutine stdlib_slaorhr_col_getrfnp2

     pure recursive module subroutine stdlib_dlaorhr_col_getrfnp2( m, n, a, lda, d, info )
     !! DLAORHR_COL_GETRFNP2 computes the modified LU factorization without
     !! pivoting of a real general M-by-N matrix A. The factorization has
     !! the form:
     !! A - S = L * U,
     !! where:
     !! S is a m-by-n diagonal sign matrix with the diagonal D, so that
     !! D(i) = S(i,i), 1 <= i <= min(M,N). The diagonal D is constructed
     !! as D(i)=-SIGN(A(i,i)), where A(i,i) is the value after performing
     !! i-1 steps of Gaussian elimination. This means that the diagonal
     !! element at each step of "modified" Gaussian elimination is at
     !! least one in absolute value (so that division-by-zero not
     !! possible during the division by the diagonal element);
     !! L is a M-by-N lower triangular matrix with unit diagonal elements
     !! (lower trapezoidal if M > N);
     !! and U is a M-by-N upper triangular matrix
     !! (upper trapezoidal if M < N).
     !! This routine is an auxiliary routine used in the Householder
     !! reconstruction routine DORHR_COL. In DORHR_COL, this routine is
     !! applied to an M-by-N matrix A with orthonormal columns, where each
     !! element is bounded by one in absolute value. With the choice of
     !! the matrix S above, one can show that the diagonal element at each
     !! step of Gaussian elimination is the largest (in absolute value) in
     !! the column on or below the diagonal, so that no pivoting is required
     !! for numerical stability [1].
     !! For more details on the Householder reconstruction algorithm,
     !! including the modified LU factorization, see [1].
     !! This is the recursive version of the LU factorization algorithm.
     !! Denote A - S by B. The algorithm divides the matrix B into four
     !! submatrices:
     !! [  B11 | B12  ]  where B11 is n1 by n1,
     !! B = [ -----|----- ]        B21 is (m-n1) by n1,
     !! [  B21 | B22  ]        B12 is n1 by n2,
     !! B22 is (m-n1) by n2,
     !! with n1 = min(m,n)/2, n2 = n-n1.
     !! The subroutine calls itself to factor B11, solves for B21,
     !! solves for B12, updates B22, then calls itself to factor B22.
     !! For more details on the recursive LU algorithm, see [2].
     !! DLAORHR_COL_GETRFNP2 is called to factorize a block by the blocked
     !! routine DLAORHR_COL_GETRFNP, which uses blocked code calling
     !! Level 3 BLAS to update the submatrix. However, DLAORHR_COL_GETRFNP2
     !! is self-sufficient and can be used without DLAORHR_COL_GETRFNP.
     !! [1] "Reconstructing Householder vectors from tall-skinny QR",
     !! G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
     !! E. Solomonik, J. Parallel Distrib. Comput.,
     !! vol. 85, pp. 3-31, 2015.
     !! [2] "Recursion leads to automatic variable blocking for dense linear
     !! algebra algorithms", F. Gustavson, IBM J. of Res. and Dev.,
     !! vol. 41, no. 6, pp. 737-755, 1997.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAORHR_COL_GETRFNP2', -info )
              return
           end if
           ! quick return if possible
           if( min( m, n )==0 )return
           if ( m==1_ilp ) then
              ! one row case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = -sign( one, a( 1_ilp, 1_ilp ) )
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
           else if( n==1_ilp ) then
              ! one column case, (also recursion termination case),
              ! use unblocked code
              ! transfer the sign
              d( 1_ilp ) = -sign( one, a( 1_ilp, 1_ilp ) )
              ! construct the row of u
              a( 1_ilp, 1_ilp ) = a( 1_ilp, 1_ilp ) - d( 1_ilp )
              ! scale the elements 2:m of the column
              ! determine machine safe minimum
              sfmin = stdlib_dlamch('S')
              ! construct the subdiagonal elements of l
              if( abs( a( 1_ilp, 1_ilp ) ) >= sfmin ) then
                 call stdlib_dscal( m-1, one / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 2, m
                    a( i, 1_ilp ) = a( i, 1_ilp ) / a( 1_ilp, 1_ilp )
                 end do
              end if
           else
              ! divide the matrix b into four submatrices
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
              ! factor b11, recursive call
              call stdlib_dlaorhr_col_getrfnp2( n1, n1, a, lda, d, iinfo )
              ! solve for b21
              call stdlib_dtrsm( 'R', 'U', 'N', 'N', m-n1, n1, one, a, lda,a( n1+1, 1_ilp ), lda )
                        
              ! solve for b12
              call stdlib_dtrsm( 'L', 'L', 'N', 'U', n1, n2, one, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update b22, i.e. compute the schur complement
              ! b22 := b22 - b21*b12
              call stdlib_dgemm( 'N', 'N', m-n1, n2, n1, -one, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, one, a( n1+1, n1+1 ), lda )
              ! factor b22, recursive call
              call stdlib_dlaorhr_col_getrfnp2( m-n1, n2, a( n1+1, n1+1 ), lda,d( n1+1 ), iinfo )
                        
           end if
           return
     end subroutine stdlib_dlaorhr_col_getrfnp2




     pure module subroutine stdlib_stpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
     !! STPQRT computes a blocked QR factorization of a real
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, mb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( nb<1_ilp .or. (nb>n .and. n>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPQRT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, n, nb
           ! compute the qr factorization of the current block
              ib = min( n-i+1, nb )
              mb = min( m-l+i+ib-1, m )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = mb-m+l-i+1
              end if
              call stdlib_stpqrt2( mb, ib, lb, a(i,i), lda, b( 1_ilp, i ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h^h to b(:,i+ib:n) from the left
              if( i+ib<=n ) then
                 call stdlib_stprfb( 'L', 'T', 'F', 'C', mb, n-i-ib+1, ib, lb,b( 1_ilp, i ), ldb, t( &
                           1_ilp, i ), ldt,a( i, i+ib ), lda, b( 1_ilp, i+ib ), ldb,work, ib )
              end if
           end do
           return
     end subroutine stdlib_stpqrt

     pure module subroutine stdlib_dtpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
     !! DTPQRT computes a blocked QR factorization of a real
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, mb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( nb<1_ilp .or. (nb>n .and. n>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPQRT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, n, nb
           ! compute the qr factorization of the current block
              ib = min( n-i+1, nb )
              mb = min( m-l+i+ib-1, m )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = mb-m+l-i+1
              end if
              call stdlib_dtpqrt2( mb, ib, lb, a(i,i), lda, b( 1_ilp, i ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**t to b(:,i+ib:n) from the left
              if( i+ib<=n ) then
                 call stdlib_dtprfb( 'L', 'T', 'F', 'C', mb, n-i-ib+1, ib, lb,b( 1_ilp, i ), ldb, t( &
                           1_ilp, i ), ldt,a( i, i+ib ), lda, b( 1_ilp, i+ib ), ldb,work, ib )
              end if
           end do
           return
     end subroutine stdlib_dtpqrt


     pure module subroutine stdlib_ctpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
     !! CTPQRT computes a blocked QR factorization of a complex
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, mb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( nb<1_ilp .or. (nb>n .and. n>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPQRT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, n, nb
           ! compute the qr factorization of the current block
              ib = min( n-i+1, nb )
              mb = min( m-l+i+ib-1, m )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = mb-m+l-i+1
              end if
              call stdlib_ctpqrt2( mb, ib, lb, a(i,i), lda, b( 1_ilp, i ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**h to b(:,i+ib:n) from the left
              if( i+ib<=n ) then
                 call stdlib_ctprfb( 'L', 'C', 'F', 'C', mb, n-i-ib+1, ib, lb,b( 1_ilp, i ), ldb, t( &
                           1_ilp, i ), ldt,a( i, i+ib ), lda, b( 1_ilp, i+ib ), ldb,work, ib )
              end if
           end do
           return
     end subroutine stdlib_ctpqrt

     pure module subroutine stdlib_ztpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
     !! ZTPQRT computes a blocked QR factorization of a complex
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, mb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( nb<1_ilp .or. (nb>n .and. n>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<nb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPQRT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, n, nb
           ! compute the qr factorization of the current block
              ib = min( n-i+1, nb )
              mb = min( m-l+i+ib-1, m )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = mb-m+l-i+1
              end if
              call stdlib_ztpqrt2( mb, ib, lb, a(i,i), lda, b( 1_ilp, i ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**h to b(:,i+ib:n) from the left
              if( i+ib<=n ) then
                 call stdlib_ztprfb( 'L', 'C', 'F', 'C', mb, n-i-ib+1, ib, lb,b( 1_ilp, i ), ldb, t( &
                           1_ilp, i ), ldt,a( i, i+ib ), lda, b( 1_ilp, i+ib ), ldb,work, ib )
              end if
           end do
           return
     end subroutine stdlib_ztpqrt




     pure module subroutine stdlib_stpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! STPQRT2 computes a QR factorization of a real "triangular-pentagonal"
     !! matrix C, which is composed of a triangular block A and pentagonal block B,
     !! using the compact WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, p, mp, np
           real(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. l>min(m,n) ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPQRT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, n
              ! generate elementary reflector h(i) to annihilate b(:,i)
              p = m-l+min( l, i )
              call stdlib_slarfg( p+1, a( i, i ), b( 1_ilp, i ), 1_ilp, t( i, 1_ilp ) )
              if( i<n ) then
                 ! w(1:n-i) := c(i:m,i+1:n)^h * c(i:m,i) [use w = t(:,n)]
                 do j = 1, n-i
                    t( j, n ) = (a( i, i+j ))
                 end do
                 call stdlib_sgemv( 'T', p, n-i, one, b( 1_ilp, i+1 ), ldb,b( 1_ilp, i ), 1_ilp, one, t( 1_ilp, n &
                           ), 1_ilp )
                 ! c(i:m,i+1:n) = c(i:m,i+1:n) + alpha*c(i:m,i)*w(1:n-1)^h
                 alpha = -(t( i, 1_ilp ))
                 do j = 1, n-i
                    a( i, i+j ) = a( i, i+j ) + alpha*(t( j, n ))
                 end do
                 call stdlib_sger( p, n-i, alpha, b( 1_ilp, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, b( 1_ilp, i+1 ), ldb )
                           
              end if
           end do
           do i = 2, n
              ! t(1:i-1,i) := c(i:m,1:i-1)^h * (alpha * c(i:m,i))
              alpha = -t( i, 1_ilp )
              do j = 1, i-1
                 t( j, i ) = zero
              end do
              p = min( i-1, l )
              mp = min( m-l+1, m )
              np = min( p+1, n )
              ! triangular part of b2
              do j = 1, p
                 t( j, i ) = alpha*b( m-l+j, i )
              end do
              call stdlib_strmv( 'U', 'T', 'N', p, b( mp, 1_ilp ), ldb,t( 1_ilp, i ), 1_ilp )
              ! rectangular part of b2
              call stdlib_sgemv( 'T', l, i-1-p, alpha, b( mp, np ), ldb,b( mp, i ), 1_ilp, zero, t( &
                        np, i ), 1_ilp )
              ! b1
              call stdlib_sgemv( 'T', m-l, i-1, alpha, b, ldb, b( 1_ilp, i ), 1_ilp,one, t( 1_ilp, i ), 1_ilp )
                        
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_strmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
              ! t(i,i) = tau(i)
              t( i, i ) = t( i, 1_ilp )
              t( i, 1_ilp ) = zero
           end do
     end subroutine stdlib_stpqrt2

     pure module subroutine stdlib_dtpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! DTPQRT2 computes a QR factorization of a real "triangular-pentagonal"
     !! matrix C, which is composed of a triangular block A and pentagonal block B,
     !! using the compact WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, p, mp, np
           real(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. l>min(m,n) ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPQRT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, n
              ! generate elementary reflector h(i) to annihilate b(:,i)
              p = m-l+min( l, i )
              call stdlib_dlarfg( p+1, a( i, i ), b( 1_ilp, i ), 1_ilp, t( i, 1_ilp ) )
              if( i<n ) then
                 ! w(1:n-i) := c(i:m,i+1:n)^h * c(i:m,i) [use w = t(:,n)]
                 do j = 1, n-i
                    t( j, n ) = (a( i, i+j ))
                 end do
                 call stdlib_dgemv( 'T', p, n-i, one, b( 1_ilp, i+1 ), ldb,b( 1_ilp, i ), 1_ilp, one, t( 1_ilp, n &
                           ), 1_ilp )
                 ! c(i:m,i+1:n) = c(i:m,i+1:n) + alpha*c(i:m,i)*w(1:n-1)^h
                 alpha = -(t( i, 1_ilp ))
                 do j = 1, n-i
                    a( i, i+j ) = a( i, i+j ) + alpha*(t( j, n ))
                 end do
                 call stdlib_dger( p, n-i, alpha, b( 1_ilp, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, b( 1_ilp, i+1 ), ldb )
                           
              end if
           end do
           do i = 2, n
              ! t(1:i-1,i) := c(i:m,1:i-1)^h * (alpha * c(i:m,i))
              alpha = -t( i, 1_ilp )
              do j = 1, i-1
                 t( j, i ) = zero
              end do
              p = min( i-1, l )
              mp = min( m-l+1, m )
              np = min( p+1, n )
              ! triangular part of b2
              do j = 1, p
                 t( j, i ) = alpha*b( m-l+j, i )
              end do
              call stdlib_dtrmv( 'U', 'T', 'N', p, b( mp, 1_ilp ), ldb,t( 1_ilp, i ), 1_ilp )
              ! rectangular part of b2
              call stdlib_dgemv( 'T', l, i-1-p, alpha, b( mp, np ), ldb,b( mp, i ), 1_ilp, zero, t( &
                        np, i ), 1_ilp )
              ! b1
              call stdlib_dgemv( 'T', m-l, i-1, alpha, b, ldb, b( 1_ilp, i ), 1_ilp,one, t( 1_ilp, i ), 1_ilp )
                        
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_dtrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
              ! t(i,i) = tau(i)
              t( i, i ) = t( i, 1_ilp )
              t( i, 1_ilp ) = zero
           end do
     end subroutine stdlib_dtpqrt2


     pure module subroutine stdlib_ctpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! CTPQRT2 computes a QR factorization of a complex "triangular-pentagonal"
     !! matrix C, which is composed of a triangular block A and pentagonal block B,
     !! using the compact WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, p, mp, np
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. l>min(m,n) ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPQRT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, n
              ! generate elementary reflector h(i) to annihilate b(:,i)
              p = m-l+min( l, i )
              call stdlib_clarfg( p+1, a( i, i ), b( 1_ilp, i ), 1_ilp, t( i, 1_ilp ) )
              if( i<n ) then
                 ! w(1:n-i) := c(i:m,i+1:n)**h * c(i:m,i) [use w = t(:,n)]
                 do j = 1, n-i
                    t( j, n ) = conjg(a( i, i+j ))
                 end do
                 call stdlib_cgemv( 'C', p, n-i, cone, b( 1_ilp, i+1 ), ldb,b( 1_ilp, i ), 1_ilp, cone, t( 1_ilp, &
                           n ), 1_ilp )
                 ! c(i:m,i+1:n) = c(i:m,i+1:n) + alpha*c(i:m,i)*w(1:n-1)**h
                 alpha = -conjg(t( i, 1_ilp ))
                 do j = 1, n-i
                    a( i, i+j ) = a( i, i+j ) + alpha*conjg(t( j, n ))
                 end do
                 call stdlib_cgerc( p, n-i, alpha, b( 1_ilp, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, b( 1_ilp, i+1 ), ldb )
                           
              end if
           end do
           do i = 2, n
              ! t(1:i-1,i) := c(i:m,1:i-1)**h * (alpha * c(i:m,i))
              alpha = -t( i, 1_ilp )
              do j = 1, i-1
                 t( j, i ) = czero
              end do
              p = min( i-1, l )
              mp = min( m-l+1, m )
              np = min( p+1, n )
              ! triangular part of b2
              do j = 1, p
                 t( j, i ) = alpha*b( m-l+j, i )
              end do
              call stdlib_ctrmv( 'U', 'C', 'N', p, b( mp, 1_ilp ), ldb,t( 1_ilp, i ), 1_ilp )
              ! rectangular part of b2
              call stdlib_cgemv( 'C', l, i-1-p, alpha, b( mp, np ), ldb,b( mp, i ), 1_ilp, czero, t( &
                        np, i ), 1_ilp )
              ! b1
              call stdlib_cgemv( 'C', m-l, i-1, alpha, b, ldb, b( 1_ilp, i ), 1_ilp,cone, t( 1_ilp, i ), 1_ilp )
                        
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_ctrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
              ! t(i,i) = tau(i)
              t( i, i ) = t( i, 1_ilp )
              t( i, 1_ilp ) = czero
           end do
     end subroutine stdlib_ctpqrt2

     pure module subroutine stdlib_ztpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! ZTPQRT2 computes a QR factorization of a complex "triangular-pentagonal"
     !! matrix C, which is composed of a triangular block A and pentagonal block B,
     !! using the compact WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, p, mp, np
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. l>min(m,n) ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPQRT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, n
              ! generate elementary reflector h(i) to annihilate b(:,i)
              p = m-l+min( l, i )
              call stdlib_zlarfg( p+1, a( i, i ), b( 1_ilp, i ), 1_ilp, t( i, 1_ilp ) )
              if( i<n ) then
                 ! w(1:n-i) := c(i:m,i+1:n)**h * c(i:m,i) [use w = t(:,n)]
                 do j = 1, n-i
                    t( j, n ) = conjg(a( i, i+j ))
                 end do
                 call stdlib_zgemv( 'C', p, n-i, cone, b( 1_ilp, i+1 ), ldb,b( 1_ilp, i ), 1_ilp, cone, t( 1_ilp, &
                           n ), 1_ilp )
                 ! c(i:m,i+1:n) = c(i:m,i+1:n) + alpha*c(i:m,i)*w(1:n-1)**h
                 alpha = -conjg(t( i, 1_ilp ))
                 do j = 1, n-i
                    a( i, i+j ) = a( i, i+j ) + alpha*conjg(t( j, n ))
                 end do
                 call stdlib_zgerc( p, n-i, alpha, b( 1_ilp, i ), 1_ilp,t( 1_ilp, n ), 1_ilp, b( 1_ilp, i+1 ), ldb )
                           
              end if
           end do
           do i = 2, n
              ! t(1:i-1,i) := c(i:m,1:i-1)**h * (alpha * c(i:m,i))
              alpha = -t( i, 1_ilp )
              do j = 1, i-1
                 t( j, i ) = czero
              end do
              p = min( i-1, l )
              mp = min( m-l+1, m )
              np = min( p+1, n )
              ! triangular part of b2
              do j = 1, p
                 t( j, i ) = alpha*b( m-l+j, i )
              end do
              call stdlib_ztrmv( 'U', 'C', 'N', p, b( mp, 1_ilp ), ldb,t( 1_ilp, i ), 1_ilp )
              ! rectangular part of b2
              call stdlib_zgemv( 'C', l, i-1-p, alpha, b( mp, np ), ldb,b( mp, i ), 1_ilp, czero, t( &
                        np, i ), 1_ilp )
              ! b1
              call stdlib_zgemv( 'C', m-l, i-1, alpha, b, ldb, b( 1_ilp, i ), 1_ilp,cone, t( 1_ilp, i ), 1_ilp )
                        
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
              call stdlib_ztrmv( 'U', 'N', 'N', i-1, t, ldt, t( 1_ilp, i ), 1_ilp )
              ! t(i,i) = tau(i)
              t( i, i ) = t( i, 1_ilp )
              t( i, 1_ilp ) = czero
           end do
     end subroutine stdlib_ztpqrt2




     pure module subroutine stdlib_stpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! STPMQRT applies a real orthogonal matrix Q obtained from a
     !! "triangular-pentagonal" real block reflector H to a general
     !! real matrix C, which consists of two blocks A and B.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           ! Array Arguments 
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, mb, lb, kf, ldaq, ldvq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldvq = max( 1_ilp, m )
              ldaq = max( 1_ilp, k )
           else if ( right ) then
              ldvq = max( 1_ilp, n )
              ldaq = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( l<0_ilp .or. l>k ) then
              info = -6_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<ldvq ) then
              info = -9_ilp
           else if( ldt<nb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_stprfb( 'L', 'T', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_stprfb( 'R', 'N', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_stprfb( 'L', 'N', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_stprfb( 'R', 'T', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_stpmqrt

     pure module subroutine stdlib_dtpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! DTPMQRT applies a real orthogonal matrix Q obtained from a
     !! "triangular-pentagonal" real block reflector H to a general
     !! real matrix C, which consists of two blocks A and B.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           ! Array Arguments 
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, mb, lb, kf, ldaq, ldvq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldvq = max( 1_ilp, m )
              ldaq = max( 1_ilp, k )
           else if ( right ) then
              ldvq = max( 1_ilp, n )
              ldaq = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( l<0_ilp .or. l>k ) then
              info = -6_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<ldvq ) then
              info = -9_ilp
           else if( ldt<nb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_dtprfb( 'L', 'T', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_dtprfb( 'R', 'N', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_dtprfb( 'L', 'N', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_dtprfb( 'R', 'T', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_dtpmqrt


     pure module subroutine stdlib_ctpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! CTPMQRT applies a complex orthogonal matrix Q obtained from a
     !! "triangular-pentagonal" complex block reflector H to a general
     !! complex matrix C, which consists of two blocks A and B.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           ! Array Arguments 
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, mb, lb, kf, ldaq, ldvq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldvq = max( 1_ilp, m )
              ldaq = max( 1_ilp, k )
           else if ( right ) then
              ldvq = max( 1_ilp, n )
              ldaq = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( l<0_ilp .or. l>k ) then
              info = -6_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<ldvq ) then
              info = -9_ilp
           else if( ldt<nb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_ctprfb( 'L', 'C', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_ctprfb( 'R', 'N', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_ctprfb( 'L', 'N', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_ctprfb( 'R', 'C', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_ctpmqrt

     pure module subroutine stdlib_ztpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! ZTPMQRT applies a complex orthogonal matrix Q obtained from a
     !! "triangular-pentagonal" complex block reflector H to a general
     !! complex matrix C, which consists of two blocks A and B.
               work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           ! Array Arguments 
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, mb, lb, kf, ldaq, ldvq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldvq = max( 1_ilp, m )
              ldaq = max( 1_ilp, k )
           else if ( right ) then
              ldvq = max( 1_ilp, n )
              ldaq = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( l<0_ilp .or. l>k ) then
              info = -6_ilp
           else if( nb<1_ilp .or. (nb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<ldvq ) then
              info = -9_ilp
           else if( ldt<nb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPMQRT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. tran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_ztprfb( 'L', 'C', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              do i = 1, k, nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_ztprfb( 'R', 'N', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. notran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-m+l-i+1
                 end if
                 call stdlib_ztprfb( 'L', 'N', 'F', 'C', mb, n, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              kf = ((k-1)/nb)*nb+1
              do i = kf, 1, -nb
                 ib = min( nb, k-i+1 )
                 mb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = mb-n+l-i+1
                 end if
                 call stdlib_ztprfb( 'R', 'C', 'F', 'C', m, mb, ib, lb,v( 1_ilp, i ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_ztpmqrt




     pure module subroutine stdlib_stprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
     !! STPRFB applies a real "triangular-pentagonal" block reflector H or its
     !! conjugate transpose H^H to a real matrix C, which is composed of two
     !! blocks A and B, either from the left or right.
               lda, b, ldb, work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! ==========================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, mp, np, kp
           logical(lk) :: left, forward, column, right, backward, row
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 .or. k<=0 .or. l<0 ) return
           if( stdlib_lsame( storev, 'C' ) ) then
              column = .true.
              row = .false.
           else if ( stdlib_lsame( storev, 'R' ) ) then
              column = .false.
              row = .true.
           else
              column = .false.
              row = .false.
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              left = .true.
              right = .false.
           else if( stdlib_lsame( side, 'R' ) ) then
              left = .false.
              right = .true.
           else
              left = .false.
              right = .false.
           end if
           if( stdlib_lsame( direct, 'F' ) ) then
              forward = .true.
              backward = .false.
           else if( stdlib_lsame( direct, 'B' ) ) then
              forward = .false.
              backward = .true.
           else
              forward = .false.
              backward = .false.
           end if
       ! ---------------------------------------------------------------------------
           if( column .and. forward .and. left  ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (m-by-k)
              ! form  h c  or  h^h c  where  c = [ a ]  (k-by-n)
                                               ! [ b ]  (m-by-n)
              ! h = i - w t w^h          or  h^h = i - w t^h w^h
              ! a = a -   t (a + v^h b)  or  a = a -   t^h (a + v^h b)
              ! b = b - v t (a + v^h b)  or  b = b - v t^h (a + v^h b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'U', 'T', 'N', l, n, one, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_sgemm( 'T', 'N', l, n, m-l, one, v, ldv, b, ldb,one, work, ldwork )
                        
              call stdlib_sgemm( 'T', 'N', k-l, n, m, one, v( 1_ilp, kp ), ldv,b, ldb, zero, work( kp,&
                         1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'U', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'N', m-l, n, k, -one, v, ldv, work, ldwork,one, b, ldb )
                        
              call stdlib_sgemm( 'N', 'N', l, n, k-l, -one, v( mp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork, one, b( mp, 1_ilp ),  ldb )
              call stdlib_strmm( 'L', 'U', 'N', 'N', l, n, one, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (n-by-k)
              ! form  c h or  c h^h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w t w^h          or  h^h = i - w t^h w^h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t^h
              ! b = b - (a + b v) t v^h  or  b = b - (a + b v) t^h v^h
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_strmm( 'R', 'U', 'N', 'N', m, l, one, v( np, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_sgemm( 'N', 'N', m, l, n-l, one, b, ldb,v, ldv, one, work, ldwork )
                        
              call stdlib_sgemm( 'N', 'N', m, k-l, n, one, b, ldb,v( 1_ilp, kp ), ldv, zero, work( 1_ilp, &
                        kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'U', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'T', m, n-l, k, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_sgemm( 'N', 'T', m, l, k-l, -one, work( 1_ilp, kp ), ldwork,v( np, kp ), &
                        ldv, one, b( 1_ilp, np ), ldb )
              call stdlib_strmm( 'R', 'U', 'T', 'N', m, l, one, v( np, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (m-by-k)
                        ! [ i ]    (k-by-k)
              ! form  h c  or  h^h c  where  c = [ b ]  (m-by-n)
                                               ! [ a ]  (k-by-n)
              ! h = i - w t w^h          or  h^h = i - w t^h w^h
              ! a = a -   t (a + v^h b)  or  a = a -   t^h (a + v^h b)
              ! b = b - v t (a + v^h b)  or  b = b - v t^h (a + v^h b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'L', 'T', 'N', l, n, one, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_sgemm( 'T', 'N', l, n, m-l, one, v( mp, kp ), ldv,b( mp, 1_ilp ), ldb, one, &
                        work( kp, 1_ilp ), ldwork )
              call stdlib_sgemm( 'T', 'N', k-l, n, m, one, v, ldv,b, ldb, zero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'L', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'N', m-l, n, k, -one, v( mp, 1_ilp ), ldv,work, ldwork, one, b( &
                        mp, 1_ilp ), ldb )
              call stdlib_sgemm( 'N', 'N', l, n, k-l, -one, v, ldv,work, ldwork, one, b,  ldb )
                        
              call stdlib_strmm( 'L', 'L', 'N', 'N', l, n, one, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (n-by-k)
                        ! [ i ]    (k-by-k)
              ! form  c h  or  c h^h  where  c = [ b a ] (b is m-by-n, a is m-by-k)
              ! h = i - w t w^h          or  h^h = i - w t^h w^h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t^h
              ! b = b - (a + b v) t v^h  or  b = b - (a + b v) t^h v^h
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'L', 'N', 'N', m, l, one, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_sgemm( 'N', 'N', m, l, n-l, one, b( 1_ilp, np ), ldb,v( np, kp ), ldv, one, &
                        work( 1_ilp, kp ), ldwork )
              call stdlib_sgemm( 'N', 'N', m, k-l, n, one, b, ldb,v, ldv, zero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'L', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'T', m, n-l, k, -one, work, ldwork,v( np, 1_ilp ), ldv, one, b( &
                        1_ilp, np ), ldb )
              call stdlib_sgemm( 'N', 'T', m, l, k-l, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_strmm( 'R', 'L', 'T', 'N', m, l, one, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h^h c  where  c = [ a ]  (k-by-n)
                                               ! [ b ]  (m-by-n)
              ! h = i - w^h t w          or  h^h = i - w^h t^h w
              ! a = a -     t (a + v b)  or  a = a -     t^h (a + v b)
              ! b = b - v^h t (a + v b)  or  b = b - v^h t^h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'L', 'N', 'N', l, n, one, v( 1_ilp, mp ), ldv,work, ldb )
                        
              call stdlib_sgemm( 'N', 'N', l, n, m-l, one, v, ldv,b, ldb,one, work, ldwork )
                        
              call stdlib_sgemm( 'N', 'N', k-l, n, m, one, v( kp, 1_ilp ), ldv,b, ldb, zero, work( kp,&
                         1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'U', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', m-l, n, k, -one, v, ldv, work, ldwork,one, b, ldb )
                        
              call stdlib_sgemm( 'T', 'N', l, n, k-l, -one, v( kp, mp ), ldv,work( kp, 1_ilp ), &
                        ldwork, one, b( mp, 1_ilp ), ldb )
              call stdlib_strmm( 'L', 'L', 'T', 'N', l, n, one, v( 1_ilp, mp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h^h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w^h t w            or  h^h = i - w^h t^h w
              ! a = a - (a + b v^h) t      or  a = a - (a + b v^h) t^h
              ! b = b - (a + b v^h) t v    or  b = b - (a + b v^h) t^h v
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_strmm( 'R', 'L', 'T', 'N', m, l, one, v( 1_ilp, np ), ldv,work, ldwork )
                        
              call stdlib_sgemm( 'N', 'T', m, l, n-l, one, b, ldb, v, ldv,one, work, ldwork )
                        
              call stdlib_sgemm( 'N', 'T', m, k-l, n, one, b, ldb,v( kp, 1_ilp ), ldv, zero, work( 1_ilp, &
                        kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'U', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'N', m, n-l, k, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_sgemm( 'N', 'N', m, l, k-l, -one, work( 1_ilp, kp ), ldwork,v( kp, np ), &
                        ldv, one, b( 1_ilp, np ), ldb )
              call stdlib_strmm( 'R', 'L', 'N', 'N', m, l, one, v( 1_ilp, np ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h^h c  where  c = [ b ]  (m-by-n)
                                               ! [ a ]  (k-by-n)
              ! h = i - w^h t w          or  h^h = i - w^h t^h w
              ! a = a -     t (a + v b)  or  a = a -     t^h (a + v b)
              ! b = b - v^h t (a + v b)  or  b = b - v^h t^h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'U', 'N', 'N', l, n, one, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_sgemm( 'N', 'N', l, n, m-l, one, v( kp, mp ), ldv,b( mp, 1_ilp ), ldb, one, &
                        work( kp, 1_ilp ), ldwork )
              call stdlib_sgemm( 'N', 'N', k-l, n, m, one, v, ldv, b, ldb,zero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'L', 'L ', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'T', 'N', m-l, n, k, -one, v( 1_ilp, mp ), ldv,work, ldwork, one, b( &
                        mp, 1_ilp ), ldb )
              call stdlib_sgemm( 'T', 'N', l, n, k-l, -one, v, ldv,work, ldwork, one, b, ldb )
                        
              call stdlib_strmm( 'L', 'U', 'T', 'N', l, n, one, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h^h  where  c = [ b a ] (a is m-by-k, b is m-by-n)
              ! h = i - w^h t w            or  h^h = i - w^h t^h w
              ! a = a - (a + b v^h) t      or  a = a - (a + b v^h) t^h
              ! b = b - (a + b v^h) t v    or  b = b - (a + b v^h) t^h v
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'U', 'T', 'N', m, l, one, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_sgemm( 'N', 'T', m, l, n-l, one, b( 1_ilp, np ), ldb,v( kp, np ), ldv, one, &
                        work( 1_ilp, kp ), ldwork )
              call stdlib_sgemm( 'N', 'T', m, k-l, n, one, b, ldb, v, ldv,zero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'L', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_sgemm( 'N', 'N', m, n-l, k, -one, work, ldwork,v( 1_ilp, np ), ldv, one, b( &
                        1_ilp, np ), ldb )
              call stdlib_sgemm( 'N', 'N', m, l, k-l , -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_strmm( 'R', 'U', 'N', 'N', m, l, one, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_stprfb

     pure module subroutine stdlib_dtprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
     !! DTPRFB applies a real "triangular-pentagonal" block reflector H or its
     !! transpose H**T to a real matrix C, which is composed of two
     !! blocks A and B, either from the left or right.
               lda, b, ldb, work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! ==========================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, mp, np, kp
           logical(lk) :: left, forward, column, right, backward, row
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 .or. k<=0 .or. l<0 ) return
           if( stdlib_lsame( storev, 'C' ) ) then
              column = .true.
              row = .false.
           else if ( stdlib_lsame( storev, 'R' ) ) then
              column = .false.
              row = .true.
           else
              column = .false.
              row = .false.
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              left = .true.
              right = .false.
           else if( stdlib_lsame( side, 'R' ) ) then
              left = .false.
              right = .true.
           else
              left = .false.
              right = .false.
           end if
           if( stdlib_lsame( direct, 'F' ) ) then
              forward = .true.
              backward = .false.
           else if( stdlib_lsame( direct, 'B' ) ) then
              forward = .false.
              backward = .true.
           else
              forward = .false.
              backward = .false.
           end if
       ! ---------------------------------------------------------------------------
           if( column .and. forward .and. left  ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (m-by-k)
              ! form  h c  or  h**t c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w t w**t          or  h**t = i - w t**t w**t
              ! a = a -   t (a + v**t b)  or  a = a -   t**t (a + v**t b)
              ! b = b - v t (a + v**t b)  or  b = b - v t**t (a + v**t b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'U', 'T', 'N', l, n, one, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_dgemm( 'T', 'N', l, n, m-l, one, v, ldv, b, ldb,one, work, ldwork )
                        
              call stdlib_dgemm( 'T', 'N', k-l, n, m, one, v( 1_ilp, kp ), ldv,b, ldb, zero, work( kp,&
                         1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'U', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'N', m-l, n, k, -one, v, ldv, work, ldwork,one, b, ldb )
                        
              call stdlib_dgemm( 'N', 'N', l, n, k-l, -one, v( mp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork, one, b( mp, 1_ilp ),  ldb )
              call stdlib_dtrmm( 'L', 'U', 'N', 'N', l, n, one, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (n-by-k)
              ! form  c h or  c h**t  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w t w**t          or  h**t = i - w t**t w**t
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**t
              ! b = b - (a + b v) t v**t  or  b = b - (a + b v) t**t v**t
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', m, l, one, v( np, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_dgemm( 'N', 'N', m, l, n-l, one, b, ldb,v, ldv, one, work, ldwork )
                        
              call stdlib_dgemm( 'N', 'N', m, k-l, n, one, b, ldb,v( 1_ilp, kp ), ldv, zero, work( 1_ilp, &
                        kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'T', m, n-l, k, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_dgemm( 'N', 'T', m, l, k-l, -one, work( 1_ilp, kp ), ldwork,v( np, kp ), &
                        ldv, one, b( 1_ilp, np ), ldb )
              call stdlib_dtrmm( 'R', 'U', 'T', 'N', m, l, one, v( np, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (m-by-k)
                        ! [ i ]    (k-by-k)
              ! form  h c  or  h**t c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w t w**t          or  h**t = i - w t**t w**t
              ! a = a -   t (a + v**t b)  or  a = a -   t**t (a + v**t b)
              ! b = b - v t (a + v**t b)  or  b = b - v t**t (a + v**t b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'L', 'T', 'N', l, n, one, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_dgemm( 'T', 'N', l, n, m-l, one, v( mp, kp ), ldv,b( mp, 1_ilp ), ldb, one, &
                        work( kp, 1_ilp ), ldwork )
              call stdlib_dgemm( 'T', 'N', k-l, n, m, one, v, ldv,b, ldb, zero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'L', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'N', m-l, n, k, -one, v( mp, 1_ilp ), ldv,work, ldwork, one, b( &
                        mp, 1_ilp ), ldb )
              call stdlib_dgemm( 'N', 'N', l, n, k-l, -one, v, ldv,work, ldwork, one, b,  ldb )
                        
              call stdlib_dtrmm( 'L', 'L', 'N', 'N', l, n, one, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (n-by-k)
                        ! [ i ]    (k-by-k)
              ! form  c h  or  c h**t  where  c = [ b a ] (b is m-by-n, a is m-by-k)
              ! h = i - w t w**t          or  h**t = i - w t**t w**t
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**t
              ! b = b - (a + b v) t v**t  or  b = b - (a + b v) t**t v**t
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'L', 'N', 'N', m, l, one, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_dgemm( 'N', 'N', m, l, n-l, one, b( 1_ilp, np ), ldb,v( np, kp ), ldv, one, &
                        work( 1_ilp, kp ), ldwork )
              call stdlib_dgemm( 'N', 'N', m, k-l, n, one, b, ldb,v, ldv, zero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'L', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'T', m, n-l, k, -one, work, ldwork,v( np, 1_ilp ), ldv, one, b( &
                        1_ilp, np ), ldb )
              call stdlib_dgemm( 'N', 'T', m, l, k-l, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_dtrmm( 'R', 'L', 'T', 'N', m, l, one, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**t c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w**t t w          or  h**t = i - w**t t**t w
              ! a = a -     t (a + v b)  or  a = a -     t**t (a + v b)
              ! b = b - v**t t (a + v b)  or  b = b - v**t t**t (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'L', 'N', 'N', l, n, one, v( 1_ilp, mp ), ldv,work, ldb )
                        
              call stdlib_dgemm( 'N', 'N', l, n, m-l, one, v, ldv,b, ldb,one, work, ldwork )
                        
              call stdlib_dgemm( 'N', 'N', k-l, n, m, one, v( kp, 1_ilp ), ldv,b, ldb, zero, work( kp,&
                         1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'U', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', m-l, n, k, -one, v, ldv, work, ldwork,one, b, ldb )
                        
              call stdlib_dgemm( 'T', 'N', l, n, k-l, -one, v( kp, mp ), ldv,work( kp, 1_ilp ), &
                        ldwork, one, b( mp, 1_ilp ), ldb )
              call stdlib_dtrmm( 'L', 'L', 'T', 'N', l, n, one, v( 1_ilp, mp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**t  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w**t t w            or  h**t = i - w**t t**t w
              ! a = a - (a + b v**t) t      or  a = a - (a + b v**t) t**t
              ! b = b - (a + b v**t) t v    or  b = b - (a + b v**t) t**t v
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'L', 'T', 'N', m, l, one, v( 1_ilp, np ), ldv,work, ldwork )
                        
              call stdlib_dgemm( 'N', 'T', m, l, n-l, one, b, ldb, v, ldv,one, work, ldwork )
                        
              call stdlib_dgemm( 'N', 'T', m, k-l, n, one, b, ldb,v( kp, 1_ilp ), ldv, zero, work( 1_ilp, &
                        kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'N', m, n-l, k, -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_dgemm( 'N', 'N', m, l, k-l, -one, work( 1_ilp, kp ), ldwork,v( kp, np ), &
                        ldv, one, b( 1_ilp, np ), ldb )
              call stdlib_dtrmm( 'R', 'L', 'N', 'N', m, l, one, v( 1_ilp, np ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**t c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w**t t w          or  h**t = i - w**t t**t w
              ! a = a -     t (a + v b)  or  a = a -     t**t (a + v b)
              ! b = b - v**t t (a + v b)  or  b = b - v**t t**t (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'U', 'N', 'N', l, n, one, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_dgemm( 'N', 'N', l, n, m-l, one, v( kp, mp ), ldv,b( mp, 1_ilp ), ldb, one, &
                        work( kp, 1_ilp ), ldwork )
              call stdlib_dgemm( 'N', 'N', k-l, n, m, one, v, ldv, b, ldb,zero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'L', 'L ', trans, 'N', k, n, one, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'T', 'N', m-l, n, k, -one, v( 1_ilp, mp ), ldv,work, ldwork, one, b( &
                        mp, 1_ilp ), ldb )
              call stdlib_dgemm( 'T', 'N', l, n, k-l, -one, v, ldv,work, ldwork, one, b, ldb )
                        
              call stdlib_dtrmm( 'L', 'U', 'T', 'N', l, n, one, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**t  where  c = [ b a ] (a is m-by-k, b is m-by-n)
              ! h = i - w**t t w            or  h**t = i - w**t t**t w
              ! a = a - (a + b v**t) t      or  a = a - (a + b v**t) t**t
              ! b = b - (a + b v**t) t v    or  b = b - (a + b v**t) t**t v
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', 'T', 'N', m, l, one, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_dgemm( 'N', 'T', m, l, n-l, one, b( 1_ilp, np ), ldb,v( kp, np ), ldv, one, &
                        work( 1_ilp, kp ), ldwork )
              call stdlib_dgemm( 'N', 'T', m, k-l, n, one, b, ldb, v, ldv,zero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'L', trans, 'N', m, k, one, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_dgemm( 'N', 'N', m, n-l, k, -one, work, ldwork,v( 1_ilp, np ), ldv, one, b( &
                        1_ilp, np ), ldb )
              call stdlib_dgemm( 'N', 'N', m, l, k-l , -one, work, ldwork,v, ldv, one, b, ldb )
                        
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', m, l, one, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_dtprfb


     pure module subroutine stdlib_ctprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
     !! CTPRFB applies a complex "triangular-pentagonal" block reflector H or its
     !! conjugate transpose H**H to a complex matrix C, which is composed of two
     !! blocks A and B, either from the left or right.
               lda, b, ldb, work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! ==========================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, mp, np, kp
           logical(lk) :: left, forward, column, right, backward, row
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 .or. k<=0 .or. l<0 ) return
           if( stdlib_lsame( storev, 'C' ) ) then
              column = .true.
              row = .false.
           else if ( stdlib_lsame( storev, 'R' ) ) then
              column = .false.
              row = .true.
           else
              column = .false.
              row = .false.
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              left = .true.
              right = .false.
           else if( stdlib_lsame( side, 'R' ) ) then
              left = .false.
              right = .true.
           else
              left = .false.
              right = .false.
           end if
           if( stdlib_lsame( direct, 'F' ) ) then
              forward = .true.
              backward = .false.
           else if( stdlib_lsame( direct, 'B' ) ) then
              forward = .false.
              backward = .true.
           else
              forward = .false.
              backward = .false.
           end if
       ! ---------------------------------------------------------------------------
           if( column .and. forward .and. left  ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (m-by-k)
              ! form  h c  or  h**h c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a -   t (a + v**h b)  or  a = a -   t**h (a + v**h b)
              ! b = b - v t (a + v**h b)  or  b = b - v t**h (a + v**h b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'U', 'C', 'N', l, n, cone, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_cgemm( 'C', 'N', l, n, m-l, cone, v, ldv, b, ldb,cone, work, ldwork )
                        
              call stdlib_cgemm( 'C', 'N', k-l, n, m, cone, v( 1_ilp, kp ), ldv,b, ldb, czero, work( &
                        kp, 1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'U', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'N', m-l, n, k, -cone, v, ldv, work, ldwork,cone, b, ldb )
                        
              call stdlib_cgemm( 'N', 'N', l, n, k-l, -cone, v( mp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork, cone, b( mp, 1_ilp ),  ldb )
              call stdlib_ctrmm( 'L', 'U', 'N', 'N', l, n, cone, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (n-by-k)
              ! form  c h or  c h**h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**h
              ! b = b - (a + b v) t v**h  or  b = b - (a + b v) t**h v**h
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', m, l, cone, v( np, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_cgemm( 'N', 'N', m, l, n-l, cone, b, ldb,v, ldv, cone, work, ldwork )
                        
              call stdlib_cgemm( 'N', 'N', m, k-l, n, cone, b, ldb,v( 1_ilp, kp ), ldv, czero, work( &
                        1_ilp, kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'C', m, n-l, k, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_cgemm( 'N', 'C', m, l, k-l, -cone, work( 1_ilp, kp ), ldwork,v( np, kp ), &
                        ldv, cone, b( 1_ilp, np ), ldb )
              call stdlib_ctrmm( 'R', 'U', 'C', 'N', m, l, cone, v( np, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (m-by-k)
                        ! [ i ]    (k-by-k)
              ! form  h c  or  h**h c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a -   t (a + v**h b)  or  a = a -   t**h (a + v**h b)
              ! b = b - v t (a + v**h b)  or  b = b - v t**h (a + v**h b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'L', 'C', 'N', l, n, cone, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_cgemm( 'C', 'N', l, n, m-l, cone, v( mp, kp ), ldv,b( mp, 1_ilp ), ldb, &
                        cone, work( kp, 1_ilp ), ldwork )
              call stdlib_cgemm( 'C', 'N', k-l, n, m, cone, v, ldv,b, ldb, czero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'L', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'N', m-l, n, k, -cone, v( mp, 1_ilp ), ldv,work, ldwork, cone, &
                        b( mp, 1_ilp ), ldb )
              call stdlib_cgemm( 'N', 'N', l, n, k-l, -cone, v, ldv,work, ldwork, cone, b,  ldb )
                        
              call stdlib_ctrmm( 'L', 'L', 'N', 'N', l, n, cone, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (n-by-k)
                        ! [ i ]    (k-by-k)
              ! form  c h  or  c h**h  where  c = [ b a ] (b is m-by-n, a is m-by-k)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**h
              ! b = b - (a + b v) t v**h  or  b = b - (a + b v) t**h v**h
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'L', 'N', 'N', m, l, cone, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_cgemm( 'N', 'N', m, l, n-l, cone, b( 1_ilp, np ), ldb,v( np, kp ), ldv, &
                        cone, work( 1_ilp, kp ), ldwork )
              call stdlib_cgemm( 'N', 'N', m, k-l, n, cone, b, ldb,v, ldv, czero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'L', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'C', m, n-l, k, -cone, work, ldwork,v( np, 1_ilp ), ldv, cone, &
                        b( 1_ilp, np ), ldb )
              call stdlib_cgemm( 'N', 'C', m, l, k-l, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_ctrmm( 'R', 'L', 'C', 'N', m, l, cone, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**h c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w**h t w          or  h**h = i - w**h t**h w
              ! a = a -     t (a + v b)  or  a = a -     t**h (a + v b)
              ! b = b - v**h t (a + v b)  or  b = b - v**h t**h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'L', 'N', 'N', l, n, cone, v( 1_ilp, mp ), ldv,work, ldb )
                        
              call stdlib_cgemm( 'N', 'N', l, n, m-l, cone, v, ldv,b, ldb,cone, work, ldwork )
                        
              call stdlib_cgemm( 'N', 'N', k-l, n, m, cone, v( kp, 1_ilp ), ldv,b, ldb, czero, work( &
                        kp, 1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'U', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'C', 'N', m-l, n, k, -cone, v, ldv, work, ldwork,cone, b, ldb )
                        
              call stdlib_cgemm( 'C', 'N', l, n, k-l, -cone, v( kp, mp ), ldv,work( kp, 1_ilp ), &
                        ldwork, cone, b( mp, 1_ilp ), ldb )
              call stdlib_ctrmm( 'L', 'L', 'C', 'N', l, n, cone, v( 1_ilp, mp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w**h t w            or  h**h = i - w**h t**h w
              ! a = a - (a + b v**h) t      or  a = a - (a + b v**h) t**h
              ! b = b - (a + b v**h) t v    or  b = b - (a + b v**h) t**h v
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'L', 'C', 'N', m, l, cone, v( 1_ilp, np ), ldv,work, ldwork )
                        
              call stdlib_cgemm( 'N', 'C', m, l, n-l, cone, b, ldb, v, ldv,cone, work, ldwork )
                        
              call stdlib_cgemm( 'N', 'C', m, k-l, n, cone, b, ldb,v( kp, 1_ilp ), ldv, czero, work( &
                        1_ilp, kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'N', m, n-l, k, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_cgemm( 'N', 'N', m, l, k-l, -cone, work( 1_ilp, kp ), ldwork,v( kp, np ), &
                        ldv, cone, b( 1_ilp, np ), ldb )
              call stdlib_ctrmm( 'R', 'L', 'N', 'N', m, l, cone, v( 1_ilp, np ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**h c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w**h t w          or  h**h = i - w**h t**h w
              ! a = a -     t (a + v b)  or  a = a -     t**h (a + v b)
              ! b = b - v**h t (a + v b)  or  b = b - v**h t**h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'U', 'N', 'N', l, n, cone, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_cgemm( 'N', 'N', l, n, m-l, cone, v( kp, mp ), ldv,b( mp, 1_ilp ), ldb, &
                        cone, work( kp, 1_ilp ), ldwork )
              call stdlib_cgemm( 'N', 'N', k-l, n, m, cone, v, ldv, b, ldb,czero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'L', 'L ', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'C', 'N', m-l, n, k, -cone, v( 1_ilp, mp ), ldv,work, ldwork, cone, &
                        b( mp, 1_ilp ), ldb )
              call stdlib_cgemm( 'C', 'N', l, n, k-l, -cone, v, ldv,work, ldwork, cone, b, ldb )
                        
              call stdlib_ctrmm( 'L', 'U', 'C', 'N', l, n, cone, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**h  where  c = [ b a ] (a is m-by-k, b is m-by-n)
              ! h = i - w**h t w            or  h**h = i - w**h t**h w
              ! a = a - (a + b v**h) t      or  a = a - (a + b v**h) t**h
              ! b = b - (a + b v**h) t v    or  b = b - (a + b v**h) t**h v
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', 'C', 'N', m, l, cone, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_cgemm( 'N', 'C', m, l, n-l, cone, b( 1_ilp, np ), ldb,v( kp, np ), ldv, &
                        cone, work( 1_ilp, kp ), ldwork )
              call stdlib_cgemm( 'N', 'C', m, k-l, n, cone, b, ldb, v, ldv,czero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'L', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_cgemm( 'N', 'N', m, n-l, k, -cone, work, ldwork,v( 1_ilp, np ), ldv, cone, &
                        b( 1_ilp, np ), ldb )
              call stdlib_cgemm( 'N', 'N', m, l, k-l , -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', m, l, cone, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ctprfb

     pure module subroutine stdlib_ztprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
     !! ZTPRFB applies a complex "triangular-pentagonal" block reflector H or its
     !! conjugate transpose H**H to a complex matrix C, which is composed of two
     !! blocks A and B, either from the left or right.
               lda, b, ldb, work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! ==========================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, mp, np, kp
           logical(lk) :: left, forward, column, right, backward, row
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 .or. k<=0 .or. l<0 ) return
           if( stdlib_lsame( storev, 'C' ) ) then
              column = .true.
              row = .false.
           else if ( stdlib_lsame( storev, 'R' ) ) then
              column = .false.
              row = .true.
           else
              column = .false.
              row = .false.
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              left = .true.
              right = .false.
           else if( stdlib_lsame( side, 'R' ) ) then
              left = .false.
              right = .true.
           else
              left = .false.
              right = .false.
           end if
           if( stdlib_lsame( direct, 'F' ) ) then
              forward = .true.
              backward = .false.
           else if( stdlib_lsame( direct, 'B' ) ) then
              forward = .false.
              backward = .true.
           else
              forward = .false.
              backward = .false.
           end if
       ! ---------------------------------------------------------------------------
           if( column .and. forward .and. left  ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (m-by-k)
              ! form  h c  or  h**h c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a -   t (a + v**h b)  or  a = a -   t**h (a + v**h b)
              ! b = b - v t (a + v**h b)  or  b = b - v t**h (a + v**h b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'U', 'C', 'N', l, n, cone, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_zgemm( 'C', 'N', l, n, m-l, cone, v, ldv, b, ldb,cone, work, ldwork )
                        
              call stdlib_zgemm( 'C', 'N', k-l, n, m, cone, v( 1_ilp, kp ), ldv,b, ldb, czero, work( &
                        kp, 1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'U', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'N', m-l, n, k, -cone, v, ldv, work, ldwork,cone, b, ldb )
                        
              call stdlib_zgemm( 'N', 'N', l, n, k-l, -cone, v( mp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork, cone, b( mp, 1_ilp ),  ldb )
              call stdlib_ztrmm( 'L', 'U', 'N', 'N', l, n, cone, v( mp, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i ]    (k-by-k)
                        ! [ v ]    (n-by-k)
              ! form  c h or  c h**h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**h
              ! b = b - (a + b v) t v**h  or  b = b - (a + b v) t**h v**h
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', m, l, cone, v( np, 1_ilp ), ldv,work, ldwork )
                        
              call stdlib_zgemm( 'N', 'N', m, l, n-l, cone, b, ldb,v, ldv, cone, work, ldwork )
                        
              call stdlib_zgemm( 'N', 'N', m, k-l, n, cone, b, ldb,v( 1_ilp, kp ), ldv, czero, work( &
                        1_ilp, kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'C', m, n-l, k, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_zgemm( 'N', 'C', m, l, k-l, -cone, work( 1_ilp, kp ), ldwork,v( np, kp ), &
                        ldv, cone, b( 1_ilp, np ), ldb )
              call stdlib_ztrmm( 'R', 'U', 'C', 'N', m, l, cone, v( np, 1_ilp ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (m-by-k)
                        ! [ i ]    (k-by-k)
              ! form  h c  or  h**h c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a -   t (a + v**h b)  or  a = a -   t**h (a + v**h b)
              ! b = b - v t (a + v**h b)  or  b = b - v t**h (a + v**h b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'L', 'C', 'N', l, n, cone, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_zgemm( 'C', 'N', l, n, m-l, cone, v( mp, kp ), ldv,b( mp, 1_ilp ), ldb, &
                        cone, work( kp, 1_ilp ), ldwork )
              call stdlib_zgemm( 'C', 'N', k-l, n, m, cone, v, ldv,b, ldb, czero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'L', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'N', m-l, n, k, -cone, v( mp, 1_ilp ), ldv,work, ldwork, cone, &
                        b( mp, 1_ilp ), ldb )
              call stdlib_zgemm( 'N', 'N', l, n, k-l, -cone, v, ldv,work, ldwork, cone, b,  ldb )
                        
              call stdlib_ztrmm( 'L', 'L', 'N', 'N', l, n, cone, v( 1_ilp, kp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( column .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v ]    (n-by-k)
                        ! [ i ]    (k-by-k)
              ! form  c h  or  c h**h  where  c = [ b a ] (b is m-by-n, a is m-by-k)
              ! h = i - w t w**h          or  h**h = i - w t**h w**h
              ! a = a - (a + b v) t      or  a = a - (a + b v) t**h
              ! b = b - (a + b v) t v**h  or  b = b - (a + b v) t**h v**h
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'L', 'N', 'N', m, l, cone, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_zgemm( 'N', 'N', m, l, n-l, cone, b( 1_ilp, np ), ldb,v( np, kp ), ldv, &
                        cone, work( 1_ilp, kp ), ldwork )
              call stdlib_zgemm( 'N', 'N', m, k-l, n, cone, b, ldb,v, ldv, czero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'L', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'C', m, n-l, k, -cone, work, ldwork,v( np, 1_ilp ), ldv, cone, &
                        b( 1_ilp, np ), ldb )
              call stdlib_zgemm( 'N', 'C', m, l, k-l, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_ztrmm( 'R', 'L', 'C', 'N', m, l, cone, v( 1_ilp, kp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**h c  where  c = [ a ]  (k-by-n)
                                                ! [ b ]  (m-by-n)
              ! h = i - w**h t w          or  h**h = i - w**h t**h w
              ! a = a -     t (a + v b)  or  a = a -     t**h (a + v b)
              ! b = b - v**h t (a + v b)  or  b = b - v**h t**h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( m-l+1, m )
              kp = min( l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( i, j ) = b( m-l+i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'L', 'N', 'N', l, n, cone, v( 1_ilp, mp ), ldv,work, ldb )
                        
              call stdlib_zgemm( 'N', 'N', l, n, m-l, cone, v, ldv,b, ldb,cone, work, ldwork )
                        
              call stdlib_zgemm( 'N', 'N', k-l, n, m, cone, v( kp, 1_ilp ), ldv,b, ldb, czero, work( &
                        kp, 1_ilp ), ldwork )
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'U', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'C', 'N', m-l, n, k, -cone, v, ldv, work, ldwork,cone, b, ldb )
                        
              call stdlib_zgemm( 'C', 'N', l, n, k-l, -cone, v( kp, mp ), ldv,work( kp, 1_ilp ), &
                        ldwork, cone, b( mp, 1_ilp ), ldb )
              call stdlib_ztrmm( 'L', 'L', 'C', 'N', l, n, cone, v( 1_ilp, mp ), ldv,work, ldwork )
                        
              do j = 1, n
                 do i = 1, l
                    b( m-l+i, j ) = b( m-l+i, j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. forward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ i v ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**h  where  c = [ a b ] (a is m-by-k, b is m-by-n)
              ! h = i - w**h t w            or  h**h = i - w**h t**h w
              ! a = a - (a + b v**h) t      or  a = a - (a + b v**h) t**h
              ! b = b - (a + b v**h) t v    or  b = b - (a + b v**h) t**h v
       ! ---------------------------------------------------------------------------
              np = min( n-l+1, n )
              kp = min( l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, j ) = b( i, n-l+j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'L', 'C', 'N', m, l, cone, v( 1_ilp, np ), ldv,work, ldwork )
                        
              call stdlib_zgemm( 'N', 'C', m, l, n-l, cone, b, ldb, v, ldv,cone, work, ldwork )
                        
              call stdlib_zgemm( 'N', 'C', m, k-l, n, cone, b, ldb,v( kp, 1_ilp ), ldv, czero, work( &
                        1_ilp, kp ), ldwork )
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'N', m, n-l, k, -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_zgemm( 'N', 'N', m, l, k-l, -cone, work( 1_ilp, kp ), ldwork,v( kp, np ), &
                        ldv, cone, b( 1_ilp, np ), ldb )
              call stdlib_ztrmm( 'R', 'L', 'N', 'N', m, l, cone, v( 1_ilp, np ), ldv,work, ldwork )
                        
              do j = 1, l
                 do i = 1, m
                    b( i, n-l+j ) = b( i, n-l+j ) - work( i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. left ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-m )
              ! form  h c  or  h**h c  where  c = [ b ]  (m-by-n)
                                                ! [ a ]  (k-by-n)
              ! h = i - w**h t w          or  h**h = i - w**h t**h w
              ! a = a -     t (a + v b)  or  a = a -     t**h (a + v b)
              ! b = b - v**h t (a + v b)  or  b = b - v**h t**h (a + v b)
       ! ---------------------------------------------------------------------------
              mp = min( l+1, m )
              kp = min( k-l+1, k )
              do j = 1, n
                 do i = 1, l
                    work( k-l+i, j ) = b( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'U', 'N', 'N', l, n, cone, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              call stdlib_zgemm( 'N', 'N', l, n, m-l, cone, v( kp, mp ), ldv,b( mp, 1_ilp ), ldb, &
                        cone, work( kp, 1_ilp ), ldwork )
              call stdlib_zgemm( 'N', 'N', k-l, n, m, cone, v, ldv, b, ldb,czero, work, ldwork )
                        
              do j = 1, n
                 do i = 1, k
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'L', 'L ', trans, 'N', k, n, cone, t, ldt,work, ldwork )
              do j = 1, n
                 do i = 1, k
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'C', 'N', m-l, n, k, -cone, v( 1_ilp, mp ), ldv,work, ldwork, cone, &
                        b( mp, 1_ilp ), ldb )
              call stdlib_zgemm( 'C', 'N', l, n, k-l, -cone, v, ldv,work, ldwork, cone, b, ldb )
                        
              call stdlib_ztrmm( 'L', 'U', 'C', 'N', l, n, cone, v( kp, 1_ilp ), ldv,work( kp, 1_ilp ), &
                        ldwork )
              do j = 1, n
                 do i = 1, l
                    b( i, j ) = b( i, j ) - work( k-l+i, j )
                 end do
              end do
       ! ---------------------------------------------------------------------------
           else if( row .and. backward .and. right ) then
       ! ---------------------------------------------------------------------------
              ! let  w =  [ v i ] ( i is k-by-k, v is k-by-n )
              ! form  c h  or  c h**h  where  c = [ b a ] (a is m-by-k, b is m-by-n)
              ! h = i - w**h t w            or  h**h = i - w**h t**h w
              ! a = a - (a + b v**h) t      or  a = a - (a + b v**h) t**h
              ! b = b - (a + b v**h) t v    or  b = b - (a + b v**h) t**h v
       ! ---------------------------------------------------------------------------
              np = min( l+1, n )
              kp = min( k-l+1, k )
              do j = 1, l
                 do i = 1, m
                    work( i, k-l+j ) = b( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', 'C', 'N', m, l, cone, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              call stdlib_zgemm( 'N', 'C', m, l, n-l, cone, b( 1_ilp, np ), ldb,v( kp, np ), ldv, &
                        cone, work( 1_ilp, kp ), ldwork )
              call stdlib_zgemm( 'N', 'C', m, k-l, n, cone, b, ldb, v, ldv,czero, work, ldwork )
                        
              do j = 1, k
                 do i = 1, m
                    work( i, j ) = work( i, j ) + a( i, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'L', trans, 'N', m, k, cone, t, ldt,work, ldwork )
              do j = 1, k
                 do i = 1, m
                    a( i, j ) = a( i, j ) - work( i, j )
                 end do
              end do
              call stdlib_zgemm( 'N', 'N', m, n-l, k, -cone, work, ldwork,v( 1_ilp, np ), ldv, cone, &
                        b( 1_ilp, np ), ldb )
              call stdlib_zgemm( 'N', 'N', m, l, k-l , -cone, work, ldwork,v, ldv, cone, b, ldb )
                        
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', m, l, cone, v( kp, 1_ilp ), ldv,work( 1_ilp, kp ), &
                        ldwork )
              do j = 1, l
                 do i = 1, m
                    b( i, j ) = b( i, j ) - work( i, k-l+j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ztprfb




     pure module subroutine stdlib_sggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! SGGQRF computes a generalized QR factorization of an N-by-M matrix A
     !! and an N-by-P matrix B:
     !! A = Q*R,        B = Q*T*Z,
     !! where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
     !! matrix, and R and T assume one of the forms:
     !! if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
     !! (  0  ) N-M                         N   M-N
     !! M
     !! where R11 is upper triangular, and
     !! if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
     !! P-N  N                           ( T21 ) P
     !! P
     !! where T12 or T21 is upper triangular.
     !! In particular, if B is square and nonsingular, the GQR factorization
     !! of A and B implicitly gives the QR factorization of inv(B)*A:
     !! inv(B)*A = Z**T*(inv(T)*R)
     !! where inv(B) denotes the inverse of the matrix B, and Z**T denotes the
     !! transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, m, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', n, p, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, m, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, n, m, p ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGQRF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! qr factorization of n-by-m matrix a: a = q*r
           call stdlib_sgeqrf( n, m, a, lda, taua, work, lwork, info )
           lopt = work( 1_ilp )
           ! update b := q**t*b.
           call stdlib_sormqr( 'LEFT', 'TRANSPOSE', n, p, min( n, m ), a, lda, taua,b, ldb, work, &
                     lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! rq factorization of n-by-p matrix b: b = t*z.
           call stdlib_sgerqf( n, p, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_sggqrf

     pure module subroutine stdlib_dggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! DGGQRF computes a generalized QR factorization of an N-by-M matrix A
     !! and an N-by-P matrix B:
     !! A = Q*R,        B = Q*T*Z,
     !! where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
     !! matrix, and R and T assume one of the forms:
     !! if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
     !! (  0  ) N-M                         N   M-N
     !! M
     !! where R11 is upper triangular, and
     !! if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
     !! P-N  N                           ( T21 ) P
     !! P
     !! where T12 or T21 is upper triangular.
     !! In particular, if B is square and nonsingular, the GQR factorization
     !! of A and B implicitly gives the QR factorization of inv(B)*A:
     !! inv(B)*A = Z**T*(inv(T)*R)
     !! where inv(B) denotes the inverse of the matrix B, and Z**T denotes the
     !! transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, m, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', n, p, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, m, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, n, m, p ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGQRF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! qr factorization of n-by-m matrix a: a = q*r
           call stdlib_dgeqrf( n, m, a, lda, taua, work, lwork, info )
           lopt = work( 1_ilp )
           ! update b := q**t*b.
           call stdlib_dormqr( 'LEFT', 'TRANSPOSE', n, p, min( n, m ), a, lda, taua,b, ldb, work, &
                     lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! rq factorization of n-by-p matrix b: b = t*z.
           call stdlib_dgerqf( n, p, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_dggqrf


     pure module subroutine stdlib_cggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! CGGQRF computes a generalized QR factorization of an N-by-M matrix A
     !! and an N-by-P matrix B:
     !! A = Q*R,        B = Q*T*Z,
     !! where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix,
     !! and R and T assume one of the forms:
     !! if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
     !! (  0  ) N-M                         N   M-N
     !! M
     !! where R11 is upper triangular, and
     !! if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
     !! P-N  N                           ( T21 ) P
     !! P
     !! where T12 or T21 is upper triangular.
     !! In particular, if B is square and nonsingular, the GQR factorization
     !! of A and B implicitly gives the QR factorization of inv(B)*A:
     !! inv(B)*A = Z**H * (inv(T)*R)
     !! where inv(B) denotes the inverse of the matrix B, and Z' denotes the
     !! conjugate transpose of matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, m, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', n, p, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, m, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p)*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, n, m, p ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGQRF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! qr factorization of n-by-m matrix a: a = q*r
           call stdlib_cgeqrf( n, m, a, lda, taua, work, lwork, info )
           lopt = real( work( 1_ilp ),KIND=sp)
           ! update b := q**h*b.
           call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', n, p, min( n, m ), a,lda, taua, b, &
                     ldb, work, lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! rq factorization of n-by-p matrix b: b = t*z.
           call stdlib_cgerqf( n, p, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_cggqrf

     pure module subroutine stdlib_zggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! ZGGQRF computes a generalized QR factorization of an N-by-M matrix A
     !! and an N-by-P matrix B:
     !! A = Q*R,        B = Q*T*Z,
     !! where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix,
     !! and R and T assume one of the forms:
     !! if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
     !! (  0  ) N-M                         N   M-N
     !! M
     !! where R11 is upper triangular, and
     !! if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
     !! P-N  N                           ( T21 ) P
     !! P
     !! where T12 or T21 is upper triangular.
     !! In particular, if B is square and nonsingular, the GQR factorization
     !! of A and B implicitly gives the QR factorization of inv(B)*A:
     !! inv(B)*A = Z**H * (inv(T)*R)
     !! where inv(B) denotes the inverse of the matrix B, and Z**H denotes the
     !! conjugate transpose of matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, m, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', n, p, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, m, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, n, m, p ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGQRF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! qr factorization of n-by-m matrix a: a = q*r
           call stdlib_zgeqrf( n, m, a, lda, taua, work, lwork, info )
           lopt = real( work( 1_ilp ),KIND=dp)
           ! update b := q**h*b.
           call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', n, p, min( n, m ), a,lda, taua, b, &
                     ldb, work, lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! rq factorization of n-by-p matrix b: b = t*z.
           call stdlib_zgerqf( n, p, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_zggqrf




     pure module subroutine stdlib_sgerqf( m, n, a, lda, tau, work, lwork, info )
     !! SGERQF computes an RQ factorization of a real M-by-N matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ki, kk, ldwork, lwkopt, mu, nb, nbmin, nu, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              k = min( m, n )
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if ( .not.lquery ) then
                 if( lwork<=0_ilp .or. ( n>0_ilp .and. lwork<max( 1_ilp, m ) ) )info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGERQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGERQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the rq factorization of the current block
                 ! a(m-k+i:m-k+i+ib-1,1:n-k+i+ib-1)
                 call stdlib_sgerq2( ib, n-k+i+ib-1, a( m-k+i, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 if( m-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_slarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_slarfb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', m-k+i-1, n-&
                    k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_sgerq2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgerqf

     pure module subroutine stdlib_dgerqf( m, n, a, lda, tau, work, lwork, info )
     !! DGERQF computes an RQ factorization of a real M-by-N matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ki, kk, ldwork, lwkopt, mu, nb, nbmin, nu, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              k = min( m, n )
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if ( .not.lquery ) then
                 if( lwork<=0_ilp .or. ( n>0_ilp .and. lwork<max( 1_ilp, m ) ) )info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGERQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGERQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the rq factorization of the current block
                 ! a(m-k+i:m-k+i+ib-1,1:n-k+i+ib-1)
                 call stdlib_dgerq2( ib, n-k+i+ib-1, a( m-k+i, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 if( m-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_dlarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_dlarfb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', m-k+i-1, n-&
                    k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_dgerq2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgerqf


     pure module subroutine stdlib_cgerqf( m, n, a, lda, tau, work, lwork, info )
     !! CGERQF computes an RQ factorization of a complex M-by-N matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ki, kk, ldwork, lwkopt, mu, nb, nbmin, nu, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              k = min( m, n )
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if ( .not.lquery ) then
                 if( lwork<=0_ilp .or. ( n>0_ilp .and. lwork<max( 1_ilp, m ) ) )info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGERQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGERQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the rq factorization of the current block
                 ! a(m-k+i:m-k+i+ib-1,1:n-k+i+ib-1)
                 call stdlib_cgerq2( ib, n-k+i+ib-1, a( m-k+i, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 if( m-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_clarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_clarfb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', m-k+i-1, n-&
                    k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_cgerq2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgerqf

     pure module subroutine stdlib_zgerqf( m, n, a, lda, tau, work, lwork, info )
     !! ZGERQF computes an RQ factorization of a complex M-by-N matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, iinfo, iws, k, ki, kk, ldwork, lwkopt, mu, nb, nbmin, nu, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              k = min( m, n )
              if( k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if ( .not.lquery ) then
                 if( lwork<=0_ilp .or. ( n>0_ilp .and. lwork<max( 1_ilp, m ) ) )info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGERQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( k==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGERQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the rq factorization of the current block
                 ! a(m-k+i:m-k+i+ib-1,1:n-k+i+ib-1)
                 call stdlib_zgerq2( ib, n-k+i+ib-1, a( m-k+i, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 if( m-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_zlarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_zlarfb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', m-k+i-1, n-&
                    k+i+ib-1, ib,a( m-k+i, 1_ilp ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_zgerq2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgerqf




     pure module subroutine stdlib_sgerq2( m, n, a, lda, tau, work, info )
     !! SGERQ2 computes an RQ factorization of a real m by n matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGERQ2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(m-k+i,1:n-k+i-1)
              call stdlib_slarfg( n-k+i, a( m-k+i, n-k+i ), a( m-k+i, 1_ilp ), lda,tau( i ) )
              ! apply h(i) to a(1:m-k+i-1,1:n-k+i) from the right
              aii = a( m-k+i, n-k+i )
              a( m-k+i, n-k+i ) = one
              call stdlib_slarf( 'RIGHT', m-k+i-1, n-k+i, a( m-k+i, 1_ilp ), lda,tau( i ), a, lda, &
                        work )
              a( m-k+i, n-k+i ) = aii
           end do
           return
     end subroutine stdlib_sgerq2

     pure module subroutine stdlib_dgerq2( m, n, a, lda, tau, work, info )
     !! DGERQ2 computes an RQ factorization of a real m by n matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGERQ2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(m-k+i,1:n-k+i-1)
              call stdlib_dlarfg( n-k+i, a( m-k+i, n-k+i ), a( m-k+i, 1_ilp ), lda,tau( i ) )
              ! apply h(i) to a(1:m-k+i-1,1:n-k+i) from the right
              aii = a( m-k+i, n-k+i )
              a( m-k+i, n-k+i ) = one
              call stdlib_dlarf( 'RIGHT', m-k+i-1, n-k+i, a( m-k+i, 1_ilp ), lda,tau( i ), a, lda, &
                        work )
              a( m-k+i, n-k+i ) = aii
           end do
           return
     end subroutine stdlib_dgerq2


     pure module subroutine stdlib_cgerq2( m, n, a, lda, tau, work, info )
     !! CGERQ2 computes an RQ factorization of a complex m by n matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGERQ2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(m-k+i,1:n-k+i-1)
              call stdlib_clacgv( n-k+i, a( m-k+i, 1_ilp ), lda )
              alpha = a( m-k+i, n-k+i )
              call stdlib_clarfg( n-k+i, alpha, a( m-k+i, 1_ilp ), lda,tau( i ) )
              ! apply h(i) to a(1:m-k+i-1,1:n-k+i) from the right
              a( m-k+i, n-k+i ) = cone
              call stdlib_clarf( 'RIGHT', m-k+i-1, n-k+i, a( m-k+i, 1_ilp ), lda,tau( i ), a, lda, &
                        work )
              a( m-k+i, n-k+i ) = alpha
              call stdlib_clacgv( n-k+i-1, a( m-k+i, 1_ilp ), lda )
           end do
           return
     end subroutine stdlib_cgerq2

     pure module subroutine stdlib_zgerq2( m, n, a, lda, tau, work, info )
     !! ZGERQ2 computes an RQ factorization of a complex m by n matrix A:
     !! A = R * Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, k
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGERQ2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(m-k+i,1:n-k+i-1)
              call stdlib_zlacgv( n-k+i, a( m-k+i, 1_ilp ), lda )
              alpha = a( m-k+i, n-k+i )
              call stdlib_zlarfg( n-k+i, alpha, a( m-k+i, 1_ilp ), lda, tau( i ) )
              ! apply h(i) to a(1:m-k+i-1,1:n-k+i) from the right
              a( m-k+i, n-k+i ) = cone
              call stdlib_zlarf( 'RIGHT', m-k+i-1, n-k+i, a( m-k+i, 1_ilp ), lda,tau( i ), a, lda, &
                        work )
              a( m-k+i, n-k+i ) = alpha
              call stdlib_zlacgv( n-k+i-1, a( m-k+i, 1_ilp ), lda )
           end do
           return
     end subroutine stdlib_zgerq2




     pure module subroutine stdlib_cungrq( m, n, k, a, lda, tau, work, lwork, info )
     !! CUNGRQ generates an M-by-N complex matrix Q with orthonormal rows,
     !! which is defined as the last M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(1)**H H(2)**H . . . H(k)**H
     !! as returned by CGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, ii, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CUNGRQ', ' ', m, n, k, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CUNGRQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNGRQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk rows are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(1:m-kk,n-kk+1:n) to czero.
              do j = n - kk + 1, n
                 do i = 1, m - kk
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_cungr2( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 ii = m - k + i
                 if( ii>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_clarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( ii, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h**h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_clarfb( 'RIGHT', 'CONJUGATE TRANSPOSE', 'BACKWARD','ROWWISE', ii-&
                    1_ilp, n-k+i+ib-1, ib, a( ii, 1_ilp ),lda, work, ldwork, a, lda, work( ib+1 ),ldwork )
                              
                 end if
                 ! apply h**h to columns 1:n-k+i+ib-1 of current block
                 call stdlib_cungr2( ib, n-k+i+ib-1, ib, a( ii, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 ! set columns n-k+i+ib:n of current block to czero
                 do l = n - k + i + ib, n
                    do j = ii, ii + ib - 1
                       a( j, l ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cungrq

     pure module subroutine stdlib_zungrq( m, n, k, a, lda, tau, work, lwork, info )
     !! ZUNGRQ generates an M-by-N complex matrix Q with orthonormal rows,
     !! which is defined as the last M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(1)**H H(2)**H . . . H(k)**H
     !! as returned by ZGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, ii, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNGRQ', ' ', m, n, k, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZUNGRQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNGRQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk rows are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(1:m-kk,n-kk+1:n) to czero.
              do j = n - kk + 1, n
                 do i = 1, m - kk
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_zungr2( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 ii = m - k + i
                 if( ii>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_zlarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( ii, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h**h to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_zlarfb( 'RIGHT', 'CONJUGATE TRANSPOSE', 'BACKWARD','ROWWISE', ii-&
                    1_ilp, n-k+i+ib-1, ib, a( ii, 1_ilp ),lda, work, ldwork, a, lda, work( ib+1 ),ldwork )
                              
                 end if
                 ! apply h**h to columns 1:n-k+i+ib-1 of current block
                 call stdlib_zungr2( ib, n-k+i+ib-1, ib, a( ii, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 ! set columns n-k+i+ib:n of current block to czero
                 do l = n - k + i + ib, n
                    do j = ii, ii + ib - 1
                       a( j, l ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zungrq




     pure module subroutine stdlib_cunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! CUNMRQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1)**H H(2)**H . . . H(k)**H
     !! as returned by CGERQF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp) :: i, i1, i2, i3, ib, iinfo, iwt, ldwork, lwkopt, mi, nb, nbmin, ni, nq, &
                     nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              if( m==0_ilp .or. n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CUNMRQ', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNMRQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_cunmr2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_clarft( 'BACKWARD', 'ROWWISE', nq-k+i+ib-1, ib,a( i, 1_ilp ), lda, tau( &
                           i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**h is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**h
                 call stdlib_clarfb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, a( i, 1_ilp ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmrq

     pure module subroutine stdlib_zunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! ZUNMRQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1)**H H(2)**H . . . H(k)**H
     !! as returned by ZGERQF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp) :: i, i1, i2, i3, ib, iinfo, iwt, ldwork, lwkopt, mi, nb, nbmin, ni, nq, &
                     nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              if( m==0_ilp .or. n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZUNMRQ', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNMRQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_zunmr2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_zlarft( 'BACKWARD', 'ROWWISE', nq-k+i+ib-1, ib,a( i, 1_ilp ), lda, tau( &
                           i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**h is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**h
                 call stdlib_zlarfb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, a( i, 1_ilp ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmrq




     pure module subroutine stdlib_cunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! CUNMR2 overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1)**H H(2)**H . . . H(k)**H
     !! as returned by CGERQF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, mi, ni, nq
           complex(sp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMR2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
           else
              mi = m
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(1:m-k+i,1:n)
                 mi = m - k + i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,1:n-k+i)
                 ni = n - k + i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = conjg( tau( i ) )
              else
                 taui = tau( i )
              end if
              call stdlib_clacgv( nq-k+i-1, a( i, 1_ilp ), lda )
              aii = a( i, nq-k+i )
              a( i, nq-k+i ) = cone
              call stdlib_clarf( side, mi, ni, a( i, 1_ilp ), lda, taui, c, ldc, work )
              a( i, nq-k+i ) = aii
              call stdlib_clacgv( nq-k+i-1, a( i, 1_ilp ), lda )
           end do
           return
     end subroutine stdlib_cunmr2

     pure module subroutine stdlib_zunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! ZUNMR2 overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1)**H H(2)**H . . . H(k)**H
     !! as returned by ZGERQF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, mi, ni, nq
           complex(dp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMR2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
           else
              mi = m
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(1:m-k+i,1:n)
                 mi = m - k + i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,1:n-k+i)
                 ni = n - k + i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = conjg( tau( i ) )
              else
                 taui = tau( i )
              end if
              call stdlib_zlacgv( nq-k+i-1, a( i, 1_ilp ), lda )
              aii = a( i, nq-k+i )
              a( i, nq-k+i ) = cone
              call stdlib_zlarf( side, mi, ni, a( i, 1_ilp ), lda, taui, c, ldc, work )
              a( i, nq-k+i ) = aii
              call stdlib_zlacgv( nq-k+i-1, a( i, 1_ilp ), lda )
           end do
           return
     end subroutine stdlib_zunmr2




     pure module subroutine stdlib_cungr2( m, n, k, a, lda, tau, work, info )
     !! CUNGR2 generates an m by n complex matrix Q with orthonormal rows,
     !! which is defined as the last m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(1)**H H(2)**H . . . H(k)**H
     !! as returned by CGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGR2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows 1:m-k to rows of the unit matrix
              do j = 1, n
                 do l = 1, m - k
                    a( l, j ) = czero
                 end do
                 if( j>n-m .and. j<=n-k )a( m-n+j, j ) = cone
              end do
           end if
           do i = 1, k
              ii = m - k + i
              ! apply h(i)**h to a(1:m-k+i,1:n-k+i) from the right
              call stdlib_clacgv( n-m+ii-1, a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = cone
              call stdlib_clarf( 'RIGHT', ii-1, n-m+ii, a( ii, 1_ilp ), lda,conjg( tau( i ) ), a, lda,&
                         work )
              call stdlib_cscal( n-m+ii-1, -tau( i ), a( ii, 1_ilp ), lda )
              call stdlib_clacgv( n-m+ii-1, a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = cone - conjg( tau( i ) )
              ! set a(m-k+i,n-k+i+1:n) to czero
              do l = n - m + ii + 1, n
                 a( ii, l ) = czero
              end do
           end do
           return
     end subroutine stdlib_cungr2

     pure module subroutine stdlib_zungr2( m, n, k, a, lda, tau, work, info )
     !! ZUNGR2 generates an m by n complex matrix Q with orthonormal rows,
     !! which is defined as the last m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(1)**H H(2)**H . . . H(k)**H
     !! as returned by ZGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGR2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows 1:m-k to rows of the unit matrix
              do j = 1, n
                 do l = 1, m - k
                    a( l, j ) = czero
                 end do
                 if( j>n-m .and. j<=n-k )a( m-n+j, j ) = cone
              end do
           end if
           do i = 1, k
              ii = m - k + i
              ! apply h(i)**h to a(1:m-k+i,1:n-k+i) from the right
              call stdlib_zlacgv( n-m+ii-1, a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = cone
              call stdlib_zlarf( 'RIGHT', ii-1, n-m+ii, a( ii, 1_ilp ), lda,conjg( tau( i ) ), a, lda,&
                         work )
              call stdlib_zscal( n-m+ii-1, -tau( i ), a( ii, 1_ilp ), lda )
              call stdlib_zlacgv( n-m+ii-1, a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = cone - conjg( tau( i ) )
              ! set a(m-k+i,n-k+i+1:n) to czero
              do l = n - m + ii + 1, n
                 a( ii, l ) = czero
              end do
           end do
           return
     end subroutine stdlib_zungr2




     pure module subroutine stdlib_sorgrq( m, n, k, a, lda, tau, work, lwork, info )
     !! SORGRQ generates an M-by-N real matrix Q with orthonormal rows,
     !! which is defined as the last M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by SGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, ii, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'SORGRQ', ' ', m, n, k, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SORGRQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORGRQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk rows are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(1:m-kk,n-kk+1:n) to zero.
              do j = n - kk + 1, n
                 do i = 1, m - kk
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_sorgr2( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 ii = m - k + i
                 if( ii>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_slarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( ii, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h**t to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_slarfb( 'RIGHT', 'TRANSPOSE', 'BACKWARD', 'ROWWISE',ii-1, n-k+i+&
                              ib-1, ib, a( ii, 1_ilp ), lda, work,ldwork, a, lda, work( ib+1 ), ldwork )
                 end if
                 ! apply h**t to columns 1:n-k+i+ib-1 of current block
                 call stdlib_sorgr2( ib, n-k+i+ib-1, ib, a( ii, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 ! set columns n-k+i+ib:n of current block to zero
                 do l = n - k + i + ib, n
                    do j = ii, ii + ib - 1
                       a( j, l ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sorgrq

     pure module subroutine stdlib_dorgrq( m, n, k, a, lda, tau, work, lwork, info )
     !! DORGRQ generates an M-by-N real matrix Q with orthonormal rows,
     !! which is defined as the last M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by DGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, ib, ii, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( m<=0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DORGRQ', ' ', m, n, k, -1_ilp )
                 lwkopt = m*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DORGRQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORGRQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk rows are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(1:m-kk,n-kk+1:n) to zero.
              do j = n - kk + 1, n
                 do i = 1, m - kk
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_dorgr2( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 ii = m - k + i
                 if( ii>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_dlarft( 'BACKWARD', 'ROWWISE', n-k+i+ib-1, ib,a( ii, 1_ilp ), lda, &
                              tau( i ), work, ldwork )
                    ! apply h**t to a(1:m-k+i-1,1:n-k+i+ib-1) from the right
                    call stdlib_dlarfb( 'RIGHT', 'TRANSPOSE', 'BACKWARD', 'ROWWISE',ii-1, n-k+i+&
                              ib-1, ib, a( ii, 1_ilp ), lda, work,ldwork, a, lda, work( ib+1 ), ldwork )
                 end if
                 ! apply h**t to columns 1:n-k+i+ib-1 of current block
                 call stdlib_dorgr2( ib, n-k+i+ib-1, ib, a( ii, 1_ilp ), lda, tau( i ),work, iinfo )
                           
                 ! set columns n-k+i+ib:n of current block to zero
                 do l = n - k + i + ib, n
                    do j = ii, ii + ib - 1
                       a( j, l ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dorgrq




     pure module subroutine stdlib_sormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! SORMRQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by SGERQF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp) :: i, i1, i2, i3, ib, iinfo, iwt, ldwork, lwkopt, mi, nb, nbmin, ni, nq, &
                     nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
           ! compute the workspace requirements
              if( m==0_ilp .or. n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SORMRQ', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORMRQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_sormr2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_slarft( 'BACKWARD', 'ROWWISE', nq-k+i+ib-1, ib,a( i, 1_ilp ), lda, tau( &
                           i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**t is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**t
                 call stdlib_slarfb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, a( i, 1_ilp ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormrq

     pure module subroutine stdlib_dormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! DORMRQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DGERQF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldt = nbmax+1
           integer(ilp), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp) :: i, i1, i2, i3, ib, iinfo, iwt, ldwork, lwkopt, mi, nb, nbmin, ni, nq, &
                     nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
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
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              if( m==0_ilp .or. n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DORMRQ', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMRQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORMRQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_dormr2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp
                 i2 = 1_ilp
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_dlarft( 'BACKWARD', 'ROWWISE', nq-k+i+ib-1, ib,a( i, 1_ilp ), lda, tau( &
                           i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**t is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**t
                 call stdlib_dlarfb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, a( i, 1_ilp ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormrq




     pure module subroutine stdlib_sormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! SORMR2 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by SGERQF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, mi, ni, nq
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMR2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran ) .or. ( .not.left .and. notran ) )then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
           else
              mi = m
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) is applied to c(1:m-k+i,1:n)
                 mi = m - k + i
              else
                 ! h(i) is applied to c(1:m,1:n-k+i)
                 ni = n - k + i
              end if
              ! apply h(i)
              aii = a( i, nq-k+i )
              a( i, nq-k+i ) = one
              call stdlib_slarf( side, mi, ni, a( i, 1_ilp ), lda, tau( i ), c, ldc,work )
              a( i, nq-k+i ) = aii
           end do
           return
     end subroutine stdlib_sormr2

     pure module subroutine stdlib_dormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! DORMR2 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DGERQF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, mi, ni, nq
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp .or. k>nq ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMR2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran ) .or. ( .not.left .and. notran ) )then
              i1 = 1_ilp
              i2 = k
              i3 = 1_ilp
           else
              i1 = k
              i2 = 1_ilp
              i3 = -1_ilp
           end if
           if( left ) then
              ni = n
           else
              mi = m
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) is applied to c(1:m-k+i,1:n)
                 mi = m - k + i
              else
                 ! h(i) is applied to c(1:m,1:n-k+i)
                 ni = n - k + i
              end if
              ! apply h(i)
              aii = a( i, nq-k+i )
              a( i, nq-k+i ) = one
              call stdlib_dlarf( side, mi, ni, a( i, 1_ilp ), lda, tau( i ), c, ldc,work )
              a( i, nq-k+i ) = aii
           end do
           return
     end subroutine stdlib_dormr2




     pure module subroutine stdlib_sorgr2( m, n, k, a, lda, tau, work, info )
     !! SORGR2 generates an m by n real matrix Q with orthonormal rows,
     !! which is defined as the last m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by SGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGR2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows 1:m-k to rows of the unit matrix
              do j = 1, n
                 do l = 1, m - k
                    a( l, j ) = zero
                 end do
                 if( j>n-m .and. j<=n-k )a( m-n+j, j ) = one
              end do
           end if
           do i = 1, k
              ii = m - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the right
              a( ii, n-m+ii ) = one
              call stdlib_slarf( 'RIGHT', ii-1, n-m+ii, a( ii, 1_ilp ), lda, tau( i ),a, lda, work )
                        
              call stdlib_sscal( n-m+ii-1, -tau( i ), a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = one - tau( i )
              ! set a(m-k+i,n-k+i+1:n) to zero
              do l = n - m + ii + 1, n
                 a( ii, l ) = zero
              end do
           end do
           return
     end subroutine stdlib_sorgr2

     pure module subroutine stdlib_dorgr2( m, n, k, a, lda, tau, work, info )
     !! DORGR2 generates an m by n real matrix Q with orthonormal rows,
     !! which is defined as the last m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(1) H(2) . . . H(k)
     !! as returned by DGERQF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ii, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGR2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows 1:m-k to rows of the unit matrix
              do j = 1, n
                 do l = 1, m - k
                    a( l, j ) = zero
                 end do
                 if( j>n-m .and. j<=n-k )a( m-n+j, j ) = one
              end do
           end if
           do i = 1, k
              ii = m - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the right
              a( ii, n-m+ii ) = one
              call stdlib_dlarf( 'RIGHT', ii-1, n-m+ii, a( ii, 1_ilp ), lda, tau( i ),a, lda, work )
                        
              call stdlib_dscal( n-m+ii-1, -tau( i ), a( ii, 1_ilp ), lda )
              a( ii, n-m+ii ) = one - tau( i )
              ! set a(m-k+i,n-k+i+1:n) to zero
              do l = n - m + ii + 1, n
                 a( ii, l ) = zero
              end do
           end do
           return
     end subroutine stdlib_dorgr2




     pure module subroutine stdlib_sggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! SGGRQF computes a generalized RQ factorization of an M-by-N matrix A
     !! and a P-by-N matrix B:
     !! A = R*Q,        B = Z*T*Q,
     !! where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
     !! matrix, and R and T assume one of the forms:
     !! if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
     !! N-M  M                           ( R21 ) N
     !! N
     !! where R12 or R21 is upper triangular, and
     !! if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
     !! (  0  ) P-N                         P   N-P
     !! N
     !! where T11 is upper triangular.
     !! In particular, if B is square and nonsingular, the GRQ factorization
     !! of A and B implicitly gives the RQ factorization of A*inv(B):
     !! A*inv(B) = (R*inv(T))*Z**T
     !! where inv(B) denotes the inverse of the matrix B, and Z**T denotes the
     !! transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', p, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'SORMRQ', ' ', m, n, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p)*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( p<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, m, p, n ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGRQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! rq factorization of m-by-n matrix a: a = r*q
           call stdlib_sgerqf( m, n, a, lda, taua, work, lwork, info )
           lopt = work( 1_ilp )
           ! update b := b*q**t
           call stdlib_sormrq( 'RIGHT', 'TRANSPOSE', p, n, min( m, n ),a( max( 1_ilp, m-n+1 ), 1_ilp ), &
                     lda, taua, b, ldb, work,lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! qr factorization of p-by-n matrix b: b = z*t
           call stdlib_sgeqrf( p, n, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_sggrqf

     pure module subroutine stdlib_dggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
     !! and a P-by-N matrix B:
     !! A = R*Q,        B = Z*T*Q,
     !! where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
     !! matrix, and R and T assume one of the forms:
     !! if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
     !! N-M  M                           ( R21 ) N
     !! N
     !! where R12 or R21 is upper triangular, and
     !! if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
     !! (  0  ) P-N                         P   N-P
     !! N
     !! where T11 is upper triangular.
     !! In particular, if B is square and nonsingular, the GRQ factorization
     !! of A and B implicitly gives the RQ factorization of A*inv(B):
     !! A*inv(B) = (R*inv(T))*Z**T
     !! where inv(B) denotes the inverse of the matrix B, and Z**T denotes the
     !! transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', p, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'DORMRQ', ' ', m, n, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( p<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, m, p, n ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGRQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! rq factorization of m-by-n matrix a: a = r*q
           call stdlib_dgerqf( m, n, a, lda, taua, work, lwork, info )
           lopt = work( 1_ilp )
           ! update b := b*q**t
           call stdlib_dormrq( 'RIGHT', 'TRANSPOSE', p, n, min( m, n ),a( max( 1_ilp, m-n+1 ), 1_ilp ), &
                     lda, taua, b, ldb, work,lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! qr factorization of p-by-n matrix b: b = z*t
           call stdlib_dgeqrf( p, n, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_dggrqf


     pure module subroutine stdlib_cggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! CGGRQF computes a generalized RQ factorization of an M-by-N matrix A
     !! and a P-by-N matrix B:
     !! A = R*Q,        B = Z*T*Q,
     !! where Q is an N-by-N unitary matrix, Z is a P-by-P unitary
     !! matrix, and R and T assume one of the forms:
     !! if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
     !! N-M  M                           ( R21 ) N
     !! N
     !! where R12 or R21 is upper triangular, and
     !! if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
     !! (  0  ) P-N                         P   N-P
     !! N
     !! where T11 is upper triangular.
     !! In particular, if B is square and nonsingular, the GRQ factorization
     !! of A and B implicitly gives the RQ factorization of A*inv(B):
     !! A*inv(B) = (R*inv(T))*Z**H
     !! where inv(B) denotes the inverse of the matrix B, and Z**H denotes the
     !! conjugate transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', p, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'CUNMRQ', ' ', m, n, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p)*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( p<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, m, p, n ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGRQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! rq factorization of m-by-n matrix a: a = r*q
           call stdlib_cgerqf( m, n, a, lda, taua, work, lwork, info )
           lopt = real( work( 1_ilp ),KIND=sp)
           ! update b := b*q**h
           call stdlib_cunmrq( 'RIGHT', 'CONJUGATE TRANSPOSE', p, n, min( m, n ),a( max( 1_ilp, m-n+1 &
                     ), 1_ilp ), lda, taua, b, ldb, work,lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! qr factorization of p-by-n matrix b: b = z*t
           call stdlib_cgeqrf( p, n, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_cggrqf

     pure module subroutine stdlib_zggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
     !! ZGGRQF computes a generalized RQ factorization of an M-by-N matrix A
     !! and a P-by-N matrix B:
     !! A = R*Q,        B = Z*T*Q,
     !! where Q is an N-by-N unitary matrix, Z is a P-by-P unitary
     !! matrix, and R and T assume one of the forms:
     !! if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
     !! N-M  M                           ( R21 ) N
     !! N
     !! where R12 or R21 is upper triangular, and
     !! if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
     !! (  0  ) P-N                         P   N-P
     !! N
     !! where T11 is upper triangular.
     !! In particular, if B is square and nonsingular, the GRQ factorization
     !! of A and B implicitly gives the RQ factorization of A*inv(B):
     !! A*inv(B) = (R*inv(T))*Z**H
     !! where inv(B) denotes the inverse of the matrix B, and Z**H denotes the
     !! conjugate transpose of the matrix Z.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkopt, nb, nb1, nb2, nb3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb1 = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp )
           nb2 = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', p, n, -1_ilp, -1_ilp )
           nb3 = stdlib_ilaenv( 1_ilp, 'ZUNMRQ', ' ', m, n, p, -1_ilp )
           nb = max( nb1, nb2, nb3 )
           lwkopt = max( n, m, p )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( p<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, m, p, n ) .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGRQF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! rq factorization of m-by-n matrix a: a = r*q
           call stdlib_zgerqf( m, n, a, lda, taua, work, lwork, info )
           lopt = real( work( 1_ilp ),KIND=dp)
           ! update b := b*q**h
           call stdlib_zunmrq( 'RIGHT', 'CONJUGATE TRANSPOSE', p, n, min( m, n ),a( max( 1_ilp, m-n+1 &
                     ), 1_ilp ), lda, taua, b, ldb, work,lwork, info )
           lopt = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           ! qr factorization of p-by-n matrix b: b = z*t
           call stdlib_zgeqrf( p, n, b, ldb, taub, work, lwork, info )
           work( 1_ilp ) = max( lopt, int( work( 1_ilp ),KIND=ilp) )
           return
     end subroutine stdlib_zggrqf



end submodule stdlib_lapack_orthogonal_factors_qr
