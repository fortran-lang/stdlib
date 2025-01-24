submodule(stdlib_lapack_orthogonal_factors) stdlib_lapack_orthogonal_factors_ql
  implicit none


  contains

     pure module subroutine stdlib_sgelq( m, n, a, lda, t, tsize, work, lwork,info )
     !! SGELQ computes an LQ factorization of a real M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           integer(ilp) :: mb, nb, mintsz, nblcks, lwmin, lwopt, lwreq
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
             mb = stdlib_ilaenv( 1_ilp, 'SGELQ ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'SGELQ ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = 1_ilp
             nb = n
           end if
           if( mb>min( m, n ) .or. mb<1_ilp ) mb = 1_ilp
           if( nb>n .or. nb<=m ) nb = n
           mintsz = m + 5_ilp
           if ( nb>m .and. n>m ) then
             if( mod( n - m, nb - m )==0_ilp ) then
               nblcks = ( n - m ) / ( nb - m )
             else
               nblcks = ( n - m ) / ( nb - m ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwmin = max( 1_ilp, n )
              lwopt = max( 1_ilp, mb*n )
           else
              lwmin = max( 1_ilp, m )
              lwopt = max( 1_ilp, mb*m )
           end if
           lminws = .false.
           if( ( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) .or. lwork<lwopt ).and. ( lwork>=lwmin ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) ) then
               lminws = .true.
               mb = 1_ilp
               nb = n
             end if
             if( lwork<lwopt ) then
               lminws = .true.
               mb = 1_ilp
             end if
           end if
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwreq = max( 1_ilp, mb*n )
           else
              lwreq = max( 1_ilp, mb*m )
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<lwreq ) .and.( .not.lquery ).and. ( .not.lminws ) ) then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = mb*m*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = lwmin
             else
               work( 1_ilp ) = lwreq
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SGELQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the lq decomposition
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
             call stdlib_sgelqt( m, n, mb, a, lda, t( 6_ilp ), mb, work, info )
           else
             call stdlib_slaswlq( m, n, mb, nb, a, lda, t( 6_ilp ), mb, work,lwork, info )
           end if
           work( 1_ilp ) = lwreq
           return
     end subroutine stdlib_sgelq

     pure module subroutine stdlib_dgelq( m, n, a, lda, t, tsize, work, lwork,info )
     !! DGELQ computes an LQ factorization of a real M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           integer(ilp) :: mb, nb, mintsz, nblcks, lwmin, lwopt, lwreq
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
             mb = stdlib_ilaenv( 1_ilp, 'DGELQ ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'DGELQ ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = 1_ilp
             nb = n
           end if
           if( mb>min( m, n ) .or. mb<1_ilp ) mb = 1_ilp
           if( nb>n .or. nb<=m ) nb = n
           mintsz = m + 5_ilp
           if ( nb>m .and. n>m ) then
             if( mod( n - m, nb - m )==0_ilp ) then
               nblcks = ( n - m ) / ( nb - m )
             else
               nblcks = ( n - m ) / ( nb - m ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwmin = max( 1_ilp, n )
              lwopt = max( 1_ilp, mb*n )
           else
              lwmin = max( 1_ilp, m )
              lwopt = max( 1_ilp, mb*m )
           end if
           lminws = .false.
           if( ( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) .or. lwork<lwopt ).and. ( lwork>=lwmin ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) ) then
                 lminws = .true.
                 mb = 1_ilp
                 nb = n
             end if
             if( lwork<lwopt ) then
                 lminws = .true.
                 mb = 1_ilp
             end if
           end if
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwreq = max( 1_ilp, mb*n )
           else
              lwreq = max( 1_ilp, mb*m )
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<lwreq ) .and.( .not.lquery ).and. ( .not.lminws ) ) then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = mb*m*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = lwmin
             else
               work( 1_ilp ) = lwreq
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DGELQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the lq decomposition
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
             call stdlib_dgelqt( m, n, mb, a, lda, t( 6_ilp ), mb, work, info )
           else
             call stdlib_dlaswlq( m, n, mb, nb, a, lda, t( 6_ilp ), mb, work,lwork, info )
           end if
           work( 1_ilp ) = lwreq
           return
     end subroutine stdlib_dgelq


     pure module subroutine stdlib_cgelq( m, n, a, lda, t, tsize, work, lwork,info )
     !! CGELQ computes an LQ factorization of a complex M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           integer(ilp) :: mb, nb, mintsz, nblcks, lwmin, lwopt, lwreq
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
             mb = stdlib_ilaenv( 1_ilp, 'CGELQ ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'CGELQ ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = 1_ilp
             nb = n
           end if
           if( mb>min( m, n ) .or. mb<1_ilp ) mb = 1_ilp
           if( nb>n .or. nb<=m ) nb = n
           mintsz = m + 5_ilp
           if( nb>m .and. n>m ) then
             if( mod( n - m, nb - m )==0_ilp ) then
               nblcks = ( n - m ) / ( nb - m )
             else
               nblcks = ( n - m ) / ( nb - m ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwmin = max( 1_ilp, n )
              lwopt = max( 1_ilp, mb*n )
           else
              lwmin = max( 1_ilp, m )
              lwopt = max( 1_ilp, mb*m )
           end if
           lminws = .false.
           if( ( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) .or. lwork<lwopt ).and. ( lwork>=lwmin ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) ) then
               lminws = .true.
               mb = 1_ilp
               nb = n
             end if
             if( lwork<lwopt ) then
               lminws = .true.
               mb = 1_ilp
             end if
           end if
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwreq = max( 1_ilp, mb*n )
           else
              lwreq = max( 1_ilp, mb*m )
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<lwreq ) .and.( .not.lquery ).and. ( .not.lminws ) ) then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = mb*m*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = lwmin
             else
               work( 1_ilp ) = lwreq
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CGELQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the lq decomposition
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
             call stdlib_cgelqt( m, n, mb, a, lda, t( 6_ilp ), mb, work, info )
           else
             call stdlib_claswlq( m, n, mb, nb, a, lda, t( 6_ilp ), mb, work,lwork, info )
           end if
           work( 1_ilp ) = lwreq
           return
     end subroutine stdlib_cgelq

     pure module subroutine stdlib_zgelq( m, n, a, lda, t, tsize, work, lwork,info )
     !! ZGELQ computes an LQ factorization of a complex M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           integer(ilp) :: mb, nb, mintsz, nblcks, lwmin, lwopt, lwreq
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
             mb = stdlib_ilaenv( 1_ilp, 'ZGELQ ', ' ', m, n, 1_ilp, -1_ilp )
             nb = stdlib_ilaenv( 1_ilp, 'ZGELQ ', ' ', m, n, 2_ilp, -1_ilp )
           else
             mb = 1_ilp
             nb = n
           end if
           if( mb>min( m, n ) .or. mb<1_ilp ) mb = 1_ilp
           if( nb>n .or. nb<=m ) nb = n
           mintsz = m + 5_ilp
           if ( nb>m .and. n>m ) then
             if( mod( n - m, nb - m )==0_ilp ) then
               nblcks = ( n - m ) / ( nb - m )
             else
               nblcks = ( n - m ) / ( nb - m ) + 1_ilp
             end if
           else
             nblcks = 1_ilp
           end if
           ! determine if the workspace size satisfies minimal size
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwmin = max( 1_ilp, n )
              lwopt = max( 1_ilp, mb*n )
           else
              lwmin = max( 1_ilp, m )
              lwopt = max( 1_ilp, mb*m )
           end if
           lminws = .false.
           if( ( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) .or. lwork<lwopt ).and. ( lwork>=lwmin ) .and. ( &
                     tsize>=mintsz ).and. ( .not.lquery ) ) then
             if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ) ) then
                 lminws = .true.
                 mb = 1_ilp
                 nb = n
             end if
             if( lwork<lwopt ) then
                 lminws = .true.
                 mb = 1_ilp
             end if
           end if
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
              lwreq = max( 1_ilp, mb*n )
           else
              lwreq = max( 1_ilp, mb*m )
           end if
           if( m<0_ilp ) then
             info = -1_ilp
           else if( n<0_ilp ) then
             info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -4_ilp
           else if( tsize<max( 1_ilp, mb*m*nblcks + 5_ilp ).and. ( .not.lquery ) .and. ( .not.lminws ) ) &
                     then
             info = -6_ilp
           else if( ( lwork<lwreq ) .and.( .not.lquery ).and. ( .not.lminws ) ) then
             info = -8_ilp
           end if
           if( info==0_ilp ) then
             if( mint ) then
               t( 1_ilp ) = mintsz
             else
               t( 1_ilp ) = mb*m*nblcks + 5_ilp
             end if
             t( 2_ilp ) = mb
             t( 3_ilp ) = nb
             if( minw ) then
               work( 1_ilp ) = lwmin
             else
               work( 1_ilp ) = lwreq
             end if
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZGELQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n )==0_ilp ) then
             return
           end if
           ! the lq decomposition
           if( ( n<=m ) .or. ( nb<=m ) .or. ( nb>=n ) ) then
             call stdlib_zgelqt( m, n, mb, a, lda, t( 6_ilp ), mb, work, info )
           else
             call stdlib_zlaswlq( m, n, mb, nb, a, lda, t( 6_ilp ), mb, work,lwork, info )
           end if
           work( 1_ilp ) = lwreq
           return
     end subroutine stdlib_zgelq




     pure module subroutine stdlib_sgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! SGEMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by short wide LQ
     !! factorization (SGELQ)
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
             lw = n * mb
             mn = m
           else
             lw = m * mb
             mn = n
           end if
           if( ( nb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, nb - k ) == 0_ilp ) then
               nblcks = ( mn - k ) / ( nb - k )
             else
               nblcks = ( mn - k ) / ( nb - k ) + 1_ilp
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
           else if( lda<max( 1_ilp, k ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = real( lw,KIND=sp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SGEMLQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( nb<=k ) .or. ( nb>=max( m, n, &
                     k ) ) ) then
             call stdlib_sgemlqt( side, trans, m, n, k, mb, a, lda,t( 6_ilp ), mb, c, ldc, work, info &
                       )
           else
             call stdlib_slamswlq( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),mb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = real( lw,KIND=sp)
           return
     end subroutine stdlib_sgemlq

     pure module subroutine stdlib_dgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! DGEMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product
     !! of blocked elementary reflectors computed by short wide LQ
     !! factorization (DGELQ)
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
             lw = n * mb
             mn = m
           else
             lw = m * mb
             mn = n
           end if
           if( ( nb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, nb - k ) == 0_ilp ) then
               nblcks = ( mn - k ) / ( nb - k )
             else
               nblcks = ( mn - k ) / ( nb - k ) + 1_ilp
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
           else if( lda<max( 1_ilp, k ) ) then
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
             call stdlib_xerbla( 'DGEMLQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( nb<=k ) .or. ( nb>=max( m, n, &
                     k ) ) ) then
             call stdlib_dgemlqt( side, trans, m, n, k, mb, a, lda,t( 6_ilp ), mb, c, ldc, work, info &
                       )
           else
             call stdlib_dlamswlq( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),mb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_dgemlq


     pure module subroutine stdlib_cgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! CGEMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by short wide
     !! LQ factorization (CGELQ)
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
             lw = n * mb
             mn = m
           else
             lw = m * mb
             mn = n
           end if
           if( ( nb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, nb - k ) == 0_ilp ) then
               nblcks = ( mn - k ) / ( nb - k )
             else
               nblcks = ( mn - k ) / ( nb - k ) + 1_ilp
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
           else if( lda<max( 1_ilp, k ) ) then
             info = -7_ilp
           else if( tsize<5_ilp ) then
             info = -9_ilp
           else if( ldc<max( 1_ilp, m ) ) then
             info = -11_ilp
           else if( ( lwork<max( 1_ilp, lw ) ) .and. ( .not.lquery ) ) then
             info = -13_ilp
           end if
           if( info==0_ilp ) then
             work( 1_ilp ) = real( lw,KIND=sp)
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CGEMLQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( nb<=k ) .or. ( nb>=max( m, n, &
                     k ) ) ) then
             call stdlib_cgemlqt( side, trans, m, n, k, mb, a, lda,t( 6_ilp ), mb, c, ldc, work, info &
                       )
           else
             call stdlib_clamswlq( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),mb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = real( lw,KIND=sp)
           return
     end subroutine stdlib_cgemlq

     pure module subroutine stdlib_zgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
     !! ZGEMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product
     !! of blocked elementary reflectors computed by short wide
     !! LQ factorization (ZGELQ)
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
             lw = n * mb
             mn = m
           else
             lw = m * mb
             mn = n
           end if
           if( ( nb>k ) .and. ( mn>k ) ) then
             if( mod( mn - k, nb - k ) == 0_ilp ) then
               nblcks = ( mn - k ) / ( nb - k )
             else
               nblcks = ( mn - k ) / ( nb - k ) + 1_ilp
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
           else if( lda<max( 1_ilp, k ) ) then
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
             call stdlib_xerbla( 'ZGEMLQ', -info )
             return
           else if( lquery ) then
             return
           end if
           ! quick return if possible
           if( min( m, n, k )==0_ilp ) then
             return
           end if
           if( ( left .and. m<=k ) .or. ( right .and. n<=k ).or. ( nb<=k ) .or. ( nb>=max( m, n, &
                     k ) ) ) then
             call stdlib_zgemlqt( side, trans, m, n, k, mb, a, lda,t( 6_ilp ), mb, c, ldc, work, info &
                       )
           else
             call stdlib_zlamswlq( side, trans, m, n, k, mb, nb, a, lda, t( 6_ilp ),mb, c, ldc, work, &
                       lwork, info )
           end if
           work( 1_ilp ) = lw
           return
     end subroutine stdlib_zgemlq




     pure module subroutine stdlib_sgelqf( m, n, a, lda, tau, work, lwork, info )
     !! SGELQF computes an LQ factorization of a real M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           nb = stdlib_ilaenv( 1_ilp, 'SGELQF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = m*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELQF', -info )
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
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGELQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGELQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the lq factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_sgelq2( ib, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_slarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(i+ib:m,i:n) from the right
                    call stdlib_slarfb( 'RIGHT', 'NO TRANSPOSE', 'FORWARD','ROWWISE', m-i-ib+1, n-&
                    i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_sgelq2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgelqf

     pure module subroutine stdlib_dgelqf( m, n, a, lda, tau, work, lwork, info )
     !! DGELQF computes an LQ factorization of a real M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           nb = stdlib_ilaenv( 1_ilp, 'DGELQF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = m*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELQF', -info )
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
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGELQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGELQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the lq factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_dgelq2( ib, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_dlarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(i+ib:m,i:n) from the right
                    call stdlib_dlarfb( 'RIGHT', 'NO TRANSPOSE', 'FORWARD','ROWWISE', m-i-ib+1, n-&
                    i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_dgelq2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgelqf


     pure module subroutine stdlib_cgelqf( m, n, a, lda, tau, work, lwork, info )
     !! CGELQF computes an LQ factorization of a complex M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           nb = stdlib_ilaenv( 1_ilp, 'CGELQF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = m*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELQF', -info )
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
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGELQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGELQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the lq factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_cgelq2( ib, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_clarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(i+ib:m,i:n) from the right
                    call stdlib_clarfb( 'RIGHT', 'NO TRANSPOSE', 'FORWARD','ROWWISE', m-i-ib+1, n-&
                    i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_cgelq2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgelqf

     pure module subroutine stdlib_zgelqf( m, n, a, lda, tau, work, lwork, info )
     !! ZGELQF computes an LQ factorization of a complex M-by-N matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a N-by-N orthogonal matrix;
     !! L is a lower-triangular M-by-M matrix;
     !! 0 is a M-by-(N-M) zero matrix, if M < N.
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
           nb = stdlib_ilaenv( 1_ilp, 'ZGELQF', ' ', m, n, -1_ilp, -1_ilp )
           lwkopt = m*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELQF', -info )
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
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGELQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGELQF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially
              do i = 1, k - nx, nb
                 ib = min( k-i+1, nb )
                 ! compute the lq factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_zgelq2( ib, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_zlarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(i+ib:m,i:n) from the right
                    call stdlib_zlarfb( 'RIGHT', 'NO TRANSPOSE', 'FORWARD','ROWWISE', m-i-ib+1, n-&
                    i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
           else
              i = 1_ilp
           end if
           ! use unblocked code to factor the last or only block.
           if( i<=k )call stdlib_zgelq2( m-i+1, n-i+1, a( i, i ), lda, tau( i ), work,iinfo )
                     
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgelqf




     pure module subroutine stdlib_sgelq2( m, n, a, lda, tau, work, info )
     !! SGELQ2 computes an LQ factorization of a real m-by-n matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a n-by-n orthogonal matrix;
     !! L is a lower-triangular m-by-m matrix;
     !! 0 is a m-by-(n-m) zero matrix, if m < n.
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
              call stdlib_xerbla( 'SGELQ2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i,i+1:n)
              call stdlib_slarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,tau( i ) )
              if( i<m ) then
                 ! apply h(i) to a(i+1:m,i:n) from the right
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_slarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda, tau( i ),a( i+1, i ), &
                           lda, work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_sgelq2

     pure module subroutine stdlib_dgelq2( m, n, a, lda, tau, work, info )
     !! DGELQ2 computes an LQ factorization of a real m-by-n matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a n-by-n orthogonal matrix;
     !! L is a lower-triangular m-by-m matrix;
     !! 0 is a m-by-(n-m) zero matrix, if m < n.
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
              call stdlib_xerbla( 'DGELQ2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i,i+1:n)
              call stdlib_dlarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,tau( i ) )
              if( i<m ) then
                 ! apply h(i) to a(i+1:m,i:n) from the right
                 aii = a( i, i )
                 a( i, i ) = one
                 call stdlib_dlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda, tau( i ),a( i+1, i ), &
                           lda, work )
                 a( i, i ) = aii
              end if
           end do
           return
     end subroutine stdlib_dgelq2


     pure module subroutine stdlib_cgelq2( m, n, a, lda, tau, work, info )
     !! CGELQ2 computes an LQ factorization of a complex m-by-n matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a n-by-n orthogonal matrix;
     !! L is a lower-triangular m-by-m matrix;
     !! 0 is a m-by-(n-m) zero matrix, if m < n.
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
              call stdlib_xerbla( 'CGELQ2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i,i+1:n)
              call stdlib_clacgv( n-i+1, a( i, i ), lda )
              alpha = a( i, i )
              call stdlib_clarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,tau( i ) )
              if( i<m ) then
                 ! apply h(i) to a(i+1:m,i:n) from the right
                 a( i, i ) = cone
                 call stdlib_clarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda, tau( i ),a( i+1, i ), &
                           lda, work )
              end if
              a( i, i ) = alpha
              call stdlib_clacgv( n-i+1, a( i, i ), lda )
           end do
           return
     end subroutine stdlib_cgelq2

     pure module subroutine stdlib_zgelq2( m, n, a, lda, tau, work, info )
     !! ZGELQ2 computes an LQ factorization of a complex m-by-n matrix A:
     !! A = ( L 0 ) *  Q
     !! where:
     !! Q is a n-by-n orthogonal matrix;
     !! L is a lower-triangular m-by-m matrix;
     !! 0 is a m-by-(n-m) zero matrix, if m < n.
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
              call stdlib_xerbla( 'ZGELQ2', -info )
              return
           end if
           k = min( m, n )
           do i = 1, k
              ! generate elementary reflector h(i) to annihilate a(i,i+1:n)
              call stdlib_zlacgv( n-i+1, a( i, i ), lda )
              alpha = a( i, i )
              call stdlib_zlarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,tau( i ) )
              if( i<m ) then
                 ! apply h(i) to a(i+1:m,i:n) from the right
                 a( i, i ) = cone
                 call stdlib_zlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda, tau( i ),a( i+1, i ), &
                           lda, work )
              end if
              a( i, i ) = alpha
              call stdlib_zlacgv( n-i+1, a( i, i ), lda )
           end do
           return
     end subroutine stdlib_zgelq2




     pure module subroutine stdlib_cunglq( m, n, k, a, lda, tau, work, lwork, info )
     !! CUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
     !! which is defined as the first M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(k)**H . . . H(2)**H H(1)**H
     !! as returned by CGELQF.
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
           nb = stdlib_ilaenv( 1_ilp, 'CUNGLQ', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, m )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGLQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CUNGLQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNGLQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(kk+1:m,1:kk) to czero.
              do j = 1, kk
                 do i = kk + 1, m
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<m )call stdlib_cungl2( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_clarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h**h to a(i+ib:m,i:n) from the right
                    call stdlib_clarfb( 'RIGHT', 'CONJUGATE TRANSPOSE', 'FORWARD','ROWWISE', m-i-&
                    ib+1, n-i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h**h to columns i:n of current block
                 call stdlib_cungl2( ib, n-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set columns 1:i-1 of current block to czero
                 do j = 1, i - 1
                    do l = i, i + ib - 1
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cunglq

     pure module subroutine stdlib_zunglq( m, n, k, a, lda, tau, work, lwork, info )
     !! ZUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
     !! which is defined as the first M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(k)**H . . . H(2)**H H(1)**H
     !! as returned by ZGELQF.
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
           nb = stdlib_ilaenv( 1_ilp, 'ZUNGLQ', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, m )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGLQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZUNGLQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNGLQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(kk+1:m,1:kk) to czero.
              do j = 1, kk
                 do i = kk + 1, m
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<m )call stdlib_zungl2( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_zlarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h**h to a(i+ib:m,i:n) from the right
                    call stdlib_zlarfb( 'RIGHT', 'CONJUGATE TRANSPOSE', 'FORWARD','ROWWISE', m-i-&
                    ib+1, n-i+1, ib, a( i, i ),lda, work, ldwork, a( i+ib, i ), lda,work( ib+1 ), &
                              ldwork )
                 end if
                 ! apply h**h to columns i:n of current block
                 call stdlib_zungl2( ib, n-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set columns 1:i-1 of current block to czero
                 do j = 1, i - 1
                    do l = i, i + ib - 1
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zunglq




     pure module subroutine stdlib_cungl2( m, n, k, a, lda, tau, work, info )
     !! CUNGL2 generates an m-by-n complex matrix Q with orthonormal rows,
     !! which is defined as the first m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(k)**H . . . H(2)**H H(1)**H
     !! as returned by CGELQF.
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
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGL2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows k+1:m to rows of the unit matrix
              do j = 1, n
                 do l = k + 1, m
                    a( l, j ) = czero
                 end do
                 if( j>k .and. j<=m )a( j, j ) = cone
              end do
           end if
           do i = k, 1, -1
              ! apply h(i)**h to a(i:m,i:n) from the right
              if( i<n ) then
                 call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                 if( i<m ) then
                    a( i, i ) = cone
                    call stdlib_clarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,conjg( tau( i ) ), a( &
                              i+1, i ), lda, work )
                 end if
                 call stdlib_cscal( n-i, -tau( i ), a( i, i+1 ), lda )
                 call stdlib_clacgv( n-i, a( i, i+1 ), lda )
              end if
              a( i, i ) = cone - conjg( tau( i ) )
              ! set a(i,1:i-1,i) to czero
              do l = 1, i - 1
                 a( i, l ) = czero
              end do
           end do
           return
     end subroutine stdlib_cungl2

     pure module subroutine stdlib_zungl2( m, n, k, a, lda, tau, work, info )
     !! ZUNGL2 generates an m-by-n complex matrix Q with orthonormal rows,
     !! which is defined as the first m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(k)**H . . . H(2)**H H(1)**H
     !! as returned by ZGELQF.
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
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGL2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows k+1:m to rows of the unit matrix
              do j = 1, n
                 do l = k + 1, m
                    a( l, j ) = czero
                 end do
                 if( j>k .and. j<=m )a( j, j ) = cone
              end do
           end if
           do i = k, 1, -1
              ! apply h(i)**h to a(i:m,i:n) from the right
              if( i<n ) then
                 call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                 if( i<m ) then
                    a( i, i ) = cone
                    call stdlib_zlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,conjg( tau( i ) ), a( &
                              i+1, i ), lda, work )
                 end if
                 call stdlib_zscal( n-i, -tau( i ), a( i, i+1 ), lda )
                 call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
              end if
              a( i, i ) = cone - conjg( tau( i ) )
              ! set a(i,1:i-1) to czero
              do l = 1, i - 1
                 a( i, l ) = czero
              end do
           end do
           return
     end subroutine stdlib_zungl2




     pure module subroutine stdlib_cunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! CUNMLQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k)**H . . . H(2)**H H(1)**H
     !! as returned by CGELQF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CUNMLQ', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMLQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. k==0_ilp ) then
              return
           end if
           ! determine the block size
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNMLQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_cunml2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_clarft( 'FORWARD', 'ROWWISE', nq-i+1, ib, a( i, i ),lda, tau( i ), &
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
                 call stdlib_clarfb( side, transt, 'FORWARD', 'ROWWISE', mi, ni, ib,a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmlq

     pure module subroutine stdlib_zunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! ZUNMLQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k)**H . . . H(2)**H H(1)**H
     !! as returned by ZGELQF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZUNMLQ', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMLQ', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNMLQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_zunml2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_zlarft( 'FORWARD', 'ROWWISE', nq-i+1, ib, a( i, i ),lda, tau( i ), &
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
                 call stdlib_zlarfb( side, transt, 'FORWARD', 'ROWWISE', mi, ni, ib,a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmlq




     pure module subroutine stdlib_cunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! CUNML2 overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k)**H . . . H(2)**H H(1)**H
     !! as returned by CGELQF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNML2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran .or. .not.left .and. .not.notran ) ) then
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
                 taui = conjg( tau( i ) )
              else
                 taui = tau( i )
              end if
              if( i<nq )call stdlib_clacgv( nq-i, a( i, i+1 ), lda )
              aii = a( i, i )
              a( i, i ) = cone
              call stdlib_clarf( side, mi, ni, a( i, i ), lda, taui, c( ic, jc ),ldc, work )
                        
              a( i, i ) = aii
              if( i<nq )call stdlib_clacgv( nq-i, a( i, i+1 ), lda )
           end do
           return
     end subroutine stdlib_cunml2

     pure module subroutine stdlib_zunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! ZUNML2 overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k)**H . . . H(2)**H H(1)**H
     !! as returned by ZGELQF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNML2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran .or. .not.left .and. .not.notran ) ) then
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
                 taui = conjg( tau( i ) )
              else
                 taui = tau( i )
              end if
              if( i<nq )call stdlib_zlacgv( nq-i, a( i, i+1 ), lda )
              aii = a( i, i )
              a( i, i ) = cone
              call stdlib_zlarf( side, mi, ni, a( i, i ), lda, taui, c( ic, jc ),ldc, work )
                        
              a( i, i ) = aii
              if( i<nq )call stdlib_zlacgv( nq-i, a( i, i+1 ), lda )
           end do
           return
     end subroutine stdlib_zunml2




     pure module subroutine stdlib_sorglq( m, n, k, a, lda, tau, work, lwork, info )
     !! SORGLQ generates an M-by-N real matrix Q with orthonormal rows,
     !! which is defined as the first M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by SGELQF.
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
           nb = stdlib_ilaenv( 1_ilp, 'SORGLQ', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, m )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGLQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SORGLQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORGLQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(kk+1:m,1:kk) to zero.
              do j = 1, kk
                 do i = kk + 1, m
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<m )call stdlib_sorgl2( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_slarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h**t to a(i+ib:m,i:n) from the right
                    call stdlib_slarfb( 'RIGHT', 'TRANSPOSE', 'FORWARD', 'ROWWISE',m-i-ib+1, n-i+&
                    1_ilp, ib, a( i, i ), lda, work,ldwork, a( i+ib, i ), lda, work( ib+1 ),ldwork )
                              
                 end if
                 ! apply h**t to columns i:n of current block
                 call stdlib_sorgl2( ib, n-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set columns 1:i-1 of current block to zero
                 do j = 1, i - 1
                    do l = i, i + ib - 1
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sorglq

     pure module subroutine stdlib_dorglq( m, n, k, a, lda, tau, work, lwork, info )
     !! DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
     !! which is defined as the first M rows of a product of K elementary
     !! reflectors of order N
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by DGELQF.
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
           nb = stdlib_ilaenv( 1_ilp, 'DORGLQ', ' ', m, n, k, -1_ilp )
           lwkopt = max( 1_ilp, m )*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( lwork<max( 1_ilp, m ) .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGLQ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m<=0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = m
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DORGLQ', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORGLQ', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the last block.
              ! the first kk rows are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              ! set a(kk+1:m,1:kk) to zero.
              do j = 1, kk
                 do i = kk + 1, m
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the last or only block.
           if( kk<m )call stdlib_dorgl2( m-kk, n-kk, k-kk, a( kk+1, kk+1 ), lda,tau( kk+1 ), work,&
                      iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = ki + 1, 1, -nb
                 ib = min( nb, k-i+1 )
                 if( i+ib<=m ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i) h(i+1) . . . h(i+ib-1)
                    call stdlib_dlarft( 'FORWARD', 'ROWWISE', n-i+1, ib, a( i, i ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h**t to a(i+ib:m,i:n) from the right
                    call stdlib_dlarfb( 'RIGHT', 'TRANSPOSE', 'FORWARD', 'ROWWISE',m-i-ib+1, n-i+&
                    1_ilp, ib, a( i, i ), lda, work,ldwork, a( i+ib, i ), lda, work( ib+1 ),ldwork )
                              
                 end if
                 ! apply h**t to columns i:n of current block
                 call stdlib_dorgl2( ib, n-i+1, ib, a( i, i ), lda, tau( i ), work,iinfo )
                 ! set columns 1:i-1 of current block to zero
                 do j = 1, i - 1
                    do l = i, i + ib - 1
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dorglq




     pure module subroutine stdlib_sorgl2( m, n, k, a, lda, tau, work, info )
     !! SORGL2 generates an m by n real matrix Q with orthonormal rows,
     !! which is defined as the first m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by SGELQF.
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
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGL2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows k+1:m to rows of the unit matrix
              do j = 1, n
                 do l = k + 1, m
                    a( l, j ) = zero
                 end do
                 if( j>k .and. j<=m )a( j, j ) = one
              end do
           end if
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the right
              if( i<n ) then
                 if( i<m ) then
                    a( i, i ) = one
                    call stdlib_slarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,tau( i ), a( i+1, i ), &
                              lda, work )
                 end if
                 call stdlib_sscal( n-i, -tau( i ), a( i, i+1 ), lda )
              end if
              a( i, i ) = one - tau( i )
              ! set a(i,1:i-1) to zero
              do l = 1, i - 1
                 a( i, l ) = zero
              end do
           end do
           return
     end subroutine stdlib_sorgl2

     pure module subroutine stdlib_dorgl2( m, n, k, a, lda, tau, work, info )
     !! DORGL2 generates an m by n real matrix Q with orthonormal rows,
     !! which is defined as the first m rows of a product of k elementary
     !! reflectors of order n
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by DGELQF.
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
           else if( n<m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGL2', -info )
              return
           end if
           ! quick return if possible
           if( m<=0 )return
           if( k<m ) then
              ! initialise rows k+1:m to rows of the unit matrix
              do j = 1, n
                 do l = k + 1, m
                    a( l, j ) = zero
                 end do
                 if( j>k .and. j<=m )a( j, j ) = one
              end do
           end if
           do i = k, 1, -1
              ! apply h(i) to a(i:m,i:n) from the right
              if( i<n ) then
                 if( i<m ) then
                    a( i, i ) = one
                    call stdlib_dlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,tau( i ), a( i+1, i ), &
                              lda, work )
                 end if
                 call stdlib_dscal( n-i, -tau( i ), a( i, i+1 ), lda )
              end if
              a( i, i ) = one - tau( i )
              ! set a(i,1:i-1) to zero
              do l = 1, i - 1
                 a( i, l ) = zero
              end do
           end do
           return
     end subroutine stdlib_dorgl2




     pure module subroutine stdlib_sormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! SORMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by SGELQF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SORMLQ', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMLQ', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORMLQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_sorml2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_slarft( 'FORWARD', 'ROWWISE', nq-i+1, ib, a( i, i ),lda, tau( i ), &
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
                 call stdlib_slarfb( side, transt, 'FORWARD', 'ROWWISE', mi, ni, ib,a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormlq

     pure module subroutine stdlib_dormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! DORMLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              ! compute the workspace requirements
              nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DORMLQ', side // trans, m, n, k,-1_ilp ) )
              lwkopt = nw*nb + tsize
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMLQ', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORMLQ', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_dorml2( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i) h(i+1) . . . h(i+ib-1)
                 call stdlib_dlarft( 'FORWARD', 'ROWWISE', nq-i+1, ib, a( i, i ),lda, tau( i ), &
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
                 call stdlib_dlarfb( side, transt, 'FORWARD', 'ROWWISE', mi, ni, ib,a( i, i ), &
                           lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormlq




     pure module subroutine stdlib_sorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! SORML2 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by SGELQF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORML2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran ) .or. ( .not.left .and. .not.notran ) )then
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
              call stdlib_slarf( side, mi, ni, a( i, i ), lda, tau( i ),c( ic, jc ), ldc, work )
                        
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_sorml2

     pure module subroutine stdlib_dorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! DORML2 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, k ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORML2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran ) .or. ( .not.left .and. .not.notran ) )then
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
              call stdlib_dlarf( side, mi, ni, a( i, i ), lda, tau( i ),c( ic, jc ), ldc, work )
                        
              a( i, i ) = aii
           end do
           return
     end subroutine stdlib_dorml2




     pure module subroutine stdlib_sgelqt( m, n, mb, a, lda, t, ldt, work, info )
     !! DGELQT computes a blocked LQ factorization of a real M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, iinfo, k
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( mb<1_ilp .or. ( mb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<mb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELQT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  mb
              ib = min( k-i+1, mb )
           ! compute the lq factorization of the current block a(i:m,i:i+ib-1)
              call stdlib_sgelqt3( ib, n-i+1, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              if( i+ib<=m ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the right
              call stdlib_slarfb( 'R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib,a( i, i ), lda, t( 1_ilp, i &
                        ), ldt,a( i+ib, i ), lda, work , m-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_sgelqt

     pure module subroutine stdlib_dgelqt( m, n, mb, a, lda, t, ldt, work, info )
     !! DGELQT computes a blocked LQ factorization of a real M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, iinfo, k
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( mb<1_ilp .or. ( mb>min(m,n) .and. min(m,n)>0_ilp ) )then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<mb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELQT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  mb
              ib = min( k-i+1, mb )
           ! compute the lq factorization of the current block a(i:m,i:i+ib-1)
              call stdlib_dgelqt3( ib, n-i+1, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              if( i+ib<=m ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the right
              call stdlib_dlarfb( 'R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib,a( i, i ), lda, t( 1_ilp, i &
                        ), ldt,a( i+ib, i ), lda, work , m-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_dgelqt


     pure module subroutine stdlib_cgelqt( m, n, mb, a, lda, t, ldt, work, info )
     !! CGELQT computes a blocked LQ factorization of a complex M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, iinfo, k
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( mb<1_ilp .or. (mb>min(m,n) .and. min(m,n)>0_ilp ))then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<mb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELQT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  mb
              ib = min( k-i+1, mb )
           ! compute the lq factorization of the current block a(i:m,i:i+ib-1)
              call stdlib_cgelqt3( ib, n-i+1, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              if( i+ib<=m ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the right
              call stdlib_clarfb( 'R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib,a( i, i ), lda, t( 1_ilp, i &
                        ), ldt,a( i+ib, i ), lda, work , m-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_cgelqt

     pure module subroutine stdlib_zgelqt( m, n, mb, a, lda, t, ldt, work, info )
     !! ZGELQT computes a blocked LQ factorization of a complex M-by-N matrix A
     !! using the compact WY representation of Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, iinfo, k
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( mb<1_ilp .or. (mb>min(m,n) .and. min(m,n)>0_ilp ))then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldt<mb ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELQT', -info )
              return
           end if
           ! quick return if possible
           k = min( m, n )
           if( k==0 ) return
           ! blocked loop of length k
           do i = 1, k,  mb
              ib = min( k-i+1, mb )
           ! compute the lq factorization of the current block a(i:m,i:i+ib-1)
              call stdlib_zgelqt3( ib, n-i+1, a(i,i), lda, t(1_ilp,i), ldt, iinfo )
              if( i+ib<=m ) then
           ! update by applying h**t to a(i:m,i+ib:n) from the right
              call stdlib_zlarfb( 'R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib,a( i, i ), lda, t( 1_ilp, i &
                        ), ldt,a( i+ib, i ), lda, work , m-i-ib+1 )
              end if
           end do
           return
     end subroutine stdlib_zgelqt




     pure recursive module subroutine stdlib_sgelqt3( m, n, a, lda, t, ldt, info )
     !! SGELQT3 recursively computes a LQ factorization of a real M-by-N
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
           integer(ilp) :: i, i1, j, j1, m1, m2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( n < m ) then
              info = -2_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, m ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGELQT3', -info )
              return
           end if
           if( m==1_ilp ) then
              ! compute householder transform when m=1
              call stdlib_slarfg( n, a(1_ilp,1_ilp), a( 1_ilp, min( 2_ilp, n ) ), lda, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              m1 = m/2_ilp
              m2 = m-m1
              i1 = min( m1+1, m )
              j1 = min( m+1, n )
              ! compute a(1:m1,1:n) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_sgelqt3( m1, n, a, lda, t, ldt, iinfo )
              ! compute a(j1:m,1:n) = q1^h a(j1:m,1:n) [workspace: t(1:n1,j1:n)]
              do i=1,m2
                 do j=1,m1
                    t(  i+m1, j ) = a( i+m1, j )
                 end do
              end do
              call stdlib_strmm( 'R', 'U', 'T', 'U', m2, m1, one,a, lda, t( i1, 1_ilp ), ldt )
              call stdlib_sgemm( 'N', 'T', m2, m1, n-m1, one, a( i1, i1 ), lda,a( 1_ilp, i1 ), lda, &
                        one, t( i1, 1_ilp ), ldt)
              call stdlib_strmm( 'R', 'U', 'N', 'N', m2, m1, one,t, ldt, t( i1, 1_ilp ), ldt )
              call stdlib_sgemm( 'N', 'N', m2, n-m1, m1, -one, t( i1, 1_ilp ), ldt,a( 1_ilp, i1 ), lda, &
                        one, a( i1, i1 ), lda )
              call stdlib_strmm( 'R', 'U', 'N', 'U', m2, m1 , one,a, lda, t( i1, 1_ilp ), ldt )
                        
              do i=1,m2
                 do j=1,m1
                    a(  i+m1, j ) = a( i+m1, j ) - t( i+m1, j )
                    t( i+m1, j )=0_ilp
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_sgelqt3( m2, n-m1, a( i1, i1 ), lda,t( i1, i1 ), ldt, iinfo )
              ! compute t3 = t(j1:n1,1:n) = -t1 y1^h y2 t2
              do i=1,m2
                 do j=1,m1
                    t( j, i+m1  ) = (a( j, i+m1 ))
                 end do
              end do
              call stdlib_strmm( 'R', 'U', 'T', 'U', m1, m2, one,a( i1, i1 ), lda, t( 1_ilp, i1 ), &
                        ldt )
              call stdlib_sgemm( 'N', 'T', m1, m2, n-m, one, a( 1_ilp, j1 ), lda,a( i1, j1 ), lda, &
                        one, t( 1_ilp, i1 ), ldt )
              call stdlib_strmm( 'L', 'U', 'N', 'N', m1, m2, -one, t, ldt,t( 1_ilp, i1 ), ldt )
                        
              call stdlib_strmm( 'R', 'U', 'N', 'N', m1, m2, one,t( i1, i1 ), ldt, t( 1_ilp, i1 ), &
                        ldt )
              ! y = (y1,y2); l = [ l1            0  ];  t = [t1 t3]
                               ! [ a(1:n1,j1:n)  l2 ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_sgelqt3

     pure recursive module subroutine stdlib_dgelqt3( m, n, a, lda, t, ldt, info )
     !! DGELQT3 recursively computes a LQ factorization of a real M-by-N
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
           integer(ilp) :: i, i1, j, j1, m1, m2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( n < m ) then
              info = -2_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, m ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGELQT3', -info )
              return
           end if
           if( m==1_ilp ) then
              ! compute householder transform when m=1
              call stdlib_dlarfg( n, a(1_ilp,1_ilp), a( 1_ilp, min( 2_ilp, n ) ), lda, t(1_ilp,1_ilp) )
           else
              ! otherwise, split a into blocks...
              m1 = m/2_ilp
              m2 = m-m1
              i1 = min( m1+1, m )
              j1 = min( m+1, n )
              ! compute a(1:m1,1:n) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_dgelqt3( m1, n, a, lda, t, ldt, iinfo )
              ! compute a(j1:m,1:n) = q1^h a(j1:m,1:n) [workspace: t(1:n1,j1:n)]
              do i=1,m2
                 do j=1,m1
                    t(  i+m1, j ) = a( i+m1, j )
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', 'T', 'U', m2, m1, one,a, lda, t( i1, 1_ilp ), ldt )
              call stdlib_dgemm( 'N', 'T', m2, m1, n-m1, one, a( i1, i1 ), lda,a( 1_ilp, i1 ), lda, &
                        one, t( i1, 1_ilp ), ldt)
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', m2, m1, one,t, ldt, t( i1, 1_ilp ), ldt )
              call stdlib_dgemm( 'N', 'N', m2, n-m1, m1, -one, t( i1, 1_ilp ), ldt,a( 1_ilp, i1 ), lda, &
                        one, a( i1, i1 ), lda )
              call stdlib_dtrmm( 'R', 'U', 'N', 'U', m2, m1 , one,a, lda, t( i1, 1_ilp ), ldt )
                        
              do i=1,m2
                 do j=1,m1
                    a(  i+m1, j ) = a( i+m1, j ) - t( i+m1, j )
                    t( i+m1, j )=0_ilp
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_dgelqt3( m2, n-m1, a( i1, i1 ), lda,t( i1, i1 ), ldt, iinfo )
              ! compute t3 = t(j1:n1,1:n) = -t1 y1^h y2 t2
              do i=1,m2
                 do j=1,m1
                    t( j, i+m1  ) = (a( j, i+m1 ))
                 end do
              end do
              call stdlib_dtrmm( 'R', 'U', 'T', 'U', m1, m2, one,a( i1, i1 ), lda, t( 1_ilp, i1 ), &
                        ldt )
              call stdlib_dgemm( 'N', 'T', m1, m2, n-m, one, a( 1_ilp, j1 ), lda,a( i1, j1 ), lda, &
                        one, t( 1_ilp, i1 ), ldt )
              call stdlib_dtrmm( 'L', 'U', 'N', 'N', m1, m2, -one, t, ldt,t( 1_ilp, i1 ), ldt )
                        
              call stdlib_dtrmm( 'R', 'U', 'N', 'N', m1, m2, one,t( i1, i1 ), ldt, t( 1_ilp, i1 ), &
                        ldt )
              ! y = (y1,y2); l = [ l1            0  ];  t = [t1 t3]
                               ! [ a(1:n1,j1:n)  l2 ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_dgelqt3


     pure recursive module subroutine stdlib_cgelqt3( m, n, a, lda, t, ldt, info )
     !! CGELQT3 recursively computes a LQ factorization of a complex M-by-N
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
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, j, j1, m1, m2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( n < m ) then
              info = -2_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, m ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGELQT3', -info )
              return
           end if
           if( m==1_ilp ) then
              ! compute householder transform when m=1
              call stdlib_clarfg( n, a(1_ilp,1_ilp), a( 1_ilp, min( 2_ilp, n ) ), lda, t(1_ilp,1_ilp) )
              t(1_ilp,1_ilp)=conjg(t(1_ilp,1_ilp))
           else
              ! otherwise, split a into blocks...
              m1 = m/2_ilp
              m2 = m-m1
              i1 = min( m1+1, m )
              j1 = min( m+1, n )
              ! compute a(1:m1,1:n) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_cgelqt3( m1, n, a, lda, t, ldt, iinfo )
              ! compute a(j1:m,1:n) =  a(j1:m,1:n) q1^h [workspace: t(1:n1,j1:n)]
              do i=1,m2
                 do j=1,m1
                    t(  i+m1, j ) = a( i+m1, j )
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', 'C', 'U', m2, m1, cone,a, lda, t( i1, 1_ilp ), ldt )
                        
              call stdlib_cgemm( 'N', 'C', m2, m1, n-m1, cone, a( i1, i1 ), lda,a( 1_ilp, i1 ), lda, &
                        cone, t( i1, 1_ilp ), ldt)
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', m2, m1, cone,t, ldt, t( i1, 1_ilp ), ldt )
                        
              call stdlib_cgemm( 'N', 'N', m2, n-m1, m1, -cone, t( i1, 1_ilp ), ldt,a( 1_ilp, i1 ), lda, &
                        cone, a( i1, i1 ), lda )
              call stdlib_ctrmm( 'R', 'U', 'N', 'U', m2, m1 , cone,a, lda, t( i1, 1_ilp ), ldt )
                        
              do i=1,m2
                 do j=1,m1
                    a(  i+m1, j ) = a( i+m1, j ) - t( i+m1, j )
                    t( i+m1, j )= czero
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_cgelqt3( m2, n-m1, a( i1, i1 ), lda,t( i1, i1 ), ldt, iinfo )
              ! compute t3 = t(j1:n1,1:n) = -t1 y1^h y2 t2
              do i=1,m2
                 do j=1,m1
                    t( j, i+m1  ) = (a( j, i+m1 ))
                 end do
              end do
              call stdlib_ctrmm( 'R', 'U', 'C', 'U', m1, m2, cone,a( i1, i1 ), lda, t( 1_ilp, i1 ), &
                        ldt )
              call stdlib_cgemm( 'N', 'C', m1, m2, n-m, cone, a( 1_ilp, j1 ), lda,a( i1, j1 ), lda, &
                        cone, t( 1_ilp, i1 ), ldt )
              call stdlib_ctrmm( 'L', 'U', 'N', 'N', m1, m2, -cone, t, ldt,t( 1_ilp, i1 ), ldt )
                        
              call stdlib_ctrmm( 'R', 'U', 'N', 'N', m1, m2, cone,t( i1, i1 ), ldt, t( 1_ilp, i1 ), &
                        ldt )
              ! y = (y1,y2); l = [ l1            0  ];  t = [t1 t3]
                               ! [ a(1:n1,j1:n)  l2 ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_cgelqt3

     pure recursive module subroutine stdlib_zgelqt3( m, n, a, lda, t, ldt, info )
     !! ZGELQT3 recursively computes a LQ factorization of a complex M-by-N
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
           integer(ilp) :: i, i1, j, j1, m1, m2, iinfo
           ! Executable Statements 
           info = 0_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( n < m ) then
              info = -2_ilp
           else if( lda < max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( ldt < max( 1_ilp, m ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGELQT3', -info )
              return
           end if
           if( m==1_ilp ) then
              ! compute householder transform when m=1
              call stdlib_zlarfg( n, a(1_ilp,1_ilp), a( 1_ilp, min( 2_ilp, n ) ), lda, t(1_ilp,1_ilp) )
              t(1_ilp,1_ilp)=conjg(t(1_ilp,1_ilp))
           else
              ! otherwise, split a into blocks...
              m1 = m/2_ilp
              m2 = m-m1
              i1 = min( m1+1, m )
              j1 = min( m+1, n )
              ! compute a(1:m1,1:n) <- (y1,r1,t1), where q1 = i - y1 t1 y1^h
              call stdlib_zgelqt3( m1, n, a, lda, t, ldt, iinfo )
              ! compute a(j1:m,1:n) =  a(j1:m,1:n) q1^h [workspace: t(1:n1,j1:n)]
              do i=1,m2
                 do j=1,m1
                    t(  i+m1, j ) = a( i+m1, j )
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', 'C', 'U', m2, m1, cone,a, lda, t( i1, 1_ilp ), ldt )
                        
              call stdlib_zgemm( 'N', 'C', m2, m1, n-m1, cone, a( i1, i1 ), lda,a( 1_ilp, i1 ), lda, &
                        cone, t( i1, 1_ilp ), ldt)
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', m2, m1, cone,t, ldt, t( i1, 1_ilp ), ldt )
                        
              call stdlib_zgemm( 'N', 'N', m2, n-m1, m1, -cone, t( i1, 1_ilp ), ldt,a( 1_ilp, i1 ), lda, &
                        cone, a( i1, i1 ), lda )
              call stdlib_ztrmm( 'R', 'U', 'N', 'U', m2, m1 , cone,a, lda, t( i1, 1_ilp ), ldt )
                        
              do i=1,m2
                 do j=1,m1
                    a(  i+m1, j ) = a( i+m1, j ) - t( i+m1, j )
                    t( i+m1, j )= czero
                 end do
              end do
              ! compute a(j1:m,j1:n) <- (y2,r2,t2) where q2 = i - y2 t2 y2^h
              call stdlib_zgelqt3( m2, n-m1, a( i1, i1 ), lda,t( i1, i1 ), ldt, iinfo )
              ! compute t3 = t(j1:n1,1:n) = -t1 y1^h y2 t2
              do i=1,m2
                 do j=1,m1
                    t( j, i+m1  ) = (a( j, i+m1 ))
                 end do
              end do
              call stdlib_ztrmm( 'R', 'U', 'C', 'U', m1, m2, cone,a( i1, i1 ), lda, t( 1_ilp, i1 ), &
                        ldt )
              call stdlib_zgemm( 'N', 'C', m1, m2, n-m, cone, a( 1_ilp, j1 ), lda,a( i1, j1 ), lda, &
                        cone, t( 1_ilp, i1 ), ldt )
              call stdlib_ztrmm( 'L', 'U', 'N', 'N', m1, m2, -cone, t, ldt,t( 1_ilp, i1 ), ldt )
                        
              call stdlib_ztrmm( 'R', 'U', 'N', 'N', m1, m2, cone,t( i1, i1 ), ldt, t( 1_ilp, i1 ), &
                        ldt )
              ! y = (y1,y2); l = [ l1            0  ];  t = [t1 t3]
                               ! [ a(1:n1,j1:n)  l2 ]       [ 0 t2]
           end if
           return
     end subroutine stdlib_zgelqt3




     pure module subroutine stdlib_sgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
     !! DGEMLQT overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'T':   Q**T C            C Q**T
     !! where Q is a real orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**T
     !! generated using the compact WY representation as returned by SGELQT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, k ) ) then
               info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_slarfb( 'L', 'T', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_slarfb( 'R', 'N', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_slarfb( 'L', 'N', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_slarfb( 'R', 'T', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_sgemlqt

     pure module subroutine stdlib_dgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
     !! DGEMLQT overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'T':   Q**T C            C Q**T
     !! where Q is a real orthogonal matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**T
     !! generated using the compact WY representation as returned by DGELQT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, k ) ) then
               info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_dlarfb( 'L', 'T', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_dlarfb( 'R', 'N', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_dlarfb( 'L', 'N', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_dlarfb( 'R', 'T', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_dgemlqt


     pure module subroutine stdlib_cgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
     !! CGEMLQT overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'C':   Q**H C            C Q**H
     !! where Q is a complex unitary matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**H
     !! generated using the compact WY representation as returned by CGELQT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, k ) ) then
               info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_clarfb( 'L', 'C', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_clarfb( 'R', 'N', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_clarfb( 'L', 'N', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_clarfb( 'R', 'C', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_cgemlqt

     pure module subroutine stdlib_zgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
     !! ZGEMLQT overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q C            C Q
     !! TRANS = 'C':   Q**H C            C Q**H
     !! where Q is a complex unitary matrix defined as the product of K
     !! elementary reflectors:
     !! Q = H(1) H(2) . . . H(K) = I - V T V**H
     !! generated using the compact WY representation as returned by ZGELQT.
     !! Q is of order M if SIDE = 'L' and of order N  if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp)) then
              info = -6_ilp
           else if( ldv<max( 1_ilp, k ) ) then
               info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_zlarfb( 'L', 'C', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 call stdlib_zlarfb( 'R', 'N', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_zlarfb( 'L', 'N', 'F', 'R', m-i+1, n, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( i, 1_ilp ), ldc, work, ldwork )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 call stdlib_zlarfb( 'R', 'C', 'F', 'R', m, n-i+1, ib,v( i, i ), ldv, t( 1_ilp, i ), &
                           ldt,c( 1_ilp, i ), ldc, work, ldwork )
              end do
           end if
           return
     end subroutine stdlib_zgemlqt




     pure module subroutine stdlib_slaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
     !! SLASWLQ computes a blocked Tall-Skinny LQ factorization of
     !! a real M-by-N matrix A for M <= N:
     !! A = ( L 0 ) *  Q,
     !! where:
     !! Q is a n-by-N orthogonal matrix, stored on exit in an implicit
     !! form in the elements above the diagonal of the array A and in
     !! the elements of the array T;
     !! L is a lower-triangular M-by-M matrix stored on exit in
     !! the elements on and below the diagonal of the array A.
     !! 0 is a M-by-(N-M) zero matrix, if M < N, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
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
           else if( n<0_ilp .or. n<m ) then
             info = -2_ilp
           else if( mb<1_ilp .or. ( mb>m .and. m>0_ilp )) then
             info = -3_ilp
           else if( nb<=0_ilp ) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<mb ) then
             info = -8_ilp
           else if( ( lwork<m*mb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
           work(1_ilp) = mb*m
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SLASWLQ', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the lq decomposition
            if((m>=n).or.(nb<=m).or.(nb>=n)) then
             call stdlib_sgelqt( m, n, mb, a, lda, t, ldt, work, info)
             return
            end if
            kk = mod((n-m),(nb-m))
            ii=n-kk+1
            ! compute the lq factorization of the first block a(1:m,1:nb)
            call stdlib_sgelqt( m, nb, mb, a(1_ilp,1_ilp), lda, t, ldt, work, info)
            ctr = 1_ilp
            do i = nb+1, ii-nb+m , (nb-m)
            ! compute the qr factorization of the current block a(1:m,i:i+nb-m)
              call stdlib_stplqt( m, nb-m, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, i ),lda, t(1_ilp, ctr * m + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
           ! compute the qr factorization of the last block a(1:m,ii:n)
            if (ii<=n) then
             call stdlib_stplqt( m, kk, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, ii ),lda, t(1_ilp, ctr * m + 1_ilp), &
                       ldt,work, info )
            end if
           work( 1_ilp ) = m * mb
           return
     end subroutine stdlib_slaswlq

     pure module subroutine stdlib_dlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
     !! DLASWLQ computes a blocked Tall-Skinny LQ factorization of
     !! a real M-by-N matrix A for M <= N:
     !! A = ( L 0 ) *  Q,
     !! where:
     !! Q is a n-by-N orthogonal matrix, stored on exit in an implicit
     !! form in the elements above the diagonal of the array A and in
     !! the elements of the array T;
     !! L is a lower-triangular M-by-M matrix stored on exit in
     !! the elements on and below the diagonal of the array A.
     !! 0 is a M-by-(N-M) zero matrix, if M < N, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
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
           else if( n<0_ilp .or. n<m ) then
             info = -2_ilp
           else if( mb<1_ilp .or. ( mb>m .and. m>0_ilp )) then
             info = -3_ilp
           else if( nb<0_ilp ) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<mb ) then
             info = -8_ilp
           else if( ( lwork<m*mb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
           work(1_ilp) = mb*m
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DLASWLQ', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the lq decomposition
            if((m>=n).or.(nb<=m).or.(nb>=n)) then
             call stdlib_dgelqt( m, n, mb, a, lda, t, ldt, work, info)
             return
            end if
            kk = mod((n-m),(nb-m))
            ii=n-kk+1
            ! compute the lq factorization of the first block a(1:m,1:nb)
            call stdlib_dgelqt( m, nb, mb, a(1_ilp,1_ilp), lda, t, ldt, work, info)
            ctr = 1_ilp
            do i = nb+1, ii-nb+m , (nb-m)
            ! compute the qr factorization of the current block a(1:m,i:i+nb-m)
              call stdlib_dtplqt( m, nb-m, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, i ),lda, t(1_ilp, ctr * m + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
           ! compute the qr factorization of the last block a(1:m,ii:n)
            if (ii<=n) then
             call stdlib_dtplqt( m, kk, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, ii ),lda, t(1_ilp, ctr * m + 1_ilp), &
                       ldt,work, info )
            end if
           work( 1_ilp ) = m * mb
           return
     end subroutine stdlib_dlaswlq


     pure module subroutine stdlib_claswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
     !! CLASWLQ computes a blocked Tall-Skinny LQ factorization of
     !! a complex M-by-N matrix A for M <= N:
     !! A = ( L 0 ) *  Q,
     !! where:
     !! Q is a n-by-N orthogonal matrix, stored on exit in an implicit
     !! form in the elements above the diagonal of the array A and in
     !! the elements of the array T;
     !! L is a lower-triangular M-by-M matrix stored on exit in
     !! the elements on and below the diagonal of the array A.
     !! 0 is a M-by-(N-M) zero matrix, if M < N, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
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
           else if( n<0_ilp .or. n<m ) then
             info = -2_ilp
           else if( mb<1_ilp .or. ( mb>m .and. m>0_ilp )) then
             info = -3_ilp
           else if( nb<=0_ilp ) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<mb ) then
             info = -8_ilp
           else if( ( lwork<m*mb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
           work(1_ilp) = mb*m
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CLASWLQ', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the lq decomposition
            if((m>=n).or.(nb<=m).or.(nb>=n)) then
             call stdlib_cgelqt( m, n, mb, a, lda, t, ldt, work, info)
             return
            end if
            kk = mod((n-m),(nb-m))
            ii=n-kk+1
            ! compute the lq factorization of the first block a(1:m,1:nb)
            call stdlib_cgelqt( m, nb, mb, a(1_ilp,1_ilp), lda, t, ldt, work, info)
            ctr = 1_ilp
            do i = nb+1, ii-nb+m , (nb-m)
            ! compute the qr factorization of the current block a(1:m,i:i+nb-m)
              call stdlib_ctplqt( m, nb-m, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, i ),lda, t(1_ilp,ctr*m+1),ldt, &
                        work, info )
              ctr = ctr + 1_ilp
            end do
           ! compute the qr factorization of the last block a(1:m,ii:n)
            if (ii<=n) then
             call stdlib_ctplqt( m, kk, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, ii ),lda, t(1_ilp,ctr*m+1), ldt,&
                       work, info )
            end if
           work( 1_ilp ) = m * mb
           return
     end subroutine stdlib_claswlq

     pure module subroutine stdlib_zlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
     !! ZLASWLQ computes a blocked Tall-Skinny LQ factorization of
     !! a complexx M-by-N matrix A for M <= N:
     !! A = ( L 0 ) *  Q,
     !! where:
     !! Q is a n-by-N orthogonal matrix, stored on exit in an implicit
     !! form in the elements above the diagonal of the array A and in
     !! the elements of the array T;
     !! L is a lower-triangular M-by-M matrix stored on exit in
     !! the elements on and below the diagonal of the array A.
     !! 0 is a M-by-(N-M) zero matrix, if M < N, and is not stored.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd. --
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
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
           else if( n<0_ilp .or. n<m ) then
             info = -2_ilp
           else if( mb<1_ilp .or. ( mb>m .and. m>0_ilp )) then
             info = -3_ilp
           else if( nb<=0_ilp ) then
             info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
             info = -6_ilp
           else if( ldt<mb ) then
             info = -8_ilp
           else if( ( lwork<m*mb) .and. (.not.lquery) ) then
             info = -10_ilp
           end if
           if( info==0_ilp)  then
           work(1_ilp) = mb*m
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZLASWLQ', -info )
             return
           else if (lquery) then
            return
           end if
           ! quick return if possible
           if( min(m,n)==0_ilp ) then
               return
           end if
           ! the lq decomposition
            if((m>=n).or.(nb<=m).or.(nb>=n)) then
             call stdlib_zgelqt( m, n, mb, a, lda, t, ldt, work, info)
             return
            end if
            kk = mod((n-m),(nb-m))
            ii=n-kk+1
            ! compute the lq factorization of the first block a(1:m,1:nb)
            call stdlib_zgelqt( m, nb, mb, a(1_ilp,1_ilp), lda, t, ldt, work, info)
            ctr = 1_ilp
            do i = nb+1, ii-nb+m , (nb-m)
            ! compute the qr factorization of the current block a(1:m,i:i+nb-m)
              call stdlib_ztplqt( m, nb-m, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, i ),lda, t(1_ilp, ctr * m + 1_ilp),&
                        ldt, work, info )
              ctr = ctr + 1_ilp
            end do
           ! compute the qr factorization of the last block a(1:m,ii:n)
            if (ii<=n) then
             call stdlib_ztplqt( m, kk, 0_ilp, mb, a(1_ilp,1_ilp), lda, a( 1_ilp, ii ),lda, t(1_ilp, ctr * m + 1_ilp), &
                       ldt,work, info )
            end if
           work( 1_ilp ) = m * mb
           return
     end subroutine stdlib_zlaswlq




     pure module subroutine stdlib_slamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! SLAMSWLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of blocked
     !! elementary reflectors computed by short wide LQ
     !! factorization (SLASWLQ)
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
           integer(ilp) :: i, ii, kk, lw, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * mb
           else
             lw = m * mb
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<mb .or. mb<1_ilp) then
             info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, mb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'SLAMSWLQ', -info )
             work(1_ilp) = lw
             return
           else if (lquery) then
             work(1_ilp) = lw
             return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((nb<=k).or.(nb>=max(m,n,k))) then
             call stdlib_sgemlqt( side, trans, m, n, k, mb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
           end if
           if(left.and.tran) then
               ! multiply q to the last block of c
               kk = mod((m-k),(nb-k))
               ctr = (m-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=m-kk+1
                 call stdlib_stpmlqt('L','T',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
               else
                 ii=m+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+nb)
                 ctr = ctr - 1_ilp
                 call stdlib_stpmlqt('L','T',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,&
                           1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:nb)
               call stdlib_sgemlqt('L','T',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (left.and.notran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(nb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_sgemlqt('L','N',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (i:i+nb,1:n)
               call stdlib_stpmlqt('L','N',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr * k+1), ldt, c(&
                         1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_stpmlqt('L','N',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.notran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(nb-k))
               ctr = (n-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_stpmlqt('R','N',m , kk, k, 0_ilp, mb, a(1_ilp, ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                  ctr = ctr - 1_ilp
                  call stdlib_stpmlqt('R','N', m, nb-k, k, 0_ilp, mb, a(1_ilp, i), lda,t(1_ilp,ctr*k+1), ldt, &
                            c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_sgemlqt('R','N',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.tran) then
             ! multiply q to the first block of c
              kk = mod((n-k),(nb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_sgemlqt('R','T',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_stpmlqt('R','T',m , nb-k, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp, ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
             ! multiply q to the last block of c
               call stdlib_stpmlqt('R','T',m , kk, k, 0_ilp,mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,1_ilp),&
                          ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_slamswlq

     pure module subroutine stdlib_dlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! DLAMSWLQ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of blocked
     !! elementary reflectors computed by short wide LQ
     !! factorization (DLASWLQ)
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
           integer(ilp) :: i, ii, kk, ctr, lw
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'T' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * mb
           else
             lw = m * mb
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<mb .or. mb<1_ilp) then
             info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, mb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'DLAMSWLQ', -info )
             work(1_ilp) = lw
             return
           else if (lquery) then
             work(1_ilp) = lw
             return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((nb<=k).or.(nb>=max(m,n,k))) then
             call stdlib_dgemlqt( side, trans, m, n, k, mb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
           end if
           if(left.and.tran) then
               ! multiply q to the last block of c
               kk = mod((m-k),(nb-k))
               ctr = (m-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=m-kk+1
                 call stdlib_dtpmlqt('L','T',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
               else
                 ii=m+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+nb)
                 ctr = ctr - 1_ilp
                 call stdlib_dtpmlqt('L','T',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp, ctr*k+1),ldt, c(&
                           1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:nb)
               call stdlib_dgemlqt('L','T',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (left.and.notran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(nb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_dgemlqt('L','N',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (i:i+nb,1:n)
               call stdlib_dtpmlqt('L','N',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_dtpmlqt('L','N',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.notran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(nb-k))
               ctr = (n-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_dtpmlqt('R','N',m , kk, k, 0_ilp, mb, a(1_ilp, ii), lda,t(1_ilp,ctr *k+1), ldt, &
                           c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                  ctr = ctr - 1_ilp
                  call stdlib_dtpmlqt('R','N', m, nb-k, k, 0_ilp, mb, a(1_ilp, i), lda,t(1_ilp,ctr*k+1), ldt, &
                            c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_dgemlqt('R','N',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.tran) then
             ! multiply q to the first block of c
              kk = mod((n-k),(nb-k))
              ctr = 1_ilp
              ii=n-kk+1
              call stdlib_dgemlqt('R','T',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_dtpmlqt('R','T',m , nb-k, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
             ! multiply q to the last block of c
               call stdlib_dtpmlqt('R','T',m , kk, k, 0_ilp,mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,1_ilp),&
                          ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_dlamswlq


     pure module subroutine stdlib_clamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! CLAMSWLQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of blocked
     !! elementary reflectors computed by short wide LQ
     !! factorization (CLASWLQ)
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
           integer(ilp) :: i, ii, kk, lw, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * mb
           else
             lw = m * mb
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<mb .or. mb<1_ilp) then
             info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, mb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'CLAMSWLQ', -info )
             work(1_ilp) = lw
             return
           else if (lquery) then
             work(1_ilp) = lw
             return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((nb<=k).or.(nb>=max(m,n,k))) then
             call stdlib_cgemlqt( side, trans, m, n, k, mb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
           end if
           if(left.and.tran) then
               ! multiply q to the last block of c
               kk = mod((m-k),(nb-k))
               ctr = (m-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=m-kk+1
                 call stdlib_ctpmlqt('L','C',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
               else
                 ii=m+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+nb)
                 ctr = ctr - 1_ilp
                 call stdlib_ctpmlqt('L','C',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,&
                           1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:nb)
               call stdlib_cgemlqt('L','C',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (left.and.notran) then
               ! multiply q to the first block of c
              kk  = mod((m-k),(nb-k))
              ii  = m-kk+1
              ctr = 1_ilp
              call stdlib_cgemlqt('L','N',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (i:i+nb,1:n)
               call stdlib_ctpmlqt('L','N',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp, ctr *k+1), ldt, c(&
                         1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_ctpmlqt('L','N',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp, ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.notran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(nb-k))
               ctr = (n-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_ctpmlqt('R','N',m , kk, k, 0_ilp, mb, a(1_ilp, ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
                   ctr = ctr - 1_ilp
                   call stdlib_ctpmlqt('R','N', m, nb-k, k, 0_ilp, mb, a(1_ilp, i), lda,t(1_ilp,ctr*k+1), ldt,&
                              c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_cgemlqt('R','N',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.tran) then
             ! multiply q to the first block of c
              kk = mod((n-k),(nb-k))
              ii=n-kk+1
              ctr = 1_ilp
              call stdlib_cgemlqt('R','C',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_ctpmlqt('R','C',m , nb-k, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
             ! multiply q to the last block of c
               call stdlib_ctpmlqt('R','C',m , kk, k, 0_ilp,mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,1_ilp),&
                          ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_clamswlq

     pure module subroutine stdlib_zlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
     !! ZLAMSWLQ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of blocked
     !! elementary reflectors computed by short wide LQ
     !! factorization (ZLASWLQ)
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
           integer(ilp) :: i, ii, kk, lw, ctr
           ! External Subroutines 
           ! Executable Statements 
           ! test the input arguments
           lquery  = lwork<0_ilp
           notran  = stdlib_lsame( trans, 'N' )
           tran    = stdlib_lsame( trans, 'C' )
           left    = stdlib_lsame( side, 'L' )
           right   = stdlib_lsame( side, 'R' )
           if (left) then
             lw = n * mb
           else
             lw = m * mb
           end if
           info = 0_ilp
           if( .not.left .and. .not.right ) then
              info = -1_ilp
           else if( .not.tran .and. .not.notran ) then
              info = -2_ilp
           else if( k<0_ilp ) then
             info = -5_ilp
           else if( m<k ) then
             info = -3_ilp
           else if( n<0_ilp ) then
             info = -4_ilp
           else if( k<mb .or. mb<1_ilp) then
             info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
             info = -9_ilp
           else if( ldt<max( 1_ilp, mb) ) then
             info = -11_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -13_ilp
           else if(( lwork<max(1_ilp,lw)).and.(.not.lquery)) then
             info = -15_ilp
           end if
           if( info/=0_ilp ) then
             call stdlib_xerbla( 'ZLAMSWLQ', -info )
             work(1_ilp) = lw
             return
           else if (lquery) then
             work(1_ilp) = lw
             return
           end if
           ! quick return if possible
           if( min(m,n,k)==0_ilp ) then
             return
           end if
           if((nb<=k).or.(nb>=max(m,n,k))) then
             call stdlib_zgemlqt( side, trans, m, n, k, mb, a, lda,t, ldt, c, ldc, work, info)
                       
             return
           end if
           if(left.and.tran) then
               ! multiply q to the last block of c
               kk = mod((m-k),(nb-k))
               ctr = (m-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=m-kk+1
                 call stdlib_ztpmlqt('L','C',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp,ctr*k+1), ldt, c(&
                           1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
               else
                 ii=m+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+nb)
                 ctr = ctr - 1_ilp
                 call stdlib_ztpmlqt('L','C',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr*k+1),ldt, c(1_ilp,&
                           1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:nb)
               call stdlib_zgemlqt('L','C',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (left.and.notran) then
               ! multiply q to the first block of c
              kk = mod((m-k),(nb-k))
              ii=m-kk+1
              ctr = 1_ilp
              call stdlib_zgemlqt('L','N',nb , n, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (i:i+nb,1:n)
               call stdlib_ztpmlqt('L','N',nb-k , n, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp, ctr * k + 1_ilp), ldt, &
                         c(1_ilp,1_ilp), ldc,c(i,1_ilp), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=m) then
               ! multiply q to the last block of c
               call stdlib_ztpmlqt('L','N',kk , n, k, 0_ilp, mb, a(1_ilp,ii), lda,t(1_ilp, ctr * k + 1_ilp), ldt, &
                         c(1_ilp,1_ilp), ldc,c(ii,1_ilp), ldc, work, info )
              end if
           else if(right.and.notran) then
               ! multiply q to the last block of c
               kk = mod((n-k),(nb-k))
               ctr = (n-k)/(nb-k)
               if (kk>0_ilp) then
                 ii=n-kk+1
                 call stdlib_ztpmlqt('R','N',m , kk, k, 0_ilp, mb, a(1_ilp, ii), lda,t(1_ilp, ctr * k + 1_ilp), &
                           ldt, c(1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
               else
                 ii=n+1
               end if
               do i=ii-(nb-k),nb+1,-(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               ctr = ctr - 1_ilp
               call stdlib_ztpmlqt('R','N', m, nb-k, k, 0_ilp, mb, a(1_ilp, i), lda,t(1_ilp, ctr * k + 1_ilp), &
                         ldt, c(1_ilp,1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               end do
               ! multiply q to the first block of c (1:m,1:mb)
               call stdlib_zgemlqt('R','N',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                         info )
           else if (right.and.tran) then
             ! multiply q to the first block of c
              kk = mod((n-k),(nb-k))
              ii=n-kk+1
              call stdlib_zgemlqt('R','C',m , nb, k, mb, a(1_ilp,1_ilp), lda, t,ldt ,c(1_ilp,1_ilp), ldc, work, &
                        info )
              ctr = 1_ilp
              do i=nb+1,ii-nb+k,(nb-k)
               ! multiply q to the current block of c (1:m,i:i+mb)
               call stdlib_ztpmlqt('R','C',m , nb-k, k, 0_ilp,mb, a(1_ilp,i), lda,t(1_ilp,ctr *k+1), ldt, c(1_ilp,&
                         1_ilp), ldc,c(1_ilp,i), ldc, work, info )
               ctr = ctr + 1_ilp
              end do
              if(ii<=n) then
             ! multiply q to the last block of c
               call stdlib_ztpmlqt('R','C',m , kk, k, 0_ilp,mb, a(1_ilp,ii), lda,t(1_ilp, ctr * k + 1_ilp),ldt, c(&
                         1_ilp,1_ilp), ldc,c(1_ilp,ii), ldc, work, info )
              end if
           end if
           work(1_ilp) = lw
           return
     end subroutine stdlib_zlamswlq




     pure module subroutine stdlib_stplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
     !! STPLQT computes a blocked LQ factorization of a real
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, nb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( mb<1_ilp .or. (mb>m .and. m>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPLQT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, m, mb
           ! compute the qr factorization of the current block
              ib = min( m-i+1, mb )
              nb = min( n-l+i+ib-1, n )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = nb-n+l-i+1
              end if
              call stdlib_stplqt2( ib, nb, lb, a(i,i), lda, b( i, 1_ilp ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**t to b(i+ib:m,:) from the right
              if( i+ib<=m ) then
                 call stdlib_stprfb( 'R', 'N', 'F', 'R', m-i-ib+1, nb, ib, lb,b( i, 1_ilp ), ldb, t( &
                           1_ilp, i ), ldt,a( i+ib, i ), lda, b( i+ib, 1_ilp ), ldb,work, m-i-ib+1)
              end if
           end do
           return
     end subroutine stdlib_stplqt

     pure module subroutine stdlib_dtplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
     !! DTPLQT computes a blocked LQ factorization of a real
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, nb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( mb<1_ilp .or. (mb>m .and. m>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPLQT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, m, mb
           ! compute the qr factorization of the current block
              ib = min( m-i+1, mb )
              nb = min( n-l+i+ib-1, n )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = nb-n+l-i+1
              end if
              call stdlib_dtplqt2( ib, nb, lb, a(i,i), lda, b( i, 1_ilp ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**t to b(i+ib:m,:) from the right
              if( i+ib<=m ) then
                 call stdlib_dtprfb( 'R', 'N', 'F', 'R', m-i-ib+1, nb, ib, lb,b( i, 1_ilp ), ldb, t( &
                           1_ilp, i ), ldt,a( i+ib, i ), lda, b( i+ib, 1_ilp ), ldb,work, m-i-ib+1)
              end if
           end do
           return
     end subroutine stdlib_dtplqt


     pure module subroutine stdlib_ctplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
     !! CTPLQT computes a blocked LQ factorization of a complex
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, nb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( mb<1_ilp .or. (mb>m .and. m>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPLQT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, m, mb
           ! compute the qr factorization of the current block
              ib = min( m-i+1, mb )
              nb = min( n-l+i+ib-1, n )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = nb-n+l-i+1
              end if
              call stdlib_ctplqt2( ib, nb, lb, a(i,i), lda, b( i, 1_ilp ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**t to b(i+ib:m,:) from the right
              if( i+ib<=m ) then
                 call stdlib_ctprfb( 'R', 'N', 'F', 'R', m-i-ib+1, nb, ib, lb,b( i, 1_ilp ), ldb, t( &
                           1_ilp, i ), ldt,a( i+ib, i ), lda, b( i+ib, 1_ilp ), ldb,work, m-i-ib+1)
              end if
           end do
           return
     end subroutine stdlib_ctplqt

     pure module subroutine stdlib_ztplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
     !! ZTPLQT computes a blocked LQ factorization of a complex
     !! "triangular-pentagonal" matrix C, which is composed of a
     !! triangular block A and pentagonal block B, using the compact
     !! WY representation for Q.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ib, lb, nb, iinfo
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( l<0_ilp .or. (l>min(m,n) .and. min(m,n)>=0_ilp)) then
              info = -3_ilp
           else if( mb<1_ilp .or. (mb>m .and. m>0_ilp)) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -8_ilp
           else if( ldt<mb ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPLQT', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 ) return
           do i = 1, m, mb
           ! compute the qr factorization of the current block
              ib = min( m-i+1, mb )
              nb = min( n-l+i+ib-1, n )
              if( i>=l ) then
                 lb = 0_ilp
              else
                 lb = nb-n+l-i+1
              end if
              call stdlib_ztplqt2( ib, nb, lb, a(i,i), lda, b( i, 1_ilp ), ldb,t(1_ilp, i ), ldt, iinfo )
                        
           ! update by applying h**t to b(i+ib:m,:) from the right
              if( i+ib<=m ) then
                 call stdlib_ztprfb( 'R', 'N', 'F', 'R', m-i-ib+1, nb, ib, lb,b( i, 1_ilp ), ldb, t( &
                           1_ilp, i ), ldt,a( i+ib, i ), lda, b( i+ib, 1_ilp ), ldb,work, m-i-ib+1)
              end if
           end do
           return
     end subroutine stdlib_ztplqt




     pure module subroutine stdlib_stplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! STPLQT2 computes a LQ a factorization of a real "triangular-pentagonal"
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
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPLQT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, m
              ! generate elementary reflector h(i) to annihilate b(i,:)
              p = n-l+min( l, i )
              call stdlib_slarfg( p+1, a( i, i ), b( i, 1_ilp ), ldb, t( 1_ilp, i ) )
              if( i<m ) then
                 ! w(m-i:1) := c(i+1:m,i:n) * c(i,i:n) [use w = t(m,:)]
                 do j = 1, m-i
                    t( m, j ) = (a( i+j, i ))
                 end do
                 call stdlib_sgemv( 'N', m-i, p, one, b( i+1, 1_ilp ), ldb,b( i, 1_ilp ), ldb, one, t( m, &
                           1_ilp ), ldt )
                 ! c(i+1:m,i:n) = c(i+1:m,i:n) + alpha * c(i,i:n)*w(m-1:1)^h
                 alpha = -(t( 1_ilp, i ))
                 do j = 1, m-i
                    a( i+j, i ) = a( i+j, i ) + alpha*(t( m, j ))
                 end do
                 call stdlib_sger( m-i, p, alpha,  t( m, 1_ilp ), ldt,b( i, 1_ilp ), ldb, b( i+1, 1_ilp ), &
                           ldb )
              end if
           end do
           do i = 2, m
              ! t(i,1:i-1) := c(i:i-1,1:n) * (alpha * c(i,i:n)^h)
              alpha = -t( 1_ilp, i )
              do j = 1, i-1
                 t( i, j ) = zero
              end do
              p = min( i-1, l )
              np = min( n-l+1, n )
              mp = min( p+1, m )
              ! triangular part of b2
              do j = 1, p
                 t( i, j ) = alpha*b( i, n-l+j )
              end do
              call stdlib_strmv( 'L', 'N', 'N', p, b( 1_ilp, np ), ldb,t( i, 1_ilp ), ldt )
              ! rectangular part of b2
              call stdlib_sgemv( 'N', i-1-p, l,  alpha, b( mp, np ), ldb,b( i, np ), ldb, zero, t(&
                         i,mp ), ldt )
              ! b1
              call stdlib_sgemv( 'N', i-1, n-l, alpha, b, ldb, b( i, 1_ilp ), ldb,one, t( i, 1_ilp ), ldt &
                        )
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(i,1:i-1)
             call stdlib_strmv( 'L', 'T', 'N', i-1, t, ldt, t( i, 1_ilp ), ldt )
              ! t(i,i) = tau(i)
              t( i, i ) = t( 1_ilp, i )
              t( 1_ilp, i ) = zero
           end do
           do i=1,m
              do j= i+1,m
                 t(i,j)=t(j,i)
                 t(j,i)= zero
              end do
           end do
     end subroutine stdlib_stplqt2

     pure module subroutine stdlib_dtplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! DTPLQT2 computes a LQ a factorization of a real "triangular-pentagonal"
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
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPLQT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, m
              ! generate elementary reflector h(i) to annihilate b(i,:)
              p = n-l+min( l, i )
              call stdlib_dlarfg( p+1, a( i, i ), b( i, 1_ilp ), ldb, t( 1_ilp, i ) )
              if( i<m ) then
                 ! w(m-i:1) := c(i+1:m,i:n) * c(i,i:n) [use w = t(m,:)]
                 do j = 1, m-i
                    t( m, j ) = (a( i+j, i ))
                 end do
                 call stdlib_dgemv( 'N', m-i, p, one, b( i+1, 1_ilp ), ldb,b( i, 1_ilp ), ldb, one, t( m, &
                           1_ilp ), ldt )
                 ! c(i+1:m,i:n) = c(i+1:m,i:n) + alpha * c(i,i:n)*w(m-1:1)^h
                 alpha = -(t( 1_ilp, i ))
                 do j = 1, m-i
                    a( i+j, i ) = a( i+j, i ) + alpha*(t( m, j ))
                 end do
                 call stdlib_dger( m-i, p, alpha,  t( m, 1_ilp ), ldt,b( i, 1_ilp ), ldb, b( i+1, 1_ilp ), &
                           ldb )
              end if
           end do
           do i = 2, m
              ! t(i,1:i-1) := c(i:i-1,1:n) * (alpha * c(i,i:n)^h)
              alpha = -t( 1_ilp, i )
              do j = 1, i-1
                 t( i, j ) = zero
              end do
              p = min( i-1, l )
              np = min( n-l+1, n )
              mp = min( p+1, m )
              ! triangular part of b2
              do j = 1, p
                 t( i, j ) = alpha*b( i, n-l+j )
              end do
              call stdlib_dtrmv( 'L', 'N', 'N', p, b( 1_ilp, np ), ldb,t( i, 1_ilp ), ldt )
              ! rectangular part of b2
              call stdlib_dgemv( 'N', i-1-p, l,  alpha, b( mp, np ), ldb,b( i, np ), ldb, zero, t(&
                         i,mp ), ldt )
              ! b1
              call stdlib_dgemv( 'N', i-1, n-l, alpha, b, ldb, b( i, 1_ilp ), ldb,one, t( i, 1_ilp ), ldt &
                        )
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(i,1:i-1)
             call stdlib_dtrmv( 'L', 'T', 'N', i-1, t, ldt, t( i, 1_ilp ), ldt )
              ! t(i,i) = tau(i)
              t( i, i ) = t( 1_ilp, i )
              t( 1_ilp, i ) = zero
           end do
           do i=1,m
              do j= i+1,m
                 t(i,j)=t(j,i)
                 t(j,i)= zero
              end do
           end do
     end subroutine stdlib_dtplqt2


     pure module subroutine stdlib_ctplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! CTPLQT2 computes a LQ a factorization of a complex "triangular-pentagonal"
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
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPLQT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, m
              ! generate elementary reflector h(i) to annihilate b(i,:)
              p = n-l+min( l, i )
              call stdlib_clarfg( p+1, a( i, i ), b( i, 1_ilp ), ldb, t( 1_ilp, i ) )
              t(1_ilp,i)=conjg(t(1_ilp,i))
              if( i<m ) then
                 do j = 1, p
                    b( i, j ) = conjg(b(i,j))
                 end do
                 ! w(m-i:1) := c(i+1:m,i:n) * c(i,i:n) [use w = t(m,:)]
                 do j = 1, m-i
                    t( m, j ) = (a( i+j, i ))
                 end do
                 call stdlib_cgemv( 'N', m-i, p, cone, b( i+1, 1_ilp ), ldb,b( i, 1_ilp ), ldb, cone, t( &
                           m, 1_ilp ), ldt )
                 ! c(i+1:m,i:n) = c(i+1:m,i:n) + alpha * c(i,i:n)*w(m-1:1)^h
                 alpha = -(t( 1_ilp, i ))
                 do j = 1, m-i
                    a( i+j, i ) = a( i+j, i ) + alpha*(t( m, j ))
                 end do
                 call stdlib_cgerc( m-i, p, (alpha),  t( m, 1_ilp ), ldt,b( i, 1_ilp ), ldb, b( i+1, 1_ilp ), &
                           ldb )
                 do j = 1, p
                    b( i, j ) = conjg(b(i,j))
                 end do
              end if
           end do
           do i = 2, m
              ! t(i,1:i-1) := c(i:i-1,1:n)**h * (alpha * c(i,i:n))
              alpha = -(t( 1_ilp, i ))
              do j = 1, i-1
                 t( i, j ) = czero
              end do
              p = min( i-1, l )
              np = min( n-l+1, n )
              mp = min( p+1, m )
              do j = 1, n-l+p
                b(i,j)=conjg(b(i,j))
              end do
              ! triangular part of b2
              do j = 1, p
                 t( i, j ) = (alpha*b( i, n-l+j ))
              end do
              call stdlib_ctrmv( 'L', 'N', 'N', p, b( 1_ilp, np ), ldb,t( i, 1_ilp ), ldt )
              ! rectangular part of b2
              call stdlib_cgemv( 'N', i-1-p, l,  alpha, b( mp, np ), ldb,b( i, np ), ldb, czero, &
                        t( i,mp ), ldt )
              ! b1
              call stdlib_cgemv( 'N', i-1, n-l, alpha, b, ldb, b( i, 1_ilp ), ldb,cone, t( i, 1_ilp ), &
                        ldt )
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(i,1:i-1)
              do j = 1, i-1
                 t(i,j)=conjg(t(i,j))
              end do
              call stdlib_ctrmv( 'L', 'C', 'N', i-1, t, ldt, t( i, 1_ilp ), ldt )
              do j = 1, i-1
                 t(i,j)=conjg(t(i,j))
              end do
              do j = 1, n-l+p
                 b(i,j)=conjg(b(i,j))
              end do
              ! t(i,i) = tau(i)
              t( i, i ) = t( 1_ilp, i )
              t( 1_ilp, i ) = czero
           end do
           do i=1,m
              do j= i+1,m
                 t(i,j)=(t(j,i))
                 t(j,i)=czero
              end do
           end do
     end subroutine stdlib_ctplqt2

     pure module subroutine stdlib_ztplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
     !! ZTPLQT2 computes a LQ a factorization of a complex "triangular-pentagonal"
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
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -7_ilp
           else if( ldt<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPLQT2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 ) return
           do i = 1, m
              ! generate elementary reflector h(i) to annihilate b(i,:)
              p = n-l+min( l, i )
              call stdlib_zlarfg( p+1, a( i, i ), b( i, 1_ilp ), ldb, t( 1_ilp, i ) )
              t(1_ilp,i)=conjg(t(1_ilp,i))
              if( i<m ) then
                 do j = 1, p
                    b( i, j ) = conjg(b(i,j))
                 end do
                 ! w(m-i:1) := c(i+1:m,i:n) * c(i,i:n) [use w = t(m,:)]
                 do j = 1, m-i
                    t( m, j ) = (a( i+j, i ))
                 end do
                 call stdlib_zgemv( 'N', m-i, p, cone, b( i+1, 1_ilp ), ldb,b( i, 1_ilp ), ldb, cone, t( &
                           m, 1_ilp ), ldt )
                 ! c(i+1:m,i:n) = c(i+1:m,i:n) + alpha * c(i,i:n)*w(m-1:1)^h
                 alpha = -(t( 1_ilp, i ))
                 do j = 1, m-i
                    a( i+j, i ) = a( i+j, i ) + alpha*(t( m, j ))
                 end do
                 call stdlib_zgerc( m-i, p, (alpha),  t( m, 1_ilp ), ldt,b( i, 1_ilp ), ldb, b( i+1, 1_ilp ), &
                           ldb )
                 do j = 1, p
                    b( i, j ) = conjg(b(i,j))
                 end do
              end if
           end do
           do i = 2, m
              ! t(i,1:i-1) := c(i:i-1,1:n)**h * (alpha * c(i,i:n))
              alpha = -(t( 1_ilp, i ))
              do j = 1, i-1
                 t( i, j ) = czero
              end do
              p = min( i-1, l )
              np = min( n-l+1, n )
              mp = min( p+1, m )
              do j = 1, n-l+p
                b(i,j)=conjg(b(i,j))
              end do
              ! triangular part of b2
              do j = 1, p
                 t( i, j ) = (alpha*b( i, n-l+j ))
              end do
              call stdlib_ztrmv( 'L', 'N', 'N', p, b( 1_ilp, np ), ldb,t( i, 1_ilp ), ldt )
              ! rectangular part of b2
              call stdlib_zgemv( 'N', i-1-p, l,  alpha, b( mp, np ), ldb,b( i, np ), ldb, czero, &
                        t( i,mp ), ldt )
              ! b1
              call stdlib_zgemv( 'N', i-1, n-l, alpha, b, ldb, b( i, 1_ilp ), ldb,cone, t( i, 1_ilp ), &
                        ldt )
              ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(i,1:i-1)
              do j = 1, i-1
                 t(i,j)=conjg(t(i,j))
              end do
              call stdlib_ztrmv( 'L', 'C', 'N', i-1, t, ldt, t( i, 1_ilp ), ldt )
              do j = 1, i-1
                 t(i,j)=conjg(t(i,j))
              end do
              do j = 1, n-l+p
                 b(i,j)=conjg(b(i,j))
              end do
              ! t(i,i) = tau(i)
              t( i, i ) = t( 1_ilp, i )
              t( 1_ilp, i ) = czero
           end do
           do i=1,m
              do j= i+1,m
                 t(i,j)=(t(j,i))
                 t(j,i)=czero
              end do
           end do
     end subroutine stdlib_ztplqt2




     pure module subroutine stdlib_stpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! STPMLQT applies a real orthogonal matrix Q obtained from a
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
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           ! Array Arguments 
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, nb, lb, kf, ldaq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldaq = max( 1_ilp, k )
           else if ( right ) then
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<k ) then
              info = -9_ilp
           else if( ldt<mb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_stprfb( 'L', 'T', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_stprfb( 'R', 'N', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_stprfb( 'L', 'N', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_stprfb( 'R', 'T', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_stpmlqt

     pure module subroutine stdlib_dtpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
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
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           ! Array Arguments 
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, nb, lb, kf, ldaq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'T' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldaq = max( 1_ilp, k )
           else if ( right ) then
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<k ) then
              info = -9_ilp
           else if( ldt<mb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_dtprfb( 'L', 'T', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_dtprfb( 'R', 'N', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_dtprfb( 'L', 'N', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_dtprfb( 'R', 'T', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_dtpmlqt


     pure module subroutine stdlib_ctpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! CTPMLQT applies a complex unitary matrix Q obtained from a
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
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           ! Array Arguments 
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, nb, lb, kf, ldaq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldaq = max( 1_ilp, k )
           else if ( right ) then
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<k ) then
              info = -9_ilp
           else if( ldt<mb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_ctprfb( 'L', 'C', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_ctprfb( 'R', 'N', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_ctprfb( 'L', 'N', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_ctprfb( 'R', 'C', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_ctpmlqt

     pure module subroutine stdlib_ztpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
     !! ZTPMLQT applies a complex unitary matrix Q obtained from a
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
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           ! Array Arguments 
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, right, tran, notran
           integer(ilp) :: i, ib, nb, lb, kf, ldaq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! Test The Input Arguments 
           info   = 0_ilp
           left   = stdlib_lsame( side,  'L' )
           right  = stdlib_lsame( side,  'R' )
           tran   = stdlib_lsame( trans, 'C' )
           notran = stdlib_lsame( trans, 'N' )
           if ( left ) then
              ldaq = max( 1_ilp, k )
           else if ( right ) then
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
           else if( mb<1_ilp .or. (mb>k .and. k>0_ilp) ) then
              info = -7_ilp
           else if( ldv<k ) then
              info = -9_ilp
           else if( ldt<mb ) then
              info = -11_ilp
           else if( lda<ldaq ) then
              info = -13_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPMLQT', -info )
              return
           end if
           ! Quick Return If Possible 
           if( m==0 .or. n==0 .or. k==0 ) return
           if( left .and. notran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_ztprfb( 'L', 'C', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. tran ) then
              do i = 1, k, mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_ztprfb( 'R', 'N', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           else if( left .and. tran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( m-l+i+ib-1, m )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = 0_ilp
                 end if
                 call stdlib_ztprfb( 'L', 'N', 'F', 'R', nb, n, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( i, 1_ilp ), lda, b, ldb, work, ib )
              end do
           else if( right .and. notran ) then
              kf = ((k-1)/mb)*mb+1
              do i = kf, 1, -mb
                 ib = min( mb, k-i+1 )
                 nb = min( n-l+i+ib-1, n )
                 if( i>=l ) then
                    lb = 0_ilp
                 else
                    lb = nb-n+l-i+1
                 end if
                 call stdlib_ztprfb( 'R', 'C', 'F', 'R', m, nb, ib, lb,v( i, 1_ilp ), ldv, t( 1_ilp, i ), &
                           ldt,a( 1_ilp, i ), lda, b, ldb, work, m )
              end do
           end if
           return
     end subroutine stdlib_ztpmlqt




     pure module subroutine stdlib_sgeqlf( m, n, a, lda, tau, work, lwork, info )
     !! SGEQLF computes a QL factorization of a real M-by-N matrix A:
     !! A = Q * L.
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
                 nb = stdlib_ilaenv( 1_ilp, 'SGEQLF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEQLF', -info )
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
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGEQLF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGEQLF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the ql factorization of the current block
                 ! a(1:m-k+i+ib-1,n-k+i:n-k+i+ib-1)
                 call stdlib_sgeql2( m-k+i+ib-1, ib, a( 1_ilp, n-k+i ), lda, tau( i ),work, iinfo )
                           
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_slarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h**t to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_slarfb( 'LEFT', 'TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-1, &
                    n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_sgeql2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgeqlf

     pure module subroutine stdlib_dgeqlf( m, n, a, lda, tau, work, lwork, info )
     !! DGEQLF computes a QL factorization of a real M-by-N matrix A:
     !! A = Q * L.
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
                 nb = stdlib_ilaenv( 1_ilp, 'DGEQLF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEQLF', -info )
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
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGEQLF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGEQLF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the ql factorization of the current block
                 ! a(1:m-k+i+ib-1,n-k+i:n-k+i+ib-1)
                 call stdlib_dgeql2( m-k+i+ib-1, ib, a( 1_ilp, n-k+i ), lda, tau( i ),work, iinfo )
                           
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_dlarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h**t to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_dlarfb( 'LEFT', 'TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-1, &
                    n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_dgeql2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgeqlf


     pure module subroutine stdlib_cgeqlf( m, n, a, lda, tau, work, lwork, info )
     !! CGEQLF computes a QL factorization of a complex M-by-N matrix A:
     !! A = Q * L.
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
                 nb = stdlib_ilaenv( 1_ilp, 'CGEQLF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEQLF', -info )
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
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGEQLF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGEQLF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the ql factorization of the current block
                 ! a(1:m-k+i+ib-1,n-k+i:n-k+i+ib-1)
                 call stdlib_cgeql2( m-k+i+ib-1, ib, a( 1_ilp, n-k+i ), lda, tau( i ),work, iinfo )
                           
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_clarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h**h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_clarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'BACKWARD','COLUMNWISE', m-&
                    k+i+ib-1, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), &
                              ldwork )
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_cgeql2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgeqlf

     pure module subroutine stdlib_zgeqlf( m, n, a, lda, tau, work, lwork, info )
     !! ZGEQLF computes a QL factorization of a complex M-by-N matrix A:
     !! A = Q * L.
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
                 nb = stdlib_ilaenv( 1_ilp, 'ZGEQLF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEQLF', -info )
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
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGEQLF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGEQLF', ' ', m, n, -1_ilp,-1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code initially.
              ! the last kk columns are handled by the block method.
              ki = ( ( k-nx-1 ) / nb )*nb
              kk = min( k, ki+nb )
              do i = k - kk + ki + 1, k - kk + 1, -nb
                 ib = min( k-i+1, nb )
                 ! compute the ql factorization of the current block
                 ! a(1:m-k+i+ib-1,n-k+i:n-k+i+ib-1)
                 call stdlib_zgeql2( m-k+i+ib-1, ib, a( 1_ilp, n-k+i ), lda, tau( i ),work, iinfo )
                           
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_zlarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h**h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_zlarfb( 'LEFT', 'CONJUGATE TRANSPOSE', 'BACKWARD','COLUMNWISE', m-&
                    k+i+ib-1, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), &
                              ldwork )
                 end if
              end do
              mu = m - k + i + nb - 1_ilp
              nu = n - k + i + nb - 1_ilp
           else
              mu = m
              nu = n
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp .and. nu>0_ilp )call stdlib_zgeql2( mu, nu, a, lda, tau, work, iinfo )
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgeqlf




     pure module subroutine stdlib_sgeql2( m, n, a, lda, tau, work, info )
     !! SGEQL2 computes a QL factorization of a real m by n matrix A:
     !! A = Q * L.
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
              call stdlib_xerbla( 'SGEQL2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(1:m-k+i-1,n-k+i)
              call stdlib_slarfg( m-k+i, a( m-k+i, n-k+i ), a( 1_ilp, n-k+i ), 1_ilp,tau( i ) )
              ! apply h(i) to a(1:m-k+i,1:n-k+i-1) from the left
              aii = a( m-k+i, n-k+i )
              a( m-k+i, n-k+i ) = one
              call stdlib_slarf( 'LEFT', m-k+i, n-k+i-1, a( 1_ilp, n-k+i ), 1_ilp, tau( i ),a, lda, work )
                        
              a( m-k+i, n-k+i ) = aii
           end do
           return
     end subroutine stdlib_sgeql2

     pure module subroutine stdlib_dgeql2( m, n, a, lda, tau, work, info )
     !! DGEQL2 computes a QL factorization of a real m by n matrix A:
     !! A = Q * L.
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
              call stdlib_xerbla( 'DGEQL2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(1:m-k+i-1,n-k+i)
              call stdlib_dlarfg( m-k+i, a( m-k+i, n-k+i ), a( 1_ilp, n-k+i ), 1_ilp,tau( i ) )
              ! apply h(i) to a(1:m-k+i,1:n-k+i-1) from the left
              aii = a( m-k+i, n-k+i )
              a( m-k+i, n-k+i ) = one
              call stdlib_dlarf( 'LEFT', m-k+i, n-k+i-1, a( 1_ilp, n-k+i ), 1_ilp, tau( i ),a, lda, work )
                        
              a( m-k+i, n-k+i ) = aii
           end do
           return
     end subroutine stdlib_dgeql2


     pure module subroutine stdlib_cgeql2( m, n, a, lda, tau, work, info )
     !! CGEQL2 computes a QL factorization of a complex m by n matrix A:
     !! A = Q * L.
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
              call stdlib_xerbla( 'CGEQL2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(1:m-k+i-1,n-k+i)
              alpha = a( m-k+i, n-k+i )
              call stdlib_clarfg( m-k+i, alpha, a( 1_ilp, n-k+i ), 1_ilp, tau( i ) )
              ! apply h(i)**h to a(1:m-k+i,1:n-k+i-1) from the left
              a( m-k+i, n-k+i ) = cone
              call stdlib_clarf( 'LEFT', m-k+i, n-k+i-1, a( 1_ilp, n-k+i ), 1_ilp,conjg( tau( i ) ), a, &
                        lda, work )
              a( m-k+i, n-k+i ) = alpha
           end do
           return
     end subroutine stdlib_cgeql2

     pure module subroutine stdlib_zgeql2( m, n, a, lda, tau, work, info )
     !! ZGEQL2 computes a QL factorization of a complex m by n matrix A:
     !! A = Q * L.
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
              call stdlib_xerbla( 'ZGEQL2', -info )
              return
           end if
           k = min( m, n )
           do i = k, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! a(1:m-k+i-1,n-k+i)
              alpha = a( m-k+i, n-k+i )
              call stdlib_zlarfg( m-k+i, alpha, a( 1_ilp, n-k+i ), 1_ilp, tau( i ) )
              ! apply h(i)**h to a(1:m-k+i,1:n-k+i-1) from the left
              a( m-k+i, n-k+i ) = cone
              call stdlib_zlarf( 'LEFT', m-k+i, n-k+i-1, a( 1_ilp, n-k+i ), 1_ilp,conjg( tau( i ) ), a, &
                        lda, work )
              a( m-k+i, n-k+i ) = alpha
           end do
           return
     end subroutine stdlib_zgeql2




     pure module subroutine stdlib_cungql( m, n, k, a, lda, tau, work, lwork, info )
     !! CUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
     !! which is defined as the last N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by CGEQLF.
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
           integer(ilp) :: i, ib, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CUNGQL', ' ', m, n, k, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGQL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CUNGQL', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNGQL', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk columns are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(m-kk+1:m,1:n-kk) to czero.
              do j = 1, n - kk
                 do i = m - kk + 1, m
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_cung2l( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_clarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_clarfb( 'LEFT', 'NO TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-&
                    1_ilp, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
                 ! apply h to rows 1:m-k+i+ib-1 of current block
                 call stdlib_cung2l( m-k+i+ib-1, ib, ib, a( 1_ilp, n-k+i ), lda,tau( i ), work, iinfo &
                           )
                 ! set rows m-k+i+ib:m of current block to czero
                 do j = n - k + i, n - k + i + ib - 1
                    do l = m - k + i + ib, m
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cungql

     pure module subroutine stdlib_zungql( m, n, k, a, lda, tau, work, lwork, info )
     !! ZUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
     !! which is defined as the last N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by ZGEQLF.
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
           integer(ilp) :: i, ib, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNGQL', ' ', m, n, k, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGQL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZUNGQL', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNGQL', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk columns are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(m-kk+1:m,1:n-kk) to czero.
              do j = 1, n - kk
                 do i = m - kk + 1, m
                    a( i, j ) = czero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_zung2l( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_zlarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_zlarfb( 'LEFT', 'NO TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-&
                    1_ilp, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
                 ! apply h to rows 1:m-k+i+ib-1 of current block
                 call stdlib_zung2l( m-k+i+ib-1, ib, ib, a( 1_ilp, n-k+i ), lda,tau( i ), work, iinfo &
                           )
                 ! set rows m-k+i+ib:m of current block to czero
                 do j = n - k + i, n - k + i + ib - 1
                    do l = m - k + i + ib, m
                       a( l, j ) = czero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zungql




     pure module subroutine stdlib_cunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! CUNMQL overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by CGEQLF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, nq ) ) then
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
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CUNMQL', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMQL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! determine the block size
           nbmin = 2_ilp
           ldwork = nw
           if( nb>1_ilp .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CUNMQL', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_cunm2l( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_clarft( 'BACKWARD', 'COLUMNWISE', nq-k+i+ib-1, ib,a( 1_ilp, i ), lda, &
                           tau( i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**h is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**h
                 call stdlib_clarfb( side, trans, 'BACKWARD', 'COLUMNWISE', mi, ni,ib, a( 1_ilp, i ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmql

     pure module subroutine stdlib_zunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! ZUNMQL overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by ZGEQLF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, nq ) ) then
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
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZUNMQL', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMQL', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZUNMQL', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_zunm2l( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_zlarft( 'BACKWARD', 'COLUMNWISE', nq-k+i+ib-1, ib,a( 1_ilp, i ), lda, &
                           tau( i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**h is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**h
                 call stdlib_zlarfb( side, trans, 'BACKWARD', 'COLUMNWISE', mi, ni,ib, a( 1_ilp, i ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmql




     pure module subroutine stdlib_cung2l( m, n, k, a, lda, tau, work, info )
     !! CUNG2L generates an m by n complex matrix Q with orthonormal columns,
     !! which is defined as the last n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by CGEQLF.
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
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNG2L', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns 1:n-k to columns of the unit matrix
           do j = 1, n - k
              do l = 1, m
                 a( l, j ) = czero
              end do
              a( m-n+j, j ) = cone
           end do
           do i = 1, k
              ii = n - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the left
              a( m-n+ii, ii ) = cone
              call stdlib_clarf( 'LEFT', m-n+ii, ii-1, a( 1_ilp, ii ), 1_ilp, tau( i ), a,lda, work )
                        
              call stdlib_cscal( m-n+ii-1, -tau( i ), a( 1_ilp, ii ), 1_ilp )
              a( m-n+ii, ii ) = cone - tau( i )
              ! set a(m-k+i+1:m,n-k+i) to czero
              do l = m - n + ii + 1, m
                 a( l, ii ) = czero
              end do
           end do
           return
     end subroutine stdlib_cung2l

     pure module subroutine stdlib_zung2l( m, n, k, a, lda, tau, work, info )
     !! ZUNG2L generates an m by n complex matrix Q with orthonormal columns,
     !! which is defined as the last n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by ZGEQLF.
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
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNG2L', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns 1:n-k to columns of the unit matrix
           do j = 1, n - k
              do l = 1, m
                 a( l, j ) = czero
              end do
              a( m-n+j, j ) = cone
           end do
           do i = 1, k
              ii = n - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the left
              a( m-n+ii, ii ) = cone
              call stdlib_zlarf( 'LEFT', m-n+ii, ii-1, a( 1_ilp, ii ), 1_ilp, tau( i ), a,lda, work )
                        
              call stdlib_zscal( m-n+ii-1, -tau( i ), a( 1_ilp, ii ), 1_ilp )
              a( m-n+ii, ii ) = cone - tau( i )
              ! set a(m-k+i+1:m,n-k+i) to czero
              do l = m - n + ii + 1, m
                 a( l, ii ) = czero
              end do
           end do
           return
     end subroutine stdlib_zung2l




     pure module subroutine stdlib_cunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! CUNM2L overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by CGEQLF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNM2L', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran .or. .not.left .and. .not.notran ) ) then
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
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              aii = a( nq-k+i, i )
              a( nq-k+i, i ) = cone
              call stdlib_clarf( side, mi, ni, a( 1_ilp, i ), 1_ilp, taui, c, ldc, work )
              a( nq-k+i, i ) = aii
           end do
           return
     end subroutine stdlib_cunm2l

     pure module subroutine stdlib_zunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! ZUNM2L overwrites the general complex m-by-n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by ZGEQLF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNM2L', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran .or. .not.left .and. .not.notran ) ) then
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
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              aii = a( nq-k+i, i )
              a( nq-k+i, i ) = cone
              call stdlib_zlarf( side, mi, ni, a( 1_ilp, i ), 1_ilp, taui, c, ldc, work )
              a( nq-k+i, i ) = aii
           end do
           return
     end subroutine stdlib_zunm2l




     pure module subroutine stdlib_sorgql( m, n, k, a, lda, tau, work, lwork, info )
     !! SORGQL generates an M-by-N real matrix Q with orthonormal columns,
     !! which is defined as the last N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by SGEQLF.
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
           integer(ilp) :: i, ib, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'SORGQL', ' ', m, n, k, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGQL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SORGQL', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORGQL', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk columns are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(m-kk+1:m,1:n-kk) to zero.
              do j = 1, n - kk
                 do i = m - kk + 1, m
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_sorg2l( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_slarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_slarfb( 'LEFT', 'NO TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-&
                    1_ilp, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
                 ! apply h to rows 1:m-k+i+ib-1 of current block
                 call stdlib_sorg2l( m-k+i+ib-1, ib, ib, a( 1_ilp, n-k+i ), lda,tau( i ), work, iinfo &
                           )
                 ! set rows m-k+i+ib:m of current block to zero
                 do j = n - k + i, n - k + i + ib - 1
                    do l = m - k + i + ib, m
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sorgql

     pure module subroutine stdlib_dorgql( m, n, k, a, lda, tau, work, lwork, info )
     !! DORGQL generates an M-by-N real matrix Q with orthonormal columns,
     !! which is defined as the last N columns of a product of K elementary
     !! reflectors of order M
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by DGEQLF.
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
           integer(ilp) :: i, ib, iinfo, iws, j, kk, l, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DORGQL', ' ', m, n, k, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
                 info = -8_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGQL', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              return
           end if
           nbmin = 2_ilp
           nx = 0_ilp
           iws = n
           if( nb>1_ilp .and. nb<k ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DORGQL', ' ', m, n, k, -1_ilp ) )
              if( nx<k ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORGQL', ' ', m, n, k, -1_ilp ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<k .and. nx<k ) then
              ! use blocked code after the first block.
              ! the last kk columns are handled by the block method.
              kk = min( k, ( ( k-nx+nb-1 ) / nb )*nb )
              ! set a(m-kk+1:m,1:n-kk) to zero.
              do j = 1, n - kk
                 do i = m - kk + 1, m
                    a( i, j ) = zero
                 end do
              end do
           else
              kk = 0_ilp
           end if
           ! use unblocked code for the first or only block.
           call stdlib_dorg2l( m-kk, n-kk, k-kk, a, lda, tau, work, iinfo )
           if( kk>0_ilp ) then
              ! use blocked code
              do i = k - kk + 1, k, nb
                 ib = min( nb, k-i+1 )
                 if( n-k+i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_dlarft( 'BACKWARD', 'COLUMNWISE', m-k+i+ib-1, ib,a( 1_ilp, n-k+i ), &
                              lda, tau( i ), work, ldwork )
                    ! apply h to a(1:m-k+i+ib-1,1:n-k+i-1) from the left
                    call stdlib_dlarfb( 'LEFT', 'NO TRANSPOSE', 'BACKWARD','COLUMNWISE', m-k+i+ib-&
                    1_ilp, n-k+i-1, ib,a( 1_ilp, n-k+i ), lda, work, ldwork, a, lda,work( ib+1 ), ldwork )
                              
                 end if
                 ! apply h to rows 1:m-k+i+ib-1 of current block
                 call stdlib_dorg2l( m-k+i+ib-1, ib, ib, a( 1_ilp, n-k+i ), lda,tau( i ), work, iinfo &
                           )
                 ! set rows m-k+i+ib:m of current block to zero
                 do j = n - k + i, n - k + i + ib - 1
                    do l = m - k + i + ib, m
                       a( l, j ) = zero
                    end do
                 end do
              end do
           end if
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dorgql




     pure module subroutine stdlib_sormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! SORMQL overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by SGEQLF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, nq ) ) then
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
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'SORMQL', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMQL', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SORMQL', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_sorm2l( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_slarft( 'BACKWARD', 'COLUMNWISE', nq-k+i+ib-1, ib,a( 1_ilp, i ), lda, &
                           tau( i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**t is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**t
                 call stdlib_slarfb( side, trans, 'BACKWARD', 'COLUMNWISE', mi, ni,ib, a( 1_ilp, i ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormql

     pure module subroutine stdlib_dormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
     !! DORMQL overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by DGEQLF. Q is of order M if SIDE = 'L' and of order N
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
           else if( lda<max( 1_ilp, nq ) ) then
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
                 nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'DORMQL', side // trans, m, n,k, -1_ilp ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMQL', -info )
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
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DORMQL', side // trans, m, n, k,-1_ilp ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_dorm2l( side, trans, m, n, k, a, lda, tau, c, ldc, work,iinfo )
           else
              ! use blocked code
              iwt = 1_ilp + nw*nb
              if( ( left .and. notran ) .or.( .not.left .and. .not.notran ) ) then
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
              do i = i1, i2, i3
                 ib = min( nb, k-i+1 )
                 ! form the triangular factor of the block reflector
                 ! h = h(i+ib-1) . . . h(i+1) h(i)
                 call stdlib_dlarft( 'BACKWARD', 'COLUMNWISE', nq-k+i+ib-1, ib,a( 1_ilp, i ), lda, &
                           tau( i ), work( iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(1:m-k+i+ib-1,1:n)
                    mi = m - k + i + ib - 1_ilp
                 else
                    ! h or h**t is applied to c(1:m,1:n-k+i+ib-1)
                    ni = n - k + i + ib - 1_ilp
                 end if
                 ! apply h or h**t
                 call stdlib_dlarfb( side, trans, 'BACKWARD', 'COLUMNWISE', mi, ni,ib, a( 1_ilp, i ), &
                           lda, work( iwt ), ldt, c, ldc,work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormql




     pure module subroutine stdlib_sorg2l( m, n, k, a, lda, tau, work, info )
     !! SORG2L generates an m by n real matrix Q with orthonormal columns,
     !! which is defined as the last n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by SGEQLF.
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
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORG2L', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns 1:n-k to columns of the unit matrix
           do j = 1, n - k
              do l = 1, m
                 a( l, j ) = zero
              end do
              a( m-n+j, j ) = one
           end do
           do i = 1, k
              ii = n - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the left
              a( m-n+ii, ii ) = one
              call stdlib_slarf( 'LEFT', m-n+ii, ii-1, a( 1_ilp, ii ), 1_ilp, tau( i ), a,lda, work )
                        
              call stdlib_sscal( m-n+ii-1, -tau( i ), a( 1_ilp, ii ), 1_ilp )
              a( m-n+ii, ii ) = one - tau( i )
              ! set a(m-k+i+1:m,n-k+i) to zero
              do l = m - n + ii + 1, m
                 a( l, ii ) = zero
              end do
           end do
           return
     end subroutine stdlib_sorg2l

     pure module subroutine stdlib_dorg2l( m, n, k, a, lda, tau, work, info )
     !! DORG2L generates an m by n real matrix Q with orthonormal columns,
     !! which is defined as the last n columns of a product of k elementary
     !! reflectors of order m
     !! Q  =  H(k) . . . H(2) H(1)
     !! as returned by DGEQLF.
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
           else if( n<0_ilp .or. n>m ) then
              info = -2_ilp
           else if( k<0_ilp .or. k>n ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORG2L', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           ! initialise columns 1:n-k to columns of the unit matrix
           do j = 1, n - k
              do l = 1, m
                 a( l, j ) = zero
              end do
              a( m-n+j, j ) = one
           end do
           do i = 1, k
              ii = n - k + i
              ! apply h(i) to a(1:m-k+i,1:n-k+i) from the left
              a( m-n+ii, ii ) = one
              call stdlib_dlarf( 'LEFT', m-n+ii, ii-1, a( 1_ilp, ii ), 1_ilp, tau( i ), a,lda, work )
                        
              call stdlib_dscal( m-n+ii-1, -tau( i ), a( 1_ilp, ii ), 1_ilp )
              a( m-n+ii, ii ) = one - tau( i )
              ! set a(m-k+i+1:m,n-k+i) to zero
              do l = m - n + ii + 1, m
                 a( l, ii ) = zero
              end do
           end do
           return
     end subroutine stdlib_dorg2l




     pure module subroutine stdlib_sorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! SORM2L overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T * C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by SGEQLF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORM2L', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran ) .or. ( .not.left .and. .not.notran ) )then
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
              aii = a( nq-k+i, i )
              a( nq-k+i, i ) = one
              call stdlib_slarf( side, mi, ni, a( 1_ilp, i ), 1_ilp, tau( i ), c, ldc,work )
              a( nq-k+i, i ) = aii
           end do
           return
     end subroutine stdlib_sorm2l

     pure module subroutine stdlib_dorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
     !! DORM2L overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T * C  if SIDE = 'L' and TRANS = 'T', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'T',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(k) . . . H(2) H(1)
     !! as returned by DGEQLF. Q is of order m if SIDE = 'L' and of order n
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
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORM2L', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. notran ) .or. ( .not.left .and. .not.notran ) )then
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
              aii = a( nq-k+i, i )
              a( nq-k+i, i ) = one
              call stdlib_dlarf( side, mi, ni, a( 1_ilp, i ), 1_ilp, tau( i ), c, ldc,work )
              a( nq-k+i, i ) = aii
           end do
           return
     end subroutine stdlib_dorm2l




     pure module subroutine stdlib_cunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(in) :: q(ldq,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, ldwork, len, lwkopt, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q;
           ! nw is the minimum dimension of work.
           if( left ) then
              nq = m
           else
              nq = n
           end if
           nw = nq
           if( n1==0_ilp .or. n2==0_ilp ) nw = 1_ilp
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( n1<0_ilp .or. n1+n2/=nq ) then
              info = -5_ilp
           else if( n2<0_ilp ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = m*n
              work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNM22', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! degenerate cases (n1 = 0 or n2 = 0) are handled using stdlib_ctrmm.
           if( n1==0_ilp ) then
              call stdlib_ctrmm( side, 'UPPER', trans, 'NON-UNIT', m, n, cone,q, ldq, c, ldc )
                        
              work( 1_ilp ) = cone
              return
           else if( n2==0_ilp ) then
              call stdlib_ctrmm( side, 'LOWER', trans, 'NON-UNIT', m, n, cone,q, ldq, c, ldc )
                        
              work( 1_ilp ) = cone
              return
           end if
           ! compute the largest chunk size available from the workspace.
           nb = max( 1_ilp, min( lwork, lwkopt ) / nq )
           if( left ) then
              if( notran ) then
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q12.
                    call stdlib_clacpy( 'ALL', n1, len, c( n2+1, i ), ldc, work,ldwork )
                    call stdlib_ctrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',n1, len, cone, &
                              q( 1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply top part of c by q11.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,cone, q, ldq, &
                              c( 1_ilp, i ), ldc, cone, work,ldwork )
                    ! multiply top part of c by q21.
                    call stdlib_clacpy( 'ALL', n2, len, c( 1_ilp, i ), ldc,work( n1+1 ), ldwork )
                              
                    call stdlib_ctrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',n2, len, cone, &
                              q( n1+1, 1_ilp ), ldq,work( n1+1 ), ldwork )
                    ! multiply bottom part of c by q22.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,cone, q( n1+1, &
                              n2+1 ), ldq, c( n2+1, i ), ldc,cone, work( n1+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_clacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              else
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q21**h.
                    call stdlib_clacpy( 'ALL', n2, len, c( n1+1, i ), ldc, work,ldwork )
                    call stdlib_ctrmm( 'LEFT', 'UPPER', 'CONJUGATE', 'NON-UNIT',n2, len, cone, q( &
                              n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply top part of c by q11**h.
                    call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', n2, len, n1,cone, q, ldq, c( &
                              1_ilp, i ), ldc, cone, work,ldwork )
                    ! multiply top part of c by q12**h.
                    call stdlib_clacpy( 'ALL', n1, len, c( 1_ilp, i ), ldc,work( n2+1 ), ldwork )
                              
                    call stdlib_ctrmm( 'LEFT', 'LOWER', 'CONJUGATE', 'NON-UNIT',n1, len, cone, q( &
                              1_ilp, n2+1 ), ldq,work( n2+1 ), ldwork )
                    ! multiply bottom part of c by q22**h.
                    call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', n1, len, n2,cone, q( n1+1, n2+&
                              1_ilp ), ldq, c( n1+1, i ), ldc,cone, work( n2+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_clacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              end if
           else
              if( notran ) then
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q21.
                    call stdlib_clacpy( 'ALL', len, n2, c( i, n1+1 ), ldc, work,ldwork )
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',len, n2, cone,&
                               q( n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply left part of c by q11.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n2, n1,cone, c( i, 1_ilp )&
                              , ldc, q, ldq, cone, work,ldwork )
                    ! multiply left part of c by q12.
                    call stdlib_clacpy( 'ALL', len, n1, c( i, 1_ilp ), ldc,work( 1_ilp + n2*ldwork ), &
                              ldwork )
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',len, n1, cone,&
                               q( 1_ilp, n2+1 ), ldq,work( 1_ilp + n2*ldwork ), ldwork )
                    ! multiply right part of c by q22.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n1, n2,cone, c( i, n1+&
                              1_ilp ), ldc, q( n1+1, n2+1 ), ldq,cone, work( 1_ilp + n2*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_clacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              else
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q12**h.
                    call stdlib_clacpy( 'ALL', len, n1, c( i, n2+1 ), ldc, work,ldwork )
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE', 'NON-UNIT',len, n1, cone, q(&
                               1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply left part of c by q11**h.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE', len, n1, n2,cone, c( i, 1_ilp ), &
                              ldc, q, ldq, cone, work,ldwork )
                    ! multiply left part of c by q21**h.
                    call stdlib_clacpy( 'ALL', len, n2, c( i, 1_ilp ), ldc,work( 1_ilp + n1*ldwork ), &
                              ldwork )
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE', 'NON-UNIT',len, n2, cone, q(&
                               n1+1, 1_ilp ), ldq,work( 1_ilp + n1*ldwork ), ldwork )
                    ! multiply right part of c by q22**h.
                    call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE', len, n2, n1,cone, c( i, n2+1 )&
                              , ldc, q( n1+1, n2+1 ), ldq,cone, work( 1_ilp + n1*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_clacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              end if
           end if
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cunm22

     pure module subroutine stdlib_zunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(in) :: q(ldq,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, ldwork, len, lwkopt, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q;
           ! nw is the minimum dimension of work.
           if( left ) then
              nq = m
           else
              nq = n
           end if
           nw = nq
           if( n1==0_ilp .or. n2==0_ilp ) nw = 1_ilp
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( n1<0_ilp .or. n1+n2/=nq ) then
              info = -5_ilp
           else if( n2<0_ilp ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = m*n
              work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNM22', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! degenerate cases (n1 = 0 or n2 = 0) are handled using stdlib_ztrmm.
           if( n1==0_ilp ) then
              call stdlib_ztrmm( side, 'UPPER', trans, 'NON-UNIT', m, n, cone,q, ldq, c, ldc )
                        
              work( 1_ilp ) = cone
              return
           else if( n2==0_ilp ) then
              call stdlib_ztrmm( side, 'LOWER', trans, 'NON-UNIT', m, n, cone,q, ldq, c, ldc )
                        
              work( 1_ilp ) = cone
              return
           end if
           ! compute the largest chunk size available from the workspace.
           nb = max( 1_ilp, min( lwork, lwkopt ) / nq )
           if( left ) then
              if( notran ) then
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q12.
                    call stdlib_zlacpy( 'ALL', n1, len, c( n2+1, i ), ldc, work,ldwork )
                    call stdlib_ztrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',n1, len, cone, &
                              q( 1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply top part of c by q11.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,cone, q, ldq, &
                              c( 1_ilp, i ), ldc, cone, work,ldwork )
                    ! multiply top part of c by q21.
                    call stdlib_zlacpy( 'ALL', n2, len, c( 1_ilp, i ), ldc,work( n1+1 ), ldwork )
                              
                    call stdlib_ztrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',n2, len, cone, &
                              q( n1+1, 1_ilp ), ldq,work( n1+1 ), ldwork )
                    ! multiply bottom part of c by q22.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,cone, q( n1+1, &
                              n2+1 ), ldq, c( n2+1, i ), ldc,cone, work( n1+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_zlacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              else
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q21**h.
                    call stdlib_zlacpy( 'ALL', n2, len, c( n1+1, i ), ldc, work,ldwork )
                    call stdlib_ztrmm( 'LEFT', 'UPPER', 'CONJUGATE', 'NON-UNIT',n2, len, cone, q( &
                              n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply top part of c by q11**h.
                    call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', n2, len, n1,cone, q, ldq, c( &
                              1_ilp, i ), ldc, cone, work,ldwork )
                    ! multiply top part of c by q12**h.
                    call stdlib_zlacpy( 'ALL', n1, len, c( 1_ilp, i ), ldc,work( n2+1 ), ldwork )
                              
                    call stdlib_ztrmm( 'LEFT', 'LOWER', 'CONJUGATE', 'NON-UNIT',n1, len, cone, q( &
                              1_ilp, n2+1 ), ldq,work( n2+1 ), ldwork )
                    ! multiply bottom part of c by q22**h.
                    call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', n1, len, n2,cone, q( n1+1, n2+&
                              1_ilp ), ldq, c( n1+1, i ), ldc,cone, work( n2+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_zlacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              end if
           else
              if( notran ) then
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q21.
                    call stdlib_zlacpy( 'ALL', len, n2, c( i, n1+1 ), ldc, work,ldwork )
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',len, n2, cone,&
                               q( n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply left part of c by q11.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n2, n1,cone, c( i, 1_ilp )&
                              , ldc, q, ldq, cone, work,ldwork )
                    ! multiply left part of c by q12.
                    call stdlib_zlacpy( 'ALL', len, n1, c( i, 1_ilp ), ldc,work( 1_ilp + n2*ldwork ), &
                              ldwork )
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',len, n1, cone,&
                               q( 1_ilp, n2+1 ), ldq,work( 1_ilp + n2*ldwork ), ldwork )
                    ! multiply right part of c by q22.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n1, n2,cone, c( i, n1+&
                              1_ilp ), ldc, q( n1+1, n2+1 ), ldq,cone, work( 1_ilp + n2*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_zlacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              else
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q12**h.
                    call stdlib_zlacpy( 'ALL', len, n1, c( i, n2+1 ), ldc, work,ldwork )
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE', 'NON-UNIT',len, n1, cone, q(&
                               1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply left part of c by q11**h.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE', len, n1, n2,cone, c( i, 1_ilp ), &
                              ldc, q, ldq, cone, work,ldwork )
                    ! multiply left part of c by q21**h.
                    call stdlib_zlacpy( 'ALL', len, n2, c( i, 1_ilp ), ldc,work( 1_ilp + n1*ldwork ), &
                              ldwork )
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE', 'NON-UNIT',len, n2, cone, q(&
                               n1+1, 1_ilp ), ldq,work( 1_ilp + n1*ldwork ), ldwork )
                    ! multiply right part of c by q22**h.
                    call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE', len, n2, n1,cone, c( i, n2+1 )&
                              , ldc, q( n1+1, n2+1 ), ldq,cone, work( 1_ilp + n1*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_zlacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              end if
           end if
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zunm22



end submodule stdlib_lapack_orthogonal_factors_ql
