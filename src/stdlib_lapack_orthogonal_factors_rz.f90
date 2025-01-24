submodule(stdlib_lapack_orthogonal_factors) stdlib_lapack_orthogonal_factors_rz
  implicit none


  contains

     pure module subroutine stdlib_stzrzf( m, n, a, lda, tau, work, lwork, info )
     !! STZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
     !! to upper triangular form by means of orthogonal transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
     !! triangular matrix.
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
           integer(ilp) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              if( m==0_ilp .or. m==n ) then
                 lwkopt = 1_ilp
                 lwkmin = 1_ilp
              else
                 ! determine the block size.
                 nb = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp, m )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<m ) then
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
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_slatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_slarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_slarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp )call stdlib_slatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_stzrzf

     pure module subroutine stdlib_dtzrzf( m, n, a, lda, tau, work, lwork, info )
     !! DTZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
     !! to upper triangular form by means of orthogonal transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
     !! triangular matrix.
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
           integer(ilp) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              if( m==0_ilp .or. m==n ) then
                 lwkopt = 1_ilp
                 lwkmin = 1_ilp
              else
                 ! determine the block size.
                 nb = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp, m )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<m ) then
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
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_dlatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_dlarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_dlarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp )call stdlib_dlatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dtzrzf


     pure module subroutine stdlib_ctzrzf( m, n, a, lda, tau, work, lwork, info )
     !! CTZRZF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
     !! to upper triangular form by means of unitary transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N unitary matrix and R is an M-by-M upper
     !! triangular matrix.
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
           integer(ilp) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              if( m==0_ilp .or. m==n ) then
                 lwkopt = 1_ilp
                 lwkmin = 1_ilp
              else
                 ! determine the block size.
                 nb = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp, m )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<m ) then
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
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_clatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_clarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_clarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp )call stdlib_clatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ctzrzf

     pure module subroutine stdlib_ztzrzf( m, n, a, lda, tau, work, lwork, info )
     !! ZTZRZF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
     !! to upper triangular form by means of unitary transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N unitary matrix and R is an M-by-M upper
     !! triangular matrix.
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
           integer(ilp) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<m ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info==0_ilp ) then
              if( m==0_ilp .or. m==n ) then
                 lwkopt = 1_ilp
                 lwkmin = 1_ilp
              else
                 ! determine the block size.
                 nb = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp, m )
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           nbmin = 2_ilp
           nx = 1_ilp
           iws = m
           if( nb>1_ilp .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp, stdlib_ilaenv( 3_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp ) )
              if( nx<m ) then
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
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_zlatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_zlarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_zlarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp )call stdlib_zlatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ztzrzf




     pure module subroutine stdlib_cunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! CUNMRZ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
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
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
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
              call stdlib_xerbla( 'CUNMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! determine the block size.
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'CUNMRQ', side // trans, m, n, k,-1_ilp ) )
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
              call stdlib_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
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
                 ja = m - l + 1_ilp
              else
                 mi = m
                 ic = 1_ilp
                 ja = n - l + 1_ilp
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
                 call stdlib_clarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
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
                 call stdlib_clarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmrz

     pure module subroutine stdlib_zunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! ZUNMRZ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
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
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<max( 1_ilp, nw ) .and. .not.lquery ) then
              info = -13_ilp
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
              call stdlib_xerbla( 'ZUNMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! determine the block size.  nb may be at most nbmax, where nbmax
           ! is used to define the local array t.
           nb = min( nbmax, stdlib_ilaenv( 1_ilp, 'ZUNMRQ', side // trans, m, n, k,-1_ilp ) )
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
              call stdlib_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
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
                 ja = m - l + 1_ilp
              else
                 mi = m
                 ic = 1_ilp
                 ja = n - l + 1_ilp
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
                 call stdlib_zlarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
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
                 call stdlib_zlarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmrz




     pure module subroutine stdlib_sormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! SORMRZ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by STZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
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
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
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
              call stdlib_xerbla( 'SORMRZ', -info )
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
              call stdlib_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
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
                 ja = m - l + 1_ilp
              else
                 mi = m
                 ic = 1_ilp
                 ja = n - l + 1_ilp
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
                 call stdlib_slarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
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
                 call stdlib_slarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormrz

     pure module subroutine stdlib_dormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! DORMRZ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
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
           integer(ilp) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
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
              call stdlib_xerbla( 'DORMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
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
              call stdlib_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
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
                 ja = m - l + 1_ilp
              else
                 mi = m
                 ic = 1_ilp
                 ja = n - l + 1_ilp
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
                 call stdlib_dlarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
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
                 call stdlib_dlarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormrz




     pure module subroutine stdlib_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! CUNMR3 overwrites the general complex m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), tau(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           complex(sp) :: taui
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMR3', -info )
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
              ja = m - l + 1_ilp
              jc = 1_ilp
           else
              mi = m
              ja = n - l + 1_ilp
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
              call stdlib_clarz( side, mi, ni, l, a( i, ja ), lda, taui,c( ic, jc ), ldc, work )
                        
           end do
           return
     end subroutine stdlib_cunmr3

     pure module subroutine stdlib_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! ZUNMR3 overwrites the general complex m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), tau(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           complex(dp) :: taui
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMR3', -info )
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
              ja = m - l + 1_ilp
              jc = 1_ilp
           else
              mi = m
              ja = n - l + 1_ilp
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
              call stdlib_zlarz( side, mi, ni, l, a( i, ja ), lda, taui,c( ic, jc ), ldc, work )
                        
           end do
           return
     end subroutine stdlib_zunmr3




     pure module subroutine stdlib_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! SORMR3 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'C',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by STZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), tau(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMR3', -info )
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
              ja = m - l + 1_ilp
              jc = 1_ilp
           else
              mi = m
              ja = n - l + 1_ilp
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**t is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) or h(i)**t is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i) or h(i)**t
              call stdlib_slarz( side, mi, ni, l, a( i, ja ), lda, tau( i ),c( ic, jc ), ldc, &
                        work )
           end do
           return
     end subroutine stdlib_sormr3

     pure module subroutine stdlib_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! DORMR3 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'C',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), tau(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
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
           else if( l<0_ilp .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, k ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMR3', -info )
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
              ja = m - l + 1_ilp
              jc = 1_ilp
           else
              mi = m
              ja = n - l + 1_ilp
              ic = 1_ilp
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**t is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp
                 ic = i
              else
                 ! h(i) or h(i)**t is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp
                 jc = i
              end if
              ! apply h(i) or h(i)**t
              call stdlib_dlarz( side, mi, ni, l, a( i, ja ), lda, tau( i ),c( ic, jc ), ldc, &
                        work )
           end do
           return
     end subroutine stdlib_dormr3




     pure module subroutine stdlib_slarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! SLARZ applies a real elementary reflector H to a real M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! H is a product of k elementary reflectors as returned by STZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=zero ) then
                 ! w( 1:n ) = c( 1, 1:n )
                 call stdlib_scopy( n, c, ldc, work, 1_ilp )
                 ! w( 1:n ) = w( 1:n ) + c( m-l+1:m, 1:n )**t * v( 1:l )
                 call stdlib_sgemv( 'TRANSPOSE', l, n, one, c( m-l+1, 1_ilp ), ldc, v,incv, one, work,&
                            1_ilp )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_saxpy( n, -tau, work, 1_ilp, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**t
                 call stdlib_sger( l, n, -tau, v, incv, work, 1_ilp, c( m-l+1, 1_ilp ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=zero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_scopy( m, c, 1_ilp, work, 1_ilp )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_sgemv( 'NO TRANSPOSE', m, l, one, c( 1_ilp, n-l+1 ), ldc,v, incv, one, &
                           work, 1_ilp )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_saxpy( m, -tau, work, 1_ilp, c, 1_ilp )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**t
                 call stdlib_sger( m, l, -tau, work, 1_ilp, v, incv, c( 1_ilp, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_slarz

     pure module subroutine stdlib_dlarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! DLARZ applies a real elementary reflector H to a real M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! H is a product of k elementary reflectors as returned by DTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=zero ) then
                 ! w( 1:n ) = c( 1, 1:n )
                 call stdlib_dcopy( n, c, ldc, work, 1_ilp )
                 ! w( 1:n ) = w( 1:n ) + c( m-l+1:m, 1:n )**t * v( 1:l )
                 call stdlib_dgemv( 'TRANSPOSE', l, n, one, c( m-l+1, 1_ilp ), ldc, v,incv, one, work,&
                            1_ilp )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_daxpy( n, -tau, work, 1_ilp, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**t
                 call stdlib_dger( l, n, -tau, v, incv, work, 1_ilp, c( m-l+1, 1_ilp ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=zero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_dcopy( m, c, 1_ilp, work, 1_ilp )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_dgemv( 'NO TRANSPOSE', m, l, one, c( 1_ilp, n-l+1 ), ldc,v, incv, one, &
                           work, 1_ilp )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_daxpy( m, -tau, work, 1_ilp, c, 1_ilp )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**t
                 call stdlib_dger( m, l, -tau, work, 1_ilp, v, incv, c( 1_ilp, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_dlarz


     pure module subroutine stdlib_clarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! CLARZ applies a complex elementary reflector H to a complex
     !! M-by-N matrix C, from either the left or the right. H is represented
     !! in the form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
     !! H is a product of k elementary reflectors as returned by CTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=czero ) then
                 ! w( 1:n ) = conjg( c( 1, 1:n ) )
                 call stdlib_ccopy( n, c, ldc, work, 1_ilp )
                 call stdlib_clacgv( n, work, 1_ilp )
                 ! w( 1:n ) = conjg( w( 1:n ) + c( m-l+1:m, 1:n )**h * v( 1:l ) )
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', l, n, cone, c( m-l+1, 1_ilp ),ldc, v, incv,&
                            cone, work, 1_ilp )
                 call stdlib_clacgv( n, work, 1_ilp )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_caxpy( n, -tau, work, 1_ilp, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**h
                 call stdlib_cgeru( l, n, -tau, v, incv, work, 1_ilp, c( m-l+1, 1_ilp ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=czero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_ccopy( m, c, 1_ilp, work, 1_ilp )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_cgemv( 'NO TRANSPOSE', m, l, cone, c( 1_ilp, n-l+1 ), ldc,v, incv, cone, &
                           work, 1_ilp )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_caxpy( m, -tau, work, 1_ilp, c, 1_ilp )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**h
                 call stdlib_cgerc( m, l, -tau, work, 1_ilp, v, incv, c( 1_ilp, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_clarz

     pure module subroutine stdlib_zlarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! ZLARZ applies a complex elementary reflector H to a complex
     !! M-by-N matrix C, from either the left or the right. H is represented
     !! in the form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
     !! H is a product of k elementary reflectors as returned by ZTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=czero ) then
                 ! w( 1:n ) = conjg( c( 1, 1:n ) )
                 call stdlib_zcopy( n, c, ldc, work, 1_ilp )
                 call stdlib_zlacgv( n, work, 1_ilp )
                 ! w( 1:n ) = conjg( w( 1:n ) + c( m-l+1:m, 1:n )**h * v( 1:l ) )
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', l, n, cone, c( m-l+1, 1_ilp ),ldc, v, incv,&
                            cone, work, 1_ilp )
                 call stdlib_zlacgv( n, work, 1_ilp )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_zaxpy( n, -tau, work, 1_ilp, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**h
                 call stdlib_zgeru( l, n, -tau, v, incv, work, 1_ilp, c( m-l+1, 1_ilp ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=czero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_zcopy( m, c, 1_ilp, work, 1_ilp )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_zgemv( 'NO TRANSPOSE', m, l, cone, c( 1_ilp, n-l+1 ), ldc,v, incv, cone, &
                           work, 1_ilp )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_zaxpy( m, -tau, work, 1_ilp, c, 1_ilp )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**h
                 call stdlib_zgerc( m, l, -tau, work, 1_ilp, v, incv, c( 1_ilp, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_zlarz




     pure module subroutine stdlib_slarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! SLARZB applies a real block reflector H or its transpose H**T to
     !! a real distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**t * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**t
              do j = 1, k
                 call stdlib_scopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**t * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, l, one,c( m-l+1, 1_ilp ), &
                        ldc, v, ldv, one, work, ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**t
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**t * w( 1:n, 1:k )**t
              if( l>0_ilp )call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -one, v, ldv,work, &
                        ldwork, one, c( m-l+1, 1_ilp ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**t
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_scopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, one,c( 1_ilp, n-l+1 ),&
                         ldc, v, ldv, one, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * t  or  w( 1:m, 1:k ) * t**t
              call stdlib_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * v( 1:k, 1:l )
              if( l>0_ilp )call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -one,work, &
                        ldwork, v, ldv, one, c( 1_ilp, n-l+1 ), ldc )
           end if
           return
     end subroutine stdlib_slarzb

     pure module subroutine stdlib_dlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! DLARZB applies a real block reflector H or its transpose H**T to
     !! a real distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**t * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**t
              do j = 1, k
                 call stdlib_dcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**t * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, l, one,c( m-l+1, 1_ilp ), &
                        ldc, v, ldv, one, work, ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**t
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**t * w( 1:n, 1:k )**t
              if( l>0_ilp )call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -one, v, ldv,work, &
                        ldwork, one, c( m-l+1, 1_ilp ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**t
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_dcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, one,c( 1_ilp, n-l+1 ),&
                         ldc, v, ldv, one, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * t  or  w( 1:m, 1:k ) * t**t
              call stdlib_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * v( 1:k, 1:l )
              if( l>0_ilp )call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -one,work, &
                        ldwork, v, ldv, one, c( 1_ilp, n-l+1 ), ldc )
           end if
           return
     end subroutine stdlib_dlarzb


     pure module subroutine stdlib_clarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! CLARZB applies a complex block reflector H or its transpose H**H
     !! to a complex distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**h * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**h
              do j = 1, k
                 call stdlib_ccopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**h * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_cgemm( 'TRANSPOSE', 'CONJUGATE TRANSPOSE', n, k, l,cone, c( m-&
                        l+1, 1_ilp ), ldc, v, ldv, cone, work,ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, cone, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**h
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**h * w( 1:n, 1:k )**h
              if( l>0_ilp )call stdlib_cgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -cone, v, ldv,work, &
                        ldwork, cone, c( m-l+1, 1_ilp ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**h
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_ccopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**h
              if( l>0_ilp )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, cone,c( 1_ilp, n-l+1 )&
                        , ldc, v, ldv, cone, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * conjg( t )  or
                              ! w( 1:m, 1:k ) * t**h
              do j = 1, k
                 call stdlib_clacgv( k-j+1, t( j, j ), 1_ilp )
              end do
              call stdlib_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, cone, t,ldt, work, &
                        ldwork )
              do j = 1, k
                 call stdlib_clacgv( k-j+1, t( j, j ), 1_ilp )
              end do
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * conjg( v( 1:k, 1:l ) )
              do j = 1, l
                 call stdlib_clacgv( k, v( 1_ilp, j ), 1_ilp )
              end do
              if( l>0_ilp )call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -cone,work, &
                        ldwork, v, ldv, cone, c( 1_ilp, n-l+1 ), ldc )
              do j = 1, l
                 call stdlib_clacgv( k, v( 1_ilp, j ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_clarzb

     pure module subroutine stdlib_zlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! ZLARZB applies a complex block reflector H or its transpose H**H
     !! to a complex distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**h * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**h
              do j = 1, k
                 call stdlib_zcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**h * v( 1:k, 1:l )**t
              if( l>0_ilp )call stdlib_zgemm( 'TRANSPOSE', 'CONJUGATE TRANSPOSE', n, k, l,cone, c( m-&
                        l+1, 1_ilp ), ldc, v, ldv, cone, work,ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, cone, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**h
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**h * w( 1:n, 1:k )**h
              if( l>0_ilp )call stdlib_zgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -cone, v, ldv,work, &
                        ldwork, cone, c( m-l+1, 1_ilp ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**h
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_zcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**h
              if( l>0_ilp )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, cone,c( 1_ilp, n-l+1 )&
                        , ldc, v, ldv, cone, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * conjg( t )  or
                              ! w( 1:m, 1:k ) * t**h
              do j = 1, k
                 call stdlib_zlacgv( k-j+1, t( j, j ), 1_ilp )
              end do
              call stdlib_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, cone, t,ldt, work, &
                        ldwork )
              do j = 1, k
                 call stdlib_zlacgv( k-j+1, t( j, j ), 1_ilp )
              end do
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * conjg( v( 1:k, 1:l ) )
              do j = 1, l
                 call stdlib_zlacgv( k, v( 1_ilp, j ), 1_ilp )
              end do
              if( l>0_ilp )call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -cone,work, &
                        ldwork, v, ldv, cone, c( 1_ilp, n-l+1 ), ldc )
              do j = 1, l
                 call stdlib_zlacgv( k, v( 1_ilp, j ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_zlarzb




     pure module subroutine stdlib_slarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! SLARZT forms the triangular factor T of a real block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==zero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = zero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**t
                    call stdlib_sgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp ), ldv, v( i, &
                              1_ilp ), ldv, zero,t( i+1, i ), 1_ilp )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_strmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_slarzt

     pure module subroutine stdlib_dlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! DLARZT forms the triangular factor T of a real block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==zero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = zero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**t
                    call stdlib_dgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp ), ldv, v( i, &
                              1_ilp ), ldv, zero,t( i+1, i ), 1_ilp )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_dtrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_dlarzt


     pure module subroutine stdlib_clarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! CLARZT forms the triangular factor T of a complex block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==czero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = czero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**h
                    call stdlib_clacgv( n, v( i, 1_ilp ), ldv )
                    call stdlib_cgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp ), ldv, v( i, &
                              1_ilp ), ldv, czero,t( i+1, i ), 1_ilp )
                    call stdlib_clacgv( n, v( i, 1_ilp ), ldv )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_ctrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_clarzt

     pure module subroutine stdlib_zlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! ZLARZT forms the triangular factor T of a complex block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==czero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = czero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**h
                    call stdlib_zlacgv( n, v( i, 1_ilp ), ldv )
                    call stdlib_zgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp ), ldv, v( i, &
                              1_ilp ), ldv, czero,t( i+1, i ), 1_ilp )
                    call stdlib_zlacgv( n, v( i, 1_ilp ), ldv )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_ztrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_zlarzt




     pure module subroutine stdlib_slatrz( m, n, l, a, lda, tau, work )
     !! SLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
     !! of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: l, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! test the input arguments
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_slarfg( l+1, a( i, i ), a( i, n-l+1 ), lda, tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_slarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,tau( i ), a( 1_ilp, i ), &
                        lda, work )
           end do
           return
     end subroutine stdlib_slatrz

     pure module subroutine stdlib_dlatrz( m, n, l, a, lda, tau, work )
     !! DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
     !! of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: l, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! test the input arguments
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_dlarfg( l+1, a( i, i ), a( i, n-l+1 ), lda, tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_dlarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,tau( i ), a( 1_ilp, i ), &
                        lda, work )
           end do
           return
     end subroutine stdlib_dlatrz


     pure module subroutine stdlib_clatrz( m, n, l, a, lda, tau, work )
     !! CLATRZ factors the M-by-(M+L) complex upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z by means
     !! of unitary transformations, where  Z is an (M+L)-by-(M+L) unitary
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: l, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_clacgv( l, a( i, n-l+1 ), lda )
              alpha = conjg( a( i, i ) )
              call stdlib_clarfg( l+1, alpha, a( i, n-l+1 ), lda, tau( i ) )
              tau( i ) = conjg( tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_clarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,conjg( tau( i ) ), a( &
                        1_ilp, i ), lda, work )
              a( i, i ) = conjg( alpha )
           end do
           return
     end subroutine stdlib_clatrz

     pure module subroutine stdlib_zlatrz( m, n, l, a, lda, tau, work )
     !! ZLATRZ factors the M-by-(M+L) complex upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z by means
     !! of unitary transformations, where  Z is an (M+L)-by-(M+L) unitary
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: l, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m==0_ilp ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_zlacgv( l, a( i, n-l+1 ), lda )
              alpha = conjg( a( i, i ) )
              call stdlib_zlarfg( l+1, alpha, a( i, n-l+1 ), lda, tau( i ) )
              tau( i ) = conjg( tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_zlarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,conjg( tau( i ) ), a( &
                        1_ilp, i ), lda, work )
              a( i, i ) = conjg( alpha )
           end do
           return
     end subroutine stdlib_zlatrz




     pure module subroutine stdlib_I64_stzrzf( m, n, a, lda, tau, work, lwork, info )
     !! STZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
     !! to upper triangular form by means of orthogonal transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
     !! triangular matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( m<0_ilp64 ) then
              info = -1_ilp64
           else if( n<m ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = -4_ilp64
           end if
           if( info==0_ilp64 ) then
              if( m==0_ilp64 .or. m==n ) then
                 lwkopt = 1_ilp64
                 lwkmin = 1_ilp64
              else
                 ! determine the block size.
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'SGERQF', ' ', m, n, -1_ilp64, -1_ilp64 )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp64, m )
              end if
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           nbmin = 2_ilp64
           nx = 1_ilp64
           iws = m
           if( nb>1_ilp64 .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp64, stdlib_I64_ilaenv( 3_ilp64, 'SGERQF', ' ', m, n, -1_ilp64, -1_ilp64 ) )
              if( nx<m ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'SGERQF', ' ', m, n, -1_ilp64,-1_ilp64 ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_I64_slatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp64 ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_I64_slarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_I64_slarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp64, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp64
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp64 )call stdlib_I64_slatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_stzrzf

     pure module subroutine stdlib_I64_dtzrzf( m, n, a, lda, tau, work, lwork, info )
     !! DTZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
     !! to upper triangular form by means of orthogonal transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
     !! triangular matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( m<0_ilp64 ) then
              info = -1_ilp64
           else if( n<m ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = -4_ilp64
           end if
           if( info==0_ilp64 ) then
              if( m==0_ilp64 .or. m==n ) then
                 lwkopt = 1_ilp64
                 lwkmin = 1_ilp64
              else
                 ! determine the block size.
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'DGERQF', ' ', m, n, -1_ilp64, -1_ilp64 )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp64, m )
              end if
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           nbmin = 2_ilp64
           nx = 1_ilp64
           iws = m
           if( nb>1_ilp64 .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp64, stdlib_I64_ilaenv( 3_ilp64, 'DGERQF', ' ', m, n, -1_ilp64, -1_ilp64 ) )
              if( nx<m ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'DGERQF', ' ', m, n, -1_ilp64,-1_ilp64 ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_I64_dlatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp64 ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_I64_dlarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_I64_dlarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp64, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp64
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp64 )call stdlib_I64_dlatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dtzrzf


     pure module subroutine stdlib_I64_ctzrzf( m, n, a, lda, tau, work, lwork, info )
     !! CTZRZF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
     !! to upper triangular form by means of unitary transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N unitary matrix and R is an M-by-M upper
     !! triangular matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( m<0_ilp64 ) then
              info = -1_ilp64
           else if( n<m ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = -4_ilp64
           end if
           if( info==0_ilp64 ) then
              if( m==0_ilp64 .or. m==n ) then
                 lwkopt = 1_ilp64
                 lwkmin = 1_ilp64
              else
                 ! determine the block size.
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'CGERQF', ' ', m, n, -1_ilp64, -1_ilp64 )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp64, m )
              end if
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           nbmin = 2_ilp64
           nx = 1_ilp64
           iws = m
           if( nb>1_ilp64 .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp64, stdlib_I64_ilaenv( 3_ilp64, 'CGERQF', ' ', m, n, -1_ilp64, -1_ilp64 ) )
              if( nx<m ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'CGERQF', ' ', m, n, -1_ilp64,-1_ilp64 ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_I64_clatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp64 ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_I64_clarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_I64_clarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp64, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp64
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp64 )call stdlib_I64_clatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_ctzrzf

     pure module subroutine stdlib_I64_ztzrzf( m, n, a, lda, tau, work, lwork, info )
     !! ZTZRZF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
     !! to upper triangular form by means of unitary transformations.
     !! The upper trapezoidal matrix A is factored as
     !! A = ( R  0 ) * Z,
     !! where Z is an N-by-N unitary matrix and R is an M-by-M upper
     !! triangular matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp64) :: i, ib, iws, ki, kk, ldwork, lwkmin, lwkopt, m1, mu, nb, nbmin, &
                     nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           if( m<0_ilp64 ) then
              info = -1_ilp64
           else if( n<m ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, m ) ) then
              info = -4_ilp64
           end if
           if( info==0_ilp64 ) then
              if( m==0_ilp64 .or. m==n ) then
                 lwkopt = 1_ilp64
                 lwkmin = 1_ilp64
              else
                 ! determine the block size.
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'ZGERQF', ' ', m, n, -1_ilp64, -1_ilp64 )
                 lwkopt = m*nb
                 lwkmin = max( 1_ilp64, m )
              end if
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -7_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTZRZF', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           nbmin = 2_ilp64
           nx = 1_ilp64
           iws = m
           if( nb>1_ilp64 .and. nb<m ) then
              ! determine when to cross over from blocked to unblocked code.
              nx = max( 0_ilp64, stdlib_I64_ilaenv( 3_ilp64, 'ZGERQF', ' ', m, n, -1_ilp64, -1_ilp64 ) )
              if( nx<m ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = m
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  reduce nb and
                    ! determine the minimum value of nb.
                    nb = lwork / ldwork
                    nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'ZGERQF', ' ', m, n, -1_ilp64,-1_ilp64 ) )
                 end if
              end if
           end if
           if( nb>=nbmin .and. nb<m .and. nx<m ) then
              ! use blocked code initially.
              ! the last kk rows are handled by the block method.
              m1 = min( m+1, n )
              ki = ( ( m-nx-1 ) / nb )*nb
              kk = min( m, ki+nb )
              do i = m - kk + ki + 1, m - kk + 1, -nb
                 ib = min( m-i+1, nb )
                 ! compute the tz factorization of the current block
                 ! a(i:i+ib-1,i:n)
                 call stdlib_I64_zlatrz( ib, n-i+1, n-m, a( i, i ), lda, tau( i ),work )
                 if( i>1_ilp64 ) then
                    ! form the triangular factor of the block reflector
                    ! h = h(i+ib-1) . . . h(i+1) h(i)
                    call stdlib_I64_zlarzt( 'BACKWARD', 'ROWWISE', n-m, ib, a( i, m1 ),lda, tau( i ), &
                              work, ldwork )
                    ! apply h to a(1:i-1,i:n) from the right
                    call stdlib_I64_zlarzb( 'RIGHT', 'NO TRANSPOSE', 'BACKWARD','ROWWISE', i-1, n-i+1,&
                     ib, n-m, a( i, m1 ),lda, work, ldwork, a( 1_ilp64, i ), lda,work( ib+1 ), ldwork )
                               
                 end if
              end do
              mu = i + nb - 1_ilp64
           else
              mu = m
           end if
           ! use unblocked code to factor the last or only block
           if( mu>0_ilp64 )call stdlib_I64_zlatrz( mu, n, n-m, a, lda, tau, work )
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_ztzrzf




     pure module subroutine stdlib_I64_cunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! CUNMRZ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: nbmax = 64_ilp64
           integer(ilp64), parameter :: ldt = nbmax+1
           integer(ilp64), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp64) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              ! compute the workspace requirements
              if( m==0_ilp64 .or. n==0_ilp64 ) then
                 lwkopt = 1_ilp64
              else
                 nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'CUNMRQ', side // trans, m, n,k, -1_ilp64 ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CUNMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              return
           end if
           ! determine the block size.
           nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'CUNMRQ', side // trans, m, n, k,-1_ilp64 ) )
           nbmin = 2_ilp64
           ldwork = nw
           if( nb>1_ilp64 .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'CUNMRQ', side // trans, m, n, k,-1_ilp64 ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_I64_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
           else
              ! use blocked code
              iwt = 1_ilp64 + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp64
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp64
                 i2 = 1_ilp64
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp64
                 ja = m - l + 1_ilp64
              else
                 mi = m
                 ic = 1_ilp64
                 ja = n - l + 1_ilp64
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
                 call stdlib_I64_clarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp64
                    ic = i
                 else
                    ! h or h**h is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp64
                    jc = i
                 end if
                 ! apply h or h**h
                 call stdlib_I64_clarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_cunmrz

     pure module subroutine stdlib_I64_zunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! ZUNMRZ overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: nbmax = 64_ilp64
           integer(ilp64), parameter :: ldt = nbmax+1
           integer(ilp64), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp64) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           else if( lwork<max( 1_ilp64, nw ) .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              ! compute the workspace requirements
              if( m==0_ilp64 .or. n==0_ilp64 ) then
                 lwkopt = 1_ilp64
              else
                 nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'ZUNMRQ', side // trans, m, n,k, -1_ilp64 ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZUNMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              return
           end if
           ! determine the block size.  nb may be at most nbmax, where nbmax
           ! is used to define the local array t.
           nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'ZUNMRQ', side // trans, m, n, k,-1_ilp64 ) )
           nbmin = 2_ilp64
           ldwork = nw
           if( nb>1_ilp64 .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'ZUNMRQ', side // trans, m, n, k,-1_ilp64 ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_I64_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
           else
              ! use blocked code
              iwt = 1_ilp64 + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp64
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp64
                 i2 = 1_ilp64
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp64
                 ja = m - l + 1_ilp64
              else
                 mi = m
                 ic = 1_ilp64
                 ja = n - l + 1_ilp64
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
                 call stdlib_I64_zlarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
                 if( left ) then
                    ! h or h**h is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp64
                    ic = i
                 else
                    ! h or h**h is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp64
                    jc = i
                 end if
                 ! apply h or h**h
                 call stdlib_I64_zlarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zunmrz




     pure module subroutine stdlib_I64_sormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! SORMRZ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by STZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: nbmax = 64_ilp64
           integer(ilp64), parameter :: ldt = nbmax+1
           integer(ilp64), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp64) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              ! compute the workspace requirements
              if( m==0_ilp64 .or. n==0_ilp64 ) then
                 lwkopt = 1_ilp64
              else
                 nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'SORMRQ', side // trans, m, n,k, -1_ilp64 ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SORMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              return
           end if
           nbmin = 2_ilp64
           ldwork = nw
           if( nb>1_ilp64 .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'SORMRQ', side // trans, m, n, k,-1_ilp64 ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_I64_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
           else
              ! use blocked code
              iwt = 1_ilp64 + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp64
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp64
                 i2 = 1_ilp64
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp64
                 ja = m - l + 1_ilp64
              else
                 mi = m
                 ic = 1_ilp64
                 ja = n - l + 1_ilp64
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
                 call stdlib_I64_slarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp64
                    ic = i
                 else
                    ! h or h**t is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp64
                    jc = i
                 end if
                 ! apply h or h**t
                 call stdlib_I64_slarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_sormrz

     pure module subroutine stdlib_I64_dormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
     !! DORMRZ overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
     !! if SIDE = 'R'.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: nbmax = 64_ilp64
           integer(ilp64), parameter :: ldt = nbmax+1
           integer(ilp64), parameter :: tsize = ldt*nbmax
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           character :: transt
           integer(ilp64) :: i, i1, i2, i3, ib, ic, iinfo, iwt, ja, jc, ldwork, lwkopt, mi, nb, &
                     nbmin, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              ! compute the workspace requirements
              if( m==0_ilp64 .or. n==0_ilp64 ) then
                 lwkopt = 1_ilp64
              else
                 nb = min( nbmax, stdlib_I64_ilaenv( 1_ilp64, 'DORMRQ', side // trans, m, n,k, -1_ilp64 ) )
                           
                 lwkopt = nw*nb + tsize
              end if
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DORMRZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           nbmin = 2_ilp64
           ldwork = nw
           if( nb>1_ilp64 .and. nb<k ) then
              if( lwork<lwkopt ) then
                 nb = (lwork-tsize) / ldwork
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'DORMRQ', side // trans, m, n, k,-1_ilp64 ) )
              end if
           end if
           if( nb<nbmin .or. nb>=k ) then
              ! use unblocked code
              call stdlib_I64_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, iinfo )
                        
           else
              ! use blocked code
              iwt = 1_ilp64 + nw*nb
              if( ( left .and. .not.notran ) .or.( .not.left .and. notran ) ) then
                 i1 = 1_ilp64
                 i2 = k
                 i3 = nb
              else
                 i1 = ( ( k-1 ) / nb )*nb + 1_ilp64
                 i2 = 1_ilp64
                 i3 = -nb
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp64
                 ja = m - l + 1_ilp64
              else
                 mi = m
                 ic = 1_ilp64
                 ja = n - l + 1_ilp64
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
                 call stdlib_I64_dlarzt( 'BACKWARD', 'ROWWISE', l, ib, a( i, ja ), lda,tau( i ), work(&
                            iwt ), ldt )
                 if( left ) then
                    ! h or h**t is applied to c(i:m,1:n)
                    mi = m - i + 1_ilp64
                    ic = i
                 else
                    ! h or h**t is applied to c(1:m,i:n)
                    ni = n - i + 1_ilp64
                    jc = i
                 end if
                 ! apply h or h**t
                 call stdlib_I64_dlarzb( side, transt, 'BACKWARD', 'ROWWISE', mi, ni,ib, l, a( i, ja )&
                           , lda, work( iwt ), ldt,c( ic, jc ), ldc, work, ldwork )
              end do
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dormrz




     pure module subroutine stdlib_I64_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! CUNMR3 overwrites the general complex m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by CTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), tau(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp64) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           complex(sp) :: taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CUNMR3', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp64
              i2 = k
              i3 = 1_ilp64
           else
              i1 = k
              i2 = 1_ilp64
              i3 = -1_ilp64
           end if
           if( left ) then
              ni = n
              ja = m - l + 1_ilp64
              jc = 1_ilp64
           else
              mi = m
              ja = n - l + 1_ilp64
              ic = 1_ilp64
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp64
                 ic = i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp64
                 jc = i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              call stdlib_I64_clarz( side, mi, ni, l, a( i, ja ), lda, taui,c( ic, jc ), ldc, work )
                        
           end do
           return
     end subroutine stdlib_I64_cunmr3

     pure module subroutine stdlib_I64_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! ZUNMR3 overwrites the general complex m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**H* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**H if SIDE = 'R' and TRANS = 'C',
     !! where Q is a complex unitary matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by ZTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), tau(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp64) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           complex(dp) :: taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZUNMR3', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp64
              i2 = k
              i3 = 1_ilp64
           else
              i1 = k
              i2 = 1_ilp64
              i3 = -1_ilp64
           end if
           if( left ) then
              ni = n
              ja = m - l + 1_ilp64
              jc = 1_ilp64
           else
              mi = m
              ja = n - l + 1_ilp64
              ic = 1_ilp64
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**h is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp64
                 ic = i
              else
                 ! h(i) or h(i)**h is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp64
                 jc = i
              end if
              ! apply h(i) or h(i)**h
              if( notran ) then
                 taui = tau( i )
              else
                 taui = conjg( tau( i ) )
              end if
              call stdlib_I64_zlarz( side, mi, ni, l, a( i, ja ), lda, taui,c( ic, jc ), ldc, work )
                        
           end do
           return
     end subroutine stdlib_I64_zunmr3




     pure module subroutine stdlib_I64_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! SORMR3 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'C',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by STZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), tau(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp64) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SORMR3', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp64
              i2 = k
              i3 = 1_ilp64
           else
              i1 = k
              i2 = 1_ilp64
              i3 = -1_ilp64
           end if
           if( left ) then
              ni = n
              ja = m - l + 1_ilp64
              jc = 1_ilp64
           else
              mi = m
              ja = n - l + 1_ilp64
              ic = 1_ilp64
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**t is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp64
                 ic = i
              else
                 ! h(i) or h(i)**t is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp64
                 jc = i
              end if
              ! apply h(i) or h(i)**t
              call stdlib_I64_slarz( side, mi, ni, l, a( i, ja ), lda, tau( i ),c( ic, jc ), ldc, &
                        work )
           end do
           return
     end subroutine stdlib_I64_sormr3

     pure module subroutine stdlib_I64_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
     !! DORMR3 overwrites the general real m by n matrix C with
     !! Q * C  if SIDE = 'L' and TRANS = 'N', or
     !! Q**T* C  if SIDE = 'L' and TRANS = 'C', or
     !! C * Q  if SIDE = 'R' and TRANS = 'N', or
     !! C * Q**T if SIDE = 'R' and TRANS = 'C',
     !! where Q is a real orthogonal matrix defined as the product of k
     !! elementary reflectors
     !! Q = H(1) H(2) . . . H(k)
     !! as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
     !! if SIDE = 'R'.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), tau(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, notran
           integer(ilp64) :: i, i1, i2, i3, ic, ja, jc, mi, ni, nq
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -2_ilp64
           else if( m<0_ilp64 ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( k<0_ilp64 .or. k>nq ) then
              info = -5_ilp64
           else if( l<0_ilp64 .or. ( left .and. ( l>m ) ) .or.( .not.left .and. ( l>n ) ) ) then
              info = -6_ilp64
           else if( lda<max( 1_ilp64, k ) ) then
              info = -8_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DORMR3', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 .or. k==0 )return
           if( ( left .and. .not.notran .or. .not.left .and. notran ) ) then
              i1 = 1_ilp64
              i2 = k
              i3 = 1_ilp64
           else
              i1 = k
              i2 = 1_ilp64
              i3 = -1_ilp64
           end if
           if( left ) then
              ni = n
              ja = m - l + 1_ilp64
              jc = 1_ilp64
           else
              mi = m
              ja = n - l + 1_ilp64
              ic = 1_ilp64
           end if
           do i = i1, i2, i3
              if( left ) then
                 ! h(i) or h(i)**t is applied to c(i:m,1:n)
                 mi = m - i + 1_ilp64
                 ic = i
              else
                 ! h(i) or h(i)**t is applied to c(1:m,i:n)
                 ni = n - i + 1_ilp64
                 jc = i
              end if
              ! apply h(i) or h(i)**t
              call stdlib_I64_dlarz( side, mi, ni, l, a( i, ja ), lda, tau( i ),c( ic, jc ), ldc, &
                        work )
           end do
           return
     end subroutine stdlib_I64_dormr3




     pure module subroutine stdlib_I64_slarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! SLARZ applies a real elementary reflector H to a real M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! H is a product of k elementary reflectors as returned by STZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=zero ) then
                 ! w( 1:n ) = c( 1, 1:n )
                 call stdlib_I64_scopy( n, c, ldc, work, 1_ilp64 )
                 ! w( 1:n ) = w( 1:n ) + c( m-l+1:m, 1:n )**t * v( 1:l )
                 call stdlib_I64_sgemv( 'TRANSPOSE', l, n, one, c( m-l+1, 1_ilp64 ), ldc, v,incv, one, work,&
                            1_ilp64 )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_I64_saxpy( n, -tau, work, 1_ilp64, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**t
                 call stdlib_I64_sger( l, n, -tau, v, incv, work, 1_ilp64, c( m-l+1, 1_ilp64 ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=zero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_I64_scopy( m, c, 1_ilp64, work, 1_ilp64 )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_I64_sgemv( 'NO TRANSPOSE', m, l, one, c( 1_ilp64, n-l+1 ), ldc,v, incv, one, &
                           work, 1_ilp64 )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_I64_saxpy( m, -tau, work, 1_ilp64, c, 1_ilp64 )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**t
                 call stdlib_I64_sger( m, l, -tau, work, 1_ilp64, v, incv, c( 1_ilp64, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_slarz

     pure module subroutine stdlib_I64_dlarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! DLARZ applies a real elementary reflector H to a real M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! H is a product of k elementary reflectors as returned by DTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=zero ) then
                 ! w( 1:n ) = c( 1, 1:n )
                 call stdlib_I64_dcopy( n, c, ldc, work, 1_ilp64 )
                 ! w( 1:n ) = w( 1:n ) + c( m-l+1:m, 1:n )**t * v( 1:l )
                 call stdlib_I64_dgemv( 'TRANSPOSE', l, n, one, c( m-l+1, 1_ilp64 ), ldc, v,incv, one, work,&
                            1_ilp64 )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_I64_daxpy( n, -tau, work, 1_ilp64, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**t
                 call stdlib_I64_dger( l, n, -tau, v, incv, work, 1_ilp64, c( m-l+1, 1_ilp64 ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=zero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_I64_dcopy( m, c, 1_ilp64, work, 1_ilp64 )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_I64_dgemv( 'NO TRANSPOSE', m, l, one, c( 1_ilp64, n-l+1 ), ldc,v, incv, one, &
                           work, 1_ilp64 )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_I64_daxpy( m, -tau, work, 1_ilp64, c, 1_ilp64 )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**t
                 call stdlib_I64_dger( m, l, -tau, work, 1_ilp64, v, incv, c( 1_ilp64, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_dlarz


     pure module subroutine stdlib_I64_clarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! CLARZ applies a complex elementary reflector H to a complex
     !! M-by-N matrix C, from either the left or the right. H is represented
     !! in the form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
     !! H is a product of k elementary reflectors as returned by CTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=czero ) then
                 ! w( 1:n ) = conjg( c( 1, 1:n ) )
                 call stdlib_I64_ccopy( n, c, ldc, work, 1_ilp64 )
                 call stdlib_I64_clacgv( n, work, 1_ilp64 )
                 ! w( 1:n ) = conjg( w( 1:n ) + c( m-l+1:m, 1:n )**h * v( 1:l ) )
                 call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', l, n, cone, c( m-l+1, 1_ilp64 ),ldc, v, incv,&
                            cone, work, 1_ilp64 )
                 call stdlib_I64_clacgv( n, work, 1_ilp64 )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_I64_caxpy( n, -tau, work, 1_ilp64, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**h
                 call stdlib_I64_cgeru( l, n, -tau, v, incv, work, 1_ilp64, c( m-l+1, 1_ilp64 ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=czero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_I64_ccopy( m, c, 1_ilp64, work, 1_ilp64 )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_I64_cgemv( 'NO TRANSPOSE', m, l, cone, c( 1_ilp64, n-l+1 ), ldc,v, incv, cone, &
                           work, 1_ilp64 )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_I64_caxpy( m, -tau, work, 1_ilp64, c, 1_ilp64 )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**h
                 call stdlib_I64_cgerc( m, l, -tau, work, 1_ilp64, v, incv, c( 1_ilp64, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_clarz

     pure module subroutine stdlib_I64_zlarz( side, m, n, l, v, incv, tau, c, ldc, work )
     !! ZLARZ applies a complex elementary reflector H to a complex
     !! M-by-N matrix C, from either the left or the right. H is represented
     !! in the form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
     !! H is a product of k elementary reflectors as returned by ZTZRZF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Executable Statements 
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c
              if( tau/=czero ) then
                 ! w( 1:n ) = conjg( c( 1, 1:n ) )
                 call stdlib_I64_zcopy( n, c, ldc, work, 1_ilp64 )
                 call stdlib_I64_zlacgv( n, work, 1_ilp64 )
                 ! w( 1:n ) = conjg( w( 1:n ) + c( m-l+1:m, 1:n )**h * v( 1:l ) )
                 call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', l, n, cone, c( m-l+1, 1_ilp64 ),ldc, v, incv,&
                            cone, work, 1_ilp64 )
                 call stdlib_I64_zlacgv( n, work, 1_ilp64 )
                 ! c( 1, 1:n ) = c( 1, 1:n ) - tau * w( 1:n )
                 call stdlib_I64_zaxpy( n, -tau, work, 1_ilp64, c, ldc )
                 ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                     ! tau * v( 1:l ) * w( 1:n )**h
                 call stdlib_I64_zgeru( l, n, -tau, v, incv, work, 1_ilp64, c( m-l+1, 1_ilp64 ),ldc )
              end if
           else
              ! form  c * h
              if( tau/=czero ) then
                 ! w( 1:m ) = c( 1:m, 1 )
                 call stdlib_I64_zcopy( m, c, 1_ilp64, work, 1_ilp64 )
                 ! w( 1:m ) = w( 1:m ) + c( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                 call stdlib_I64_zgemv( 'NO TRANSPOSE', m, l, cone, c( 1_ilp64, n-l+1 ), ldc,v, incv, cone, &
                           work, 1_ilp64 )
                 ! c( 1:m, 1 ) = c( 1:m, 1 ) - tau * w( 1:m )
                 call stdlib_I64_zaxpy( m, -tau, work, 1_ilp64, c, 1_ilp64 )
                 ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                     ! tau * w( 1:m ) * v( 1:l )**h
                 call stdlib_I64_zgerc( m, l, -tau, work, 1_ilp64, v, incv, c( 1_ilp64, n-l+1 ),ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_zlarz




     pure module subroutine stdlib_I64_slarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! SLARZB applies a real block reflector H or its transpose H**T to
     !! a real distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**t * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**t
              do j = 1, k
                 call stdlib_I64_scopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**t * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, l, one,c( m-l+1, 1_ilp64 ), &
                        ldc, v, ldv, one, work, ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_I64_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**t
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**t * w( 1:n, 1:k )**t
              if( l>0_ilp64 )call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -one, v, ldv,work, &
                        ldwork, one, c( m-l+1, 1_ilp64 ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**t
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_I64_scopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, one,c( 1_ilp64, n-l+1 ),&
                         ldc, v, ldv, one, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * t  or  w( 1:m, 1:k ) * t**t
              call stdlib_I64_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * v( 1:k, 1:l )
              if( l>0_ilp64 )call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -one,work, &
                        ldwork, v, ldv, one, c( 1_ilp64, n-l+1 ), ldc )
           end if
           return
     end subroutine stdlib_I64_slarzb

     pure module subroutine stdlib_I64_dlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! DLARZB applies a real block reflector H or its transpose H**T to
     !! a real distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**t * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**t
              do j = 1, k
                 call stdlib_I64_dcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**t * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, l, one,c( m-l+1, 1_ilp64 ), &
                        ldc, v, ldv, one, work, ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**t
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**t * w( 1:n, 1:k )**t
              if( l>0_ilp64 )call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -one, v, ldv,work, &
                        ldwork, one, c( m-l+1, 1_ilp64 ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**t
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_I64_dcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, one,c( 1_ilp64, n-l+1 ),&
                         ldc, v, ldv, one, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * t  or  w( 1:m, 1:k ) * t**t
              call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, one, t,ldt, work, &
                        ldwork )
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * v( 1:k, 1:l )
              if( l>0_ilp64 )call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -one,work, &
                        ldwork, v, ldv, one, c( 1_ilp64, n-l+1 ), ldc )
           end if
           return
     end subroutine stdlib_I64_dlarzb


     pure module subroutine stdlib_I64_clarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! CLARZB applies a complex block reflector H or its transpose H**H
     !! to a complex distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**h * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**h
              do j = 1, k
                 call stdlib_I64_ccopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**h * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_cgemm( 'TRANSPOSE', 'CONJUGATE TRANSPOSE', n, k, l,cone, c( m-&
                        l+1, 1_ilp64 ), ldc, v, ldv, cone, work,ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, cone, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**h
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**h * w( 1:n, 1:k )**h
              if( l>0_ilp64 )call stdlib_I64_cgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -cone, v, ldv,work, &
                        ldwork, cone, c( m-l+1, 1_ilp64 ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**h
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_I64_ccopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**h
              if( l>0_ilp64 )call stdlib_I64_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, cone,c( 1_ilp64, n-l+1 )&
                        , ldc, v, ldv, cone, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * conjg( t )  or
                              ! w( 1:m, 1:k ) * t**h
              do j = 1, k
                 call stdlib_I64_clacgv( k-j+1, t( j, j ), 1_ilp64 )
              end do
              call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, cone, t,ldt, work, &
                        ldwork )
              do j = 1, k
                 call stdlib_I64_clacgv( k-j+1, t( j, j ), 1_ilp64 )
              end do
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * conjg( v( 1:k, 1:l ) )
              do j = 1, l
                 call stdlib_I64_clacgv( k, v( 1_ilp64, j ), 1_ilp64 )
              end do
              if( l>0_ilp64 )call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -cone,work, &
                        ldwork, v, ldv, cone, c( 1_ilp64, n-l+1 ), ldc )
              do j = 1, l
                 call stdlib_I64_clacgv( k, v( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           return
     end subroutine stdlib_I64_clarzb

     pure module subroutine stdlib_I64_zlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
     !! ZLARZB applies a complex block reflector H or its transpose H**H
     !! to a complex distributed M-by-N  C from the left or the right.
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
               ldc, work, ldwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -3_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLARZB', -info )
              return
           end if
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c  or  h**h * c
              ! w( 1:n, 1:k ) = c( 1:k, 1:n )**h
              do j = 1, k
                 call stdlib_I64_zcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) + ...
                              ! c( m-l+1:m, 1:n )**h * v( 1:k, 1:l )**t
              if( l>0_ilp64 )call stdlib_I64_zgemm( 'TRANSPOSE', 'CONJUGATE TRANSPOSE', n, k, l,cone, c( m-&
                        l+1, 1_ilp64 ), ldc, v, ldv, cone, work,ldwork )
              ! w( 1:n, 1:k ) = w( 1:n, 1:k ) * t**t  or  w( 1:m, 1:k ) * t
              call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k, cone, t,ldt, work, &
                        ldwork )
              ! c( 1:k, 1:n ) = c( 1:k, 1:n ) - w( 1:n, 1:k )**h
              do j = 1, n
                 do i = 1, k
                    c( i, j ) = c( i, j ) - work( j, i )
                 end do
              end do
              ! c( m-l+1:m, 1:n ) = c( m-l+1:m, 1:n ) - ...
                                  ! v( 1:k, 1:l )**h * w( 1:n, 1:k )**h
              if( l>0_ilp64 )call stdlib_I64_zgemm( 'TRANSPOSE', 'TRANSPOSE', l, n, k, -cone, v, ldv,work, &
                        ldwork, cone, c( m-l+1, 1_ilp64 ), ldc )
           else if( stdlib_lsame( side, 'R' ) ) then
              ! form  c * h  or  c * h**h
              ! w( 1:m, 1:k ) = c( 1:m, 1:k )
              do j = 1, k
                 call stdlib_I64_zcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
              end do
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) + ...
                              ! c( 1:m, n-l+1:n ) * v( 1:k, 1:l )**h
              if( l>0_ilp64 )call stdlib_I64_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, l, cone,c( 1_ilp64, n-l+1 )&
                        , ldc, v, ldv, cone, work, ldwork )
              ! w( 1:m, 1:k ) = w( 1:m, 1:k ) * conjg( t )  or
                              ! w( 1:m, 1:k ) * t**h
              do j = 1, k
                 call stdlib_I64_zlacgv( k-j+1, t( j, j ), 1_ilp64 )
              end do
              call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k, cone, t,ldt, work, &
                        ldwork )
              do j = 1, k
                 call stdlib_I64_zlacgv( k-j+1, t( j, j ), 1_ilp64 )
              end do
              ! c( 1:m, 1:k ) = c( 1:m, 1:k ) - w( 1:m, 1:k )
              do j = 1, k
                 do i = 1, m
                    c( i, j ) = c( i, j ) - work( i, j )
                 end do
              end do
              ! c( 1:m, n-l+1:n ) = c( 1:m, n-l+1:n ) - ...
                                  ! w( 1:m, 1:k ) * conjg( v( 1:k, 1:l ) )
              do j = 1, l
                 call stdlib_I64_zlacgv( k, v( 1_ilp64, j ), 1_ilp64 )
              end do
              if( l>0_ilp64 )call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, l, k, -cone,work, &
                        ldwork, v, ldv, cone, c( 1_ilp64, n-l+1 ), ldc )
              do j = 1, l
                 call stdlib_I64_zlacgv( k, v( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           return
     end subroutine stdlib_I64_zlarzb




     pure module subroutine stdlib_I64_slarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! SLARZT forms the triangular factor T of a real block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==zero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = zero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**t
                    call stdlib_I64_sgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i, &
                              1_ilp64 ), ldv, zero,t( i+1, i ), 1_ilp64 )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_I64_strmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp64 )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_I64_slarzt

     pure module subroutine stdlib_I64_dlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! DLARZT forms the triangular factor T of a real block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==zero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = zero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**t
                    call stdlib_I64_dgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i, &
                              1_ilp64 ), ldv, zero,t( i+1, i ), 1_ilp64 )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_I64_dtrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp64 )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_I64_dlarzt


     pure module subroutine stdlib_I64_clarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! CLARZT forms the triangular factor T of a complex block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==czero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = czero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**h
                    call stdlib_I64_clacgv( n, v( i, 1_ilp64 ), ldv )
                    call stdlib_I64_cgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i, &
                              1_ilp64 ), ldv, czero,t( i+1, i ), 1_ilp64 )
                    call stdlib_I64_clacgv( n, v( i, 1_ilp64 ), ldv )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_I64_ctrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp64 )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_I64_clarzt

     pure module subroutine stdlib_I64_zlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! ZLARZT forms the triangular factor T of a complex block reflector
     !! H of order > n, which is defined as a product of k elementary
     !! reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
     !! Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, j
           ! Executable Statements 
           ! check for currently supported options
           info = 0_ilp64
           if( .not.stdlib_lsame( direct, 'B' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( storev, 'R' ) ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLARZT', -info )
              return
           end if
           do i = k, 1, -1
              if( tau( i )==czero ) then
                 ! h(i)  =  i
                 do j = i, k
                    t( j, i ) = czero
                 end do
              else
                 ! general case
                 if( i<k ) then
                    ! t(i+1:k,i) = - tau(i) * v(i+1:k,1:n) * v(i,1:n)**h
                    call stdlib_I64_zlacgv( n, v( i, 1_ilp64 ), ldv )
                    call stdlib_I64_zgemv( 'NO TRANSPOSE', k-i, n, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i, &
                              1_ilp64 ), ldv, czero,t( i+1, i ), 1_ilp64 )
                    call stdlib_I64_zlacgv( n, v( i, 1_ilp64 ), ldv )
                    ! t(i+1:k,i) = t(i+1:k,i+1:k) * t(i+1:k,i)
                    call stdlib_I64_ztrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                              ldt, t( i+1, i ), 1_ilp64 )
                 end if
                 t( i, i ) = tau( i )
              end if
           end do
           return
     end subroutine stdlib_I64_zlarzt




     pure module subroutine stdlib_I64_slatrz( m, n, l, a, lda, tau, work )
     !! SLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
     !! of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: l, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! test the input arguments
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_I64_slarfg( l+1, a( i, i ), a( i, n-l+1 ), lda, tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_I64_slarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,tau( i ), a( 1_ilp64, i ), &
                        lda, work )
           end do
           return
     end subroutine stdlib_I64_slatrz

     pure module subroutine stdlib_I64_dlatrz( m, n, l, a, lda, tau, work )
     !! DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
     !! of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: l, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! test the input arguments
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = zero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_I64_dlarfg( l+1, a( i, i ), a( i, n-l+1 ), lda, tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_I64_dlarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,tau( i ), a( 1_ilp64, i ), &
                        lda, work )
           end do
           return
     end subroutine stdlib_I64_dlatrz


     pure module subroutine stdlib_I64_clatrz( m, n, l, a, lda, tau, work )
     !! CLATRZ factors the M-by-(M+L) complex upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z by means
     !! of unitary transformations, where  Z is an (M+L)-by-(M+L) unitary
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: l, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_I64_clacgv( l, a( i, n-l+1 ), lda )
              alpha = conjg( a( i, i ) )
              call stdlib_I64_clarfg( l+1, alpha, a( i, n-l+1 ), lda, tau( i ) )
              tau( i ) = conjg( tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_I64_clarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,conjg( tau( i ) ), a( &
                        1_ilp64, i ), lda, work )
              a( i, i ) = conjg( alpha )
           end do
           return
     end subroutine stdlib_I64_clatrz

     pure module subroutine stdlib_I64_zlatrz( m, n, l, a, lda, tau, work )
     !! ZLATRZ factors the M-by-(M+L) complex upper trapezoidal matrix
     !! [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z by means
     !! of unitary transformations, where  Z is an (M+L)-by-(M+L) unitary
     !! matrix and, R and A1 are M-by-M upper triangular matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: l, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m==0_ilp64 ) then
              return
           else if( m==n ) then
              do i = 1, n
                 tau( i ) = czero
              end do
              return
           end if
           do i = m, 1, -1
              ! generate elementary reflector h(i) to annihilate
              ! [ a(i,i) a(i,n-l+1:n) ]
              call stdlib_I64_zlacgv( l, a( i, n-l+1 ), lda )
              alpha = conjg( a( i, i ) )
              call stdlib_I64_zlarfg( l+1, alpha, a( i, n-l+1 ), lda, tau( i ) )
              tau( i ) = conjg( tau( i ) )
              ! apply h(i) to a(1:i-1,i:n) from the right
              call stdlib_I64_zlarz( 'RIGHT', i-1, n-i+1, l, a( i, n-l+1 ), lda,conjg( tau( i ) ), a( &
                        1_ilp64, i ), lda, work )
              a( i, i ) = conjg( alpha )
           end do
           return
     end subroutine stdlib_I64_zlatrz



end submodule stdlib_lapack_orthogonal_factors_rz
